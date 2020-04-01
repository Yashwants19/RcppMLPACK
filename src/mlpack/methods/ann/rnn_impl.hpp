/**
 * @file rnn_impl.hpp
 * @author Marcus Edel
 *
 * Definition of the RNN class, which implements recurrent neural networks.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_RNN_IMPL_HPP
#define MLPACK_METHODS_ANN_RNN_IMPL_HPP

// In case it hasn't been included yet.
#include "rnn.hpp"

#include "visitor/load_output_parameter_visitor.hpp"
#include "visitor/save_output_parameter_visitor.hpp"
#include "visitor/forward_visitor.hpp"
#include "visitor/backward_visitor.hpp"
#include "visitor/reset_cell_visitor.hpp"
#include "visitor/deterministic_set_visitor.hpp"
#include "visitor/gradient_set_visitor.hpp"
#include "visitor/gradient_visitor.hpp"
#include "visitor/weight_set_visitor.hpp"

#include <boost/serialization/variant.hpp>

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::RNN(
    const size_t rho,
    const bool single,
    OutputLayerType outputLayer,
    InitializationRuleType initializeRule) :
    rho(rho),
    outputLayer(std::move(outputLayer)),
    initializeRule(std::move(initializeRule)),
    inputSize(0),
    outputSize(0),
    targetSize(0),
    reset(false),
    single(single),
    numFunctions(0),
    deterministic(true)
{
  /* Nothing to do here */
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::~RNN()
{
  for (LayerTypes<CustomLayers...>& layer : network)
  {
    boost::apply_visitor(deleteVisitor, layer);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename OptimizerType, typename... CallbackTypes>
double RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Train(
    arma::cube predictors,
    arma::cube responses,
    OptimizerType& optimizer,
    CallbackTypes&&... callbacks)
{
  numFunctions = responses.n_cols;

  this->predictors = std::move(predictors);
  this->responses = std::move(responses);

  this->deterministic = true;
  ResetDeterministic();

  if (!reset)
  {
    ResetParameters();
  }

  // Train the model.
  Timer::Start("rnn_optimization");
  const double out = optimizer.Optimize(*this, parameter, callbacks...);
  Timer::Stop("rnn_optimization");

  Log::Info << "RNN::RNN(): final objective of trained model is " << out
      << "." << std::endl;
  return out;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetCells()
{
  for (size_t i = 1; i < network.size(); ++i)
  {
    boost::apply_visitor(ResetCellVisitor(rho), network[i]);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename OptimizerType, typename... CallbackTypes>
double RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Train(
    arma::cube predictors,
    arma::cube responses,
    CallbackTypes&&... callbacks)
{
  numFunctions = responses.n_cols;

  this->predictors = std::move(predictors);
  this->responses = std::move(responses);

  this->deterministic = true;
  ResetDeterministic();

  if (!reset)
  {
    ResetParameters();
  }

  OptimizerType optimizer;

  // Train the model.
  Timer::Start("rnn_optimization");
  const double out = optimizer.Optimize(*this, parameter, callbacks...);
  Timer::Stop("rnn_optimization");

  Log::Info << "RNN::RNN(): final objective of trained model is " << out
      << "." << std::endl;
  return out;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Predict(
    arma::cube predictors, arma::cube& results, const size_t batchSize)
{
  ResetCells();

  if (parameter.is_empty())
  {
    ResetParameters();
  }

  if (!deterministic)
  {
    deterministic = true;
    ResetDeterministic();
  }

  const size_t effectiveBatchSize = std::min(batchSize,
      size_t(predictors.n_cols));

  Forward(std::move(arma::mat(predictors.slice(0).colptr(0),
      predictors.n_rows, effectiveBatchSize, false, true)));
  arma::mat resultsTemp = boost::apply_visitor(outputParameterVisitor,
      network.back());

  outputSize = resultsTemp.n_rows;
  results = arma::zeros<arma::cube>(outputSize, predictors.n_cols, rho);
  results.slice(0).submat(0, 0, results.n_rows - 1,
      effectiveBatchSize - 1) = resultsTemp;

  // Process in accordance with the given batch size.
  for (size_t begin = 0; begin < predictors.n_cols; begin += batchSize)
  {
    const size_t effectiveBatchSize = std::min(batchSize,
        size_t(predictors.n_cols - begin));
    for (size_t seqNum = !begin; seqNum < rho; ++seqNum)
    {
      Forward(std::move(arma::mat(predictors.slice(seqNum).colptr(begin),
          predictors.n_rows, effectiveBatchSize, false, true)));

      results.slice(seqNum).submat(0, begin, results.n_rows - 1, begin +
          effectiveBatchSize - 1) = boost::apply_visitor(outputParameterVisitor,
          network.back());
    }
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Evaluate(
    const arma::mat& /* parameters */,
    const size_t begin,
    const size_t batchSize,
    const bool deterministic)
{
  if (parameter.is_empty())
  {
    ResetParameters();
  }

  if (deterministic != this->deterministic)
  {
    this->deterministic = deterministic;
    ResetDeterministic();
  }

  if (!inputSize)
  {
    inputSize = predictors.n_rows;
    targetSize = responses.n_rows;
  }
  else if (targetSize == 0)
  {
    targetSize = responses.n_rows;
  }

  ResetCells();

  double performance = 0;
  size_t responseSeq = 0;

  for (size_t seqNum = 0; seqNum < rho; ++seqNum)
  {
    // Wrap a matrix around our data to avoid a copy.
    arma::mat stepData(predictors.slice(seqNum).colptr(begin),
        predictors.n_rows, batchSize, false, true);
    Forward(std::move(stepData));
    if (!single)
    {
      responseSeq = seqNum;
    }

    performance += outputLayer.Forward(std::move(boost::apply_visitor(
        outputParameterVisitor, network.back())),
        std::move(arma::mat(responses.slice(responseSeq).colptr(begin),
            responses.n_rows, batchSize, false, true)));
  }

  if (outputSize == 0)
  {
    outputSize = boost::apply_visitor(outputParameterVisitor,
        network.back()).n_elem / batchSize;
  }

  return performance;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Evaluate(
    const arma::mat& parameters,
    const size_t begin,
    const size_t batchSize)
{
  return Evaluate(parameters, begin, batchSize, true);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename GradType>
double RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::
EvaluateWithGradient(const arma::mat& /* parameters */,
                     const size_t begin,
                     GradType& gradient,
                     const size_t batchSize)
{
  // Initialize passed gradient.
  if (gradient.is_empty())
  {
    if (parameter.is_empty())
    {
      ResetParameters();
    }

    gradient = arma::zeros<arma::mat>(parameter.n_rows, parameter.n_cols);
  }
  else
  {
    gradient.zeros();
  }

  if (this->deterministic)
  {
    this->deterministic = false;
    ResetDeterministic();
  }

  if (!inputSize)
  {
    inputSize = predictors.n_rows;
    targetSize = responses.n_rows;
  }
  else if (targetSize == 0)
  {
    targetSize = responses.n_rows;
  }

  ResetCells();

  double performance = 0;
  size_t responseSeq = 0;
  const size_t effectiveRho = std::min(rho, size_t(responses.size()));

  for (size_t seqNum = 0; seqNum < effectiveRho; ++seqNum)
  {
    // Wrap a matrix around our data to avoid a copy.
    arma::mat stepData(predictors.slice(seqNum).colptr(begin),
        predictors.n_rows, batchSize, false, true);
    Forward(std::move(stepData));
    if (!single)
    {
      responseSeq = seqNum;
    }

    for (size_t l = 0; l < network.size(); ++l)
    {
      boost::apply_visitor(SaveOutputParameterVisitor(
          std::move(moduleOutputParameter)), network[l]);
    }

    performance += outputLayer.Forward(std::move(boost::apply_visitor(
        outputParameterVisitor, network.back())),
        std::move(arma::mat(responses.slice(responseSeq).colptr(begin),
            responses.n_rows, batchSize, false, true)));
  }

  if (outputSize == 0)
  {
    outputSize = boost::apply_visitor(outputParameterVisitor,
        network.back()).n_elem / batchSize;
  }

  // Initialize current/working gradient.
  if (currentGradient.is_empty())
  {
    currentGradient = arma::zeros<arma::mat>(parameter.n_rows,
        parameter.n_cols);
  }

  ResetGradients(currentGradient);

  for (size_t seqNum = 0; seqNum < effectiveRho; ++seqNum)
  {
    currentGradient.zeros();
    for (size_t l = 0; l < network.size(); ++l)
    {
      boost::apply_visitor(LoadOutputParameterVisitor(
          std::move(moduleOutputParameter)), network[network.size() - 1 - l]);
    }

    if (single && seqNum > 0)
    {
      error.zeros();
    }
    else if (single && seqNum == 0)
    {
      outputLayer.Backward(std::move(boost::apply_visitor(
          outputParameterVisitor, network.back())),
          std::move(arma::mat(responses.slice(0).colptr(begin),
          responses.n_rows, batchSize, false, true)), std::move(error));
    }
    else
    {
      outputLayer.Backward(std::move(boost::apply_visitor(
          outputParameterVisitor, network.back())),
          std::move(arma::mat(
          responses.slice(effectiveRho - seqNum - 1).colptr(begin),
          responses.n_rows, batchSize, false, true)), std::move(error));
    }

    Backward();
    Gradient(std::move(
        arma::mat(predictors.slice(effectiveRho - seqNum - 1).colptr(begin),
        predictors.n_rows, batchSize, false, true)));
    gradient += currentGradient;
  }

  return performance;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Gradient(
    const arma::mat& parameters,
    const size_t begin,
    arma::mat& gradient,
    const size_t batchSize)
{
  this->EvaluateWithGradient(parameters, begin, gradient, batchSize);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Shuffle()
{
  arma::cube newPredictors, newResponses;
  math::ShuffleData(predictors, responses, newPredictors, newResponses);

  predictors = std::move(newPredictors);
  responses = std::move(newResponses);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetParameters()
{
  ResetDeterministic();

  // Reset the network parameter with the given initialization rule.
  NetworkInitialization<InitializationRuleType,
                        CustomLayers...> networkInit(initializeRule);
  networkInit.Initialize(network, parameter);

  reset = true;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Reset()
{
  ResetParameters();
  ResetCells();
  currentGradient.zeros();
  ResetGradients(currentGradient);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetDeterministic()
{
  DeterministicSetVisitor deterministicSetVisitor(deterministic);
  std::for_each(network.begin(), network.end(),
      boost::apply_visitor(deterministicSetVisitor));
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetGradients(
    arma::mat& gradient)
{
  size_t offset = 0;
  for (LayerTypes<CustomLayers...>& layer : network)
  {
    offset += boost::apply_visitor(GradientSetVisitor(std::move(gradient),
        offset), layer);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::Forward(arma::mat&& input)
{
  boost::apply_visitor(ForwardVisitor(std::move(input), std::move(
      boost::apply_visitor(outputParameterVisitor, network.front()))),
      network.front());

  for (size_t i = 1; i < network.size(); ++i)
  {
    boost::apply_visitor(ForwardVisitor(
        std::move(boost::apply_visitor(outputParameterVisitor, network[i - 1])),
        std::move(boost::apply_visitor(outputParameterVisitor, network[i]))),
        network[i]);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::Backward()
{
  boost::apply_visitor(BackwardVisitor(
        std::move(boost::apply_visitor(outputParameterVisitor, network.back())),
        std::move(error), std::move(boost::apply_visitor(deltaVisitor,
        network.back()))), network.back());

  for (size_t i = 2; i < network.size(); ++i)
  {
    boost::apply_visitor(BackwardVisitor(
        std::move(boost::apply_visitor(outputParameterVisitor,
        network[network.size() - i])), std::move(boost::apply_visitor(
        deltaVisitor, network[network.size() - i + 1])), std::move(
        boost::apply_visitor(deltaVisitor, network[network.size() - i]))),
        network[network.size() - i]);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename InputType>
void RNN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::Gradient(InputType&& input)
{
  boost::apply_visitor(GradientVisitor(std::move(input), std::move(
      boost::apply_visitor(deltaVisitor, network[1]))), network.front());

  for (size_t i = 1; i < network.size() - 1; ++i)
  {
    boost::apply_visitor(GradientVisitor(
        std::move(boost::apply_visitor(outputParameterVisitor, network[i - 1])),
        std::move(boost::apply_visitor(deltaVisitor, network[i + 1]))),
        network[i]);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename Archive>
void RNN<OutputLayerType, InitializationRuleType, CustomLayers...>::serialize(
    Archive& ar, const unsigned int version)
{
  ar & BOOST_SERIALIZATION_NVP(parameter);
  ar & BOOST_SERIALIZATION_NVP(rho);
  ar & BOOST_SERIALIZATION_NVP(single);
  ar & BOOST_SERIALIZATION_NVP(inputSize);
  ar & BOOST_SERIALIZATION_NVP(outputSize);
  ar & BOOST_SERIALIZATION_NVP(targetSize);

  // Earlier versions of the RNN code did not serialize the 'reset' variable.
  if (version > 0)
  {
    ar & BOOST_SERIALIZATION_NVP(reset);
  }

  if (Archive::is_loading::value)
  {
    std::for_each(network.begin(), network.end(),
        boost::apply_visitor(deleteVisitor));
    network.clear();
  }

  ar & BOOST_SERIALIZATION_NVP(network);

  // If we are loading, we need to initialize the weights.
  if (Archive::is_loading::value)
  {
    // Earlier versions of the RNN code assumed that the weights needed to be
    // reset on load.
    if (version == 0)
      reset = false;

    size_t offset = 0;
    for (LayerTypes<CustomLayers...>& layer : network)
    {
      offset += boost::apply_visitor(WeightSetVisitor(std::move(parameter),
          offset), layer);

      boost::apply_visitor(resetVisitor, layer);
    }

    deterministic = true;
    ResetDeterministic();
  }
}

} // namespace ann
} // namespace mlpack

#endif
