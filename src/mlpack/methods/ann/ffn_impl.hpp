/**
 * @file ffn_impl.hpp
 * @author Marcus Edel
 *
 * Definition of the FFN class, which implements feed forward neural networks.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_FFN_IMPL_HPP
#define MLPACK_METHODS_ANN_FFN_IMPL_HPP

// In case it hasn't been included yet.
#include "ffn.hpp"

#include "visitor/forward_visitor.hpp"
#include "visitor/backward_visitor.hpp"
#include "visitor/deterministic_set_visitor.hpp"
#include "visitor/gradient_set_visitor.hpp"
#include "visitor/gradient_visitor.hpp"
#include "visitor/set_input_height_visitor.hpp"
#include "visitor/set_input_width_visitor.hpp"

#include <boost/serialization/variant.hpp>

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {


template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::FFN(
    OutputLayerType outputLayer, InitializationRuleType initializeRule) :
    outputLayer(std::move(outputLayer)),
    initializeRule(std::move(initializeRule)),
    width(0),
    height(0),
    reset(false),
    numFunctions(0),
    deterministic(true)
{
  /* Nothing to do here */
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::~FFN()
{
  std::for_each(network.begin(), network.end(),
      boost::apply_visitor(deleteVisitor));
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::ResetData(
    arma::mat predictors, arma::mat responses)
{
  numFunctions = responses.n_cols;
  this->predictors = std::move(predictors);
  this->responses = std::move(responses);
  this->deterministic = true;
  ResetDeterministic();

  if (!reset)
    ResetParameters();
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename OptimizerType, typename... CallbackTypes>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Train(
      arma::mat predictors,
      arma::mat responses,
      OptimizerType& optimizer,
      CallbackTypes&&... callbacks)
{
  ResetData(std::move(predictors), std::move(responses));

  // Train the model.
  Timer::Start("ffn_optimization");
  const double out = optimizer.Optimize(*this, parameter, callbacks...);
  Timer::Stop("ffn_optimization");

  Log::Info << "FFN::FFN(): final objective of trained model is " << out
      << "." << std::endl;
  return out;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename OptimizerType, typename... CallbackTypes>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Train(
    arma::mat predictors,
    arma::mat responses,
    CallbackTypes&&... callbacks)
{
  ResetData(std::move(predictors), std::move(responses));

  OptimizerType optimizer;

  // Train the model.
  Timer::Start("ffn_optimization");
  const double out = optimizer.Optimize(*this, parameter, callbacks...);
  Timer::Stop("ffn_optimization");

  Log::Info << "FFN::FFN(): final objective of trained model is " << out
      << "." << std::endl;
  return out;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Forward(
    arma::mat inputs, arma::mat& results)
{
  if (parameter.is_empty())
    ResetParameters();

  if (!deterministic)
  {
    deterministic = true;
    ResetDeterministic();
  }

  currentInput = std::move(inputs);
  Forward(std::move(currentInput));
  results = boost::apply_visitor(outputParameterVisitor, network.back());
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Forward(
    arma::mat inputs, arma::mat& results, const size_t begin, const size_t end)
{
  boost::apply_visitor(ForwardVisitor(std::move(inputs), std::move(
      boost::apply_visitor(outputParameterVisitor, network[begin]))),
      network[begin]);

  for (size_t i = 1; i < end - begin + 1; ++i)
  {
    boost::apply_visitor(ForwardVisitor(std::move(boost::apply_visitor(
        outputParameterVisitor, network[begin + i - 1])), std::move(
        boost::apply_visitor(outputParameterVisitor, network[begin + i]))),
        network[begin + i]);
  }

  results = boost::apply_visitor(outputParameterVisitor, network[end]);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Backward(
    arma::mat targets, arma::mat& gradients)
{
  double res = outputLayer.Forward(std::move(boost::apply_visitor(
      outputParameterVisitor, network.back())), std::move(targets));

  for (size_t i = 0; i < network.size(); ++i)
  {
    res += boost::apply_visitor(lossVisitor, network[i]);
  }

  outputLayer.Backward(std::move(boost::apply_visitor(outputParameterVisitor,
      network.back())), std::move(targets), std::move(error));

  gradients = arma::zeros<arma::mat>(parameter.n_rows, parameter.n_cols);

  Backward();
  ResetGradients(gradients);
  Gradient(std::move(currentInput));

  return res;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Predict(
    arma::mat predictors, arma::mat& results)
{
  if (parameter.is_empty())
    ResetParameters();

  if (!deterministic)
  {
    deterministic = true;
    ResetDeterministic();
  }

  arma::mat resultsTemp;
  Forward(std::move(arma::mat(predictors.colptr(0),
      predictors.n_rows, 1, false, true)));
  resultsTemp = boost::apply_visitor(outputParameterVisitor,
      network.back()).col(0);

  results = arma::mat(resultsTemp.n_elem, predictors.n_cols);
  results.col(0) = resultsTemp.col(0);

  for (size_t i = 1; i < predictors.n_cols; i++)
  {
    Forward(std::move(arma::mat(predictors.colptr(i),
        predictors.n_rows, 1, false, true)));

    resultsTemp = boost::apply_visitor(outputParameterVisitor,
        network.back());
    results.col(i) = resultsTemp.col(0);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Evaluate(
    arma::mat predictors, arma::mat responses)
{
  if (parameter.is_empty())
    ResetParameters();

  if (!deterministic)
  {
    deterministic = true;
    ResetDeterministic();
  }

  Forward(std::move(predictors));

  double res = outputLayer.Forward(std::move(boost::apply_visitor(
      outputParameterVisitor, network.back())), std::move(responses));

  for (size_t i = 0; i < network.size(); ++i)
  {
    res += boost::apply_visitor(lossVisitor, network[i]);
  }

  return res;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Evaluate(
    const arma::mat& parameters)
{
  double res = 0;
  for (size_t i = 0; i < predictors.n_cols; ++i)
    res += Evaluate(parameters, i, 1, true);

  return res;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Evaluate(
    const arma::mat& /* parameters */,
    const size_t begin,
    const size_t batchSize,
    const bool deterministic)
{
  if (parameter.is_empty())
    ResetParameters();

  if (deterministic != this->deterministic)
  {
    this->deterministic = deterministic;
    ResetDeterministic();
  }

  Forward(std::move(predictors.cols(begin, begin + batchSize - 1)));
  double res = outputLayer.Forward(
      std::move(boost::apply_visitor(outputParameterVisitor, network.back())),
      std::move(responses.cols(begin, begin + batchSize - 1)));

  for (size_t i = 0; i < network.size(); ++i)
  {
    res += boost::apply_visitor(lossVisitor, network[i]);
  }

  return res;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Evaluate(
    const arma::mat& parameters, const size_t begin, const size_t batchSize)
{
  return Evaluate(parameters, begin, batchSize, true);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename GradType>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::
EvaluateWithGradient(const arma::mat& parameters, GradType& gradient)
{
  double res = 0;
  for (size_t i = 0; i < predictors.n_cols; ++i)
    res += EvaluateWithGradient(parameters, i, gradient, 1);

  return res;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename GradType>
double FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::
EvaluateWithGradient(const arma::mat& /* parameters */,
                     const size_t begin,
                     GradType& gradient,
                     const size_t batchSize)
{
  if (gradient.is_empty())
  {
    if (parameter.is_empty())
      ResetParameters();

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

  Forward(std::move(predictors.cols(begin, begin + batchSize - 1)));
  double res = outputLayer.Forward(
      std::move(boost::apply_visitor(outputParameterVisitor, network.back())),
      std::move(responses.cols(begin, begin + batchSize - 1)));

  for (size_t i = 0; i < network.size(); ++i)
  {
    res += boost::apply_visitor(lossVisitor, network[i]);
  }

  outputLayer.Backward(
      std::move(boost::apply_visitor(outputParameterVisitor, network.back())),
      std::move(responses.cols(begin, begin + batchSize - 1)),
      std::move(error));

  Backward();
  ResetGradients(gradient);
  Gradient(std::move(predictors.cols(begin, begin + batchSize - 1)));

  return res;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Gradient(
    const arma::mat& parameters,
    const size_t begin,
    arma::mat& gradient,
    const size_t batchSize)
{
  this->EvaluateWithGradient(parameters, begin, gradient, batchSize);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Shuffle()
{
  math::ShuffleData(predictors, responses, predictors, responses);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetParameters()
{
  ResetDeterministic();

  // Reset the network parameter with the given initialization rule.
  NetworkInitialization<InitializationRuleType,
                        CustomLayers...> networkInit(initializeRule);
  networkInit.Initialize(network, parameter);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetDeterministic()
{
  DeterministicSetVisitor deterministicSetVisitor(deterministic);
  std::for_each(network.begin(), network.end(),
      boost::apply_visitor(deterministicSetVisitor));
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::ResetGradients(arma::mat& gradient)
{
  size_t offset = 0;
  for (size_t i = 0; i < network.size(); ++i)
  {
    offset += boost::apply_visitor(GradientSetVisitor(std::move(gradient),
        offset), network[i]);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::Forward(arma::mat&& input)
{
  boost::apply_visitor(ForwardVisitor(std::move(input), std::move(
      boost::apply_visitor(outputParameterVisitor, network.front()))),
      network.front());

  if (!reset)
  {
    if (boost::apply_visitor(outputWidthVisitor, network.front()) != 0)
    {
      width = boost::apply_visitor(outputWidthVisitor, network.front());
    }

    if (boost::apply_visitor(outputHeightVisitor, network.front()) != 0)
    {
      height = boost::apply_visitor(outputHeightVisitor, network.front());
    }
  }

  for (size_t i = 1; i < network.size(); ++i)
  {
    if (!reset)
    {
      // Set the input width.
      boost::apply_visitor(SetInputWidthVisitor(width), network[i]);

      // Set the input height.
      boost::apply_visitor(SetInputHeightVisitor(height), network[i]);
    }

    boost::apply_visitor(ForwardVisitor(std::move(boost::apply_visitor(
        outputParameterVisitor, network[i - 1])), std::move(
        boost::apply_visitor(outputParameterVisitor, network[i]))), network[i]);

    if (!reset)
    {
      // Get the output width.
      if (boost::apply_visitor(outputWidthVisitor, network[i]) != 0)
      {
        width = boost::apply_visitor(outputWidthVisitor, network[i]);
      }

      // Get the output height.
      if (boost::apply_visitor(outputHeightVisitor, network[i]) != 0)
      {
        height = boost::apply_visitor(outputHeightVisitor, network[i]);
      }
    }
  }

  if (!reset)
    reset = true;
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::Backward()
{
  boost::apply_visitor(BackwardVisitor(std::move(boost::apply_visitor(
      outputParameterVisitor, network.back())), std::move(error), std::move(
      boost::apply_visitor(deltaVisitor, network.back()))), network.back());

  for (size_t i = 2; i < network.size(); ++i)
  {
    boost::apply_visitor(BackwardVisitor(std::move(boost::apply_visitor(
        outputParameterVisitor, network[network.size() - i])), std::move(
        boost::apply_visitor(deltaVisitor, network[network.size() - i + 1])),
        std::move(boost::apply_visitor(deltaVisitor,
        network[network.size() - i]))), network[network.size() - i]);
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::Gradient(arma::mat&& input)
{
  boost::apply_visitor(GradientVisitor(std::move(input), std::move(
      boost::apply_visitor(deltaVisitor, network[1]))), network.front());

  for (size_t i = 1; i < network.size() - 1; ++i)
  {
    boost::apply_visitor(GradientVisitor(std::move(boost::apply_visitor(
        outputParameterVisitor, network[i - 1])), std::move(
        boost::apply_visitor(deltaVisitor, network[i + 1]))), network[i]);
  }

  boost::apply_visitor(GradientVisitor(std::move(boost::apply_visitor(
      outputParameterVisitor, network[network.size() - 2])), std::move(error)),
      network[network.size() - 1]);
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
template<typename Archive>
void FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::serialize(
    Archive& ar, const unsigned int version)
{
  ar & BOOST_SERIALIZATION_NVP(parameter);
  ar & BOOST_SERIALIZATION_NVP(width);
  ar & BOOST_SERIALIZATION_NVP(height);
  ar & BOOST_SERIALIZATION_NVP(currentInput);

  // Earlier versions of the FFN code did not serialize whether or not the model
  // was reset.
  if (version > 0)
  {
    ar & BOOST_SERIALIZATION_NVP(reset);
  }

  // Be sure to clear other layers before loading.
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
    // The behavior in earlier versions was to always assume the weights needed
    // to be reset.
    if (version == 0)
      reset = false;

    size_t offset = 0;
    for (size_t i = 0; i < network.size(); ++i)
    {
      offset += boost::apply_visitor(WeightSetVisitor(std::move(parameter),
          offset), network[i]);

      boost::apply_visitor(resetVisitor, network[i]);
    }

    deterministic = true;
    ResetDeterministic();
  }
}

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
void FFN<OutputLayerType, InitializationRuleType,
         CustomLayers...>::Swap(FFN& network)
{
  std::swap(outputLayer, network.outputLayer);
  std::swap(initializeRule, network.initializeRule);
  std::swap(width, network.width);
  std::swap(height, network.height);
  std::swap(reset, network.reset);
  std::swap(this->network, network.network);
  std::swap(predictors, network.predictors);
  std::swap(responses, network.responses);
  std::swap(parameter, network.parameter);
  std::swap(numFunctions, network.numFunctions);
  std::swap(error, network.error);
  std::swap(currentInput, network.currentInput);
  std::swap(deterministic, network.deterministic);
  std::swap(delta, network.delta);
  std::swap(inputParameter, network.inputParameter);
  std::swap(outputParameter, network.outputParameter);
  std::swap(gradient, network.gradient);
};

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::FFN(
    const FFN& network):
    outputLayer(network.outputLayer),
    initializeRule(network.initializeRule),
    width(network.width),
    height(network.height),
    reset(network.reset),
    predictors(network.predictors),
    responses(network.responses),
    parameter(network.parameter),
    numFunctions(network.numFunctions),
    error(network.error),
    currentInput(network.currentInput),
    deterministic(network.deterministic),
    delta(network.delta),
    inputParameter(network.inputParameter),
    outputParameter(network.outputParameter),
    gradient(network.gradient)
{
  // Build new layers according to source network
  for (size_t i = 0; i < network.network.size(); ++i)
  {
    this->network.push_back(boost::apply_visitor(copyVisitor,
        network.network[i]));
  }
};

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
FFN<OutputLayerType, InitializationRuleType, CustomLayers...>::FFN(
    FFN&& network):
    outputLayer(std::move(network.outputLayer)),
    initializeRule(std::move(network.initializeRule)),
    width(network.width),
    height(network.height),
    reset(network.reset),
    predictors(std::move(network.predictors)),
    responses(std::move(network.responses)),
    parameter(std::move(network.parameter)),
    numFunctions(network.numFunctions),
    error(std::move(network.error)),
    currentInput(std::move(network.currentInput)),
    deterministic(network.deterministic),
    delta(std::move(network.delta)),
    inputParameter(std::move(network.inputParameter)),
    outputParameter(std::move(network.outputParameter)),
    gradient(std::move(network.gradient))
{
  this->network = std::move(network.network);
};

template<typename OutputLayerType, typename InitializationRuleType,
         typename... CustomLayers>
FFN<OutputLayerType, InitializationRuleType, CustomLayers...>&
FFN<OutputLayerType, InitializationRuleType,
    CustomLayers...>::operator = (FFN network)
{
  Swap(network);
  return *this;
};

} // namespace ann
} // namespace mlpack

#endif
