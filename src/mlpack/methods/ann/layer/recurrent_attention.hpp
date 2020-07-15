/**
 * @file methods/ann/layer/recurrent_attention.hpp
 * @author Marcus Edel
 *
 * Definition of the RecurrentAttention class.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_LAYER_RECURRENT_ATTENTION_HPP
#define MLPACK_METHODS_ANN_LAYER_RECURRENT_ATTENTION_HPP

#include <mlpack/prereqs.hpp>
#include <boost/ptr_container/ptr_vector.hpp>

#include "../visitor/delta_visitor.hpp"
#include "../visitor/output_parameter_visitor.hpp"
#include "../visitor/reset_visitor.hpp"
#include "../visitor/weight_size_visitor.hpp"

#include "layer_types.hpp"
#include "add_merge.hpp"
#include "sequential.hpp"

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

/**
 * This class implements the Recurrent Model for Visual Attention, using a
 * variety of possible layer implementations.
 *
 * For more information, see the following paper.
 *
 * @code
 * @article{MnihHGK14,
 *   title   = {Recurrent Models of Visual Attention},
 *   author  = {Volodymyr Mnih, Nicolas Heess, Alex Graves, Koray Kavukcuoglu},
 *   journal = {CoRR},
 *   volume  = {abs/1406.6247},
 *   year    = {2014},
 *   url     = {https://arxiv.org/abs/1406.6247}
 * }
 * @endcode
 *
 * @tparam InputDataType Type of the input data (arma::colvec, arma::mat,
 *         arma::sp_mat or arma::cube).
 * @tparam OutputDataType Type of the output data (arma::colvec, arma::mat,
 *         arma::sp_mat or arma::cube).
 */
template <
    typename InputDataType = arma::mat,
    typename OutputDataType = arma::mat
>
class RecurrentAttention
{
 public:
  /**
   * Default constructor: this will not give a usable RecurrentAttention object,
   * so be sure to set all the parameters before use.
   */
  RecurrentAttention();

  /**
   * Create the RecurrentAttention object using the specified modules.
   *
   * @param outSize The module output size.
   * @param rnn The recurrent neural network module.
   * @param action The action module.
   * @param rho Maximum number of steps to backpropagate through time (BPTT).
   */
  template<typename RNNModuleType, typename ActionModuleType>
  RecurrentAttention(const size_t outSize,
                     const RNNModuleType& rnn,
                     const ActionModuleType& action,
                     const size_t rho);

  /**
   * Ordinary feed forward pass of a neural network, evaluating the function
   * f(x) by propagating the activity forward through f.
   *
   * @param input Input data used for evaluating the specified function.
   * @param output Resulting output activation.
   */
  template<typename eT>
  void Forward(const arma::Mat<eT>& input, arma::Mat<eT>& output);

  /**
   * Ordinary feed backward pass of a neural network, calculating the function
   * f(x) by propagating x backwards trough f. Using the results from the feed
   * forward pass.
   *
   * @param * (input) The propagated input activation.
   * @param gy The backpropagated error.
   * @param g The calculated gradient.
   */
  template<typename eT>
  void Backward(const arma::Mat<eT>& /* input */,
                const arma::Mat<eT>& gy,
                arma::Mat<eT>& g);

  /*
   * Calculate the gradient using the output delta and the input activation.
   *
   * @param * (input) The input parameter used for calculating the gradient.
   * @param * (error) The calculated error.
   * @param * (gradient) The calculated gradient.
   */
  template<typename eT>
  void Gradient(const arma::Mat<eT>& /* input */,
                const arma::Mat<eT>& /* error */,
                arma::Mat<eT>& /* gradient */);

  //! Get the model modules.
  std::vector<LayerTypes<>>& Model() { return network; }

    //! The value of the deterministic parameter.
  bool Deterministic() const { return deterministic; }
  //! Modify the value of the deterministic parameter.
  bool& Deterministic() { return deterministic; }

  //! Get the parameters.
  OutputDataType const& Parameters() const { return parameters; }
  //! Modify the parameters.
  OutputDataType& Parameters() { return parameters; }

  //! Get the output parameter.
  OutputDataType const& OutputParameter() const { return outputParameter; }
  //! Modify the output parameter.
  OutputDataType& OutputParameter() { return outputParameter; }

  //! Get the delta.
  OutputDataType const& Delta() const { return delta; }
  //! Modify the delta.
  OutputDataType& Delta() { return delta; }

  //! Get the gradient.
  OutputDataType const& Gradient() const { return gradient; }
  //! Modify the gradient.
  OutputDataType& Gradient() { return gradient; }

  //! Get the module output size.
  size_t OutSize() const { return outSize; }

  //! Get the number of steps to backpropagate through time.
  size_t const& Rho() const { return rho; }

  /**
   * Serialize the layer
   */
  template<typename Archive>
  void serialize(Archive& ar, const unsigned int /* version */);

 private:
  //! Calculate the gradient of the attention module.
  void IntermediateGradient()
  {
    intermediateGradient.zeros();

    // Gradient of the action module.
    if (backwardStep == (rho - 1))
    {
      boost::apply_visitor(GradientVisitor(initialInput, actionError),
          actionModule);
    }
    else
    {
      boost::apply_visitor(GradientVisitor(boost::apply_visitor(
          outputParameterVisitor, actionModule), actionError),
          actionModule);
    }

    // Gradient of the recurrent module.
    boost::apply_visitor(GradientVisitor(boost::apply_visitor(
        outputParameterVisitor, rnnModule), recurrentError),
        rnnModule);

    attentionGradient += intermediateGradient;
  }

  //! Locally-stored module output size.
  size_t outSize;

  //! Locally-stored start module.
  LayerTypes<> rnnModule;

  //! Locally-stored input module.
  LayerTypes<> actionModule;

  //! Number of steps to backpropagate through time (BPTT).
  size_t rho;

  //! Locally-stored number of forward steps.
  size_t forwardStep;

  //! Locally-stored number of backward steps.
  size_t backwardStep;

  //! If true dropout and scaling is disabled, see notes above.
  bool deterministic;

  //! Locally-stored weight object.
  OutputDataType parameters;

  //! Locally-stored model modules.
  std::vector<LayerTypes<>> network;

  //! Locally-stored weight size visitor.
  WeightSizeVisitor weightSizeVisitor;

  //! Locally-stored delta visitor.
  DeltaVisitor deltaVisitor;

  //! Locally-stored output parameter visitor.
  OutputParameterVisitor outputParameterVisitor;

  //! Locally-stored feedback output parameters.
  std::vector<arma::mat> feedbackOutputParameter;

  //! List of all module parameters for the backward pass (BBTT).
  std::vector<arma::mat> moduleOutputParameter;

  //! Locally-stored delta object.
  OutputDataType delta;

  //! Locally-stored gradient object.
  OutputDataType gradient;

  //! Locally-stored output parameter object.
  OutputDataType outputParameter;

  //! Locally-stored recurrent error parameter.
  arma::mat recurrentError;

  //! Locally-stored action error parameter.
  arma::mat actionError;

  //! Locally-stored action delta.
  arma::mat actionDelta;

  //! Locally-stored recurrent delta.
  arma::mat rnnDelta;

  //! Locally-stored initial action input.
  arma::mat initialInput;

  //! Locally-stored reset visitor.
  ResetVisitor resetVisitor;

  //! Locally-stored attention gradient.
  arma::mat attentionGradient;

  //! Locally-stored intermediate gradient for the attention module.
  arma::mat intermediateGradient;
}; // class RecurrentAttention

} // namespace ann
} // namespace mlpack

// Include implementation.
#include "recurrent_attention_impl.hpp"

#endif
