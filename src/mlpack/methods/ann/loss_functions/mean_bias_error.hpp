/**
 * @file methods/ann/loss_functions/mean_bias_error.hpp
 * @author Saksham Rastogi
 *
 * Definition of the mean bias error performance function.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_LOSS_FUNCTION_MEAN_BIAS_ERROR_HPP
#define MLPACK_METHODS_ANN_LOSS_FUNCTION_MEAN_BIAS_ERROR_HPP

#include <mlpack/prereqs.hpp>

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

/**
 * The mean bias error performance function measures the network's
 * performance according to the mean of errors.
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
class MeanBiasError
{
 public:
  /**
   * Create the MeanBiasError object.
   */
  MeanBiasError();

  /**
   * Computes the mean bias error function.
   *
   * @param input Input data used for evaluating the specified function.
   * @param target The target vector.
   */
  template<typename InputType, typename TargetType>
  typename InputType::elem_type Forward(const InputType& input,
                                        const TargetType& target);

  /**
   * Ordinary feed backward pass of a neural network.
   *
   * @param input The propagated input activation.
   * @param target The target vector.
   * @param output The calculated error.
   */
  template<typename InputType, typename TargetType, typename OutputType>
  void Backward(const InputType& input,
                const TargetType& target,
                OutputType& output);

  //! Get the output parameter.
  OutputDataType& OutputParameter() const { return outputParameter; }
  //! Modify the output parameter.
  OutputDataType& OutputParameter() { return outputParameter; }

  /**
   * Serialize the layer.
   */
  template<typename Archive>
  void serialize(Archive& ar, const unsigned int /* version */);

 private:
  //! Locally-stored output parameter object.
  OutputDataType outputParameter;
}; // class MeanBiasError

} // namespace ann
} // namespace mlpack

// Include implementation.
#include "mean_bias_error_impl.hpp"

#endif
