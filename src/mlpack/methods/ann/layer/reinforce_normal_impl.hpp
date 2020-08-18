/**
 * @file methods/ann/layer/reinforce_normal_impl.hpp
 * @author Marcus Edel
 *
 * Implementation of the ReinforceNormalLayer class, which implements the
 * REINFORCE algorithm for the normal distribution.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_LAYER_REINFORCE_NORMAL_IMPL_HPP
#define MLPACK_METHODS_ANN_LAYER_REINFORCE_NORMAL_IMPL_HPP

// In case it hasn't yet been included.
#include "reinforce_normal.hpp"

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

template<typename InputDataType, typename OutputDataType>
ReinforceNormal<InputDataType, OutputDataType>::ReinforceNormal(
    const double stdev) : stdev(stdev)
{
  // Nothing to do here.
}

template<typename InputDataType, typename OutputDataType>
template<typename eT>
void ReinforceNormal<InputDataType, OutputDataType>::Forward(
    const arma::Mat<eT>& input, arma::Mat<eT>& output)
{
  if (!deterministic)
  {
    // Multiply by standard deviations and re-center the means to the mean.
    output = arma::randn<arma::Mat<eT> >(input.n_rows, input.n_cols) *
        stdev + input;

    moduleInputParameter.push_back(input);
  }
  else
  {
    // Use maximum a posteriori.
    output = input;
  }
}

template<typename InputDataType, typename OutputDataType>
template<typename DataType>
void ReinforceNormal<InputDataType, OutputDataType>::Backward(
    const DataType& input, const DataType& /* gy */, DataType& g)
{
  g = (input - moduleInputParameter.back()) / std::pow(stdev, 2.0);

  // Multiply by reward and multiply by -1.
  g *= reward;
  g *= -1;

  moduleInputParameter.pop_back();
}

template<typename InputDataType, typename OutputDataType>
template<typename Archive>
void ReinforceNormal<InputDataType, OutputDataType>::serialize(
    Archive& ar)
{
  uint8_t version = 1;
  ar & CEREAL_NVP(version);

  ar & CEREAL_NVP(stdev);
}

} // namespace ann
} // namespace mlpack

#endif
