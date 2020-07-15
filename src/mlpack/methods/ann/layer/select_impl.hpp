/**
 * @file methods/ann/layer/select_impl.hpp
 * @author Marcus Edel
 *
 * Implementation of the Select module.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_LAYER_SELECT_IMPL_HPP
#define MLPACK_METHODS_ANN_LAYER_SELECT_IMPL_HPP

// In case it hasn't yet been included.
#include "constant.hpp"

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

template<typename InputDataType, typename OutputDataType>
Select<InputDataType, OutputDataType>::Select(
    const size_t index,
    const size_t elements) :
    index(index),
    elements(elements)
  {
    // Nothing to do here.
  }

template<typename InputDataType, typename OutputDataType>
template<typename eT>
void Select<InputDataType, OutputDataType>::Forward(
    const arma::Mat<eT>& input, arma::Mat<eT>& output)
{
  if (elements == 0)
  {
    output = input.col(index);
  }
  else
  {
    output = input.submat(0, index, elements - 1, index);
  }
}

template<typename InputDataType, typename OutputDataType>
template<typename eT>
void Select<InputDataType, OutputDataType>::Backward(
    const arma::Mat<eT>& /* input */,
    const arma::Mat<eT>& gy,
    arma::Mat<eT>& g)
{
  if (elements == 0)
  {
    g = gy;
  }
  else
  {
    g = gy.submat(0, 0, elements - 1, 0);
  }
}

template<typename InputDataType, typename OutputDataType>
template<typename Archive>
void Select<InputDataType, OutputDataType>::serialize(
    Archive& ar, const unsigned int /* version */)
{
  ar & BOOST_SERIALIZATION_NVP(index);
  ar & BOOST_SERIALIZATION_NVP(elements);
}

} // namespace ann
} // namespace mlpack

#endif
