/**
 * @file transposed_convolution.hpp
 * @author Shikhar Jaiswal
 * @author Marcus Edel
 *
 * Definition of the Transposed Convolution module class.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_ANN_LAYER_TRANSPOSED_CONVOLUTION_HPP
#define MLPACK_METHODS_ANN_LAYER_TRANSPOSED_CONVOLUTION_HPP

#include <mlpack/prereqs.hpp>

#include <mlpack/methods/ann/convolution_rules/border_modes.hpp>
#include <mlpack/methods/ann/convolution_rules/naive_convolution.hpp>
#include <mlpack/methods/ann/convolution_rules/fft_convolution.hpp>
#include <mlpack/methods/ann/convolution_rules/svd_convolution.hpp>

#include "layer_types.hpp"
#include "padding.hpp"

namespace mlpack {
namespace ann /** Artificial Neural Network. */ {

/**
 * Implementation of the Transposed Convolution class. The Transposed
 * Convolution class represents a single layer of a neural network.
 *
 * @tparam ForwardConvolutionRule Convolution to perform forward process.
 * @tparam BackwardConvolutionRule Convolution to perform backward process.
 * @tparam GradientConvolutionRule Convolution to calculate gradient.
 * @tparam InputDataType Type of the input data (arma::colvec, arma::mat,
 *         arma::sp_mat or arma::cube).
 * @tparam OutputDataType Type of the output data (arma::colvec, arma::mat,
 *         arma::sp_mat or arma::cube).
 */
template <
    typename ForwardConvolutionRule = NaiveConvolution<ValidConvolution>,
    typename BackwardConvolutionRule = NaiveConvolution<ValidConvolution>,
    typename GradientConvolutionRule = NaiveConvolution<ValidConvolution>,
    typename InputDataType = arma::mat,
    typename OutputDataType = arma::mat
>
class TransposedConvolution
{
 public:
  //! Create the Transposed Convolution object.
  TransposedConvolution();

  /**
   * Create the Transposed Convolution object using the specified number of
   * input maps, output maps, filter size, stride and padding parameter.
   *
   * Note: The equivalent stride of a transposed convolution operation is always
   * equal to 1. In this implementation, stride of filter represents the stride
   * of the associated convolution operation.
   * Note: Padding of input represents padding of associated convolution
   * operation.
   *
   * @param inSize The number of input maps.
   * @param outSize The number of output maps.
   * @param kernelWidth Width of the filter/kernel.
   * @param kernelHeight Height of the filter/kernel.
   * @param strideWidth Stride of filter application in the x direction.
   * @param strideHeight Stride of filter application in the y direction.
   * @param padW Padding width of the input.
   * @param padH Padding height of the input.
   * @param inputWidth The width of the input data.
   * @param inputHeight The height of the input data.
   * @param outputWidth The width of the output data.
   * @param outputHeight The height of the output data.
   * @param paddingType The type of padding (Valid or Same). Defaults to None.
   */
  TransposedConvolution(const size_t inSize,
                        const size_t outSize,
                        const size_t kernelWidth,
                        const size_t kernelHeight,
                        const size_t strideWidth = 1,
                        const size_t strideHeight = 1,
                        const size_t padW = 0,
                        const size_t padH = 0,
                        const size_t inputWidth = 0,
                        const size_t inputHeight = 0,
                        const size_t outputWidth = 0,
                        const size_t outputHeight = 0,
                        const std::string& paddingType = "None");

  /**
   * Create the Transposed Convolution object using the specified number of
   * input maps, output maps, filter size, stride and padding parameter.
   *
   * Note: The equivalent stride of a transposed convolution operation is always
   * equal to 1. In this implementation, stride of filter represents the stride
   * of the associated convolution operation.
   * Note: Padding of input represents padding of associated convolution
   * operation.
   *
   * @param inSize The number of input maps.
   * @param outSize The number of output maps.
   * @param kernelWidth Width of the filter/kernel.
   * @param kernelHeight Height of the filter/kernel.
   * @param strideWidth Stride of filter application in the x direction.
   * @param strideHeight Stride of filter application in the y direction.
   * @param padW A two-value tuple indicating padding widths of the input.
   *             First value is padding at left side. Second value is padding on
   *             right side.
   * @param padH A two-value tuple indicating padding heights of the input.
   *             First value is padding at top. Second value is padding on
   *             bottom.
   * @param inputWidth The width of the input data.
   * @param inputHeight The height of the input data.
   * @param outputWidth The width of the output data.
   * @param outputHeight The height of the output data.
   * @param paddingType The type of padding (Valid or Same). Defaults to None.
   */
  TransposedConvolution(const size_t inSize,
                        const size_t outSize,
                        const size_t kernelWidth,
                        const size_t kernelHeight,
                        const size_t strideWidth,
                        const size_t strideHeight,
                        const std::tuple<size_t, size_t>& padW,
                        const std::tuple<size_t, size_t>& padH,
                        const size_t inputWidth = 0,
                        const size_t inputHeight = 0,
                        const size_t outputWidth = 0,
                        const size_t outputHeight = 0,
                        const std::string& paddingType = "None");

  /*
   * Set the weight and bias term.
   */
  void Reset();

  /**
   * Ordinary feed forward pass of a neural network, evaluating the function
   * f(x) by propagating the activity forward through f.
   *
   * @param input Input data used for evaluating the specified function.
   * @param output Resulting output activation.
   */
  template<typename eT>
  void Forward(const arma::Mat<eT>&& input, arma::Mat<eT>&& output);

  /**
   * Ordinary feed backward pass of a neural network, calculating the function
   * f(x) by propagating x backwards through f. Using the results from the feed
   * forward pass.
   *
   * @param input The propagated input activation.
   * @param gy The backpropagated error.
   * @param g The calculated gradient.
   */
  template<typename eT>
  void Backward(const arma::Mat<eT>&& /* input */,
                arma::Mat<eT>&& gy,
                arma::Mat<eT>&& g);

  /*
   * Calculate the gradient using the output delta and the input activation.
   *
   * @param input The input parameter used for calculating the gradient.
   * @param error The calculated error.
   * @param gradient The calculated gradient.
   */
  template<typename eT>
  void Gradient(const arma::Mat<eT>&& /* input */,
                arma::Mat<eT>&& error,
                arma::Mat<eT>&& gradient);

  //! Get the parameters.
  OutputDataType const& Parameters() const { return weights; }
  //! Modify the parameters.
  OutputDataType& Parameters() { return weights; }

  //! Get the input parameter.
  InputDataType const& InputParameter() const { return inputParameter; }
  //! Modify the input parameter.
  InputDataType& InputParameter() { return inputParameter; }

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

  //! Get the input width.
  size_t const& InputWidth() const { return inputWidth; }
  //! Modify input the width.
  size_t& InputWidth() { return inputWidth; }

  //! Get the input height.
  size_t const& InputHeight() const { return inputHeight; }
  //! Modify the input height.
  size_t& InputHeight() { return inputHeight; }

  //! Get the output width.
  size_t const& OutputWidth() const { return outputWidth; }
  //! Modify the output width.
  size_t& OutputWidth() { return outputWidth; }

  //! Get the output height.
  size_t const& OutputHeight() const { return outputHeight; }
  //! Modify the output height.
  size_t& OutputHeight() { return outputHeight; }

  //! Get the input size.
  size_t InputSize() const { return inSize; }

  //! Get the output size.
  size_t OutputSize() const { return outSize; }

  //! Get the kernel width.
  size_t KernelWidth() const { return kernelWidth; }
  //! Modify the kernel width.
  size_t& KernelWidth() { return kernelWidth; }

  //! Get the kernel height.
  size_t KernelHeight() const { return kernelHeight; }
  //! Modify the kernel height.
  size_t& KernelHeight() { return kernelHeight; }

  //! Get the stride width.
  size_t StrideWidth() const { return strideWidth; }
  //! Modify the stride width.
  size_t& StrideWidth() { return strideWidth; }

  //! Get the stride height.
  size_t StrideHeight() const { return strideHeight; }
  //! Modify the stride height.
  size_t& StrideHeight() { return strideHeight; }

  //! Get the top padding height.
  size_t PadHTop() const { return padHTop; }
  //! Modify the top padding height.
  size_t& PadHTop() { return padHTop; }

  //! Get the bottom padding height.
  size_t PadHBottom() const { return padHBottom; }
  //! Modify the bottom padding height.
  size_t& PadHBottom() { return padHBottom; }

  //! Get the left padding width.
  size_t PadWLeft() const { return padWLeft; }
  //! Modify the left padding width.
  size_t& PadWLeft() { return padWLeft; }

  //! Get the right padding width.
  size_t PadWRight() const { return padWRight; }
  //! Modify the right padding width.
  size_t& PadWRight() { return padWRight; }

  //! Modify the bias weights of the layer.
  arma::mat& Bias() { return bias; }

  /**
   * Serialize the layer.
   */
  template<typename Archive>
  void serialize(Archive& ar, const unsigned int /* version */);

 private:
  /*
   * Rotates a 3rd-order tensor counterclockwise by 180 degrees.
   *
   * @param input The input data to be rotated.
   * @param output The rotated output.
   */
  template<typename eT>
  void Rotate180(const arma::Cube<eT>& input, arma::Cube<eT>& output)
  {
    output = arma::Cube<eT>(input.n_rows, input.n_cols, input.n_slices);

    // * left-right flip, up-down flip */
    for (size_t s = 0; s < output.n_slices; s++)
      output.slice(s) = arma::fliplr(arma::flipud(input.slice(s)));
  }

  /*
   * Function to assign padding such that output size is same as input size.
   */
  void InitializeSamePadding();

  /*
   * Rotates a dense matrix counterclockwise by 180 degrees.
   *
   * @param input The input data to be rotated.
   * @param output The rotated output.
   */
  template<typename eT>
  void Rotate180(const arma::Mat<eT>& input, arma::Mat<eT>& output)
  {
    // * left-right flip, up-down flip */
    output = arma::fliplr(arma::flipud(input));
  }


  /*
   * Insert zeros between the units of the given input data.
   * Note: This function should be used before using padding layer.
   *
   * @param input The input to be padded.
   * @param strideWidth Stride of filter application in the x direction.
   * @param strideHeight Stride of filter application in the y direction.
   * @param output The padded output data.
   */
  template<typename eT>
  void InsertZeros(const arma::Mat<eT>& input,
                   const size_t strideWidth,
                   const size_t strideHeight,
                   arma::Mat<eT>& output)
  {
    if (output.n_rows != input.n_rows * strideWidth - strideWidth + 1 ||
        output.n_cols != input.n_cols * strideHeight - strideHeight + 1)
    {
      output = arma::zeros(input.n_rows * strideWidth - strideWidth + 1,
          input.n_cols * strideHeight - strideHeight + 1);
    }

    for (size_t i = 0; i < output.n_rows; i += strideHeight)
    {
      for (size_t j = 0; j < output.n_cols; j += strideWidth)
      {
        // TODO: Use [] instead of () for speedup after this is completely
        // debugged and approved.
        output(i, j) = input(i / strideHeight, j / strideWidth);
      }
    }
  }

  /*
   * Insert zeros between the units of the given input data.
   * Note: This function should be used before using padding layer.
   *
   * @param input The input to be padded.
   * @param strideWidth Stride of filter application in the x direction.
   * @param strideHeight Stride of filter application in the y direction.
   * @param output The padded output data.
   */
  template<typename eT>
  void InsertZeros(const arma::Cube<eT>& input,
                   const size_t strideWidth,
                   const size_t strideHeight,
                   arma::Cube<eT>& output)
  {
    output = arma::zeros(input.n_rows * strideWidth - strideWidth + 1,
        input.n_cols * strideHeight - strideHeight + 1, input.n_slices);

    for (size_t i = 0; i < input.n_slices; ++i)
    {
      InsertZeros<eT>(input.slice(i), strideWidth, strideHeight,
          output.slice(i));
    }
  }

  //! Locally-stored number of input channels.
  size_t inSize;

  //! Locally-stored number of output channels.
  size_t outSize;

  //! Locally-stored number of input units.
  size_t batchSize;

  //! Locally-stored filter/kernel width.
  size_t kernelWidth;

  //! Locally-stored filter/kernel height.
  size_t kernelHeight;

  //! Locally-stored stride of the filter in x-direction.
  size_t strideWidth;

  //! Locally-stored stride of the filter in y-direction.
  size_t strideHeight;

  //! Locally-stored left-side padding width.
  size_t padWLeft;

  //! Locally-stored right-side padding width.
  size_t padWRight;

  //! Locally-stored bottom padding height.
  size_t padHBottom;

  //! Locally-stored top padding height.
  size_t padHTop;

  //! Locally-stored number of zeros added to the right of input.
  size_t aW;

  //! Locally-stored number of zeros added to the bottom of input.
  size_t aH;

  //! Locally-stored weight object.
  OutputDataType weights;

  //! Locally-stored weight object.
  arma::cube weight;

  //! Locally-stored bias term object.
  arma::mat bias;

  //! Locally-stored input width.
  size_t inputWidth;

  //! Locally-stored input height.
  size_t inputHeight;

  //! Locally-stored output width.
  size_t outputWidth;

  //! Locally-stored output height.
  size_t outputHeight;

  //! Locally-stored transformed output parameter.
  arma::cube outputTemp;

  //! Locally-stored transformed input parameter.
  arma::cube inputTemp;

  //! Locally-stored transformed padded input parameter.
  arma::cube inputPaddedTemp;

  //! Locally-stored transformed expanded input parameter.
  arma::cube inputExpandedTemp;

  //! Locally-stored transformed error parameter.
  arma::cube gTemp;

  //! Locally-stored transformed gradient parameter.
  arma::cube gradientTemp;

  //! Locally-stored padding layer for forward propagation.
  ann::Padding<> paddingForward;

  //! Locally-stored padding layer for back propagation.
  ann::Padding<> paddingBackward;

  //! Locally-stored delta object.
  OutputDataType delta;

  //! Locally-stored gradient object.
  OutputDataType gradient;

  //! Locally-stored input parameter object.
  InputDataType inputParameter;

  //! Locally-stored output parameter object.
  OutputDataType outputParameter;
}; // class TransposedConvolution

} // namespace ann
} // namespace mlpack

//! Set the serialization version of the Transposed Convolution class.
namespace boost {
namespace serialization {

template<
    typename ForwardConvolutionRule,
    typename BackwardConvolutionRule,
    typename GradientConvolutionRule,
    typename InputDataType,
    typename OutputDataType
>
struct version<
    mlpack::ann::TransposedConvolution<ForwardConvolutionRule,
        BackwardConvolutionRule, GradientConvolutionRule, InputDataType,
        OutputDataType> >
{
  BOOST_STATIC_CONSTANT(int, value = 1);
};

} // namespace serialization
} // namespace boost

// Include implementation.
#include "transposed_convolution_impl.hpp"

#endif
