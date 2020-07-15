#' @title Kernel Principal Components Analysis
#'
#' @description
#' An implementation of Kernel Principal Components Analysis (KPCA).  This can
#' be used to perform nonlinear dimensionality reduction or preprocessing on a
#' given dataset.
#'
#' @param input Input dataset to perform KPCA on.
#' @param kernel The kernel to use; see the above documentation for the list of
#'   usable kernels.
#' @param bandwidth Bandwidth, for 'gaussian' and 'laplacian' kernels.  Default value
#'   "1".
#' @param center If set, the transformed data will be centered about the origin. 
#'   Default value "FALSE".
#' @param degree Degree of polynomial, for 'polynomial' kernel.  Default value "1".
#' @param kernel_scale Scale, for 'hyptan' kernel.  Default value "1".
#' @param new_dimensionality If not 0, reduce the dimensionality of the output
#'   dataset by ignoring the dimensions with the smallest eigenvalues.  Default
#'   value "0".
#' @param nystroem_method If set, the Nystroem method will be used.  Default value
#'   "FALSE".
#' @param offset Offset, for 'hyptan' and 'polynomial' kernels.  Default value "0".
#' @param sampling Sampling scheme to use for the Nystroem method: 'kmeans',
#'   'random', 'ordered'  Default value "kmeans".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{Matrix to save modified dataset to.}
#'
#' @details
#' This program performs Kernel Principal Components Analysis (KPCA) on the
#' specified dataset with the specified kernel.  This will transform the data
#' onto the kernel principal components, and optionally reduce the
#' dimensionality by ignoring the kernel principal components with the smallest
#' eigenvalues.
#' 
#' For the case where a linear kernel is used, this reduces to regular PCA.
#' 
#' The kernels that are supported are listed below:
#' 
#'  * 'linear': the standard linear dot product (same as normal PCA):
#'     K(x, y) = x^T y
#' 
#'  * 'gaussian': a Gaussian kernel; requires bandwidth:
#'     K(x, y) = exp(-(|| x - y || ^ 2) / (2 * (bandwidth ^ 2)))
#' 
#'  * 'polynomial': polynomial kernel; requires offset and degree:
#'     K(x, y) = (x^T y + offset) ^ degree
#' 
#'  * 'hyptan': hyperbolic tangent kernel; requires scale and offset:
#'     K(x, y) = tanh(scale * (x^T y) + offset)
#' 
#'  * 'laplacian': Laplacian kernel; requires bandwidth:
#'     K(x, y) = exp(-(|| x - y ||) / bandwidth)
#' 
#'  * 'epanechnikov': Epanechnikov kernel; requires bandwidth:
#'     K(x, y) = max(0, 1 - || x - y ||^2 / bandwidth^2)
#' 
#'  * 'cosine': cosine distance:
#'     K(x, y) = 1 - (x^T y) / (|| x || * || y ||)
#' 
#' The parameters for each of the kernels should be specified with the options
#' "bandwidth", "kernel_scale", "offset", or "degree" (or a combination of those
#' parameters).
#' 
#' Optionally, the Nystroem method ("Using the Nystroem method to speed up
#' kernel machines", 2001) can be used to calculate the kernel matrix by
#' specifying the "nystroem_method" parameter. This approach works by using a
#' subset of the data as basis to reconstruct the kernel matrix; to specify the
#' sampling scheme, the "sampling" parameter is used.  The sampling scheme for
#' the Nystroem method can be chosen from the following list: 'kmeans',
#' 'random', 'ordered'.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following command will perform KPCA on the dataset "input"
#' # using the Gaussian kernel, and saving the transformed data to
#' # "transformed": 
#' 
#' \donttest{
#' output <- kernel_pca(input=input, kernel="gaussian")
#' transformed <- output$output
#' }
kernel_pca <- function(input,
                       kernel,
                       bandwidth=NA,
                       center=FALSE,
                       degree=NA,
                       kernel_scale=NA,
                       new_dimensionality=NA,
                       nystroem_method=FALSE,
                       offset=NA,
                       sampling=NA,
                       verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Kernel Principal Components Analysis")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  IO_SetParamString("kernel", kernel)

  if (!identical(bandwidth, NA)) {
    IO_SetParamDouble("bandwidth", bandwidth)
  }

  if (!identical(center, FALSE)) {
    IO_SetParamBool("center", center)
  }

  if (!identical(degree, NA)) {
    IO_SetParamDouble("degree", degree)
  }

  if (!identical(kernel_scale, NA)) {
    IO_SetParamDouble("kernel_scale", kernel_scale)
  }

  if (!identical(new_dimensionality, NA)) {
    IO_SetParamInt("new_dimensionality", new_dimensionality)
  }

  if (!identical(nystroem_method, FALSE)) {
    IO_SetParamBool("nystroem_method", nystroem_method)
  }

  if (!identical(offset, NA)) {
    IO_SetParamDouble("offset", offset)
  }

  if (!identical(sampling, NA)) {
    IO_SetParamString("sampling", sampling)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  kernel_pca_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
