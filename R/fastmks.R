#' @title FastMKS (Fast Max-Kernel Search)
#'
#' @description
#' An implementation of the single-tree and dual-tree fast max-kernel search
#' (FastMKS) algorithm.  Given a set of reference points and a set of query
#' points, this can find the reference point with maximum kernel value for each
#' query point; trained models can be reused for future queries.
#'
#' @param bandwidth Bandwidth (for Gaussian, Epanechnikov, and triangular kernels). 
#'   Default value "1".
#' @param base Base to use during cover tree construction.  Default value "2".
#' @param degree Degree of polynomial kernel.  Default value "2".
#' @param input_model Input FastMKS model to use.
#' @param k Number of maximum kernels to find.  Default value "0".
#' @param kernel Kernel type to use: 'linear', 'polynomial', 'cosine', 'gaussian',
#'   'epanechnikov', 'triangular', 'hyptan'.  Default value "linear".
#' @param naive If true, O(n^2) naive mode is used for computation.  Default value
#'   "FALSE".
#' @param offset Offset of kernel (for polynomial and hyptan kernels).  Default value
#'   "0".
#' @param query The query dataset.
#' @param reference The reference dataset.
#' @param scale Scale of kernel (for hyptan kernel).  Default value "1".
#' @param single If true, single-tree search is used (as opposed to dual-tree search.
#'    Default value "FALSE".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{indices}{Output matrix of indices.}
#' \item{kernels}{Output matrix of kernels.}
#' \item{output_model}{Output for FastMKS model.}
#'
#' @details
#' This program will find the k maximum kernels of a set of points, using a
#' query set and a reference set (which can optionally be the same set). More
#' specifically, for each point in the query set, the k points in the reference
#' set with maximum kernel evaluations are found.  The kernel function used is
#' specified with the "kernel" parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following command will calculate, for each point in the
#' # query set "query", the five points in the reference set "reference" with
#' # maximum kernel evaluation using the linear kernel.  The kernel evaluations
#' # may be saved with the  "kernels" output parameter and the indices may be
#' # saved with the "indices" output parameter.
#' 
#' \donttest{
#' output <- fastmks(k=5, reference=reference, query=query, kernel="linear")
#' indices <- output$indices
#' kernels <- output$kernels
#' }
#' 
#' # The output matrices are organized such that row i and column j in the
#' # indices matrix corresponds to the index of the point in the reference set
#' # that has j'th largest kernel evaluation with the point in the query set
#' # with index i.  Row i and column j in the kernels matrix corresponds to the
#' # kernel evaluation between those two points.
#' # 
#' # This program performs FastMKS using a cover tree.  The base used to build
#' # the cover tree can be specified with the "base" parameter.
fastmks <- function(bandwidth=NA,
                    base=NA,
                    degree=NA,
                    input_model=NA,
                    k=NA,
                    kernel=NA,
                    naive=FALSE,
                    offset=NA,
                    query=NA,
                    reference=NA,
                    scale=NA,
                    single=FALSE,
                    verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("FastMKS (Fast Max-Kernel Search)")

  # Process each input argument before calling mlpackMain().
  if (!identical(bandwidth, NA)) {
    IO_SetParamDouble("bandwidth", bandwidth)
  }

  if (!identical(base, NA)) {
    IO_SetParamDouble("base", base)
  }

  if (!identical(degree, NA)) {
    IO_SetParamDouble("degree", degree)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamFastMKSModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    IO_SetParamInt("k", k)
  }

  if (!identical(kernel, NA)) {
    IO_SetParamString("kernel", kernel)
  }

  if (!identical(naive, FALSE)) {
    IO_SetParamBool("naive", naive)
  }

  if (!identical(offset, NA)) {
    IO_SetParamDouble("offset", offset)
  }

  if (!identical(query, NA)) {
    IO_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    IO_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(scale, NA)) {
    IO_SetParamDouble("scale", scale)
  }

  if (!identical(single, FALSE)) {
    IO_SetParamBool("single", single)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("indices")
  IO_SetPassed("kernels")
  IO_SetPassed("output_model")

  # Call the program.
  fastmks_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamFastMKSModelPtr("output_model")
  attr(output_model, "type") <- "FastMKSModel"

  # Extract the results in order.
  out <- list(
      "indices" = IO_GetParamUMat("indices"),
      "kernels" = IO_GetParamMat("kernels"),
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
