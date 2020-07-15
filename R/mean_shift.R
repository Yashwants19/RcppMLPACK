#' @title Mean Shift Clustering
#'
#' @description
#' A fast implementation of mean-shift clustering using dual-tree range search. 
#' Given a dataset, this uses the mean shift algorithm to produce and return a
#' clustering of the data.
#'
#' @param input Input dataset to perform clustering on.
#' @param force_convergence If specified, the mean shift algorithm will continue
#'   running regardless of max_iterations until the clusters converge.  Default
#'   value "FALSE".
#' @param in_place If specified, a column containing the learned cluster assignments
#'   will be added to the input dataset file.  In this case, --output_file is
#'   overridden.  (Do not use with Python.)  Default value "FALSE".
#' @param labels_only If specified, only the output labels will be written to the
#'   file specified by --output_file.  Default value "FALSE".
#' @param max_iterations Maximum number of iterations before mean shift terminates. 
#'   Default value "1000".
#' @param radius If the distance between two centroids is less than the given radius,
#'   one will be removed.  A radius of 0 or less means an estimate will be
#'   calculated and used for the radius.  Default value "0".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{centroid}{If specified, the centroids of each cluster will be written to
#'   the given matrix.}
#' \item{output}{Matrix to write output labels or labeled data to.}
#'
#' @details
#' This program performs mean shift clustering on the given dataset, storing the
#' learned cluster assignments either as a column of labels in the input dataset
#' or separately.
#' 
#' The input dataset should be specified with the "input" parameter, and the
#' radius used for search can be specified with the "radius" parameter.  The
#' maximum number of iterations before algorithm termination is controlled with
#' the "max_iterations" parameter.
#' 
#' The output labels may be saved with the "output" output parameter and the
#' centroids of each cluster may be saved with the "centroid" output parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to run mean shift clustering on the dataset "data" and store
#' # the centroids to "centroids", the following command may be used: 
#' 
#' \donttest{
#' output <- mean_shift(input=data)
#' centroids <- output$centroid
#' }
mean_shift <- function(input,
                       force_convergence=FALSE,
                       in_place=FALSE,
                       labels_only=FALSE,
                       max_iterations=NA,
                       radius=NA,
                       verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Mean Shift Clustering")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(force_convergence, FALSE)) {
    IO_SetParamBool("force_convergence", force_convergence)
  }

  if (!identical(in_place, FALSE)) {
    IO_SetParamBool("in_place", in_place)
  }

  if (!identical(labels_only, FALSE)) {
    IO_SetParamBool("labels_only", labels_only)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(radius, NA)) {
    IO_SetParamDouble("radius", radius)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("centroid")
  IO_SetPassed("output")

  # Call the program.
  mean_shift_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "centroid" = IO_GetParamMat("centroid"),
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
