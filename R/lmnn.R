#' @title Large Margin Nearest Neighbors (LMNN)
#'
#' @description
#' An implementation of Large Margin Nearest Neighbors (LMNN), a distance
#' learning technique.  Given a labeled dataset, this learns a transformation of
#' the data that improves k-nearest-neighbor performance; this can be useful as
#' a preprocessing step.
#'
#' @param input Input dataset to run LMNN on.
#' @param batch_size Batch size for mini-batch SGD.  Default value "50".
#' @param center Perform mean-centering on the dataset. It is useful when the
#'   centroid of the data is far from the origin.  Default value "FALSE".
#' @param distance Initial distance matrix to be used as starting point
#' @param k Number of target neighbors to use for each datapoint.  Default value
#'   "1".
#' @param labels Labels for input dataset.
#' @param linear_scan Don't shuffle the order in which data points are visited for
#'   SGD or mini-batch SGD.  Default value "FALSE".
#' @param max_iterations Maximum number of iterations for L-BFGS (0 indicates no
#'   limit).  Default value "100000".
#' @param normalize Use a normalized starting point for optimization. Itis useful for
#'   when points are far apart, or when SGD is returning NaN.  Default value
#'   "FALSE".
#' @param optimizer Optimizer to use; 'amsgrad', 'bbsgd', 'sgd', or 'lbfgs'.  Default
#'   value "amsgrad".
#' @param passes Maximum number of full passes over dataset for AMSGrad, BB_SGD and
#'   SGD.  Default value "50".
#' @param print_accuracy Print accuracies on initial and transformed dataset  Default
#'   value "FALSE".
#' @param range Number of iterations after which impostors needs to be recalculated 
#'   Default value "1".
#' @param rank Rank of distance matrix to be optimized.   Default value "0".
#' @param regularization Regularization for LMNN objective function   Default value
#'   "0.5".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param step_size Step size for AMSGrad, BB_SGD and SGD (alpha).  Default value
#'   "0.01".
#' @param tolerance Maximum tolerance for termination of AMSGrad, BB_SGD, SGD or
#'   L-BFGS.  Default value "1e-07".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{centered_data}{Output matrix for mean-centered dataset.}
#' \item{output}{Output matrix for learned distance matrix.}
#' \item{transformed_data}{Output matrix for transformed dataset.}
#'
#' @details
#' This program implements Large Margin Nearest Neighbors, a distance learning
#' technique.  The method seeks to improve k-nearest-neighbor classification on
#' a dataset.  The method employes the strategy of reducing distance between
#' similar labeled data points (a.k.a target neighbors) and increasing distance
#' between differently labeled points (a.k.a impostors) using standard
#' optimization techniques over the gradient of the distance between data
#' points.
#' 
#' To work, this algorithm needs labeled data.  It can be given as the last row
#' of the input dataset (specified with "input"), or alternatively as a separate
#' matrix (specified with "labels").  Additionally, a starting point for
#' optimization (specified with "distance"can be given, having (r x d)
#' dimensionality.  Here r should satisfy 1 <= r <= d, Consequently a Low-Rank
#' matrix will be optimized. Alternatively, Low-Rank distance can be learned by
#' specifying the "rank"parameter (A Low-Rank matrix with uniformly distributed
#' values will be used as initial learning point). 
#' 
#' The program also requires number of targets neighbors to work with (
#' specified with "k"), A regularization parameter can also be passed, It acts
#' as a trade of between the pulling and pushing terms (specified with
#' "regularization"), In addition, this implementation of LMNN includes a
#' parameter to decide the interval after which impostors must be re-calculated
#' (specified with "range").
#' 
#' Output can either be the learned distance matrix (specified with "output"),
#' or the transformed dataset  (specified with "transformed_data"), or both.
#' Additionally mean-centered dataset (specified with "centered_data") can be
#' accessed given mean-centering (specified with "center") is performed on the
#' dataset. Accuracy on initial dataset and final transformed dataset can be
#' printed by specifying the "print_accuracy"parameter. 
#' 
#' This implementation of LMNN uses AdaGrad, BigBatch_SGD, stochastic gradient
#' descent, mini-batch stochastic gradient descent, or the L_BFGS optimizer. 
#' 
#' AdaGrad, specified by the value 'adagrad' for the parameter "optimizer", uses
#' maximum of past squared gradients. It primarily on six parameters: the step
#' size (specified with "step_size"), the batch size (specified with
#' "batch_size"), the maximum number of passes (specified with "passes").
#' Inaddition, a normalized starting point can be used by specifying the
#' "normalize" parameter. 
#' 
#' BigBatch_SGD, specified by the value 'bbsgd' for the parameter "optimizer",
#' depends primarily on four parameters: the step size (specified with
#' "step_size"), the batch size (specified with "batch_size"), the maximum
#' number of passes (specified with "passes").  In addition, a normalized
#' starting point can be used by specifying the "normalize" parameter. 
#' 
#' Stochastic gradient descent, specified by the value 'sgd' for the parameter
#' "optimizer", depends primarily on three parameters: the step size (specified
#' with "step_size"), the batch size (specified with "batch_size"), and the
#' maximum number of passes (specified with "passes").  In addition, a
#' normalized starting point can be used by specifying the "normalize"
#' parameter. Furthermore, mean-centering can be performed on the dataset by
#' specifying the "center"parameter. 
#' 
#' The L-BFGS optimizer, specified by the value 'lbfgs' for the parameter
#' "optimizer", uses a back-tracking line search algorithm to minimize a
#' function.  The following parameters are used by L-BFGS: "max_iterations",
#' "tolerance"(the optimization is terminated when the gradient norm is below
#' this value).  For more details on the L-BFGS optimizer, consult either the
#' mlpack L-BFGS documentation (in lbfgs.hpp) or the vast set of published
#' literature on L-BFGS.  In addition, a normalized starting point can be used
#' by specifying the "normalize" parameter.
#' 
#' By default, the AMSGrad optimizer is used.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # Example - Let's say we want to learn distance on iris dataset with number
#' # of targets as 3 using BigBatch_SGD optimizer. A simple call for the same
#' # will look like: 
#' 
#' \donttest{
#' output <- mlpack_lmnn(input=iris, labels=iris_labels, k=3, optimizer="bbsgd")
#' output <- output$output
#' }
#' 
#' # An another program call making use of range & regularization parameter with
#' # dataset having labels as last column can be made as: 
#' 
#' \donttest{
#' output <- mlpack_lmnn(input=letter_recognition, k=5, range=10,
#'   regularization=0.4)
#' output <- output$output
#' }
lmnn <- function(input,
                 batch_size=NA,
                 center=FALSE,
                 distance=NA,
                 k=NA,
                 labels=NA,
                 linear_scan=FALSE,
                 max_iterations=NA,
                 normalize=FALSE,
                 optimizer=NA,
                 passes=NA,
                 print_accuracy=FALSE,
                 range=NA,
                 rank=NA,
                 regularization=NA,
                 seed=NA,
                 step_size=NA,
                 tolerance=NA,
                 verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Large Margin Nearest Neighbors (LMNN)")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(batch_size, NA)) {
    IO_SetParamInt("batch_size", batch_size)
  }

  if (!identical(center, FALSE)) {
    IO_SetParamBool("center", center)
  }

  if (!identical(distance, NA)) {
    IO_SetParamMat("distance", to_matrix(distance))
  }

  if (!identical(k, NA)) {
    IO_SetParamInt("k", k)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(linear_scan, FALSE)) {
    IO_SetParamBool("linear_scan", linear_scan)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(normalize, FALSE)) {
    IO_SetParamBool("normalize", normalize)
  }

  if (!identical(optimizer, NA)) {
    IO_SetParamString("optimizer", optimizer)
  }

  if (!identical(passes, NA)) {
    IO_SetParamInt("passes", passes)
  }

  if (!identical(print_accuracy, FALSE)) {
    IO_SetParamBool("print_accuracy", print_accuracy)
  }

  if (!identical(range, NA)) {
    IO_SetParamInt("range", range)
  }

  if (!identical(rank, NA)) {
    IO_SetParamInt("rank", rank)
  }

  if (!identical(regularization, NA)) {
    IO_SetParamDouble("regularization", regularization)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(step_size, NA)) {
    IO_SetParamDouble("step_size", step_size)
  }

  if (!identical(tolerance, NA)) {
    IO_SetParamDouble("tolerance", tolerance)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("centered_data")
  IO_SetPassed("output")
  IO_SetPassed("transformed_data")

  # Call the program.
  lmnn_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "centered_data" = IO_GetParamMat("centered_data"),
      "output" = IO_GetParamMat("output"),
      "transformed_data" = IO_GetParamMat("transformed_data")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
