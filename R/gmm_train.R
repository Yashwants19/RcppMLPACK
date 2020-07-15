#' @title Gaussian Mixture Model (GMM) Training
#'
#' @description
#' An implementation of the EM algorithm for training Gaussian mixture models
#' (GMMs).  Given a dataset, this can train a GMM for future use with other
#' tools.
#'
#' @param gaussians Number of Gaussians in the GMM.
#' @param input The training data on which the model will be fit.
#' @param diagonal_covariance Force the covariance of the Gaussians to be diagonal. 
#'   This can accelerate training time significantly.  Default value "FALSE".
#' @param input_model Initial input GMM model to start training with.
#' @param kmeans_max_iterations Maximum number of iterations for the k-means
#'   algorithm (used to initialize EM).  Default value "1000".
#' @param max_iterations Maximum number of iterations of EM algorithm (passing 0 will
#'   run until convergence).  Default value "250".
#' @param no_force_positive Do not force the covariance matrices to be positive
#'   definite.  Default value "FALSE".
#' @param noise Variance of zero-mean Gaussian noise to add to data.  Default value
#'   "0".
#' @param percentage If using --refined_start, specify the percentage of the dataset
#'   used for each sampling (should be between 0.0 and 1.0).  Default value
#'   "0.02".
#' @param refined_start During the initialization, use refined initial positions for
#'   k-means clustering (Bradley and Fayyad, 1998).  Default value "FALSE".
#' @param samplings If using --refined_start, specify the number of samplings used
#'   for initial points.  Default value "100".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param tolerance Tolerance for convergence of EM.  Default value "1e-10".
#' @param trials Number of trials to perform in training GMM.  Default value "1".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output_model}{Output for trained GMM model.}
#'
#' @details
#' This program takes a parametric estimate of a Gaussian mixture model (GMM)
#' using the EM algorithm to find the maximum likelihood estimate.  The model
#' may be saved and reused by other mlpack GMM tools.
#' 
#' The input data to train on must be specified with the "input" parameter, and
#' the number of Gaussians in the model must be specified with the "gaussians"
#' parameter.  Optionally, many trials with different random initializations may
#' be run, and the result with highest log-likelihood on the training data will
#' be taken.  The number of trials to run is specified with the "trials"
#' parameter.  By default, only one trial is run.
#' 
#' The tolerance for convergence and maximum number of iterations of the EM
#' algorithm are specified with the "tolerance" and "max_iterations" parameters,
#' respectively.  The GMM may be initialized for training with another model,
#' specified with the "input_model" parameter. Otherwise, the model is
#' initialized by running k-means on the data.  The k-means clustering
#' initialization can be controlled with the "kmeans_max_iterations",
#' "refined_start", "samplings", and "percentage" parameters.  If
#' "refined_start" is specified, then the Bradley-Fayyad refined start
#' initialization will be used.  This can often lead to better clustering
#' results.
#' 
#' The 'diagonal_covariance' flag will cause the learned covariances to be
#' diagonal matrices.  This significantly simplifies the model itself and causes
#' training to be faster, but restricts the ability to fit more complex GMMs.
#' 
#' If GMM training fails with an error indicating that a covariance matrix could
#' not be inverted, make sure that the "no_force_positive" parameter is not
#' specified.  Alternately, adding a small amount of Gaussian noise (using the
#' "noise" parameter) to the entire dataset may help prevent Gaussians with zero
#' variance in a particular dimension, which is usually the cause of
#' non-invertible covariance matrices.
#' 
#' The "no_force_positive" parameter, if set, will avoid the checks after each
#' iteration of the EM algorithm which ensure that the covariance matrices are
#' positive definite.  Specifying the flag can cause faster runtime, but may
#' also cause non-positive definite covariance matrices, which will cause the
#' program to crash.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # As an example, to train a 6-Gaussian GMM on the data in "data" with a
#' # maximum of 100 iterations of EM and 3 trials, saving the trained GMM to
#' # "gmm", the following command can be used:
#' 
#' \donttest{
#' output <- gmm_train(input=data, gaussians=6, trials=3)
#' gmm <- output$output_model
#' }
#' 
#' # To re-train that GMM on another set of data "data2", the following command
#' # may be used: 
#' 
#' \donttest{
#' output <- gmm_train(input_model=gmm, input=data2, gaussians=6)
#' new_gmm <- output$output_model
#' }
gmm_train <- function(gaussians,
                      input,
                      diagonal_covariance=FALSE,
                      input_model=NA,
                      kmeans_max_iterations=NA,
                      max_iterations=NA,
                      no_force_positive=FALSE,
                      noise=NA,
                      percentage=NA,
                      refined_start=FALSE,
                      samplings=NA,
                      seed=NA,
                      tolerance=NA,
                      trials=NA,
                      verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Gaussian Mixture Model (GMM) Training")

  # Process each input argument before calling mlpackMain().
  IO_SetParamInt("gaussians", gaussians)

  IO_SetParamMat("input", to_matrix(input))

  if (!identical(diagonal_covariance, FALSE)) {
    IO_SetParamBool("diagonal_covariance", diagonal_covariance)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamGMMPtr("input_model", input_model)
  }

  if (!identical(kmeans_max_iterations, NA)) {
    IO_SetParamInt("kmeans_max_iterations", kmeans_max_iterations)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(no_force_positive, FALSE)) {
    IO_SetParamBool("no_force_positive", no_force_positive)
  }

  if (!identical(noise, NA)) {
    IO_SetParamDouble("noise", noise)
  }

  if (!identical(percentage, NA)) {
    IO_SetParamDouble("percentage", percentage)
  }

  if (!identical(refined_start, FALSE)) {
    IO_SetParamBool("refined_start", refined_start)
  }

  if (!identical(samplings, NA)) {
    IO_SetParamInt("samplings", samplings)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(tolerance, NA)) {
    IO_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(trials, NA)) {
    IO_SetParamInt("trials", trials)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output_model")

  # Call the program.
  gmm_train_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamGMMPtr("output_model")
  attr(output_model, "type") <- "GMM"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
