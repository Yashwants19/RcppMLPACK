#' @title LARS
#'
#' @description
#' An implementation of Least Angle Regression (Stagewise/laSso), also known as
#' LARS.  This can train a LARS/LASSO/Elastic Net model and use that model or a
#' pre-trained model to output regression predictions for a test set.
#'
#' @param input Matrix of covariates (X).
#' @param input_model Trained LARS model to use.
#' @param lambda1 Regularization parameter for l1-norm penalty.  Default value "0".
#' @param lambda2 Regularization parameter for l2-norm penalty.  Default value "0".
#' @param responses Matrix of responses/observations (y).
#' @param test Matrix containing points to regress on (test points).
#' @param use_cholesky Use Cholesky decomposition during computation rather than
#'   explicitly computing the full Gram matrix.  Default value "FALSE".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output_model}{Output LARS model.}
#' \item{output_predictions}{If --test_file is specified, this file is where the
#'   predicted responses will be saved.}
#'
#' @details
#' An implementation of LARS: Least Angle Regression (Stagewise/laSso).  This is
#' a stage-wise homotopy-based algorithm for L1-regularized linear regression
#' (LASSO) and L1+L2-regularized linear regression (Elastic Net).
#' 
#' This program is able to train a LARS/LASSO/Elastic Net model or load a model
#' from file, output regression predictions for a test set, and save the trained
#' model to a file.  The LARS algorithm is described in more detail below:
#' 
#' Let X be a matrix where each row is a point and each column is a dimension,
#' and let y be a vector of targets.
#' 
#' The Elastic Net problem is to solve
#' 
#'   min_beta 0.5 || X * beta - y ||_2^2 + lambda_1 ||beta||_1 +
#'       0.5 lambda_2 ||beta||_2^2
#' 
#' If lambda1 > 0 and lambda2 = 0, the problem is the LASSO.
#' If lambda1 > 0 and lambda2 > 0, the problem is the Elastic Net.
#' If lambda1 = 0 and lambda2 > 0, the problem is ridge regression.
#' If lambda1 = 0 and lambda2 = 0, the problem is unregularized linear
#' regression.
#' 
#' For efficiency reasons, it is not recommended to use this algorithm with
#' "lambda1" = 0.  In that case, use the 'linear_regression' program, which
#' implements both unregularized linear regression and ridge regression.
#' 
#' To train a LARS/LASSO/Elastic Net model, the "input" and "responses"
#' parameters must be given.  The "lambda1", "lambda2", and "use_cholesky"
#' parameters control the training options.  A trained model can be saved with
#' the "output_model".  If no training is desired at all, a model can be passed
#' via the "input_model" parameter.
#' 
#' The program can also provide predictions for test data using either the
#' trained model or the given input model.  Test points can be specified with
#' the "test" parameter.  Predicted responses to the test points can be saved
#' with the "output_predictions" output parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following command trains a model on the data "data" and
#' # responses "responses" with lambda1 set to 0.4 and lambda2 set to 0 (so,
#' # LASSO is being solved), and then the model is saved to "lasso_model":
#' 
#' \donttest{
#' output <- lars(input=data, responses=responses, lambda1=0.4, lambda2=0)
#' lasso_model <- output$output_model
#' }
#' 
#' # The following command uses the "lasso_model" to provide predicted responses
#' # for the data "test" and save those responses to "test_predictions": 
#' 
#' \donttest{
#' output <- lars(input_model=lasso_model, test=test)
#' test_predictions <- output$output_predictions
#' }
lars <- function(input=NA,
                 input_model=NA,
                 lambda1=NA,
                 lambda2=NA,
                 responses=NA,
                 test=NA,
                 use_cholesky=FALSE,
                 verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("LARS")

  # Process each input argument before calling mlpackMain().
  if (!identical(input, NA)) {
    IO_SetParamMat("input", to_matrix(input))
  }

  if (!identical(input_model, NA)) {
    IO_SetParamLARSPtr("input_model", input_model)
  }

  if (!identical(lambda1, NA)) {
    IO_SetParamDouble("lambda1", lambda1)
  }

  if (!identical(lambda2, NA)) {
    IO_SetParamDouble("lambda2", lambda2)
  }

  if (!identical(responses, NA)) {
    IO_SetParamMat("responses", to_matrix(responses))
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (!identical(use_cholesky, FALSE)) {
    IO_SetParamBool("use_cholesky", use_cholesky)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output_model")
  IO_SetPassed("output_predictions")

  # Call the program.
  lars_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamLARSPtr("output_model")
  attr(output_model, "type") <- "LARS"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "output_predictions" = IO_GetParamMat("output_predictions")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
