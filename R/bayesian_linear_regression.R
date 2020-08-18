#' @title BayesianLinearRegression
#'
#' @description
#' An implementation of the bayesian linear regression.
#'
#' @param center Center the data and fit the intercept if enabled.  Default
#'   value "FALSE" (logical).
#' @param input Matrix of covariates (X) (numeric matrix).
#' @param input_model Trained BayesianLinearRegression model to use
#'   (BayesianLinearRegression).
#' @param responses Matrix of responses/observations (y) (numeric row).
#' @param scale Scale each feature by their standard deviations if enabled.
#'    Default value "FALSE" (logical).
#' @param test Matrix containing points to regress on (test points)
#'   (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output_model}{Output BayesianLinearRegression model
#'   (BayesianLinearRegression).}
#' \item{predictions}{If --test_file is specified, this file is where the
#'   predicted responses will be saved (numeric matrix).}
#' \item{stds}{If specified, this is where the standard deviations of the
#'   predictive distribution will be saved (numeric matrix).}
#'
#' @details
#' An implementation of the bayesian linear regression.
#' This model is a probabilistic view and implementation of the linear
#' regression. The final solution is obtained by computing a posterior
#' distribution from gaussian likelihood and a zero mean gaussian isotropic 
#' prior distribution on the solution. 
#' Optimization is AUTOMATIC and does not require cross validation. The
#' optimization is performed by maximization of the evidence function.
#' Parameters are tuned during the maximization of the marginal likelihood. This
#' procedure includes the Ockham's razor that penalizes over complex solutions. 
#' 
#' This program is able to train a Bayesian linear regression model or load a
#' model from file, output regression predictions for a test set, and save the
#' trained model to a file.
#' 
#' To train a BayesianLinearRegression model, the "input" and
#' "responses"parameters must be given. The "center"and "scale" parameters
#' control the centering and the normalizing options. A trained model can be
#' saved with the "output_model". If no training is desired at all, a model can
#' be passed via the "input_model" parameter.
#' 
#' The program can also provide predictions for test data using either the
#' trained model or the given input model.  Test points can be specified with
#' the "test" parameter.  Predicted responses to the test points can be saved
#' with the "predictions" output parameter. The corresponding standard deviation
#' can be save by precising the "stds" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following command trains a model on the data "data" and
#' # responses "responses"with center set to true and scale set to false (so,
#' # Bayesian linear regression is being solved, and then the model is saved to
#' # "blr_model":
#' 
#' \donttest{
#' output <- bayesian_linear_regression(input=data, responses=responses,
#'   center=1, scale=0)
#' blr_model <- output$output_model
#' }
#' 
#' # The following command uses the "blr_model" to provide predicted  responses
#' # for the data "test" and save those  responses to "test_predictions": 
#' 
#' \donttest{
#' output <- bayesian_linear_regression(input_model=blr_model, test=test)
#' test_predictions <- output$predictions
#' }
#' 
#' # Because the estimator computes a predictive distribution instead of simple
#' # point estimate, the "stds" parameter allows to save the prediction
#' # uncertainties: 
#' 
#' \donttest{
#' output <- bayesian_linear_regression(input_model=blr_model, test=test)
#' test_predictions <- output$predictions
#' stds <- output$stds
#' }
bayesian_linear_regression <- function(center=FALSE,
                                       input=NA,
                                       input_model=NA,
                                       responses=NA,
                                       scale=FALSE,
                                       test=NA,
                                       verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("BayesianLinearRegression")

  # Process each input argument before calling mlpackMain().
  if (!identical(center, FALSE)) {
    IO_SetParamBool("center", center)
  }

  if (!identical(input, NA)) {
    IO_SetParamMat("input", to_matrix(input))
  }

  if (!identical(input_model, NA)) {
    IO_SetParamBayesianLinearRegressionPtr("input_model", input_model)
  }

  if (!identical(responses, NA)) {
    IO_SetParamRow("responses", to_matrix(responses))
  }

  if (!identical(scale, FALSE)) {
    IO_SetParamBool("scale", scale)
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output_model")
  IO_SetPassed("predictions")
  IO_SetPassed("stds")

  # Call the program.
  bayesian_linear_regression_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamBayesianLinearRegressionPtr("output_model")
  attr(output_model, "type") <- "BayesianLinearRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = IO_GetParamMat("predictions"),
      "stds" = IO_GetParamMat("stds")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
