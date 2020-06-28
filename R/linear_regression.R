#' @export
linear_regression <- function(input_model=NA,
                              lambda=NA,
                              test=NA,
                              training=NA,
                              training_responses=NA,
                              verbose=FALSE) {

  CLI_RestoreSettings("Simple Linear Regression and Prediction")

  if (!identical(input_model, NA)) {
    CLI_SetParamLinearRegressionPtr("input_model", input_model)
  }

  if (!identical(lambda, NA)) {
    CLI_SetParamDouble("lambda", lambda)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(training, NA)) {
    CLI_SetParamMat("training", to_matrix(training))
  }

  if (!identical(training_responses, NA)) {
    CLI_SetParamRow("training_responses", to_matrix(training_responses))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("output_predictions")

  linear_regression_mlpackMain()

  output_model <- CLI_GetParamLinearRegressionPtr("output_model")
  attr(output_model, "type") <- "LinearRegression"

  out <- list(
      "output_model" = output_model,
      "output_predictions" = CLI_GetParamRow("output_predictions")
  )

  CLI_ClearSettings()

  return(out)
}
