#' @export
logistic_regression <- function(batch_size=NA,
                                decision_boundary=NA,
                                input_model=NA,
                                labels=NA,
                                lambda=NA,
                                max_iterations=NA,
                                optimizer=NA,
                                step_size=NA,
                                test=NA,
                                tolerance=NA,
                                training=NA,
                                verbose=FALSE) {

  CLI_RestoreSettings("L2-regularized Logistic Regression and Prediction")

  if (!identical(batch_size, NA)) {
    CLI_SetParamInt("batch_size", batch_size)
  }

  if (!identical(decision_boundary, NA)) {
    CLI_SetParamDouble("decision_boundary", decision_boundary)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamLogisticRegressionPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(lambda, NA)) {
    CLI_SetParamDouble("lambda", lambda)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(optimizer, NA)) {
    CLI_SetParamString("optimizer", optimizer)
  }

  if (!identical(step_size, NA)) {
    CLI_SetParamDouble("step_size", step_size)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(tolerance, NA)) {
    CLI_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(training, NA)) {
    CLI_SetParamMat("training", to_matrix(training))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")
  CLI_SetPassed("output_model")
  CLI_SetPassed("output_probabilities")
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  logistic_regression_mlpackMain()

  output_model <- CLI_GetParamLogisticRegressionPtr("output_model")
  attr(output_model, "type") <- "LogisticRegression"

  out <- list(
      "output" = CLI_GetParamURow("output"),
      "output_model" = output_model,
      "output_probabilities" = CLI_GetParamMat("output_probabilities"),
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
