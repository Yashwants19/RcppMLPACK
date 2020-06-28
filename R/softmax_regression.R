#' @export
softmax_regression <- function(input_model=NA,
                               labels=NA,
                               lambda=NA,
                               max_iterations=NA,
                               no_intercept=FALSE,
                               number_of_classes=NA,
                               test=NA,
                               test_labels=NA,
                               training=NA,
                               verbose=FALSE) {

  CLI_RestoreSettings("Softmax Regression")

  if (!identical(input_model, NA)) {
    CLI_SetParamSoftmaxRegressionPtr("input_model", input_model)
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

  if (!identical(no_intercept, FALSE)) {
    CLI_SetParamBool("no_intercept", no_intercept)
  }

  if (!identical(number_of_classes, NA)) {
    CLI_SetParamInt("number_of_classes", number_of_classes)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(test_labels, NA)) {
    CLI_SetParamURow("test_labels", to_matrix(test_labels))
  }

  if (!identical(training, NA)) {
    CLI_SetParamMat("training", to_matrix(training))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")

  softmax_regression_mlpackMain()

  output_model <- CLI_GetParamSoftmaxRegressionPtr("output_model")
  attr(output_model, "type") <- "SoftmaxRegression"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions")
  )

  CLI_ClearSettings()

  return(out)
}
