#' @export
linear_svm <- function(delta=NA,
                       epochs=NA,
                       input_model=NA,
                       labels=NA,
                       lambda=NA,
                       max_iterations=NA,
                       no_intercept=FALSE,
                       num_classes=NA,
                       optimizer=NA,
                       seed=NA,
                       shuffle=FALSE,
                       step_size=NA,
                       test=NA,
                       test_labels=NA,
                       tolerance=NA,
                       training=NA,
                       verbose=FALSE) {

  CLI_RestoreSettings("Linear SVM is an L2-regularized support vector machine.")

  if (!identical(delta, NA)) {
    CLI_SetParamDouble("delta", delta)
  }

  if (!identical(epochs, NA)) {
    CLI_SetParamInt("epochs", epochs)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamLinearSVMModelPtr("input_model", input_model)
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

  if (!identical(num_classes, NA)) {
    CLI_SetParamInt("num_classes", num_classes)
  }

  if (!identical(optimizer, NA)) {
    CLI_SetParamString("optimizer", optimizer)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(shuffle, FALSE)) {
    CLI_SetParamBool("shuffle", shuffle)
  }

  if (!identical(step_size, NA)) {
    CLI_SetParamDouble("step_size", step_size)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(test_labels, NA)) {
    CLI_SetParamURow("test_labels", to_matrix(test_labels))
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

  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  linear_svm_mlpackMain()

  output_model <- CLI_GetParamLinearSVMModelPtr("output_model")
  attr(output_model, "type") <- "LinearSVMModel"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
