#' @export
perceptron <- function(input_model=NA,
                       labels=NA,
                       max_iterations=NA,
                       test=NA,
                       training=NA,
                       verbose=FALSE) {

  CLI_RestoreSettings("Perceptron")

  if (!identical(input_model, NA)) {
    CLI_SetParamPerceptronModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
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
  CLI_SetPassed("predictions")

  perceptron_mlpackMain()

  output_model <- CLI_GetParamPerceptronModelPtr("output_model")
  attr(output_model, "type") <- "PerceptronModel"

  out <- list(
      "output" = CLI_GetParamURow("output"),
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions")
  )

  CLI_ClearSettings()

  return(out)
}
