#' @export
nbc <- function(incremental_variance=FALSE,
                input_model=NA,
                labels=NA,
                test=NA,
                training=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("Parametric Naive Bayes Classifier")

  if (!identical(incremental_variance, FALSE)) {
    CLI_SetParamBool("incremental_variance", incremental_variance)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamNBCModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
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
  CLI_SetPassed("output_probs")
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  nbc_mlpackMain()

  output_model <- CLI_GetParamNBCModelPtr("output_model")
  attr(output_model, "type") <- "NBCModel"

  out <- list(
      "output" = CLI_GetParamURow("output"),
      "output_model" = output_model,
      "output_probs" = CLI_GetParamMat("output_probs"),
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
