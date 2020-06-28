#' @export
decision_stump <- function(bucket_size=NA,
                           input_model=NA,
                           labels=NA,
                           test=NA,
                           training=NA,
                           verbose=FALSE) {

  CLI_RestoreSettings("Decision Stump")

  if (!identical(bucket_size, NA)) {
    CLI_SetParamInt("bucket_size", bucket_size)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamDSModelPtr("input_model", input_model)
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

  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")

  decision_stump_mlpackMain()

  output_model <- CLI_GetParamDSModelPtr("output_model")
  attr(output_model, "type") <- "DSModel"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions")
  )

  CLI_ClearSettings()

  return(out)
}
