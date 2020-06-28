#' @export
adaboost <- function(input_model=NA,
                     iterations=NA,
                     labels=NA,
                     test=NA,
                     tolerance=NA,
                     training=NA,
                     verbose=FALSE,
                     weak_learner=NA) {

  CLI_RestoreSettings("AdaBoost")

  if (!identical(input_model, NA)) {
    CLI_SetParamAdaBoostModelPtr("input_model", input_model)
  }

  if (!identical(iterations, NA)) {
    CLI_SetParamInt("iterations", iterations)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
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

  if (!identical(weak_learner, NA)) {
    CLI_SetParamString("weak_learner", weak_learner)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")
  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  adaboost_mlpackMain()

  output_model <- CLI_GetParamAdaBoostModelPtr("output_model")
  attr(output_model, "type") <- "AdaBoostModel"

  out <- list(
      "output" = CLI_GetParamURow("output"),
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
