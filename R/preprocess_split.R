#' @export
preprocess_split <- function(input,
                             input_labels=NA,
                             no_shuffle=FALSE,
                             seed=NA,
                             test_ratio=NA,
                             verbose=FALSE) {

  CLI_RestoreSettings("Split Data")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(input_labels, NA)) {
    CLI_SetParamUMat("input_labels", to_matrix(input_labels))
  }

  if (!identical(no_shuffle, FALSE)) {
    CLI_SetParamBool("no_shuffle", no_shuffle)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(test_ratio, NA)) {
    CLI_SetParamDouble("test_ratio", test_ratio)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("test")
  CLI_SetPassed("test_labels")
  CLI_SetPassed("training")
  CLI_SetPassed("training_labels")

  preprocess_split_mlpackMain()

  out <- list(
      "test" = CLI_GetParamMat("test"),
      "test_labels" = CLI_GetParamUMat("test_labels"),
      "training" = CLI_GetParamMat("training"),
      "training_labels" = CLI_GetParamUMat("training_labels")
  )

  CLI_ClearSettings()

  return(out)
}
