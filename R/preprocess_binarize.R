#' @export
preprocess_binarize <- function(input,
                                dimension=NA,
                                threshold=NA,
                                verbose=FALSE) {

  CLI_RestoreSettings("Binarize Data")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(dimension, NA)) {
    CLI_SetParamInt("dimension", dimension)
  }

  if (!identical(threshold, NA)) {
    CLI_SetParamDouble("threshold", threshold)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  preprocess_binarize_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
