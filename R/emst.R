#' @export
emst <- function(input,
                 leaf_size=NA,
                 naive=FALSE,
                 verbose=FALSE) {

  CLI_RestoreSettings("Fast Euclidean Minimum Spanning Tree")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(leaf_size, NA)) {
    CLI_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(naive, FALSE)) {
    CLI_SetParamBool("naive", naive)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  emst_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
