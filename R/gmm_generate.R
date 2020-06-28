#' @export
gmm_generate <- function(input_model,
                         samples,
                         seed=NA,
                         verbose=FALSE) {

  CLI_RestoreSettings("GMM Sample Generator")

  CLI_SetParamGMMPtr("input_model", input_model)

  CLI_SetParamInt("samples", samples)

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  gmm_generate_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
