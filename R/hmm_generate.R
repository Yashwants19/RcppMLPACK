#' @export
hmm_generate <- function(length,
                         model,
                         seed=NA,
                         start_state=NA,
                         verbose=FALSE) {

  CLI_RestoreSettings("Hidden Markov Model (HMM) Sequence Generator")

  CLI_SetParamInt("length", length)

  CLI_SetParamHMMModelPtr("model", model)

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(start_state, NA)) {
    CLI_SetParamInt("start_state", start_state)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")
  CLI_SetPassed("state")

  hmm_generate_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output"),
      "state" = CLI_GetParamUMat("state")
  )

  CLI_ClearSettings()

  return(out)
}
