#' @export
hmm_viterbi <- function(input,
                        input_model,
                        verbose=FALSE) {

  CLI_RestoreSettings("Hidden Markov Model (HMM) Viterbi State Prediction")

  CLI_SetParamMat("input", to_matrix(input))

  CLI_SetParamHMMModelPtr("input_model", input_model)

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  hmm_viterbi_mlpackMain()

  out <- list(
      "output" = CLI_GetParamUMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
