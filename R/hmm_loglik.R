#' @export
hmm_loglik <- function(input,
                       input_model,
                       verbose=FALSE) {

  CLI_RestoreSettings("Hidden Markov Model (HMM) Sequence Log-Likelihood")

  CLI_SetParamMat("input", to_matrix(input))

  CLI_SetParamHMMModelPtr("input_model", input_model)

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("log_likelihood")

  hmm_loglik_mlpackMain()

  out <- list(
      "log_likelihood" = CLI_GetParamDouble("log_likelihood")
  )

  CLI_ClearSettings()

  return(out)
}
