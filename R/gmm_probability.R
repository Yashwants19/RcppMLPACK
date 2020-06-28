#' @export
gmm_probability <- function(input,
                            input_model,
                            verbose=FALSE) {

  CLI_RestoreSettings("GMM Probability Calculator")

  CLI_SetParamMat("input", to_matrix(input))

  CLI_SetParamGMMPtr("input_model", input_model)

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  gmm_probability_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
