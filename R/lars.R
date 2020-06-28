#' @export
lars <- function(input=NA,
                 input_model=NA,
                 lambda1=NA,
                 lambda2=NA,
                 responses=NA,
                 test=NA,
                 use_cholesky=FALSE,
                 verbose=FALSE) {

  CLI_RestoreSettings("LARS")

  if (!identical(input, NA)) {
    CLI_SetParamMat("input", to_matrix(input))
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamLARSPtr("input_model", input_model)
  }

  if (!identical(lambda1, NA)) {
    CLI_SetParamDouble("lambda1", lambda1)
  }

  if (!identical(lambda2, NA)) {
    CLI_SetParamDouble("lambda2", lambda2)
  }

  if (!identical(responses, NA)) {
    CLI_SetParamMat("responses", to_matrix(responses))
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(use_cholesky, FALSE)) {
    CLI_SetParamBool("use_cholesky", use_cholesky)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("output_predictions")

  lars_mlpackMain()

  output_model <- CLI_GetParamLARSPtr("output_model")
  attr(output_model, "type") <- "LARS"

  out <- list(
      "output_model" = output_model,
      "output_predictions" = CLI_GetParamMat("output_predictions")
  )

  CLI_ClearSettings()

  return(out)
}
