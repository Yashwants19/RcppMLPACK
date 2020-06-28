#' @export
pca <- function(input,
                decomposition_method=NA,
                new_dimensionality=NA,
                scale=FALSE,
                var_to_retain=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("Principal Components Analysis")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(decomposition_method, NA)) {
    CLI_SetParamString("decomposition_method", decomposition_method)
  }

  if (!identical(new_dimensionality, NA)) {
    CLI_SetParamInt("new_dimensionality", new_dimensionality)
  }

  if (!identical(scale, FALSE)) {
    CLI_SetParamBool("scale", scale)
  }

  if (!identical(var_to_retain, NA)) {
    CLI_SetParamDouble("var_to_retain", var_to_retain)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  pca_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
