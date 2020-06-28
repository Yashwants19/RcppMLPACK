#' @export
nmf <- function(input,
                rank,
                initial_h=NA,
                initial_w=NA,
                max_iterations=NA,
                min_residue=NA,
                seed=NA,
                update_rules=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("Non-negative Matrix Factorization")

  CLI_SetParamMat("input", to_matrix(input))

  CLI_SetParamInt("rank", rank)

  if (!identical(initial_h, NA)) {
    CLI_SetParamMat("initial_h", to_matrix(initial_h))
  }

  if (!identical(initial_w, NA)) {
    CLI_SetParamMat("initial_w", to_matrix(initial_w))
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(min_residue, NA)) {
    CLI_SetParamDouble("min_residue", min_residue)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(update_rules, NA)) {
    CLI_SetParamString("update_rules", update_rules)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("h")
  CLI_SetPassed("w")

  nmf_mlpackMain()

  out <- list(
      "h" = CLI_GetParamMat("h"),
      "w" = CLI_GetParamMat("w")
  )

  CLI_ClearSettings()

  return(out)
}
