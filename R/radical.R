#' @export
radical <- function(input,
                    angles=NA,
                    noise_std_dev=NA,
                    objective=FALSE,
                    replicates=NA,
                    seed=NA,
                    sweeps=NA,
                    verbose=FALSE) {

  CLI_RestoreSettings("RADICAL")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(angles, NA)) {
    CLI_SetParamInt("angles", angles)
  }

  if (!identical(noise_std_dev, NA)) {
    CLI_SetParamDouble("noise_std_dev", noise_std_dev)
  }

  if (!identical(objective, FALSE)) {
    CLI_SetParamBool("objective", objective)
  }

  if (!identical(replicates, NA)) {
    CLI_SetParamInt("replicates", replicates)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(sweeps, NA)) {
    CLI_SetParamInt("sweeps", sweeps)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_ic")
  CLI_SetPassed("output_unmixing")

  radical_mlpackMain()

  out <- list(
      "output_ic" = CLI_GetParamMat("output_ic"),
      "output_unmixing" = CLI_GetParamMat("output_unmixing")
  )

  CLI_ClearSettings()

  return(out)
}
