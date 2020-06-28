#' @export
local_coordinate_coding <- function(atoms=NA,
                                    initial_dictionary=NA,
                                    input_model=NA,
                                    lambda=NA,
                                    max_iterations=NA,
                                    normalize=FALSE,
                                    seed=NA,
                                    test=NA,
                                    tolerance=NA,
                                    training=NA,
                                    verbose=FALSE) {

  CLI_RestoreSettings("Local Coordinate Coding")

  if (!identical(atoms, NA)) {
    CLI_SetParamInt("atoms", atoms)
  }

  if (!identical(initial_dictionary, NA)) {
    CLI_SetParamMat("initial_dictionary", to_matrix(initial_dictionary))
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamLocalCoordinateCodingPtr("input_model", input_model)
  }

  if (!identical(lambda, NA)) {
    CLI_SetParamDouble("lambda", lambda)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(normalize, FALSE)) {
    CLI_SetParamBool("normalize", normalize)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(tolerance, NA)) {
    CLI_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(training, NA)) {
    CLI_SetParamMat("training", to_matrix(training))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("codes")
  CLI_SetPassed("dictionary")
  CLI_SetPassed("output_model")

  local_coordinate_coding_mlpackMain()

  output_model <- CLI_GetParamLocalCoordinateCodingPtr("output_model")
  attr(output_model, "type") <- "LocalCoordinateCoding"

  out <- list(
      "codes" = CLI_GetParamMat("codes"),
      "dictionary" = CLI_GetParamMat("dictionary"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
