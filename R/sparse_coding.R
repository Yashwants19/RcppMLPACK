#' @export
sparse_coding <- function(atoms=NA,
                          initial_dictionary=NA,
                          input_model=NA,
                          lambda1=NA,
                          lambda2=NA,
                          max_iterations=NA,
                          newton_tolerance=NA,
                          normalize=FALSE,
                          objective_tolerance=NA,
                          seed=NA,
                          test=NA,
                          training=NA,
                          verbose=FALSE) {

  CLI_RestoreSettings("Sparse Coding")

  if (!identical(atoms, NA)) {
    CLI_SetParamInt("atoms", atoms)
  }

  if (!identical(initial_dictionary, NA)) {
    CLI_SetParamMat("initial_dictionary", to_matrix(initial_dictionary))
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamSparseCodingPtr("input_model", input_model)
  }

  if (!identical(lambda1, NA)) {
    CLI_SetParamDouble("lambda1", lambda1)
  }

  if (!identical(lambda2, NA)) {
    CLI_SetParamDouble("lambda2", lambda2)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(newton_tolerance, NA)) {
    CLI_SetParamDouble("newton_tolerance", newton_tolerance)
  }

  if (!identical(normalize, FALSE)) {
    CLI_SetParamBool("normalize", normalize)
  }

  if (!identical(objective_tolerance, NA)) {
    CLI_SetParamDouble("objective_tolerance", objective_tolerance)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
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

  sparse_coding_mlpackMain()

  output_model <- CLI_GetParamSparseCodingPtr("output_model")
  attr(output_model, "type") <- "SparseCoding"

  out <- list(
      "codes" = CLI_GetParamMat("codes"),
      "dictionary" = CLI_GetParamMat("dictionary"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
