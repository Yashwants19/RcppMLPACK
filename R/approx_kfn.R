#' @export
approx_kfn <- function(algorithm=NA,
                       calculate_error=FALSE,
                       exact_distances=NA,
                       input_model=NA,
                       k=NA,
                       num_projections=NA,
                       num_tables=NA,
                       query=NA,
                       reference=NA,
                       verbose=FALSE) {

  CLI_RestoreSettings("Approximate furthest neighbor search")

  if (!identical(algorithm, NA)) {
    CLI_SetParamString("algorithm", algorithm)
  }

  if (!identical(calculate_error, FALSE)) {
    CLI_SetParamBool("calculate_error", calculate_error)
  }

  if (!identical(exact_distances, NA)) {
    CLI_SetParamMat("exact_distances", to_matrix(exact_distances))
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamApproxKFNModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    CLI_SetParamInt("k", k)
  }

  if (!identical(num_projections, NA)) {
    CLI_SetParamInt("num_projections", num_projections)
  }

  if (!identical(num_tables, NA)) {
    CLI_SetParamInt("num_tables", num_tables)
  }

  if (!identical(query, NA)) {
    CLI_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    CLI_SetParamMat("reference", to_matrix(reference))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("distances")
  CLI_SetPassed("neighbors")
  CLI_SetPassed("output_model")

  approx_kfn_mlpackMain()

  output_model <- CLI_GetParamApproxKFNModelPtr("output_model")
  attr(output_model, "type") <- "ApproxKFNModel"

  out <- list(
      "distances" = CLI_GetParamMat("distances"),
      "neighbors" = CLI_GetParamUMat("neighbors"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
