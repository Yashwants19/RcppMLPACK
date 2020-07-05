#' @export
cf <- function(algorithm=NA,
               all_user_recommendations=FALSE,
               input_model=NA,
               interpolation=NA,
               iteration_only_termination=FALSE,
               max_iterations=NA,
               min_residue=NA,
               neighbor_search=NA,
               neighborhood=NA,
               normalization=NA,
               query=NA,
               rank=NA,
               recommendations=NA,
               seed=NA,
               test=NA,
               training=NA,
               verbose=FALSE) {

  CLI_RestoreSettings("Collaborative Filtering")

  if (!identical(algorithm, NA)) {
    CLI_SetParamString("algorithm", algorithm)
  }

  if (!identical(all_user_recommendations, FALSE)) {
    CLI_SetParamBool("all_user_recommendations", all_user_recommendations)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamCFModelPtr("input_model", input_model)
  }

  if (!identical(interpolation, NA)) {
    CLI_SetParamString("interpolation", interpolation)
  }

  if (!identical(iteration_only_termination, FALSE)) {
    CLI_SetParamBool("iteration_only_termination", iteration_only_termination)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(min_residue, NA)) {
    CLI_SetParamDouble("min_residue", min_residue)
  }

  if (!identical(neighbor_search, NA)) {
    CLI_SetParamString("neighbor_search", neighbor_search)
  }

  if (!identical(neighborhood, NA)) {
    CLI_SetParamInt("neighborhood", neighborhood)
  }

  if (!identical(normalization, NA)) {
    CLI_SetParamString("normalization", normalization)
  }

  if (!identical(query, NA)) {
    CLI_SetParamUMat("query", to_matrix(query))
  }

  if (!identical(rank, NA)) {
    CLI_SetParamInt("rank", rank)
  }

  if (!identical(recommendations, NA)) {
    CLI_SetParamInt("recommendations", recommendations)
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

  CLI_SetPassed("output")
  CLI_SetPassed("output_model")

  cf_mlpackMain()

  output_model <- CLI_GetParamCFModelPtr("output_model")
  attr(output_model, "type") <- "CFModel"

  out <- list(
      "output" = CLI_GetParamUMat("output"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
