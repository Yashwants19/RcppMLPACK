#' @export
range_search <- function(input_model=NA,
                         leaf_size=NA,
                         max=NA,
                         min=NA,
                         naive=FALSE,
                         query=NA,
                         random_basis=FALSE,
                         reference=NA,
                         seed=NA,
                         single_mode=FALSE,
                         tree_type=NA,
                         verbose=FALSE) {

  CLI_RestoreSettings("Range Search")

  if (!identical(input_model, NA)) {
    CLI_SetParamRSModelPtr("input_model", input_model)
  }

  if (!identical(leaf_size, NA)) {
    CLI_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(max, NA)) {
    CLI_SetParamDouble("max", max)
  }

  if (!identical(min, NA)) {
    CLI_SetParamDouble("min", min)
  }

  if (!identical(naive, FALSE)) {
    CLI_SetParamBool("naive", naive)
  }

  if (!identical(query, NA)) {
    CLI_SetParamMat("query", to_matrix(query))
  }

  if (!identical(random_basis, FALSE)) {
    CLI_SetParamBool("random_basis", random_basis)
  }

  if (!identical(reference, NA)) {
    CLI_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(single_mode, FALSE)) {
    CLI_SetParamBool("single_mode", single_mode)
  }

  if (!identical(tree_type, NA)) {
    CLI_SetParamString("tree_type", tree_type)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("distances_file")
  CLI_SetPassed("neighbors_file")
  CLI_SetPassed("output_model")

  range_search_mlpackMain()

  output_model <- CLI_GetParamRSModelPtr("output_model")
  attr(output_model, "type") <- "RSModel"

  out <- list(
      "distances_file" = CLI_GetParamString("distances_file"),
      "neighbors_file" = CLI_GetParamString("neighbors_file"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
