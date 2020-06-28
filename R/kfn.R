#' @export
kfn <- function(algorithm=NA,
                epsilon=NA,
                input_model=NA,
                k=NA,
                leaf_size=NA,
                percentage=NA,
                query=NA,
                random_basis=FALSE,
                reference=NA,
                seed=NA,
                tree_type=NA,
                true_distances=NA,
                true_neighbors=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("k-Furthest-Neighbors Search")

  if (!identical(algorithm, NA)) {
    CLI_SetParamString("algorithm", algorithm)
  }

  if (!identical(epsilon, NA)) {
    CLI_SetParamDouble("epsilon", epsilon)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamKFNModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    CLI_SetParamInt("k", k)
  }

  if (!identical(leaf_size, NA)) {
    CLI_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(percentage, NA)) {
    CLI_SetParamDouble("percentage", percentage)
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

  if (!identical(tree_type, NA)) {
    CLI_SetParamString("tree_type", tree_type)
  }

  if (!identical(true_distances, NA)) {
    CLI_SetParamMat("true_distances", to_matrix(true_distances))
  }

  if (!identical(true_neighbors, NA)) {
    CLI_SetParamUMat("true_neighbors", to_matrix(true_neighbors))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("distances")
  CLI_SetPassed("neighbors")
  CLI_SetPassed("output_model")

  kfn_mlpackMain()

  output_model <- CLI_GetParamKFNModelPtr("output_model")
  attr(output_model, "type") <- "KFNModel"

  out <- list(
      "distances" = CLI_GetParamMat("distances"),
      "neighbors" = CLI_GetParamUMat("neighbors"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
