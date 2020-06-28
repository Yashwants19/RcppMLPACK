#' @export
krann <- function(alpha=NA,
                  first_leaf_exact=FALSE,
                  input_model=NA,
                  k=NA,
                  leaf_size=NA,
                  naive=FALSE,
                  query=NA,
                  random_basis=FALSE,
                  reference=NA,
                  sample_at_leaves=FALSE,
                  seed=NA,
                  single_mode=FALSE,
                  single_sample_limit=NA,
                  tau=NA,
                  tree_type=NA,
                  verbose=FALSE) {

  CLI_RestoreSettings("K-Rank-Approximate-Nearest-Neighbors (kRANN)")

  if (!identical(alpha, NA)) {
    CLI_SetParamDouble("alpha", alpha)
  }

  if (!identical(first_leaf_exact, FALSE)) {
    CLI_SetParamBool("first_leaf_exact", first_leaf_exact)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamRANNModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    CLI_SetParamInt("k", k)
  }

  if (!identical(leaf_size, NA)) {
    CLI_SetParamInt("leaf_size", leaf_size)
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

  if (!identical(sample_at_leaves, FALSE)) {
    CLI_SetParamBool("sample_at_leaves", sample_at_leaves)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(single_mode, FALSE)) {
    CLI_SetParamBool("single_mode", single_mode)
  }

  if (!identical(single_sample_limit, NA)) {
    CLI_SetParamInt("single_sample_limit", single_sample_limit)
  }

  if (!identical(tau, NA)) {
    CLI_SetParamDouble("tau", tau)
  }

  if (!identical(tree_type, NA)) {
    CLI_SetParamString("tree_type", tree_type)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("distances")
  CLI_SetPassed("neighbors")
  CLI_SetPassed("output_model")

  krann_mlpackMain()

  output_model <- CLI_GetParamRANNModelPtr("output_model")
  attr(output_model, "type") <- "RANNModel"

  out <- list(
      "distances" = CLI_GetParamMat("distances"),
      "neighbors" = CLI_GetParamUMat("neighbors"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
