#' @export
lsh <- function(bucket_size=NA,
                hash_width=NA,
                input_model=NA,
                k=NA,
                num_probes=NA,
                projections=NA,
                query=NA,
                reference=NA,
                second_hash_size=NA,
                seed=NA,
                tables=NA,
                true_neighbors=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("K-Approximate-Nearest-Neighbor Search with LSH")

  if (!identical(bucket_size, NA)) {
    CLI_SetParamInt("bucket_size", bucket_size)
  }

  if (!identical(hash_width, NA)) {
    CLI_SetParamDouble("hash_width", hash_width)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamLSHSearchPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    CLI_SetParamInt("k", k)
  }

  if (!identical(num_probes, NA)) {
    CLI_SetParamInt("num_probes", num_probes)
  }

  if (!identical(projections, NA)) {
    CLI_SetParamInt("projections", projections)
  }

  if (!identical(query, NA)) {
    CLI_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    CLI_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(second_hash_size, NA)) {
    CLI_SetParamInt("second_hash_size", second_hash_size)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(tables, NA)) {
    CLI_SetParamInt("tables", tables)
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

  lsh_mlpackMain()

  output_model <- CLI_GetParamLSHSearchPtr("output_model")
  attr(output_model, "type") <- "LSHSearch"

  out <- list(
      "distances" = CLI_GetParamMat("distances"),
      "neighbors" = CLI_GetParamUMat("neighbors"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
