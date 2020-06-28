#' @export
kmeans <- function(clusters,
                   input,
                   algorithm=NA,
                   allow_empty_clusters=FALSE,
                   in_place=FALSE,
                   initial_centroids=NA,
                   kill_empty_clusters=FALSE,
                   labels_only=FALSE,
                   max_iterations=NA,
                   percentage=NA,
                   refined_start=FALSE,
                   samplings=NA,
                   seed=NA,
                   verbose=FALSE) {

  CLI_RestoreSettings("K-Means Clustering")

  CLI_SetParamInt("clusters", clusters)

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(algorithm, NA)) {
    CLI_SetParamString("algorithm", algorithm)
  }

  if (!identical(allow_empty_clusters, FALSE)) {
    CLI_SetParamBool("allow_empty_clusters", allow_empty_clusters)
  }

  if (!identical(in_place, FALSE)) {
    CLI_SetParamBool("in_place", in_place)
  }

  if (!identical(initial_centroids, NA)) {
    CLI_SetParamMat("initial_centroids", to_matrix(initial_centroids))
  }

  if (!identical(kill_empty_clusters, FALSE)) {
    CLI_SetParamBool("kill_empty_clusters", kill_empty_clusters)
  }

  if (!identical(labels_only, FALSE)) {
    CLI_SetParamBool("labels_only", labels_only)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(percentage, NA)) {
    CLI_SetParamDouble("percentage", percentage)
  }

  if (!identical(refined_start, FALSE)) {
    CLI_SetParamBool("refined_start", refined_start)
  }

  if (!identical(samplings, NA)) {
    CLI_SetParamInt("samplings", samplings)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("centroid")
  CLI_SetPassed("output")

  kmeans_mlpackMain()

  out <- list(
      "centroid" = CLI_GetParamMat("centroid"),
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
