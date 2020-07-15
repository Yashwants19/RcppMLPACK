#' @title K-Means Clustering
#'
#' @description
#' An implementation of several strategies for efficient k-means clustering.
#' Given a dataset and a value of k, this computes and returns a k-means
#' clustering on that data.
#'
#' @param clusters Number of clusters to find (0 autodetects from initial
#'   centroids).
#' @param input Input dataset to perform clustering on.
#' @param algorithm Algorithm to use for the Lloyd iteration ('naive',
#'   'pelleg-moore', 'elkan', 'hamerly', 'dualtree', or 'dualtree-covertree'). 
#'   Default value "naive".
#' @param allow_empty_clusters Allow empty clusters to be persist.  Default value
#'   "FALSE".
#' @param in_place If specified, a column containing the learned cluster assignments
#'   will be added to the input dataset file.  In this case, --output_file is
#'   overridden. (Do not use in Python.)  Default value "FALSE".
#' @param initial_centroids Start with the specified initial centroids.
#' @param kill_empty_clusters Remove empty clusters when they occur.  Default value
#'   "FALSE".
#' @param labels_only Only output labels into output file.  Default value "FALSE".
#' @param max_iterations Maximum number of iterations before k-means terminates. 
#'   Default value "1000".
#' @param percentage Percentage of dataset to use for each refined start sampling
#'   (use when --refined_start is specified).  Default value "0.02".
#' @param refined_start Use the refined initial point strategy by Bradley and Fayyad
#'   to choose initial points.  Default value "FALSE".
#' @param samplings Number of samplings to perform for refined start (use when
#'   --refined_start is specified).  Default value "100".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{centroid}{If specified, the centroids of each cluster will  be written to
#'   the given file.}
#' \item{output}{Matrix to store output labels or labeled data to.}
#'
#' @details
#' This program performs K-Means clustering on the given dataset.  It can return
#' the learned cluster assignments, and the centroids of the clusters.  Empty
#' clusters are not allowed by default; when a cluster becomes empty, the point
#' furthest from the centroid of the cluster with maximum variance is taken to
#' fill that cluster.
#' 
#' Optionally, the Bradley and Fayyad approach ("Refining initial points for
#' k-means clustering", 1998) can be used to select initial points by specifying
#' the "refined_start" parameter.  This approach works by taking random
#' samplings of the dataset; to specify the number of samplings, the "samplings"
#' parameter is used, and to specify the percentage of the dataset to be used in
#' each sample, the "percentage" parameter is used (it should be a value between
#' 0.0 and 1.0).
#' 
#' There are several options available for the algorithm used for each Lloyd
#' iteration, specified with the "algorithm"  option.  The standard O(kN)
#' approach can be used ('naive').  Other options include the Pelleg-Moore
#' tree-based algorithm ('pelleg-moore'), Elkan's triangle-inequality based
#' algorithm ('elkan'), Hamerly's modification to Elkan's algorithm ('hamerly'),
#' the dual-tree k-means algorithm ('dualtree'), and the dual-tree k-means
#' algorithm using the cover tree ('dualtree-covertree').
#' 
#' The behavior for when an empty cluster is encountered can be modified with
#' the "allow_empty_clusters" option.  When this option is specified and there
#' is a cluster owning no points at the end of an iteration, that cluster's
#' centroid will simply remain in its position from the previous iteration. If
#' the "kill_empty_clusters" option is specified, then when a cluster owns no
#' points at the end of an iteration, the cluster centroid is simply filled with
#' DBL_MAX, killing it and effectively reducing k for the rest of the
#' computation.  Note that the default option when neither empty cluster option
#' is specified can be time-consuming to calculate; therefore, specifying either
#' of these parameters will often accelerate runtime.
#' 
#' Initial clustering assignments may be specified using the "initial_centroids"
#' parameter, and the maximum number of iterations may be specified with the
#' "max_iterations" parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # As an example, to use Hamerly's algorithm to perform k-means clustering
#' # with k=10 on the dataset "data", saving the centroids to "centroids" and
#' # the assignments for each point to "assignments", the following command
#' # could be used:
#' 
#' \donttest{
#' output <- kmeans(input=data, clusters=10)
#' assignments <- output$output
#' centroids <- output$centroid
#' }
#' 
#' # To run k-means on that same dataset with initial centroids specified in
#' # "initial" with a maximum of 500 iterations, storing the output centroids in
#' # "final" the following command may be used:
#' 
#' \donttest{
#' output <- kmeans(input=data, initial_centroids=initial, clusters=10,
#'   max_iterations=500)
#' final <- output$centroid
#' }
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
  # Restore IO settings.
  IO_RestoreSettings("K-Means Clustering")

  # Process each input argument before calling mlpackMain().
  IO_SetParamInt("clusters", clusters)

  IO_SetParamMat("input", to_matrix(input))

  if (!identical(algorithm, NA)) {
    IO_SetParamString("algorithm", algorithm)
  }

  if (!identical(allow_empty_clusters, FALSE)) {
    IO_SetParamBool("allow_empty_clusters", allow_empty_clusters)
  }

  if (!identical(in_place, FALSE)) {
    IO_SetParamBool("in_place", in_place)
  }

  if (!identical(initial_centroids, NA)) {
    IO_SetParamMat("initial_centroids", to_matrix(initial_centroids))
  }

  if (!identical(kill_empty_clusters, FALSE)) {
    IO_SetParamBool("kill_empty_clusters", kill_empty_clusters)
  }

  if (!identical(labels_only, FALSE)) {
    IO_SetParamBool("labels_only", labels_only)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(percentage, NA)) {
    IO_SetParamDouble("percentage", percentage)
  }

  if (!identical(refined_start, FALSE)) {
    IO_SetParamBool("refined_start", refined_start)
  }

  if (!identical(samplings, NA)) {
    IO_SetParamInt("samplings", samplings)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("centroid")
  IO_SetPassed("output")

  # Call the program.
  kmeans_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "centroid" = IO_GetParamMat("centroid"),
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
