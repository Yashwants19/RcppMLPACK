#' @title DBSCAN clustering
#'
#' @description
#' An implementation of DBSCAN clustering.  Given a dataset, this can compute
#' and return a clustering of that dataset.
#'
#' @param input Input dataset to cluster.
#' @param epsilon Radius of each range search.  Default value "1".
#' @param min_size Minimum number of points for a cluster.  Default value "5".
#' @param naive If set, brute-force range search (not tree-based) will be used. 
#'   Default value "FALSE".
#' @param selection_type If using point selection policy, the type of selection to
#'   use ('ordered', 'random').  Default value "ordered".
#' @param single_mode If set, single-tree range search (not dual-tree) will be used. 
#'   Default value "FALSE".
#' @param tree_type If using single-tree or dual-tree search, the type of tree to use
#'   ('kd', 'r', 'r-star', 'x', 'hilbert-r', 'r-plus', 'r-plus-plus', 'cover',
#'   'ball').  Default value "kd".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{assignments}{Output matrix for assignments of each point.}
#' \item{centroids}{Matrix to save output centroids to.}
#'
#' @details
#' This program implements the DBSCAN algorithm for clustering using accelerated
#' tree-based range search.  The type of tree that is used may be parameterized,
#' or brute-force range search may also be used.
#' 
#' The input dataset to be clustered may be specified with the "input"
#' parameter; the radius of each range search may be specified with the
#' "epsilon" parameters, and the minimum number of points in a cluster may be
#' specified with the "min_size" parameter.
#' 
#' The "assignments" and "centroids" output parameters may be used to save the
#' output of the clustering. "assignments" contains the cluster assignments of
#' each point, and "centroids" contains the centroids of each cluster.
#' 
#' The range search may be controlled with the "tree_type", "single_mode", and
#' "naive" parameters.  "tree_type" can control the type of tree used for range
#' search; this can take a variety of values: 'kd', 'r', 'r-star', 'x',
#' 'hilbert-r', 'r-plus', 'r-plus-plus', 'cover', 'ball'. The "single_mode"
#' parameter will force single-tree search (as opposed to the default dual-tree
#' search), and '"naive" will force brute-force range search.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # An example usage to run DBSCAN on the dataset in "input" with a radius of
#' # 0.5 and a minimum cluster size of 5 is given below:
#' 
#' \donttest{
#' output <- dbscan(input=input, epsilon=0.5, min_size=5)
#' }
dbscan <- function(input,
                   epsilon=NA,
                   min_size=NA,
                   naive=FALSE,
                   selection_type=NA,
                   single_mode=FALSE,
                   tree_type=NA,
                   verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("DBSCAN clustering")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(epsilon, NA)) {
    IO_SetParamDouble("epsilon", epsilon)
  }

  if (!identical(min_size, NA)) {
    IO_SetParamInt("min_size", min_size)
  }

  if (!identical(naive, FALSE)) {
    IO_SetParamBool("naive", naive)
  }

  if (!identical(selection_type, NA)) {
    IO_SetParamString("selection_type", selection_type)
  }

  if (!identical(single_mode, FALSE)) {
    IO_SetParamBool("single_mode", single_mode)
  }

  if (!identical(tree_type, NA)) {
    IO_SetParamString("tree_type", tree_type)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("assignments")
  IO_SetPassed("centroids")

  # Call the program.
  dbscan_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "assignments" = IO_GetParamURow("assignments"),
      "centroids" = IO_GetParamMat("centroids")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
