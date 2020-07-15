#' @title k-Nearest-Neighbors Search
#'
#' @description
#' An implementation of k-nearest-neighbor search using single-tree and
#' dual-tree algorithms.  Given a set of reference points and query points, this
#' can find the k nearest neighbors in the reference set of each query point
#' using trees; trees that are built can be saved for future use.
#'
#' @param algorithm Type of neighbor search: 'naive', 'single_tree', 'dual_tree',
#'   'greedy'.  Default value "dual_tree".
#' @param epsilon If specified, will do approximate nearest neighbor search with
#'   given relative error.  Default value "0".
#' @param input_model Pre-trained kNN model.
#' @param k Number of nearest neighbors to find.  Default value "0".
#' @param leaf_size Leaf size for tree building (used for kd-trees, vp trees, random
#'   projection trees, UB trees, R trees, R* trees, X trees, Hilbert R trees, R+
#'   trees, R++ trees, spill trees, and octrees).  Default value "20".
#' @param query Matrix containing query points (optional).
#' @param random_basis Before tree-building, project the data onto a random
#'   orthogonal basis.  Default value "FALSE".
#' @param reference Matrix containing the reference dataset.
#' @param rho Balance threshold (only valid for spill trees).  Default value "0.7".
#' @param seed Random seed (if 0, std::time(NULL) is used).  Default value "0".
#' @param tau Overlapping size (only valid for spill trees).  Default value "0".
#' @param tree_type Type of tree to use: 'kd', 'vp', 'rp', 'max-rp', 'ub', 'cover',
#'   'r', 'r-star', 'x', 'ball', 'hilbert-r', 'r-plus', 'r-plus-plus', 'spill',
#'   'oct'.  Default value "kd".
#' @param true_distances Matrix of true distances to compute the effective error
#'   (average relative error) (it is printed when -v is specified).
#' @param true_neighbors Matrix of true neighbors to compute the recall (it is
#'   printed when -v is specified).
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{distances}{Matrix to output distances into.}
#' \item{neighbors}{Matrix to output neighbors into.}
#' \item{output_model}{If specified, the kNN model will be output here.}
#'
#' @details
#' This program will calculate the k-nearest-neighbors of a set of points using
#' kd-trees or cover trees (cover tree support is experimental and may be slow).
#' You may specify a separate set of reference points and query points, or just
#' a reference set which will be used as both the reference and query set.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following command will calculate the 5 nearest neighbors
#' # of each point in "input" and store the distances in "distances" and the
#' # neighbors in "neighbors": 
#' 
#' \donttest{
#' output <- knn(k=5, reference=input)
#' neighbors <- output$neighbors
#' distances <- output$distances
#' }
#' 
#' # The output is organized such that row i and column j in the neighbors
#' # output matrix corresponds to the index of the point in the reference set
#' # which is the j'th nearest neighbor from the point in the query set with
#' # index i.  Row j and column i in the distances output matrix corresponds to
#' # the distance between those two points.
knn <- function(algorithm=NA,
                epsilon=NA,
                input_model=NA,
                k=NA,
                leaf_size=NA,
                query=NA,
                random_basis=FALSE,
                reference=NA,
                rho=NA,
                seed=NA,
                tau=NA,
                tree_type=NA,
                true_distances=NA,
                true_neighbors=NA,
                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("k-Nearest-Neighbors Search")

  # Process each input argument before calling mlpackMain().
  if (!identical(algorithm, NA)) {
    IO_SetParamString("algorithm", algorithm)
  }

  if (!identical(epsilon, NA)) {
    IO_SetParamDouble("epsilon", epsilon)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamKNNModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    IO_SetParamInt("k", k)
  }

  if (!identical(leaf_size, NA)) {
    IO_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(query, NA)) {
    IO_SetParamMat("query", to_matrix(query))
  }

  if (!identical(random_basis, FALSE)) {
    IO_SetParamBool("random_basis", random_basis)
  }

  if (!identical(reference, NA)) {
    IO_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(rho, NA)) {
    IO_SetParamDouble("rho", rho)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(tau, NA)) {
    IO_SetParamDouble("tau", tau)
  }

  if (!identical(tree_type, NA)) {
    IO_SetParamString("tree_type", tree_type)
  }

  if (!identical(true_distances, NA)) {
    IO_SetParamMat("true_distances", to_matrix(true_distances))
  }

  if (!identical(true_neighbors, NA)) {
    IO_SetParamUMat("true_neighbors", to_matrix(true_neighbors))
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("distances")
  IO_SetPassed("neighbors")
  IO_SetPassed("output_model")

  # Call the program.
  knn_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamKNNModelPtr("output_model")
  attr(output_model, "type") <- "KNNModel"

  # Extract the results in order.
  out <- list(
      "distances" = IO_GetParamMat("distances"),
      "neighbors" = IO_GetParamUMat("neighbors"),
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
