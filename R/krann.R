#' @title K-Rank-Approximate-Nearest-Neighbors (kRANN)
#'
#' @description
#' An implementation of rank-approximate k-nearest-neighbor search (kRANN) 
#' using single-tree and dual-tree algorithms.  Given a set of reference points
#' and query points, this can find the k nearest neighbors in the reference set
#' of each query point using trees; trees that are built can be saved for future
#' use.
#'
#' @param alpha The desired success probability.  Default value "0.95".
#' @param first_leaf_exact The flag to trigger sampling only after exactly exploring
#'   the first leaf.  Default value "FALSE".
#' @param input_model Pre-trained kNN model.
#' @param k Number of nearest neighbors to find.  Default value "0".
#' @param leaf_size Leaf size for tree building (used for kd-trees, UB trees, R
#'   trees, R* trees, X trees, Hilbert R trees, R+ trees, R++ trees, and
#'   octrees).  Default value "20".
#' @param naive If true, sampling will be done without using a tree.  Default value
#'   "FALSE".
#' @param query Matrix containing query points (optional).
#' @param random_basis Before tree-building, project the data onto a random
#'   orthogonal basis.  Default value "FALSE".
#' @param reference Matrix containing the reference dataset.
#' @param sample_at_leaves The flag to trigger sampling at leaves.  Default value
#'   "FALSE".
#' @param seed Random seed (if 0, std::time(NULL) is used).  Default value "0".
#' @param single_mode If true, single-tree search is used (as opposed to dual-tree
#'   search.  Default value "FALSE".
#' @param single_sample_limit The limit on the maximum number of samples (and hence
#'   the largest node you can approximate).  Default value "20".
#' @param tau The allowed rank-error in terms of the percentile of the data.  Default
#'   value "5".
#' @param tree_type Type of tree to use: 'kd', 'ub', 'cover', 'r', 'x', 'r-star',
#'   'hilbert-r', 'r-plus', 'r-plus-plus', 'oct'.  Default value "kd".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{distances}{Matrix to output distances into.}
#' \item{neighbors}{Matrix to output neighbors into.}
#' \item{output_model}{If specified, the kNN model will be output here.}
#'
#' @details
#' This program will calculate the k rank-approximate-nearest-neighbors of a set
#' of points. You may specify a separate set of reference points and query
#' points, or just a reference set which will be used as both the reference and
#' query set. You must specify the rank approximation (in %) (and optionally the
#' success probability).
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following will return 5 neighbors from the top 0.1% of the
#' # data (with probability 0.95) for each point in "input" and store the
#' # distances in "distances" and the neighbors in "neighbors.csv":
#' 
#' \donttest{
#' output <- krann(reference=input, k=5, tau=0.1)
#' distances <- output$distances
#' neighbors <- output$neighbors
#' }
#' 
#' # Note that tau must be set such that the number of points in the
#' # corresponding percentile of the data is greater than k.  Thus, if we choose
#' # tau = 0.1 with a dataset of 1000 points and k = 5, then we are attempting
#' # to choose 5 nearest neighbors out of the closest 1 point -- this is invalid
#' # and the program will terminate with an error message.
#' # 
#' # The output matrices are organized such that row i and column j in the
#' # neighbors output file corresponds to the index of the point in the
#' # reference set which is the i'th nearest neighbor from the point in the
#' # query set with index j.  Row i and column j in the distances output file
#' # corresponds to the distance between those two points.
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
  # Restore IO settings.
  IO_RestoreSettings("K-Rank-Approximate-Nearest-Neighbors (kRANN)")

  # Process each input argument before calling mlpackMain().
  if (!identical(alpha, NA)) {
    IO_SetParamDouble("alpha", alpha)
  }

  if (!identical(first_leaf_exact, FALSE)) {
    IO_SetParamBool("first_leaf_exact", first_leaf_exact)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamRANNModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    IO_SetParamInt("k", k)
  }

  if (!identical(leaf_size, NA)) {
    IO_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(naive, FALSE)) {
    IO_SetParamBool("naive", naive)
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

  if (!identical(sample_at_leaves, FALSE)) {
    IO_SetParamBool("sample_at_leaves", sample_at_leaves)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(single_mode, FALSE)) {
    IO_SetParamBool("single_mode", single_mode)
  }

  if (!identical(single_sample_limit, NA)) {
    IO_SetParamInt("single_sample_limit", single_sample_limit)
  }

  if (!identical(tau, NA)) {
    IO_SetParamDouble("tau", tau)
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
  IO_SetPassed("distances")
  IO_SetPassed("neighbors")
  IO_SetPassed("output_model")

  # Call the program.
  krann_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamRANNModelPtr("output_model")
  attr(output_model, "type") <- "RANNModel"

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
