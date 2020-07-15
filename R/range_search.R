#' @title Range Search
#'
#' @description
#' An implementation of range search with single-tree and dual-tree algorithms. 
#' Given a set of reference points and a set of query points and a range, this
#' can find the set of reference points within the desired range for each query
#' point, and any trees built during the computation can be saved for reuse with
#' future range searches.
#'
#' @param input_model File containing pre-trained range search model.
#' @param leaf_size Leaf size for tree building (used for kd-trees, vp trees, random
#'   projection trees, UB trees, R trees, R* trees, X trees, Hilbert R trees, R+
#'   trees, R++ trees, and octrees).  Default value "20".
#' @param max Upper bound in range (if not specified, +inf will be used.  Default
#'   value "0".
#' @param min Lower bound in range.  Default value "0".
#' @param naive If true, O(n^2) naive mode is used for computation.  Default value
#'   "FALSE".
#' @param query File containing query points (optional).
#' @param random_basis Before tree-building, project the data onto a random
#'   orthogonal basis.  Default value "FALSE".
#' @param reference Matrix containing the reference dataset.
#' @param seed Random seed (if 0, std::time(NULL) is used).  Default value "0".
#' @param single_mode If true, single-tree search is used (as opposed to dual-tree
#'   search).  Default value "FALSE".
#' @param tree_type Type of tree to use: 'kd', 'vp', 'rp', 'max-rp', 'ub', 'cover',
#'   'r', 'r-star', 'x', 'ball', 'hilbert-r', 'r-plus', 'r-plus-plus', 'oct'. 
#'   Default value "kd".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{distances_file}{File to output distances into.  Default value "".}
#' \item{neighbors_file}{File to output neighbors into.  Default value "".}
#' \item{output_model}{If specified, the range search model will be saved to the
#'   given file.}
#'
#' @details
#' This program implements range search with a Euclidean distance metric. For a
#' given query point, a given range, and a given set of reference points, the
#' program will return all of the reference points with distance to the query
#' point in the given range.  This is performed for an entire set of query
#' points. You may specify a separate set of reference and query points, or only
#' a reference set -- which is then used as both the reference and query set. 
#' The given range is taken to be inclusive (that is, points with a distance
#' exactly equal to the minimum and maximum of the range are included in the
#' results).
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following will calculate the points within the range [2,
#' # 5] of each point in "input" and store the distances in"distances" and the
#' # neighbors in "neighbors"
#' 
#' \donttest{
#' output <- range_search(min=2, max=5)
#' input <- output$distances_file
#' distances <- output$distances_file
#' neighbors <- output$neighbors_file
#' }
#' 
#' # The output files are organized such that line i corresponds to the points
#' # found for query point i.  Because sometimes 0 points may be found in the
#' # given range, lines of the output files may be empty.  The points are not
#' # ordered in any specific manner.
#' # 
#' # Because the number of points returned for each query point may differ, the
#' # resultant CSV-like files may not be loadable by many programs.  However, at
#' # this time a better way to store this non-square result is not known.  As a
#' # result, any output files will be written as CSVs in this manner, regardless
#' # of the given extension.
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
  # Restore IO settings.
  IO_RestoreSettings("Range Search")

  # Process each input argument before calling mlpackMain().
  if (!identical(input_model, NA)) {
    IO_SetParamRSModelPtr("input_model", input_model)
  }

  if (!identical(leaf_size, NA)) {
    IO_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(max, NA)) {
    IO_SetParamDouble("max", max)
  }

  if (!identical(min, NA)) {
    IO_SetParamDouble("min", min)
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

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
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
  IO_SetPassed("distances_file")
  IO_SetPassed("neighbors_file")
  IO_SetPassed("output_model")

  # Call the program.
  range_search_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamRSModelPtr("output_model")
  attr(output_model, "type") <- "RSModel"

  # Extract the results in order.
  out <- list(
      "distances_file" = IO_GetParamString("distances_file"),
      "neighbors_file" = IO_GetParamString("neighbors_file"),
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
