#' @title Fast Euclidean Minimum Spanning Tree
#'
#' @description
#' An implementation of the Dual-Tree Boruvka algorithm for computing the
#' Euclidean minimum spanning tree of a set of input points.
#'
#' @param input Input data matrix.
#' @param leaf_size Leaf size in the kd-tree.  One-element leaves give the
#'   empirically best performance, but at the cost of greater memory
#'   requirements.  Default value "1".
#' @param naive Compute the MST using O(n^2) naive algorithm.  Default value
#'   "FALSE".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{Output data.  Stored as an edge list.}
#'
#' @details
#' This program can compute the Euclidean minimum spanning tree of a set of
#' input points using the dual-tree Boruvka algorithm.
#' 
#' The set to calculate the minimum spanning tree of is specified with the
#' "input" parameter, and the output may be saved with the "output" output
#' parameter.
#' 
#' The "leaf_size" parameter controls the leaf size of the kd-tree that is used
#' to calculate the minimum spanning tree, and if the "naive" option is given,
#' then brute-force search is used (this is typically much slower in low
#' dimensions).  The leaf size does not affect the results, but it may have some
#' effect on the runtime of the algorithm.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the minimum spanning tree of the input dataset "data" can be
#' # calculated with a leaf size of 20 and stored as "spanning_tree" using the
#' # following command:
#' 
#' \donttest{
#' output <- emst(input=data, leaf_size=20)
#' spanning_tree <- output$output
#' }
#' 
#' # The output matrix is a three-dimensional matrix, where each row indicates
#' # an edge.  The first dimension corresponds to the lesser index of the edge;
#' # the second dimension corresponds to the greater index of the edge; and the
#' # third column corresponds to the distance between the two points.
emst <- function(input,
                 leaf_size=NA,
                 naive=FALSE,
                 verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Fast Euclidean Minimum Spanning Tree")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(leaf_size, NA)) {
    IO_SetParamInt("leaf_size", leaf_size)
  }

  if (!identical(naive, FALSE)) {
    IO_SetParamBool("naive", naive)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  emst_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
