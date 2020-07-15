#' @title K-Approximate-Nearest-Neighbor Search with LSH
#'
#' @description
#' An implementation of approximate k-nearest-neighbor search with
#' locality-sensitive hashing (LSH).  Given a set of reference points and a set
#' of query points, this will compute the k approximate nearest neighbors of
#' each query point in the reference set; models can be saved for future use.
#'
#' @param bucket_size The size of a bucket in the second level hash.  Default value
#'   "500".
#' @param hash_width The hash width for the first-level hashing in the LSH
#'   preprocessing. By default, the LSH class automatically estimates a hash
#'   width for its use.  Default value "0".
#' @param input_model Input LSH model.
#' @param k Number of nearest neighbors to find.  Default value "0".
#' @param num_probes Number of additional probes for multiprobe LSH; if 0,
#'   traditional LSH is used.  Default value "0".
#' @param projections The number of hash functions for each table  Default value
#'   "10".
#' @param query Matrix containing query points (optional).
#' @param reference Matrix containing the reference dataset.
#' @param second_hash_size The size of the second level hash table.  Default value
#'   "99901".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param tables The number of hash tables to be used.  Default value "30".
#' @param true_neighbors Matrix of true neighbors to compute recall with (the recall
#'   is printed when -v is specified).
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{distances}{Matrix to output distances into.}
#' \item{neighbors}{Matrix to output neighbors into.}
#' \item{output_model}{Output for trained LSH model.}
#'
#' @details
#' This program will calculate the k approximate-nearest-neighbors of a set of
#' points using locality-sensitive hashing. You may specify a separate set of
#' reference points and query points, or just a reference set which will be used
#' as both the reference and query set. 
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, the following will return 5 neighbors from the data for each
#' # point in "input" and store the distances in "distances" and the neighbors
#' # in "neighbors":
#' 
#' \donttest{
#' output <- lsh(k=5, reference=input)
#' distances <- output$distances
#' neighbors <- output$neighbors
#' }
#' 
#' # The output is organized such that row i and column j in the neighbors
#' # output corresponds to the index of the point in the reference set which is
#' # the j'th nearest neighbor from the point in the query set with index i. 
#' # Row j and column i in the distances output file corresponds to the distance
#' # between those two points.
#' # 
#' # Because this is approximate-nearest-neighbors search, results may be
#' # different from run to run.  Thus, the "seed" parameter can be specified to
#' # set the random seed.
#' # 
#' # This program also has many other parameters to control its functionality;
#' # see the parameter-specific documentation for more information.
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
  # Restore IO settings.
  IO_RestoreSettings("K-Approximate-Nearest-Neighbor Search with LSH")

  # Process each input argument before calling mlpackMain().
  if (!identical(bucket_size, NA)) {
    IO_SetParamInt("bucket_size", bucket_size)
  }

  if (!identical(hash_width, NA)) {
    IO_SetParamDouble("hash_width", hash_width)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamLSHSearchPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    IO_SetParamInt("k", k)
  }

  if (!identical(num_probes, NA)) {
    IO_SetParamInt("num_probes", num_probes)
  }

  if (!identical(projections, NA)) {
    IO_SetParamInt("projections", projections)
  }

  if (!identical(query, NA)) {
    IO_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    IO_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(second_hash_size, NA)) {
    IO_SetParamInt("second_hash_size", second_hash_size)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(tables, NA)) {
    IO_SetParamInt("tables", tables)
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
  lsh_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamLSHSearchPtr("output_model")
  attr(output_model, "type") <- "LSHSearch"

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
