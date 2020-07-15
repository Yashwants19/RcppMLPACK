#' @title Approximate furthest neighbor search
#'
#' @description
#' An implementation of two strategies for furthest neighbor search.  This can
#' be used to compute the furthest neighbor of query point(s) from a set of
#' points; furthest neighbor models can be saved and reused with future query
#' point(s).
#'
#' @param algorithm Algorithm to use: 'ds' or 'qdafn'.  Default value "ds".
#' @param calculate_error If set, calculate the average distance error for the first
#'   furthest neighbor only.  Default value "FALSE".
#' @param exact_distances Matrix containing exact distances to furthest neighbors;
#'   this can be used to avoid explicit calculation when --calculate_error is
#'   set.
#' @param input_model File containing input model.
#' @param k Number of furthest neighbors to search for.  Default value "0".
#' @param num_projections Number of projections to use in each hash table.  Default
#'   value "5".
#' @param num_tables Number of hash tables to use.  Default value "5".
#' @param query Matrix containing query points.
#' @param reference Matrix containing the reference dataset.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{distances}{Matrix to save furthest neighbor distances to.}
#' \item{neighbors}{Matrix to save neighbor indices to.}
#' \item{output_model}{File to save output model to.}
#'
#' @details
#' This program implements two strategies for furthest neighbor search. These
#' strategies are:
#' 
#'  - The 'qdafn' algorithm from "Approximate Furthest Neighbor in High
#' Dimensions" by R. Pagh, F. Silvestri, J. Sivertsen, and M. Skala, in
#' Similarity Search and Applications 2015 (SISAP).
#'  - The 'DrusillaSelect' algorithm from "Fast approximate furthest neighbors
#' with data-dependent candidate selection", by R.R. Curtin and A.B. Gardner, in
#' Similarity Search and Applications 2016 (SISAP).
#' 
#' These two strategies give approximate results for the furthest neighbor
#' search problem and can be used as fast replacements for other furthest
#' neighbor techniques such as those found in the mlpack_kfn program.  Note that
#' typically, the 'ds' algorithm requires far fewer tables and projections than
#' the 'qdafn' algorithm.
#' 
#' Specify a reference set (set to search in) with "reference", specify a query
#' set with "query", and specify algorithm parameters with "num_tables" and
#' "num_projections" (or don't and defaults will be used).  The algorithm to be
#' used (either 'ds'---the default---or 'qdafn')  may be specified with
#' "algorithm".  Also specify the number of neighbors to search for with "k".
#' 
#' Note that for 'qdafn' in lower dimensions, "num_projections" may need to be
#' set to a high value in order to return results for each query point.
#' 
#' If no query set is specified, the reference set will be used as the query
#' set.  The "output_model" output parameter may be used to store the built
#' model, and an input model may be loaded instead of specifying a reference set
#' with the "input_model" option.
#' 
#' Results for each query point can be stored with the "neighbors" and
#' "distances" output parameters.  Each row of these output matrices holds the k
#' distances or neighbor indices for each query point.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to find the 5 approximate furthest neighbors with
#' # "reference_set" as the reference set and "query_set" as the query set using
#' # DrusillaSelect, storing the furthest neighbor indices to "neighbors" and
#' # the furthest neighbor distances to "distances", one could call
#' 
#' \donttest{
#' output <- approx_kfn(query=query_set, reference=reference_set, k=5,
#'   algorithm="ds")
#' neighbors <- output$neighbors
#' distances <- output$distances
#' }
#' 
#' # and to perform approximate all-furthest-neighbors search with k=1 on the
#' # set "data" storing only the furthest neighbor distances to "distances", one
#' # could call
#' 
#' \donttest{
#' output <- approx_kfn(reference=reference_set, k=1)
#' distances <- output$distances
#' }
#' 
#' # A trained model can be re-used.  If a model has been previously saved to
#' # "model", then we may find 3 approximate furthest neighbors on a query set
#' # "new_query_set" using that model and store the furthest neighbor indices
#' # into "neighbors" by calling
#' 
#' \donttest{
#' output <- approx_kfn(input_model=model, query=new_query_set, k=3)
#' neighbors <- output$neighbors
#' }
approx_kfn <- function(algorithm=NA,
                       calculate_error=FALSE,
                       exact_distances=NA,
                       input_model=NA,
                       k=NA,
                       num_projections=NA,
                       num_tables=NA,
                       query=NA,
                       reference=NA,
                       verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Approximate furthest neighbor search")

  # Process each input argument before calling mlpackMain().
  if (!identical(algorithm, NA)) {
    IO_SetParamString("algorithm", algorithm)
  }

  if (!identical(calculate_error, FALSE)) {
    IO_SetParamBool("calculate_error", calculate_error)
  }

  if (!identical(exact_distances, NA)) {
    IO_SetParamMat("exact_distances", to_matrix(exact_distances))
  }

  if (!identical(input_model, NA)) {
    IO_SetParamApproxKFNModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    IO_SetParamInt("k", k)
  }

  if (!identical(num_projections, NA)) {
    IO_SetParamInt("num_projections", num_projections)
  }

  if (!identical(num_tables, NA)) {
    IO_SetParamInt("num_tables", num_tables)
  }

  if (!identical(query, NA)) {
    IO_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    IO_SetParamMat("reference", to_matrix(reference))
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
  approx_kfn_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamApproxKFNModelPtr("output_model")
  attr(output_model, "type") <- "ApproxKFNModel"

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
