#' @title Collaborative Filtering
#'
#' @description
#' An implementation of several collaborative filtering (CF) techniques for
#' recommender systems.  This can be used to train a new CF model, or use an
#' existing CF model to compute recommendations.
#'
#' @param algorithm Algorithm used for matrix factorization.  Default value
#'   "NMF" (character).
#' @param all_user_recommendations Generate recommendations for all users. 
#'   Default value "FALSE" (logical).
#' @param input_model Trained CF model to load (CFModel).
#' @param interpolation Algorithm used for weight interpolation.  Default
#'   value "average" (character).
#' @param iteration_only_termination Terminate only when the maximum number
#'   of iterations is reached.  Default value "FALSE" (logical).
#' @param max_iterations Maximum number of iterations. If set to zero,
#'   there is no limit on the number of iterations.  Default value "1000"
#'   (integer).
#' @param min_residue Residue required to terminate the factorization
#'   (lower values generally mean better fits).  Default value "1e-05"
#'   (numeric).
#' @param neighbor_search Algorithm used for neighbor search.  Default
#'   value "euclidean" (character).
#' @param neighborhood Size of the neighborhood of similar users to
#'   consider for each query user.  Default value "5" (integer).
#' @param normalization Normalization performed on the ratings.  Default
#'   value "none" (character).
#' @param query List of query users for which recommendations should be
#'   generated (integer matrix).
#' @param rank Rank of decomposed matrices (if 0, a heuristic is used to
#'   estimate the rank).  Default value "0" (integer).
#' @param recommendations Number of recommendations to generate for each
#'   query user.  Default value "5" (integer).
#' @param seed Set the random seed (0 uses std::time(NULL)).  Default value
#'   "0" (integer).
#' @param test Test set to calculate RMSE on (numeric matrix).
#' @param training Input dataset to perform CF on (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{Matrix that will store output recommendations (integer
#'   matrix).}
#' \item{output_model}{Output for trained CF model (CFModel).}
#'
#' @details
#' This program performs collaborative filtering (CF) on the given dataset.
#' Given a list of user, item and preferences (the "training" parameter), the
#' program will perform a matrix decomposition and then can perform a series of
#' actions related to collaborative filtering.  Alternately, the program can
#' load an existing saved CF model with the "input_model" parameter and then use
#' that model to provide recommendations or predict values.
#' 
#' The input matrix should be a 3-dimensional matrix of ratings, where the first
#' dimension is the user, the second dimension is the item, and the third
#' dimension is that user's rating of that item.  Both the users and items
#' should be numeric indices, not names. The indices are assumed to start from
#' 0.
#' 
#' A set of query users for which recommendations can be generated may be
#' specified with the "query" parameter; alternately, recommendations may be
#' generated for every user in the dataset by specifying the
#' "all_user_recommendations" parameter.  In addition, the number of
#' recommendations per user to generate can be specified with the
#' "recommendations" parameter, and the number of similar users (the size of the
#' neighborhood) to be considered when generating recommendations can be
#' specified with the "neighborhood" parameter.
#' 
#' For performing the matrix decomposition, the following optimization
#' algorithms can be specified via the "algorithm" parameter: 
#'  - 'RegSVD' -- Regularized SVD using a SGD optimizer
#'  - 'NMF' -- Non-negative matrix factorization with alternating least squares
#' update rules
#'  - 'BatchSVD' -- SVD batch learning
#'  - 'SVDIncompleteIncremental' -- SVD incomplete incremental learning
#'  - 'SVDCompleteIncremental' -- SVD complete incremental learning
#'  - 'BiasSVD' -- Bias SVD using a SGD optimizer
#'  - 'SVDPP' -- SVD++ using a SGD optimizer
#' 
#' 
#' The following neighbor search algorithms can be specified via the
#' "neighbor_search" parameter:
#'  - 'cosine'  -- Cosine Search Algorithm
#'  - 'euclidean'  -- Euclidean Search Algorithm
#'  - 'pearson'  -- Pearson Search Algorithm
#' 
#' 
#' The following weight interpolation algorithms can be specified via the
#' "interpolation" parameter:
#'  - 'average'  -- Average Interpolation Algorithm
#'  - 'regression'  -- Regression Interpolation Algorithm
#'  - 'similarity'  -- Similarity Interpolation Algorithm
#' 
#' 
#' The following ranking normalization algorithms can be specified via the
#' "normalization" parameter:
#'  - 'none'  -- No Normalization
#'  - 'item_mean'  -- Item Mean Normalization
#'  - 'overall_mean'  -- Overall Mean Normalization
#'  - 'user_mean'  -- User Mean Normalization
#'  - 'z_score'  -- Z-Score Normalization
#' 
#' A trained model may be saved to with the "output_model" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # To train a CF model on a dataset "training_set" using NMF for decomposition
#' # and saving the trained model to "model", one could call: 
#' 
#' \donttest{
#' output <- cf(training=training_set, algorithm="NMF")
#' model <- output$output_model
#' }
#' 
#' # Then, to use this model to generate recommendations for the list of users
#' # in the query set "users", storing 5 recommendations in "recommendations",
#' # one could call 
#' 
#' \donttest{
#' output <- cf(input_model=model, query=users, recommendations=5)
#' recommendations <- output$output
#' }
cf <- function(algorithm=NA,
               all_user_recommendations=FALSE,
               input_model=NA,
               interpolation=NA,
               iteration_only_termination=FALSE,
               max_iterations=NA,
               min_residue=NA,
               neighbor_search=NA,
               neighborhood=NA,
               normalization=NA,
               query=NA,
               rank=NA,
               recommendations=NA,
               seed=NA,
               test=NA,
               training=NA,
               verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Collaborative Filtering")

  # Process each input argument before calling mlpackMain().
  if (!identical(algorithm, NA)) {
    IO_SetParamString("algorithm", algorithm)
  }

  if (!identical(all_user_recommendations, FALSE)) {
    IO_SetParamBool("all_user_recommendations", all_user_recommendations)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamCFModelPtr("input_model", input_model)
  }

  if (!identical(interpolation, NA)) {
    IO_SetParamString("interpolation", interpolation)
  }

  if (!identical(iteration_only_termination, FALSE)) {
    IO_SetParamBool("iteration_only_termination", iteration_only_termination)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(min_residue, NA)) {
    IO_SetParamDouble("min_residue", min_residue)
  }

  if (!identical(neighbor_search, NA)) {
    IO_SetParamString("neighbor_search", neighbor_search)
  }

  if (!identical(neighborhood, NA)) {
    IO_SetParamInt("neighborhood", neighborhood)
  }

  if (!identical(normalization, NA)) {
    IO_SetParamString("normalization", normalization)
  }

  if (!identical(query, NA)) {
    IO_SetParamUMat("query", to_matrix(query))
  }

  if (!identical(rank, NA)) {
    IO_SetParamInt("rank", rank)
  }

  if (!identical(recommendations, NA)) {
    IO_SetParamInt("recommendations", recommendations)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (!identical(training, NA)) {
    IO_SetParamMat("training", to_matrix(training))
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")
  IO_SetPassed("output_model")

  # Call the program.
  cf_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamCFModelPtr("output_model")
  attr(output_model, "type") <- "CFModel"

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamUMat("output"),
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
