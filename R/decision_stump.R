#' @title Decision Stump
#'
#' @description
#' An implementation of a decision stump, which is a single-level decision tree.
#'  Given labeled data, a new decision stump can be trained; or, an existing
#' decision stump can be used to classify points.
#'
#' @param bucket_size The minimum number of training points in each decision stump
#'   bucket.  Default value "6".
#' @param input_model Decision stump model to load.
#' @param labels Labels for the training set. If not specified, the labels are
#'   assumed to be the last row of the training data.
#' @param test A dataset to calculate predictions for.
#' @param training The dataset to train on.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output_model}{Output decision stump model to save.}
#' \item{predictions}{The output matrix that will hold the predicted labels for the
#'   test set.}
#'
#' @details
#' This program implements a decision stump, which is a single-level decision
#' tree.  The decision stump will split on one dimension of the input data, and
#' will split into multiple buckets.  The dimension and bins are selected by
#' maximizing the information gain of the split.  Optionally, the minimum number
#' of training points in each bin can be specified with the "bucket_size"
#' parameter.
#' 
#' The decision stump is parameterized by a splitting dimension and a vector of
#' values that denote the splitting values of each bin.
#' 
#' This program enables several applications: a decision tree may be trained or
#' loaded, and then that decision tree may be used to classify a given set of
#' test points.  The decision tree may also be saved to a file for later usage.
#' 
#' To train a decision stump, training data should be passed with the "training"
#' parameter, and their corresponding labels should be passed with the "labels"
#' option.  Optionally, if "labels" is not specified, the labels are assumed to
#' be the last dimension of the training dataset.  The "bucket_size" parameter
#' controls the minimum number of training points in each decision stump bucket.
#' 
#' For classifying a test set, a decision stump may be loaded with the
#' "input_model" parameter (useful for the situation where a stump has already
#' been trained), and a test set may be specified with the "test" parameter. 
#' The predicted labels can be saved with the "predictions" output parameter.
#' 
#' Because decision stumps are trained in batch, retraining does not make sense
#' and thus it is not possible to pass both "training" and "input_model";
#' instead, simply build a new decision stump with the training data.
#' 
#' After training, a decision stump can be saved with the "output_model" output
#' parameter.  That stump may later be re-used in subsequent calls to this
#' program (or others).
#' @author
#' MLPACK Developers
#'
#' @export

decision_stump <- function(bucket_size=NA,
                           input_model=NA,
                           labels=NA,
                           test=NA,
                           training=NA,
                           verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Decision Stump")

  # Process each input argument before calling mlpackMain().
  if (!identical(bucket_size, NA)) {
    IO_SetParamInt("bucket_size", bucket_size)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamDSModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
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
  IO_SetPassed("output_model")
  IO_SetPassed("predictions")

  # Call the program.
  decision_stump_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamDSModelPtr("output_model")
  attr(output_model, "type") <- "DSModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = IO_GetParamURow("predictions")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
