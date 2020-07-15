#' @title Parametric Naive Bayes Classifier
#'
#' @description
#' An implementation of the Naive Bayes Classifier, used for classification.
#' Given labeled data, an NBC model can be trained and saved, or, a pre-trained
#' model can be used for classification.
#'
#' @param incremental_variance The variance of each class will be calculated
#'   incrementally.  Default value "FALSE".
#' @param input_model Input Naive Bayes model.
#' @param labels A file containing labels for the training set.
#' @param test A matrix containing the test set.
#' @param training A matrix containing the training set.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{The matrix in which the predicted labels for the test set will be
#'   written (deprecated).}
#' \item{output_model}{File to save trained Naive Bayes model to.}
#' \item{output_probs}{The matrix in which the predicted probability of labels for
#'   the test set will be written (deprecated).}
#' \item{predictions}{The matrix in which the predicted labels for the test set will
#'   be written.}
#' \item{probabilities}{The matrix in which the predicted probability of labels for
#'   the test set will be written.}
#'
#' @details
#' This program trains the Naive Bayes classifier on the given labeled training
#' set, or loads a model from the given model file, and then may use that
#' trained model to classify the points in a given test set.
#' 
#' The training set is specified with the "training" parameter.  Labels may be
#' either the last row of the training set, or alternately the "labels"
#' parameter may be specified to pass a separate matrix of labels.
#' 
#' If training is not desired, a pre-existing model may be loaded with the
#' "input_model" parameter.
#' 
#' 
#' 
#' The "incremental_variance" parameter can be used to force the training to use
#' an incremental algorithm for calculating variance.  This is slower, but can
#' help avoid loss of precision in some cases.
#' 
#' If classifying a test set is desired, the test set may be specified with the
#' "test" parameter, and the classifications may be saved with the
#' "predictions"predictions  parameter.  If saving the trained model is desired,
#' this may be done with the "output_model" output parameter.
#' 
#' Note: the "output" and "output_probs" parameters are deprecated and will be
#' removed in mlpack 4.0.0.  Use "predictions" and "probabilities" instead.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to train a Naive Bayes classifier on the dataset "data" with
#' # labels "labels" and save the model to "nbc_model", the following command
#' # may be used:
#' 
#' \donttest{
#' output <- nbc(training=data, labels=labels)
#' nbc_model <- output$output_model
#' }
#' 
#' # Then, to use "nbc_model" to predict the classes of the dataset "test_set"
#' # and save the predicted classes to "predictions", the following command may
#' # be used:
#' 
#' \donttest{
#' output <- nbc(input_model=nbc_model, test=test_set)
#' predictions <- output$output
#' }
nbc <- function(incremental_variance=FALSE,
                input_model=NA,
                labels=NA,
                test=NA,
                training=NA,
                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Parametric Naive Bayes Classifier")

  # Process each input argument before calling mlpackMain().
  if (!identical(incremental_variance, FALSE)) {
    IO_SetParamBool("incremental_variance", incremental_variance)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamNBCModelPtr("input_model", input_model)
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
  IO_SetPassed("output")
  IO_SetPassed("output_model")
  IO_SetPassed("output_probs")
  IO_SetPassed("predictions")
  IO_SetPassed("probabilities")

  # Call the program.
  nbc_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamNBCModelPtr("output_model")
  attr(output_model, "type") <- "NBCModel"

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamURow("output"),
      "output_model" = output_model,
      "output_probs" = IO_GetParamMat("output_probs"),
      "predictions" = IO_GetParamURow("predictions"),
      "probabilities" = IO_GetParamMat("probabilities")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
