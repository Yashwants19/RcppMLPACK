#' @title AdaBoost
#'
#' @description
#' An implementation of the AdaBoost.MH (Adaptive Boosting) algorithm for
#' classification.  This can be used to train an AdaBoost model on labeled data
#' or use an existing AdaBoost model to predict the classes of new points.
#'
#' @param input_model Input AdaBoost model.
#' @param iterations The maximum number of boosting iterations to be run (0 will run
#'   until convergence.)  Default value "1000".
#' @param labels Labels for the training set.
#' @param test Test dataset.
#' @param tolerance The tolerance for change in values of the weighted error during
#'   training.  Default value "1e-10".
#' @param training Dataset for training AdaBoost.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#' @param weak_learner The type of weak learner to use: 'decision_stump', or
#'   'perceptron'.  Default value "decision_stump".
#'
#' @return A list with several components:
#' \item{output}{Predicted labels for the test set.}
#' \item{output_model}{Output trained AdaBoost model.}
#' \item{predictions}{Predicted labels for the test set.}
#' \item{probabilities}{Predicted class probabilities for each point in the test
#'   set.}
#'
#' @details
#' This program implements the AdaBoost (or Adaptive Boosting) algorithm. The
#' variant of AdaBoost implemented here is AdaBoost.MH. It uses a weak learner,
#' either decision stumps or perceptrons, and over many iterations, creates a
#' strong learner that is a weighted ensemble of weak learners. It runs these
#' iterations until a tolerance value is crossed for change in the value of the
#' weighted training error.
#' 
#' For more information about the algorithm, see the paper "Improved Boosting
#' Algorithms Using Confidence-Rated Predictions", by R.E. Schapire and Y.
#' Singer.
#' 
#' This program allows training of an AdaBoost model, and then application of
#' that model to a test dataset.  To train a model, a dataset must be passed
#' with the "training" option.  Labels can be given with the "labels" option; if
#' no labels are specified, the labels will be assumed to be the last column of
#' the input dataset.  Alternately, an AdaBoost model may be loaded with the
#' "input_model" option.
#' 
#' Once a model is trained or loaded, it may be used to provide class
#' predictions for a given test dataset.  A test dataset may be specified with
#' the "test" parameter.  The predicted classes for each point in the test
#' dataset are output to the "predictions" output parameter.  The AdaBoost model
#' itself is output to the "output_model" output parameter.
#' 
#' Note: the following parameter is deprecated and will be removed in mlpack
#' 4.0.0: "output".
#' Use "predictions" instead of "output".
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to run AdaBoost on an input dataset "data" with labels
#' # "labels"and perceptrons as the weak learner type, storing the trained model
#' # in "model", one could use the following command: 
#' 
#' \donttest{
#' output <- adaboost(training=data, labels=labels, weak_learner="perceptron")
#' model <- output$output_model
#' }
#' 
#' # Similarly, an already-trained model in "model" can be used to provide class
#' # predictions from test data "test_data" and store the output in
#' # "predictions" with the following command: 
#' 
#' \donttest{
#' output <- adaboost(input_model=model, test=test_data)
#' predictions <- output$predictions
#' }
adaboost <- function(input_model=NA,
                     iterations=NA,
                     labels=NA,
                     test=NA,
                     tolerance=NA,
                     training=NA,
                     verbose=FALSE,
                     weak_learner=NA) {
  # Restore IO settings.
  IO_RestoreSettings("AdaBoost")

  # Process each input argument before calling mlpackMain().
  if (!identical(input_model, NA)) {
    IO_SetParamAdaBoostModelPtr("input_model", input_model)
  }

  if (!identical(iterations, NA)) {
    IO_SetParamInt("iterations", iterations)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (!identical(tolerance, NA)) {
    IO_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(training, NA)) {
    IO_SetParamMat("training", to_matrix(training))
  }

  if (!identical(weak_learner, NA)) {
    IO_SetParamString("weak_learner", weak_learner)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")
  IO_SetPassed("output_model")
  IO_SetPassed("predictions")
  IO_SetPassed("probabilities")

  # Call the program.
  adaboost_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamAdaBoostModelPtr("output_model")
  attr(output_model, "type") <- "AdaBoostModel"

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamURow("output"),
      "output_model" = output_model,
      "predictions" = IO_GetParamURow("predictions"),
      "probabilities" = IO_GetParamMat("probabilities")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
