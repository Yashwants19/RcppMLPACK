#' @title Softmax Regression
#'
#' @description
#' An implementation of softmax regression for classification, which is a
#' multiclass generalization of logistic regression.  Given labeled data, a
#' softmax regression model can be trained and saved for future use, or, a
#' pre-trained softmax regression model can be used for classification of new
#' points.
#'
#' @param input_model File containing existing model (parameters).
#' @param labels A matrix containing labels (0 or 1) for the points in the training
#'   set (y). The labels must order as a row.
#' @param lambda L2-regularization constant  Default value "0.0001".
#' @param max_iterations Maximum number of iterations before termination.  Default
#'   value "400".
#' @param no_intercept Do not add the intercept term to the model.  Default value
#'   "FALSE".
#' @param number_of_classes Number of classes for classification; if unspecified (or
#'   0), the number of classes found in the labels will be used.  Default value
#'   "0".
#' @param test Matrix containing test dataset.
#' @param test_labels Matrix containing test labels.
#' @param training A matrix containing the training set (the matrix of predictors,
#'   X).
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output_model}{File to save trained softmax regression model to.}
#' \item{predictions}{Matrix to save predictions for test dataset into.}
#'
#' @details
#' This program performs softmax regression, a generalization of logistic
#' regression to the multiclass case, and has support for L2 regularization. 
#' The program is able to train a model, load  an existing model, and give
#' predictions (and optionally their accuracy) for test data.
#' 
#' Training a softmax regression model is done by giving a file of training
#' points with the "training" parameter and their corresponding labels with the
#' "labels" parameter. The number of classes can be manually specified with the
#' "number_of_classes" parameter, and the maximum number of iterations of the
#' L-BFGS optimizer can be specified with the "max_iterations" parameter.  The
#' L2 regularization constant can be specified with the "lambda" parameter and
#' if an intercept term is not desired in the model, the "no_intercept"
#' parameter can be specified.
#' 
#' The trained model can be saved with the "output_model" output parameter. If
#' training is not desired, but only testing is, a model can be loaded with the
#' "input_model" parameter.  At the current time, a loaded model cannot be
#' trained further, so specifying both "input_model" and "training" is not
#' allowed.
#' 
#' The program is also able to evaluate a model on test data.  A test dataset
#' can be specified with the "test" parameter. Class predictions can be saved
#' with the "predictions" output parameter.  If labels are specified for the
#' test data with the "test_labels" parameter, then the program will print the
#' accuracy of the predictions on the given test set and its corresponding
#' labels.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to train a softmax regression model on the data "dataset" with
#' # labels "labels" with a maximum of 1000 iterations for training, saving the
#' # trained model to "sr_model", the following command can be used: 
#' 
#' \donttest{
#' output <- softmax_regression(training=dataset, labels=labels)
#' sr_model <- output$output_model
#' }
#' 
#' # Then, to use "sr_model" to classify the test points in "test_points",
#' # saving the output predictions to "predictions", the following command can
#' # be used:
#' 
#' \donttest{
#' output <- softmax_regression(input_model=sr_model, test=test_points)
#' predictions <- output$predictions
#' }
softmax_regression <- function(input_model=NA,
                               labels=NA,
                               lambda=NA,
                               max_iterations=NA,
                               no_intercept=FALSE,
                               number_of_classes=NA,
                               test=NA,
                               test_labels=NA,
                               training=NA,
                               verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Softmax Regression")

  # Process each input argument before calling mlpackMain().
  if (!identical(input_model, NA)) {
    IO_SetParamSoftmaxRegressionPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(lambda, NA)) {
    IO_SetParamDouble("lambda", lambda)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(no_intercept, FALSE)) {
    IO_SetParamBool("no_intercept", no_intercept)
  }

  if (!identical(number_of_classes, NA)) {
    IO_SetParamInt("number_of_classes", number_of_classes)
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (!identical(test_labels, NA)) {
    IO_SetParamURow("test_labels", to_matrix(test_labels))
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
  softmax_regression_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamSoftmaxRegressionPtr("output_model")
  attr(output_model, "type") <- "SoftmaxRegression"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = IO_GetParamURow("predictions")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
