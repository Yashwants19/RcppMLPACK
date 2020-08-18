#' @title L2-regularized Logistic Regression and Prediction
#'
#' @description
#' An implementation of L2-regularized logistic regression for two-class
#' classification.  Given labeled data, a model can be trained and saved for
#' future use; or, a pre-trained model can be used to classify new points.
#'
#' @param batch_size Batch size for SGD.  Default value "64" (integer).
#' @param decision_boundary Decision boundary for prediction; if the
#'   logistic function for a point is less than the boundary, the class is taken
#'   to be 0; otherwise, the class is 1.  Default value "0.5" (numeric).
#' @param input_model Existing model (parameters) (LogisticRegression).
#' @param labels A matrix containing labels (0 or 1) for the points in the
#'   training set (y) (integer row).
#' @param lambda L2-regularization parameter for training.  Default value
#'   "0" (numeric).
#' @param max_iterations Maximum iterations for optimizer (0 indicates no
#'   limit).  Default value "10000" (integer).
#' @param optimizer Optimizer to use for training ('lbfgs' or 'sgd'). 
#'   Default value "lbfgs" (character).
#' @param step_size Step size for SGD optimizer.  Default value "0.01"
#'   (numeric).
#' @param test Matrix containing test dataset (numeric matrix).
#' @param tolerance Convergence tolerance for optimizer.  Default value
#'   "1e-10" (numeric).
#' @param training A matrix containing the training set (the matrix of
#'   predictors, X) (numeric matrix).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{If test data is specified, this matrix is where the
#'   predictions for the test set will be saved (integer row).}
#' \item{output_model}{Output for trained logistic regression model
#'   (LogisticRegression).}
#' \item{output_probabilities}{If test data is specified, this matrix is
#'   where the class probabilities for the test set will be saved (numeric
#'   matrix).}
#' \item{predictions}{If test data is specified, this matrix is where the
#'   predictions for the test set will be saved (integer row).}
#' \item{probabilities}{If test data is specified, this matrix is where the
#'   class probabilities for the test set will be saved (numeric matrix).}
#'
#' @details
#' An implementation of L2-regularized logistic regression using either the
#' L-BFGS optimizer or SGD (stochastic gradient descent).  This solves the
#' regression problem
#' 
#'   y = (1 / 1 + e^-(X * b))
#' 
#' where y takes values 0 or 1.
#' 
#' This program allows loading a logistic regression model (via the
#' "input_model" parameter) or training a logistic regression model given
#' training data (specified with the "training" parameter), or both those things
#' at once.  In addition, this program allows classification on a test dataset
#' (specified with the "test" parameter) and the classification results may be
#' saved with the "predictions" output parameter. The trained logistic
#' regression model may be saved using the "output_model" output parameter.
#' 
#' The training data, if specified, may have class labels as its last dimension.
#'  Alternately, the "labels" parameter may be used to specify a separate matrix
#' of labels.
#' 
#' When a model is being trained, there are many options.  L2 regularization (to
#' prevent overfitting) can be specified with the "lambda" option, and the
#' optimizer used to train the model can be specified with the "optimizer"
#' parameter.  Available options are 'sgd' (stochastic gradient descent) and
#' 'lbfgs' (the L-BFGS optimizer).  There are also various parameters for the
#' optimizer; the "max_iterations" parameter specifies the maximum number of
#' allowed iterations, and the "tolerance" parameter specifies the tolerance for
#' convergence.  For the SGD optimizer, the "step_size" parameter controls the
#' step size taken at each iteration by the optimizer.  The batch size for SGD
#' is controlled with the "batch_size" parameter. If the objective function for
#' your data is oscillating between Inf and 0, the step size is probably too
#' large.  There are more parameters for the optimizers, but the C++ interface
#' must be used to access these.
#' 
#' For SGD, an iteration refers to a single point. So to take a single pass over
#' the dataset with SGD, "max_iterations" should be set to the number of points
#' in the dataset.
#' 
#' Optionally, the model can be used to predict the responses for another matrix
#' of data points, if "test" is specified.  The "test" parameter can be
#' specified without the "training" parameter, so long as an existing logistic
#' regression model is given with the "input_model" parameter.  The output
#' predictions from the logistic regression model may be saved with the
#' "predictions" parameter.
#' 
#' Note : The following parameters are deprecated and will be removed in mlpack
#' 4: "output", "output_probabilities"
#' Use "predictions" instead of "output"
#' Use "probabilities" instead of "output_probabilities"
#' 
#' This implementation of logistic regression does not support the general
#' multi-class case but instead only the two-class case.  Any labels must be
#' either 0 or 1.  For more classes, see the softmax_regression program.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # As an example, to train a logistic regression model on the data '"data"'
#' # with labels '"labels"' with L2 regularization of 0.1, saving the model to
#' # '"lr_model"', the following command may be used:
#' 
#' \donttest{
#' output <- logistic_regression(training=data, labels=labels, lambda=0.1)
#' lr_model <- output$output_model
#' }
#' 
#' # Then, to use that model to predict classes for the dataset '"test"',
#' # storing the output predictions in '"predictions"', the following command
#' # may be used: 
#' 
#' \donttest{
#' output <- logistic_regression(input_model=lr_model, test=test)
#' predictions <- output$output
#' }
logistic_regression <- function(batch_size=NA,
                                decision_boundary=NA,
                                input_model=NA,
                                labels=NA,
                                lambda=NA,
                                max_iterations=NA,
                                optimizer=NA,
                                step_size=NA,
                                test=NA,
                                tolerance=NA,
                                training=NA,
                                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("L2-regularized Logistic Regression and Prediction")

  # Process each input argument before calling mlpackMain().
  if (!identical(batch_size, NA)) {
    IO_SetParamInt("batch_size", batch_size)
  }

  if (!identical(decision_boundary, NA)) {
    IO_SetParamDouble("decision_boundary", decision_boundary)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamLogisticRegressionPtr("input_model", input_model)
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

  if (!identical(optimizer, NA)) {
    IO_SetParamString("optimizer", optimizer)
  }

  if (!identical(step_size, NA)) {
    IO_SetParamDouble("step_size", step_size)
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

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")
  IO_SetPassed("output_model")
  IO_SetPassed("output_probabilities")
  IO_SetPassed("predictions")
  IO_SetPassed("probabilities")

  # Call the program.
  logistic_regression_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamLogisticRegressionPtr("output_model")
  attr(output_model, "type") <- "LogisticRegression"

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamURow("output"),
      "output_model" = output_model,
      "output_probabilities" = IO_GetParamMat("output_probabilities"),
      "predictions" = IO_GetParamURow("predictions"),
      "probabilities" = IO_GetParamMat("probabilities")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
