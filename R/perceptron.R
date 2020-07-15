#' @title Perceptron
#'
#' @description
#' An implementation of a perceptron---a single level neural network--=for
#' classification.  Given labeled data, a perceptron can be trained and saved
#' for future use; or, a pre-trained perceptron can be used for classification
#' on new points.
#'
#' @param input_model Input perceptron model.
#' @param labels A matrix containing labels for the training set.
#' @param max_iterations The maximum number of iterations the perceptron is to be run
#'    Default value "1000".
#' @param test A matrix containing the test set.
#' @param training A matrix containing the training set.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{The matrix in which the predicted labels for the test set will be
#'   written.}
#' \item{output_model}{Output for trained perceptron model.}
#' \item{predictions}{The matrix in which the predicted labels for the test set will
#'   be written.}
#'
#' @details
#' This program implements a perceptron, which is a single level neural network.
#' The perceptron makes its predictions based on a linear predictor function
#' combining a set of weights with the feature vector.  The perceptron learning
#' rule is able to converge, given enough iterations (specified using the
#' "max_iterations" parameter), if the data supplied is linearly separable.  The
#' perceptron is parameterized by a matrix of weight vectors that denote the
#' numerical weights of the neural network.
#' 
#' This program allows loading a perceptron from a model (via the "input_model"
#' parameter) or training a perceptron given training data (via the "training"
#' parameter), or both those things at once.  In addition, this program allows
#' classification on a test dataset (via the "test" parameter) and the
#' classification results on the test set may be saved with the "predictions"
#' output parameter.  The perceptron model may be saved with the "output_model"
#' output parameter.
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
#' # The training data given with the "training" option may have class labels as
#' # its last dimension (so, if the training data is in CSV format, labels
#' # should be the last column).  Alternately, the "labels" parameter may be
#' # used to specify a separate matrix of labels.
#' # 
#' # All these options make it easy to train a perceptron, and then re-use that
#' # perceptron for later classification.  The invocation below trains a
#' # perceptron on "training_data" with labels "training_labels", and saves the
#' # model to "perceptron_model".
#' 
#' \donttest{
#' output <- perceptron(training=training_data, labels=training_labels)
#' perceptron_model <- output$output_model
#' }
#' 
#' # Then, this model can be re-used for classification on the test data
#' # "test_data".  The example below does precisely that, saving the predicted
#' # classes to "predictions".
#' 
#' \donttest{
#' output <- perceptron(input_model=perceptron_model, test=test_data)
#' predictions <- output$predictions
#' }
#' 
#' # Note that all of the options may be specified at once: predictions may be
#' # calculated right after training a model, and model training can occur even
#' # if an existing perceptron model is passed with the "input_model" parameter.
#' #  However, note that the number of classes and the dimensionality of all
#' # data must match.  So you cannot pass a perceptron model trained on 2
#' # classes and then re-train with a 4-class dataset.  Similarly, attempting
#' # classification on a 3-dimensional dataset with a perceptron that has been
#' # trained on 8 dimensions will cause an error.
perceptron <- function(input_model=NA,
                       labels=NA,
                       max_iterations=NA,
                       test=NA,
                       training=NA,
                       verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Perceptron")

  # Process each input argument before calling mlpackMain().
  if (!identical(input_model, NA)) {
    IO_SetParamPerceptronModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
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
  IO_SetPassed("predictions")

  # Call the program.
  perceptron_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamPerceptronModelPtr("output_model")
  attr(output_model, "type") <- "PerceptronModel"

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamURow("output"),
      "output_model" = output_model,
      "predictions" = IO_GetParamURow("predictions")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
