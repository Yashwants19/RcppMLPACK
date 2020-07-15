#' @title Random forests
#'
#' @description
#' An implementation of the standard random forest algorithm by Leo Breiman for
#' classification.  Given labeled data, a random forest can be trained and saved
#' for future use; or, a pre-trained random forest can be used for
#' classification.
#'
#' @param input_model Pre-trained random forest to use for classification.
#' @param labels Labels for training dataset.
#' @param maximum_depth Maximum depth of the tree (0 means no limit).  Default value
#'   "0".
#' @param minimum_gain_split Minimum gain needed to make a split when building a
#'   tree.  Default value "0".
#' @param minimum_leaf_size Minimum number of points in each leaf node.  Default
#'   value "1".
#' @param num_trees Number of trees in the random forest.  Default value "10".
#' @param print_training_accuracy If set, then the accuracy of the model on the
#'   training set will be predicted (verbose must also be specified).  Default
#'   value "FALSE".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param subspace_dim Dimensionality of random subspace to use for each split.  '0'
#'   will autoselect the square root of data dimensionality.  Default value
#'   "0".
#' @param test Test dataset to produce predictions for.
#' @param test_labels Test dataset labels, if accuracy calculation is desired.
#' @param training Training dataset.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output_model}{Model to save trained random forest to.}
#' \item{predictions}{Predicted classes for each point in the test set.}
#' \item{probabilities}{Predicted class probabilities for each point in the test
#'   set.}
#'
#' @details
#' This program is an implementation of the standard random forest
#' classification algorithm by Leo Breiman.  A random forest can be trained and
#' saved for later use, or a random forest may be loaded and predictions or
#' class probabilities for points may be generated.
#' 
#' The training set and associated labels are specified with the "training" and
#' "labels" parameters, respectively.  The labels should be in the range [0,
#' num_classes - 1]. Optionally, if "labels" is not specified, the labels are
#' assumed to be the last dimension of the training dataset.
#' 
#' When a model is trained, the "output_model" output parameter may be used to
#' save the trained model.  A model may be loaded for predictions with the
#' "input_model"parameter. The "input_model" parameter may not be specified when
#' the "training" parameter is specified.  The "minimum_leaf_size" parameter
#' specifies the minimum number of training points that must fall into each leaf
#' for it to be split.  The "num_trees" controls the number of trees in the
#' random forest.  The "minimum_gain_split" parameter controls the minimum
#' required gain for a decision tree node to split.  Larger values will force
#' higher-confidence splits.  The "maximum_depth" parameter specifies the
#' maximum depth of the tree.  The "subspace_dim" parameter is used to control
#' the number of random dimensions chosen for an individual node's split.  If
#' "print_training_accuracy" is specified, the calculated accuracy on the
#' training set will be printed.
#' 
#' Test data may be specified with the "test" parameter, and if performance
#' measures are desired for that test set, labels for the test points may be
#' specified with the "test_labels" parameter.  Predictions for each test point
#' may be saved via the "predictions"output parameter.  Class probabilities for
#' each prediction may be saved with the "probabilities" output parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to train a random forest with a minimum leaf size of 20 using
#' # 10 trees on the dataset contained in "data"with labels "labels", saving the
#' # output random forest to "rf_model" and printing the training error, one
#' # could call
#' 
#' \donttest{
#' output <- random_forest(training=data, labels=labels, minimum_leaf_size=20,
#'   num_trees=10, print_training_accuracy=TRUE)
#' rf_model <- output$output_model
#' }
#' 
#' # Then, to use that model to classify points in "test_set" and print the test
#' # error given the labels "test_labels" using that model, while saving the
#' # predictions for each point to "predictions", one could call 
#' 
#' \donttest{
#' output <- random_forest(input_model=rf_model, test=test_set,
#'   test_labels=test_labels)
#' predictions <- output$predictions
#' }
random_forest <- function(input_model=NA,
                          labels=NA,
                          maximum_depth=NA,
                          minimum_gain_split=NA,
                          minimum_leaf_size=NA,
                          num_trees=NA,
                          print_training_accuracy=FALSE,
                          seed=NA,
                          subspace_dim=NA,
                          test=NA,
                          test_labels=NA,
                          training=NA,
                          verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Random forests")

  # Process each input argument before calling mlpackMain().
  if (!identical(input_model, NA)) {
    IO_SetParamRandomForestModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(maximum_depth, NA)) {
    IO_SetParamInt("maximum_depth", maximum_depth)
  }

  if (!identical(minimum_gain_split, NA)) {
    IO_SetParamDouble("minimum_gain_split", minimum_gain_split)
  }

  if (!identical(minimum_leaf_size, NA)) {
    IO_SetParamInt("minimum_leaf_size", minimum_leaf_size)
  }

  if (!identical(num_trees, NA)) {
    IO_SetParamInt("num_trees", num_trees)
  }

  if (!identical(print_training_accuracy, FALSE)) {
    IO_SetParamBool("print_training_accuracy", print_training_accuracy)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(subspace_dim, NA)) {
    IO_SetParamInt("subspace_dim", subspace_dim)
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
  IO_SetPassed("probabilities")

  # Call the program.
  random_forest_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamRandomForestModelPtr("output_model")
  attr(output_model, "type") <- "RandomForestModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = IO_GetParamURow("predictions"),
      "probabilities" = IO_GetParamMat("probabilities")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
