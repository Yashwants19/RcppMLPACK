#' @title Decision tree
#'
#' @description
#' An implementation of an ID3-style decision tree for classification, which
#' supports categorical data.  Given labeled data with numeric or categorical
#' features, a decision tree can be trained and saved; or, an existing decision
#' tree can be used for classification on new points.
#'
#' @param input_model Pre-trained decision tree, to be used with test points.
#' @param labels Training labels.
#' @param maximum_depth Maximum depth of the tree (0 means no limit).  Default value
#'   "0".
#' @param minimum_gain_split Minimum gain for node splitting.  Default value
#'   "1e-07".
#' @param minimum_leaf_size Minimum number of points in a leaf.  Default value "20".
#' @param print_training_accuracy Print the training accuracy.  Default value
#'   "FALSE".
#' @param print_training_error Print the training error (deprecated; will be removed
#'   in mlpack 4.0.0).  Default value "FALSE".
#' @param test Testing dataset (may be categorical).
#' @param test_labels Test point labels, if accuracy calculation is desired.
#' @param training Training dataset (may be categorical).
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#' @param weights The weight of labels
#'
#' @return A list with several components:
#' \item{output_model}{Output for trained decision tree.}
#' \item{predictions}{Class predictions for each test point.}
#' \item{probabilities}{Class probabilities for each test point.}
#'
#' @details
#' Train and evaluate using a decision tree.  Given a dataset containing numeric
#' or categorical features, and associated labels for each point in the dataset,
#' this program can train a decision tree on that data.
#' 
#' The training set and associated labels are specified with the "training" and
#' "labels" parameters, respectively.  The labels should be in the range [0,
#' num_classes - 1]. Optionally, if "labels" is not specified, the labels are
#' assumed to be the last dimension of the training dataset.
#' 
#' When a model is trained, the "output_model" output parameter may be used to
#' save the trained model.  A model may be loaded for predictions with the
#' "input_model" parameter.  The "input_model" parameter may not be specified
#' when the "training" parameter is specified.  The "minimum_leaf_size"
#' parameter specifies the minimum number of training points that must fall into
#' each leaf for it to be split.  The "minimum_gain_split" parameter specifies
#' the minimum gain that is needed for the node to split.  The "maximum_depth"
#' parameter specifies the maximum depth of the tree.  If "print_training_error"
#' is specified, the training error will be printed.
#' 
#' Test data may be specified with the "test" parameter, and if performance
#' numbers are desired for that test set, labels may be specified with the
#' "test_labels" parameter.  Predictions for each test point may be saved via
#' the "predictions" output parameter.  Class probabilities for each prediction
#' may be saved with the "probabilities" output parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to train a decision tree with a minimum leaf size of 20 on the
#' # dataset contained in "data" with labels "labels", saving the output model
#' # to "tree" and printing the training error, one could call
#' 
#' \donttest{
#' output <- decision_tree(training=data, labels=labels, minimum_leaf_size=20,
#'   minimum_gain_split=0.001, print_training_accuracy=TRUE)
#' tree <- output$output_model
#' }
#' 
#' # Then, to use that model to classify points in "test_set" and print the test
#' # error given the labels "test_labels" using that model, while saving the
#' # predictions for each point to "predictions", one could call 
#' 
#' \donttest{
#' output <- decision_tree(input_model=tree, test=test_set,
#'   test_labels=test_labels)
#' predictions <- output$predictions
#' }
decision_tree <- function(input_model=NA,
                          labels=NA,
                          maximum_depth=NA,
                          minimum_gain_split=NA,
                          minimum_leaf_size=NA,
                          print_training_accuracy=FALSE,
                          print_training_error=FALSE,
                          test=NA,
                          test_labels=NA,
                          training=NA,
                          verbose=FALSE,
                          weights=NA) {
  # Restore IO settings.
  IO_RestoreSettings("Decision tree")

  # Process each input argument before calling mlpackMain().
  if (!identical(input_model, NA)) {
    IO_SetParamDecisionTreeModelPtr("input_model", input_model)
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

  if (!identical(print_training_accuracy, FALSE)) {
    IO_SetParamBool("print_training_accuracy", print_training_accuracy)
  }

  if (!identical(print_training_error, FALSE)) {
    IO_SetParamBool("print_training_error", print_training_error)
  }

  if (!identical(test, NA)) {
    test <- to_matrix_with_info(test)
    IO_SetParamMatWithInfo("test", test$info, test$data)
  }

  if (!identical(test_labels, NA)) {
    IO_SetParamURow("test_labels", to_matrix(test_labels))
  }

  if (!identical(training, NA)) {
    training <- to_matrix_with_info(training)
    IO_SetParamMatWithInfo("training", training$info, training$data)
  }

  if (!identical(weights, NA)) {
    IO_SetParamMat("weights", to_matrix(weights))
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
  decision_tree_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamDecisionTreeModelPtr("output_model")
  attr(output_model, "type") <- "DecisionTreeModel"

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
