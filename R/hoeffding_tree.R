#' @title Hoeffding trees
#'
#' @description
#' An implementation of Hoeffding trees, a form of streaming decision tree for
#' classification.  Given labeled data, a Hoeffding tree can be trained and
#' saved for later use, or a pre-trained Hoeffding tree can be used for
#' predicting the classifications of new points.
#'
#' @param batch_mode If true, samples will be considered in batch instead
#'   of as a stream.  This generally results in better trees but at the cost of
#'   memory usage and runtime.  Default value "FALSE" (logical).
#' @param bins If the 'domingos' split strategy is used, this specifies the
#'   number of bins for each numeric split.  Default value "10" (integer).
#' @param confidence Confidence before splitting (between 0 and 1). 
#'   Default value "0.95" (numeric).
#' @param info_gain If set, information gain is used instead of Gini
#'   impurity for calculating Hoeffding bounds.  Default value "FALSE"
#'   (logical).
#' @param input_model Input trained Hoeffding tree model
#'   (HoeffdingTreeModel).
#' @param labels Labels for training dataset (integer row).
#' @param max_samples Maximum number of samples before splitting.  Default
#'   value "5000" (integer).
#' @param min_samples Minimum number of samples before splitting.  Default
#'   value "100" (integer).
#' @param numeric_split_strategy The splitting strategy to use for numeric
#'   features: 'domingos' or 'binary'.  Default value "binary" (character).
#' @param observations_before_binning If the 'domingos' split strategy is
#'   used, this specifies the number of samples observed before binning is
#'   performed.  Default value "100" (integer).
#' @param passes Number of passes to take over the dataset.  Default value
#'   "1" (integer).
#' @param test Testing dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param test_labels Labels of test data (integer row).
#' @param training Training dataset (may be categorical) (numeric
#'   matrix/data.frame with info).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output_model}{Output for trained Hoeffding tree model
#'   (HoeffdingTreeModel).}
#' \item{predictions}{Matrix to output label predictions for test data into
#'   (integer row).}
#' \item{probabilities}{In addition to predicting labels, provide rediction
#'   probabilities in this matrix (numeric matrix).}
#'
#' @details
#' This program implements Hoeffding trees, a form of streaming decision tree
#' suited best for large (or streaming) datasets.  This program supports both
#' categorical and numeric data.  Given an input dataset, this program is able
#' to train the tree with numerous training options, and save the model to a
#' file.  The program is also able to use a trained model or a model from file
#' in order to predict classes for a given test set.
#' 
#' The training file and associated labels are specified with the "training" and
#' "labels" parameters, respectively. Optionally, if "labels" is not specified,
#' the labels are assumed to be the last dimension of the training dataset.
#' 
#' The training may be performed in batch mode (like a typical decision tree
#' algorithm) by specifying the "batch_mode" option, but this may not be the
#' best option for large datasets.
#' 
#' When a model is trained, it may be saved via the "output_model" output
#' parameter.  A model may be loaded from file for further training or testing
#' with the "input_model" parameter.
#' 
#' Test data may be specified with the "test" parameter, and if performance
#' statistics are desired for that test set, labels may be specified with the
#' "test_labels" parameter.  Predictions for each test point may be saved with
#' the "predictions" output parameter, and class probabilities for each
#' prediction may be saved with the "probabilities" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to train a Hoeffding tree with confidence 0.99 with data
#' # "dataset", saving the trained tree to "tree", the following command may be
#' # used:
#' 
#' \donttest{
#' output <- hoeffding_tree(training=dataset, confidence=0.99)
#' tree <- output$output_model
#' }
#' 
#' # Then, this tree may be used to make predictions on the test set "test_set",
#' # saving the predictions into "predictions" and the class probabilities into
#' # "class_probs" with the following command: 
#' 
#' \donttest{
#' output <- hoeffding_tree(input_model=tree, test=test_set)
#' predictions <- output$predictions
#' class_probs <- output$probabilities
#' }
hoeffding_tree <- function(batch_mode=FALSE,
                           bins=NA,
                           confidence=NA,
                           info_gain=FALSE,
                           input_model=NA,
                           labels=NA,
                           max_samples=NA,
                           min_samples=NA,
                           numeric_split_strategy=NA,
                           observations_before_binning=NA,
                           passes=NA,
                           test=NA,
                           test_labels=NA,
                           training=NA,
                           verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Hoeffding trees")

  # Process each input argument before calling mlpackMain().
  if (!identical(batch_mode, FALSE)) {
    IO_SetParamBool("batch_mode", batch_mode)
  }

  if (!identical(bins, NA)) {
    IO_SetParamInt("bins", bins)
  }

  if (!identical(confidence, NA)) {
    IO_SetParamDouble("confidence", confidence)
  }

  if (!identical(info_gain, FALSE)) {
    IO_SetParamBool("info_gain", info_gain)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamHoeffdingTreeModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    IO_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(max_samples, NA)) {
    IO_SetParamInt("max_samples", max_samples)
  }

  if (!identical(min_samples, NA)) {
    IO_SetParamInt("min_samples", min_samples)
  }

  if (!identical(numeric_split_strategy, NA)) {
    IO_SetParamString("numeric_split_strategy", numeric_split_strategy)
  }

  if (!identical(observations_before_binning, NA)) {
    IO_SetParamInt("observations_before_binning", observations_before_binning)
  }

  if (!identical(passes, NA)) {
    IO_SetParamInt("passes", passes)
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
  hoeffding_tree_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamHoeffdingTreeModelPtr("output_model")
  attr(output_model, "type") <- "HoeffdingTreeModel"

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
