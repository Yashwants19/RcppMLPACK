#' @title Split Data
#'
#' @description
#' A utility to split data into a training and testing dataset.  This can also
#' split labels according to the same split.
#'
#' @param input Matrix containing data.
#' @param input_labels Matrix containing labels.
#' @param no_shuffle Avoid shuffling and splitting the data.  Default value "FALSE".
#' @param seed Random seed (0 for std::time(NULL)).  Default value "0".
#' @param test_ratio Ratio of test set; if not set,the ratio defaults to 0.2  Default
#'   value "0.2".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{test}{Matrix to save test data to.}
#' \item{test_labels}{Matrix to save test labels to.}
#' \item{training}{Matrix to save training data to.}
#' \item{training_labels}{Matrix to save train labels to.}
#'
#' @details
#' This utility takes a dataset and optionally labels and splits them into a
#' training set and a test set. Before the split, the points in the dataset are
#' randomly reordered. The percentage of the dataset to be used as the test set
#' can be specified with the "test_ratio" parameter; the default is 0.2 (20%).
#' 
#' The output training and test matrices may be saved with the "training" and
#' "test" output parameters.
#' 
#' Optionally, labels can be also be split along with the data by specifying the
#' "input_labels" parameter.  Splitting labels works the same way as splitting
#' the data. The output training and test labels may be saved with the
#' "training_labels" and "test_labels" output parameters, respectively.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to split the dataset "X" into "X_train"
#' # and "X_test" with 60% of the data in the training set and 40% of the
#' # dataset in the test set, we could run 
#' 
#' \donttest{
#' output <- preprocess_split(input=X, test_ratio=0.4)
#' X_train <- output$training
#' X_test <- output$test
#' }
#' 
#' # Also by default the dataset is shuffled and split; you can provide the
#' # "no_shuffle" option to avoid shuffling the data; an example to avoid
#' # shuffling of data is:
#' 
#' \donttest{
#' output <- preprocess_split(input=X, test_ratio=0.4, no_shuffle=TRUE)
#' X_train <- output$training
#' X_test <- output$test
#' }
#' 
#' # If we had a dataset "X" and associated labels "y", and we wanted to split
#' # these into "X_train", "y_train", "X_test", and "y_test", with 30% of the
#' # data in the test set, we could run
#' 
#' \donttest{
#' output <- preprocess_split(input=X, input_labels=y, test_ratio=0.3)
#' X_train <- output$training
#' y_train <- output$training_labels
#' X_test <- output$test
#' y_test <- output$test_labels
#' }
preprocess_split <- function(input,
                             input_labels=NA,
                             no_shuffle=FALSE,
                             seed=NA,
                             test_ratio=NA,
                             verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Split Data")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(input_labels, NA)) {
    IO_SetParamUMat("input_labels", to_matrix(input_labels))
  }

  if (!identical(no_shuffle, FALSE)) {
    IO_SetParamBool("no_shuffle", no_shuffle)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(test_ratio, NA)) {
    IO_SetParamDouble("test_ratio", test_ratio)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("test")
  IO_SetPassed("test_labels")
  IO_SetPassed("training")
  IO_SetPassed("training_labels")

  # Call the program.
  preprocess_split_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "test" = IO_GetParamMat("test"),
      "test_labels" = IO_GetParamUMat("test_labels"),
      "training" = IO_GetParamMat("training"),
      "training_labels" = IO_GetParamUMat("training_labels")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
