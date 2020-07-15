#' @title Descriptive Statistics
#'
#' @description
#' A utility for printing descriptive statistics about a dataset.  This prints a
#' number of details about a dataset in a tabular format.
#'
#' @param input Matrix containing data,
#' @param dimension Dimension of the data. Use this to specify a dimension  Default
#'   value "0".
#' @param population If specified, the program will calculate statistics assuming the
#'   dataset is the population. By default, the program will assume the dataset
#'   as a sample.  Default value "FALSE".
#' @param precision Precision of the output statistics.  Default value "4".
#' @param row_major If specified, the program will calculate statistics across rows,
#'   not across columns.  (Remember that in mlpack, a column represents a point,
#'   so this option is generally not necessary.)  Default value "FALSE".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#' @param width Width of the output table.  Default value "8".
#'
#' @return A list with several components:
#'
#' @details
#' This utility takes a dataset and prints out the descriptive statistics of the
#' data. Descriptive statistics is the discipline of quantitatively describing
#' the main features of a collection of information, or the quantitative
#' description itself. The program does not modify the original file, but
#' instead prints out the statistics to the console. The printed result will
#' look like a table.
#' 
#' Optionally, width and precision of the output can be adjusted by a user using
#' the "width" and "precision" parameters. A user can also select a specific
#' dimension to analyze if there are too many dimensions. The "population"
#' parameter can be specified when the dataset should be considered as a
#' population.  Otherwise, the dataset will be considered as a sample.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to print out statistical facts about the
#' # dataset "X" using the default settings, we could run 
#' 
#' \donttest{
#' output <- preprocess_describe(input=X, verbose=TRUE)
#' }
#' 
#' # If we want to customize the width to 10 and precision to 5 and consider the
#' # dataset as a population, we could run
#' 
#' \donttest{
#' output <- preprocess_describe(input=X, width=10, precision=5, verbose=TRUE)
#' }
preprocess_describe <- function(input,
                                dimension=NA,
                                population=FALSE,
                                precision=NA,
                                row_major=FALSE,
                                verbose=FALSE,
                                width=NA) {
  # Restore IO settings.
  IO_RestoreSettings("Descriptive Statistics")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(dimension, NA)) {
    IO_SetParamInt("dimension", dimension)
  }

  if (!identical(population, FALSE)) {
    IO_SetParamBool("population", population)
  }

  if (!identical(precision, NA)) {
    IO_SetParamInt("precision", precision)
  }

  if (!identical(row_major, FALSE)) {
    IO_SetParamBool("row_major", row_major)
  }

  if (!identical(width, NA)) {
    IO_SetParamInt("width", width)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.

  # Call the program.
  preprocess_describe_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(

  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
