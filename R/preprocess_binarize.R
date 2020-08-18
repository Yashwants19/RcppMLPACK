#' @title Binarize Data
#'
#' @description
#' A utility to binarize a dataset.  Given a dataset, this utility converts each
#' value in the desired dimension(s) to 0 or 1; this can be a useful
#' preprocessing step.
#'
#' @param input Input data matrix (numeric matrix).
#' @param dimension Dimension to apply the binarization. If not set, the
#'   program will binarize every dimension by default.  Default value "0"
#'   (integer).
#' @param threshold Threshold to be applied for binarization. If not set,
#'   the threshold defaults to 0.0.  Default value "0" (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{Matrix in which to save the output (numeric matrix).}
#'
#' @details
#' This utility takes a dataset and binarizes the variables into either 0 or 1
#' given threshold. User can apply binarization on a dimension or the whole
#' dataset.  The dimension to apply binarization to can be specified using the
#' "dimension" parameter; if left unspecified, every dimension will be
#' binarized.  The threshold for binarization can also be specified with the
#' "threshold" parameter; the default threshold is 0.0.
#' 
#' The binarized matrix may be saved with the "output" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, if we want to set all variables greater than 5 in the dataset
#' # "X" to 1 and variables less than or equal to 5.0 to 0, and save the result
#' # to "Y", we could run
#' 
#' \donttest{
#' output <- preprocess_binarize(input=X, threshold=5)
#' Y <- output$output
#' }
#' 
#' # But if we want to apply this to only the first (0th) dimension of "X",  we
#' # could instead run
#' 
#' \donttest{
#' output <- preprocess_binarize(input=X, threshold=5, dimension=0)
#' Y <- output$output
#' }
preprocess_binarize <- function(input,
                                dimension=NA,
                                threshold=NA,
                                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Binarize Data")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(dimension, NA)) {
    IO_SetParamInt("dimension", dimension)
  }

  if (!identical(threshold, NA)) {
    IO_SetParamDouble("threshold", threshold)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  preprocess_binarize_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
