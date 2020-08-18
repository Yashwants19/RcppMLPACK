#' @title Principal Components Analysis
#'
#' @description
#' An implementation of several strategies for principal components analysis
#' (PCA), a common preprocessing step.  Given a dataset and a desired new
#' dimensionality, this can reduce the dimensionality of the data using the
#' linear transformation determined by PCA.
#'
#' @param input Input dataset to perform PCA on (numeric matrix).
#' @param decomposition_method Method used for the principal components
#'   analysis: 'exact', 'randomized', 'randomized-block-krylov', 'quic'. 
#'   Default value "exact" (character).
#' @param new_dimensionality Desired dimensionality of output dataset. If
#'   0, no dimensionality reduction is performed.  Default value "0" (integer).
#' @param scale If set, the data will be scaled before running PCA, such
#'   that the variance of each feature is 1.  Default value "FALSE" (logical).
#' @param var_to_retain Amount of variance to retain; should be between 0
#'   and 1.  If 1, all variance is retained.  Overrides -d.  Default value "0"
#'   (numeric).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{Matrix to save modified dataset to (numeric matrix).}
#'
#' @details
#' This program performs principal components analysis on the given dataset
#' using the exact, randomized, randomized block Krylov, or QUIC SVD method. It
#' will transform the data onto its principal components, optionally performing
#' dimensionality reduction by ignoring the principal components with the
#' smallest eigenvalues.
#' 
#' Use the "input" parameter to specify the dataset to perform PCA on.  A
#' desired new dimensionality can be specified with the "new_dimensionality"
#' parameter, or the desired variance to retain can be specified with the
#' "var_to_retain" parameter.  If desired, the dataset can be scaled before
#' running PCA with the "scale" parameter.
#' 
#' Multiple different decomposition techniques can be used.  The method to use
#' can be specified with the "decomposition_method" parameter, and it may take
#' the values 'exact', 'randomized', or 'quic'.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to reduce the dimensionality of the matrix "data" to 5
#' # dimensions using randomized SVD for the decomposition, storing the output
#' # matrix to "data_mod", the following command can be used:
#' 
#' \donttest{
#' output <- pca(input=data, new_dimensionality=5,
#'   decomposition_method="randomized")
#' data_mod <- output$output
#' }
pca <- function(input,
                decomposition_method=NA,
                new_dimensionality=NA,
                scale=FALSE,
                var_to_retain=NA,
                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Principal Components Analysis")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(decomposition_method, NA)) {
    IO_SetParamString("decomposition_method", decomposition_method)
  }

  if (!identical(new_dimensionality, NA)) {
    IO_SetParamInt("new_dimensionality", new_dimensionality)
  }

  if (!identical(scale, FALSE)) {
    IO_SetParamBool("scale", scale)
  }

  if (!identical(var_to_retain, NA)) {
    IO_SetParamDouble("var_to_retain", var_to_retain)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  pca_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
