#' @title Principal Components Analysis
#'
#' @description
#' This program performs principal components analysis on the given dataset using
#' the exact, randomized, randomized block Krylov, or QUIC SVD method. It will
#' transform the data onto its principal components, optionally performing
#' dimensionality reduction by ignoring the principal components with the smallest
#' eigenvalues.
#'
#' Use the `input` parameter to specify the dataset to perform PCA on.  A desired
#' new dimensionality can be specified with the `new_dimensionality` parameter, or
#' the desired variance to retain can be specified with the `var_to_retain`
#' parameter.  If desired, the dataset can be scaled before running PCA with the
#' `scale` parameter.
#'
#' Multiple different decomposition techniques can be used.  The method to use can
#' be specified with the `decomposition_method` parameter, and it may take the
#' values 'exact', 'randomized', or 'quic'.
#'
#' @examples 
#' \dontrun{
#' For example, to reduce the dimensionality of the matrix `Data` to 5 dimensions
#' using randomized SVD for the decomposition, storing the output matrix to
#' `DataMod`, the following command can be used:
#'   
#' DataMod = pca(input = Data, decomposition_method="randomized",
#'                                          new_dimensionality=5)
#' }
#' @param input input dataset to perform PCA on.
#' @param decomposition_method Method used for the principal
#' components analysis: 'exact', 'randomized', 'randomized-block-krylov',
#' 'quic'.  Default value `exact`.
#' @param new_dimensionality Desired dimensionality of output dataset. If
#' 0, no dimensionality reduction is performed.  Default value `0`.
#' @param scale If set, the data will be scaled before running PCA, such
#' that the variance of each feature is 1.  Default value `FALSE`.
#' @param var_to_retain Amount of variance to retain; should be
#' between 0 and 1.  If 1, all variance is retained.  Overrides -d.  Default
#' value `0`.
#' @param copy_all_inputs If specified, all input parameters will be
#' deep copied before the method is run.  This is useful for debugging
#' problems where the input parameters are being modified by the algorithm,
#' but can slow down the code.
#' @param verbose Display informational messages and the full list of
#' parameters and timers at the end of execution.  Default value `FALSE`.
#'
#'
#' @return
#'
#' \item{output}{ Matrix to save modified dataset to.}

pca <- function(input,
                decomposition_method = "exact",
                new_dimensionality = 0,
                scale = FALSE,
                var_to_retain = 0,
                copy_all_inputs = FALSE,
                verbose = FALSE) {
  CLI_RestoreSettings("Principal Components Analysis")

  # Process each input argument before calling mlpackMain().
  CLI_SetParamMat("input", input, copy_all_inputs)

  if (decomposition_method != "exact") {
    CLI_SetParamString("decomposition_method", decomposition_method)
  }

  if (new_dimensionality != 0) {
    CLI_SetParamInt("new_dimensionality", new_dimensionality)
  }

  if (scale != FALSE) {
    CLI_SetParamBool("scale", scale)
  }

  if (var_to_retain != 0.0) {
    CLI_SetParamDouble("var_to_retain", var_to_retain)
  }

  if (verbose != FALSE || verbose == TRUE) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  pca_mlpackMain()

  out <- list("output" = CLIGetParamMat("output"))

  CLI_ClearSettings()

  return(out)
}
