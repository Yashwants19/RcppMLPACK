#' @title Principal Components Analysis
#' 
#' @description 
#' This program performs principal components analysis on the given dataset using
#' the exact, randomized, randomized block Krylov, or QUIC SVD method. It will
#' transform the data onto its principal components, optionally performing
#' dimensionality reduction by ignoring the principal components with the smallest
#' eigenvalues.
#' 
#' Use the `Input` parameter to specify the dataset to perform PCA on.  A desired
#' new dimensionality can be specified with the `NewDimensionality` parameter, or
#' the desired variance to retain can be specified with the `VarToRetain`
#' parameter.  If desired, the dataset can be scaled before running PCA with the
#' `Scale` parameter.
#' 
#' Multiple different decomposition techniques can be used.  The method to use can
#' be specified with the `DecompositionMethod` parameter, and it may take the
#' values 'exact', 'randomized', or 'quic'.
#' 
#' @examples 
#' \dontrun{
#' For example, to reduce the dimensionality of the matrix `Data` to 5 dimensions
#' using randomized SVD for the decomposition, storing the output matrix to
#' `DataMod`, the following command can be used:
#'   
#' DataMod = Pca(Input = Data, DecompositionMethod="randomized",
#'                                          NewDimensionality=5)
#'}
#' @param Input Input dataset to perform PCA on.
#' @param DecompositionMethod Method used for the principal
#' components analysis: 'exact', 'randomized', 'randomized-block-krylov',
#' 'quic'.  Default value `exact`.
#' @param NewDimensionality Desired dimensionality of output dataset. If
#' 0, no dimensionality reduction is performed.  Default value `0`.
#' @param Scale If set, the data will be scaled before running PCA, such
#' that the variance of each feature is 1.  Default value `FALSE`.
#' @param VarToRetain Amount of variance to retain; should be
#' between 0 and 1.  If 1, all variance is retained.  Overrides -d.  Default
#' value `0`.
#' 
#' @param Verbose Display informational messages and the full list of
#' parameters and timers at the end of execution.  Default value `FALSE`.
#' 
#' 
#' @return
#' 
#' \item{Output}{ Matrix to save modified dataset to.}

Pca <- function(Input,
    DecompositionMethod = "exact",
    NewDimensionality = 0,
    Scale = FALSE,
    VarToRetain = 0,
    Verbose = FALSE
)
{
  CLI_RestoreSettings("Principal Components Analysis")

  # Process each input argument before calling mlpackMain().
  CLI_SetParamMat("input", Input)
  if (DecompositionMethod != "exact")
    CLI_SetParamString("decomposition_method", DecompositionMethod)
 
  if (NewDimensionality != 0)
    CLI_SetParamInt("new_dimensionality", NewDimensionality)

  if (Scale != FALSE)
    CLI_SetParamBool("scale", Scale)

  if (VarToRetain != 0.0)
    CLI_SetParamDouble("var_to_retain", VarToRetain)

  if (Verbose != FALSE || Verbose == TRUE)
    CLI_EnableVerbose()
  else
    CLI_DisableVerbose()

  CLI_SetPassed("output")

    pca_mlpackMain()

  Output = CLI_GetParamMat("output")
  out <- list("Output"= Output)
  return(out)
}
