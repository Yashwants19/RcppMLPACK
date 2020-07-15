#' @title RADICAL
#'
#' @description
#' An implementation of RADICAL, a method for independent component analysis
#' (ICA).  Given a dataset, this can decompose the dataset into an unmixing
#' matrix and an independent component matrix; this can be useful for
#' preprocessing.
#'
#' @param input Input dataset for ICA.
#' @param angles Number of angles to consider in brute-force search during Radical2D.
#'    Default value "150".
#' @param noise_std_dev Standard deviation of Gaussian noise.  Default value
#'   "0.175".
#' @param objective If set, an estimate of the final objective function is printed. 
#'   Default value "FALSE".
#' @param replicates Number of Gaussian-perturbed replicates to use (per point) in
#'   Radical2D.  Default value "30".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param sweeps Number of sweeps; each sweep calls Radical2D once for each pair of
#'   dimensions.  Default value "0".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output_ic}{Matrix to save independent components to.}
#' \item{output_unmixing}{Matrix to save unmixing matrix to.}
#'
#' @details
#' An implementation of RADICAL, a method for independent component analysis
#' (ICA).  Assuming that we have an input matrix X, the goal is to find a square
#' unmixing matrix W such that Y = W * X and the dimensions of Y are independent
#' components.  If the algorithm is running particularly slowly, try reducing
#' the number of replicates.
#' 
#' The input matrix to perform ICA on should be specified with the "input"
#' parameter.  The output matrix Y may be saved with the "output_ic" output
#' parameter, and the output unmixing matrix W may be saved with the
#' "output_unmixing" output parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to perform ICA on the matrix "X" with 40 replicates, saving
#' # the independent components to "ic", the following command may be used: 
#' 
#' \donttest{
#' output <- radical(input=X, replicates=40)
#' ic <- output$output_ic
#' }
radical <- function(input,
                    angles=NA,
                    noise_std_dev=NA,
                    objective=FALSE,
                    replicates=NA,
                    seed=NA,
                    sweeps=NA,
                    verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("RADICAL")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(angles, NA)) {
    IO_SetParamInt("angles", angles)
  }

  if (!identical(noise_std_dev, NA)) {
    IO_SetParamDouble("noise_std_dev", noise_std_dev)
  }

  if (!identical(objective, FALSE)) {
    IO_SetParamBool("objective", objective)
  }

  if (!identical(replicates, NA)) {
    IO_SetParamInt("replicates", replicates)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(sweeps, NA)) {
    IO_SetParamInt("sweeps", sweeps)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output_ic")
  IO_SetPassed("output_unmixing")

  # Call the program.
  radical_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output_ic" = IO_GetParamMat("output_ic"),
      "output_unmixing" = IO_GetParamMat("output_unmixing")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
