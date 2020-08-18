#' @title GMM Sample Generator
#'
#' @description
#' A sample generator for pre-trained GMMs.  Given a pre-trained GMM, this can
#' sample new points randomly from that distribution.
#'
#' @param input_model Input GMM model to generate samples from (GMM).
#' @param samples Number of samples to generate (integer).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{Matrix to save output samples in (numeric matrix).}
#'
#' @details
#' This program is able to generate samples from a pre-trained GMM (use
#' gmm_train to train a GMM).  The pre-trained GMM must be specified with the
#' "input_model" parameter.  The number of samples to generate is specified by
#' the "samples" parameter.  Output samples may be saved with the "output"
#' output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # The following command can be used to generate 100 samples from the
#' # pre-trained GMM "gmm" and store those generated samples in "samples":
#' 
#' \donttest{
#' output <- gmm_generate(input_model=gmm, samples=100)
#' samples <- output$output
#' }
gmm_generate <- function(input_model,
                         samples,
                         seed=NA,
                         verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("GMM Sample Generator")

  # Process each input argument before calling mlpackMain().
  IO_SetParamGMMPtr("input_model", input_model)

  IO_SetParamInt("samples", samples)

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  gmm_generate_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
