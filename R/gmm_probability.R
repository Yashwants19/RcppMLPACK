#' @title GMM Probability Calculator
#'
#' @description
#' A probability calculator for GMMs.  Given a pre-trained GMM and a set of
#' points, this can compute the probability that each point is from the given
#' GMM.
#'
#' @param input Input matrix to calculate probabilities of (numeric
#'   matrix).
#' @param input_model Input GMM to use as model (GMM).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output}{Matrix to store calculated probabilities in (numeric
#'   matrix).}
#'
#' @details
#' This program calculates the probability that given points came from a given
#' GMM (that is, P(X | gmm)).  The GMM is specified with the "input_model"
#' parameter, and the points are specified with the "input" parameter.  The
#' output probabilities may be saved via the "output" output parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # So, for example, to calculate the probabilities of each point in "points"
#' # coming from the pre-trained GMM "gmm", while storing those probabilities in
#' # "probs", the following command could be used:
#' 
#' \donttest{
#' output <- gmm_probability(input_model=gmm, input=points)
#' probs <- output$output
#' }
gmm_probability <- function(input,
                            input_model,
                            verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("GMM Probability Calculator")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  IO_SetParamGMMPtr("input_model", input_model)

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  gmm_probability_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
