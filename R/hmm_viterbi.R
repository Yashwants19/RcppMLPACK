#' @title Hidden Markov Model (HMM) Viterbi State Prediction
#'
#' @description
#' A utility for computing the most probable hidden state sequence for Hidden
#' Markov Models (HMMs).  Given a pre-trained HMM and an observed sequence, this
#' uses the Viterbi algorithm to compute and return the most probable hidden
#' state sequence.
#'
#' @param input Matrix containing observations,
#' @param input_model Trained HMM to use.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{File to save predicted state sequence to.}
#'
#' @details
#' This utility takes an already-trained HMM, specified as "input_model", and
#' evaluates the most probable hidden state sequence of a given sequence of
#' observations (specified as '"input", using the Viterbi algorithm.  The
#' computed state sequence may be saved using the "output" output parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to predict the state sequence of the observations "obs" using
#' # the HMM "hmm", storing the predicted state sequence to "states", the
#' # following command could be used:
#' 
#' \donttest{
#' output <- hmm_viterbi(input=obs, input_model=hmm)
#' states <- output$output
#' }
hmm_viterbi <- function(input,
                        input_model,
                        verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Hidden Markov Model (HMM) Viterbi State Prediction")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  IO_SetParamHMMModelPtr("input_model", input_model)

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  hmm_viterbi_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamUMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
