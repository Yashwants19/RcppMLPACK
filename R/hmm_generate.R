#' @title Hidden Markov Model (HMM) Sequence Generator
#'
#' @description
#' A utility to generate random sequences from a pre-trained Hidden Markov Model
#' (HMM).  The length of the desired sequence can be specified, and a random
#' sequence of observations is returned.
#'
#' @param length Length of sequence to generate.
#' @param model Trained HMM to generate sequences with.
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param start_state Starting state of sequence.  Default value "0".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{Matrix to save observation sequence to.}
#' \item{state}{Matrix to save hidden state sequence to.}
#'
#' @details
#' This utility takes an already-trained HMM, specified as the "model"
#' parameter, and generates a random observation sequence and hidden state
#' sequence based on its parameters. The observation sequence may be saved with
#' the "output" output parameter, and the internal state  sequence may be saved
#' with the "state" output parameter.
#' 
#' The state to start the sequence in may be specified with the "start_state"
#' parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to generate a sequence of length 150 from the HMM "hmm" and
#' # save the observation sequence to "observations" and the hidden state
#' # sequence to "states", the following command may be used: 
#' 
#' \donttest{
#' output <- hmm_generate(model=hmm, length=150)
#' observations <- output$output
#' states <- output$state
#' }
hmm_generate <- function(length,
                         model,
                         seed=NA,
                         start_state=NA,
                         verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Hidden Markov Model (HMM) Sequence Generator")

  # Process each input argument before calling mlpackMain().
  IO_SetParamInt("length", length)

  IO_SetParamHMMModelPtr("model", model)

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(start_state, NA)) {
    IO_SetParamInt("start_state", start_state)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")
  IO_SetPassed("state")

  # Call the program.
  hmm_generate_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output"),
      "state" = IO_GetParamUMat("state")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
