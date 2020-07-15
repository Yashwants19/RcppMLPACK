#' @title Hidden Markov Model (HMM) Sequence Log-Likelihood
#'
#' @description
#' A utility for computing the log-likelihood of a sequence for Hidden Markov
#' Models (HMMs).  Given a pre-trained HMM and an observation sequence, this
#' computes and returns the log-likelihood of that sequence being observed from
#' that HMM.
#'
#' @param input File containing observations,
#' @param input_model File containing HMM.
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{log_likelihood}{Log-likelihood of the sequence.  Default value "0".}
#'
#' @details
#' This utility takes an already-trained HMM, specified with the "input_model"
#' parameter, and evaluates the log-likelihood of a sequence of observations,
#' given with the "input" parameter.  The computed log-likelihood is given as
#' output.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to compute the log-likelihood of the sequence "seq" with the
#' # pre-trained HMM "hmm", the following command may be used: 
#' 
#' \donttest{
#' output <- hmm_loglik(input=seq, input_model=hmm)
#' }
hmm_loglik <- function(input,
                       input_model,
                       verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Hidden Markov Model (HMM) Sequence Log-Likelihood")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  IO_SetParamHMMModelPtr("input_model", input_model)

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("log_likelihood")

  # Call the program.
  hmm_loglik_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "log_likelihood" = IO_GetParamDouble("log_likelihood")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
