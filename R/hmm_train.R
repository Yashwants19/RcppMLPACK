#' @title Hidden Markov Model (HMM) Training
#'
#' @description
#' An implementation of training algorithms for Hidden Markov Models (HMMs).
#' Given labeled or unlabeled data, an HMM can be trained for further use with
#' other mlpack HMM tools.
#'
#' @param input_file File containing input observations (character).
#' @param batch If true, input_file (and if passed, labels_file) are
#'   expected to contain a list of files to use as input observation sequences
#'   (and label sequences).  Default value "FALSE" (logical).
#' @param gaussians Number of gaussians in each GMM (necessary when type is
#'   'gmm').  Default value "0" (integer).
#' @param input_model Pre-existing HMM model to initialize training with
#'   (HMMModel).
#' @param labels_file Optional file of hidden states, used for labeled
#'   training.  Default value "" (character).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param states Number of hidden states in HMM (necessary, unless
#'   model_file is specified).  Default value "0" (integer).
#' @param tolerance Tolerance of the Baum-Welch algorithm.  Default value
#'   "1e-05" (numeric).
#' @param type Type of HMM: discrete | gaussian | diag_gmm | gmm.  Default
#'   value "gaussian" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output_model}{Output for trained HMM (HMMModel).}
#'
#' @details
#' This program allows a Hidden Markov Model to be trained on labeled or
#' unlabeled data.  It supports four types of HMMs: Discrete HMMs, Gaussian
#' HMMs, GMM HMMs, or Diagonal GMM HMMs
#' 
#' Either one input sequence can be specified (with "input_file"), or, a file
#' containing files in which input sequences can be found (when
#' "input_file"and"batch" are used together).  In addition, labels can be
#' provided in the file specified by "labels_file", and if "batch" is used, the
#' file given to "labels_file" should contain a list of files of labels
#' corresponding to the sequences in the file given to "input_file".
#' 
#' The HMM is trained with the Baum-Welch algorithm if no labels are provided. 
#' The tolerance of the Baum-Welch algorithm can be set with the
#' "tolerance"option.  By default, the transition matrix is randomly initialized
#' and the emission distributions are initialized to fit the extent of the data.
#' 
#' Optionally, a pre-created HMM model can be used as a guess for the transition
#' matrix and emission probabilities; this is specifiable with "output_model".
#'
#' @author
#' mlpack developers
#'
#' @export

hmm_train <- function(input_file,
                      batch=FALSE,
                      gaussians=NA,
                      input_model=NA,
                      labels_file=NA,
                      seed=NA,
                      states=NA,
                      tolerance=NA,
                      type=NA,
                      verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Hidden Markov Model (HMM) Training")

  # Process each input argument before calling mlpackMain().
  IO_SetParamString("input_file", input_file)

  if (!identical(batch, FALSE)) {
    IO_SetParamBool("batch", batch)
  }

  if (!identical(gaussians, NA)) {
    IO_SetParamInt("gaussians", gaussians)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamHMMModelPtr("input_model", input_model)
  }

  if (!identical(labels_file, NA)) {
    IO_SetParamString("labels_file", labels_file)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(states, NA)) {
    IO_SetParamInt("states", states)
  }

  if (!identical(tolerance, NA)) {
    IO_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(type, NA)) {
    IO_SetParamString("type", type)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output_model")

  # Call the program.
  hmm_train_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamHMMModelPtr("output_model")
  attr(output_model, "type") <- "HMMModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
