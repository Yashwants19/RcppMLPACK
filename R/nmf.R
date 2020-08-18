#' @title Non-negative Matrix Factorization
#'
#' @description
#' An implementation of non-negative matrix factorization.  This can be used to
#' decompose an input dataset into two low-rank non-negative components.
#'
#' @param input Input dataset to perform NMF on (numeric matrix).
#' @param rank Rank of the factorization (integer).
#' @param initial_h Initial H matrix (numeric matrix).
#' @param initial_w Initial W matrix (numeric matrix).
#' @param max_iterations Number of iterations before NMF terminates (0 runs
#'   until convergence.  Default value "10000" (integer).
#' @param min_residue The minimum root mean square residue allowed for each
#'   iteration, below which the program terminates.  Default value "1e-05"
#'   (numeric).
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default
#'   value "0" (integer).
#' @param update_rules Update rules for each iteration; ( multdist |
#'   multdiv | als ).  Default value "multdist" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{h}{Matrix to save the calculated H to (numeric matrix).}
#' \item{w}{Matrix to save the calculated W to (numeric matrix).}
#'
#' @details
#' This program performs non-negative matrix factorization on the given dataset,
#' storing the resulting decomposed matrices in the specified files.  For an
#' input dataset V, NMF decomposes V into two matrices W and H such that 
#' 
#' V = W * H
#' 
#' where all elements in W and H are non-negative.  If V is of size (n x m),
#' then W will be of size (n x r) and H will be of size (r x m), where r is the
#' rank of the factorization (specified by the "rank" parameter).
#' 
#' Optionally, the desired update rules for each NMF iteration can be chosen
#' from the following list:
#' 
#'  - multdist: multiplicative distance-based update rules (Lee and Seung 1999)
#'  - multdiv: multiplicative divergence-based update rules (Lee and Seung 1999)
#'  - als: alternating least squares update rules (Paatero and Tapper 1994)
#' 
#' The maximum number of iterations is specified with "max_iterations", and the
#' minimum residue required for algorithm termination is specified with the
#' "min_residue" parameter.
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, to run NMF on the input matrix "V" using the 'multdist' update
#' # rules with a rank-10 decomposition and storing the decomposed matrices into
#' # "W" and "H", the following command could be used: 
#' 
#' \donttest{
#' output <- nmf(input=V, rank=10, update_rules="multdist")
#' W <- output$w
#' H <- output$h
#' }
nmf <- function(input,
                rank,
                initial_h=NA,
                initial_w=NA,
                max_iterations=NA,
                min_residue=NA,
                seed=NA,
                update_rules=NA,
                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Non-negative Matrix Factorization")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  IO_SetParamInt("rank", rank)

  if (!identical(initial_h, NA)) {
    IO_SetParamMat("initial_h", to_matrix(initial_h))
  }

  if (!identical(initial_w, NA)) {
    IO_SetParamMat("initial_w", to_matrix(initial_w))
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(min_residue, NA)) {
    IO_SetParamDouble("min_residue", min_residue)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(update_rules, NA)) {
    IO_SetParamString("update_rules", update_rules)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("h")
  IO_SetPassed("w")

  # Call the program.
  nmf_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "h" = IO_GetParamMat("h"),
      "w" = IO_GetParamMat("w")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
