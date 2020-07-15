#' @title Sparse Coding
#'
#' @description
#' An implementation of Sparse Coding with Dictionary Learning.  Given a
#' dataset, this will decompose the dataset into a sparse combination of a few
#' dictionary elements, where the dictionary is learned during computation; a
#' dictionary can be reused for future sparse coding of new points.
#'
#' @param atoms Number of atoms in the dictionary.  Default value "15".
#' @param initial_dictionary Optional initial dictionary matrix.
#' @param input_model File containing input sparse coding model.
#' @param lambda1 Sparse coding l1-norm regularization parameter.  Default value
#'   "0".
#' @param lambda2 Sparse coding l2-norm regularization parameter.  Default value
#'   "0".
#' @param max_iterations Maximum number of iterations for sparse coding (0 indicates
#'   no limit).  Default value "0".
#' @param newton_tolerance Tolerance for convergence of Newton method.  Default value
#'   "1e-06".
#' @param normalize If set, the input data matrix will be normalized before coding. 
#'   Default value "FALSE".
#' @param objective_tolerance Tolerance for convergence of the objective function. 
#'   Default value "0.01".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param test Optional matrix to be encoded by trained model.
#' @param training Matrix of training data (X).
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{codes}{Matrix to save the output sparse codes of the test matrix
#'   (--test_file) to.}
#' \item{dictionary}{Matrix to save the output dictionary to.}
#' \item{output_model}{File to save trained sparse coding model to.}
#'
#' @details
#' An implementation of Sparse Coding with Dictionary Learning, which achieves
#' sparsity via an l1-norm regularizer on the codes (LASSO) or an (l1+l2)-norm
#' regularizer on the codes (the Elastic Net).  Given a dense data matrix X with
#' d dimensions and n points, sparse coding seeks to find a dense dictionary
#' matrix D with k atoms in d dimensions, and a sparse coding matrix Z with n
#' points in k dimensions.
#' 
#' The original data matrix X can then be reconstructed as Z * D.  Therefore,
#' this program finds a representation of each point in X as a sparse linear
#' combination of atoms in the dictionary D.
#' 
#' The sparse coding is found with an algorithm which alternates between a
#' dictionary step, which updates the dictionary D, and a sparse coding step,
#' which updates the sparse coding matrix.
#' 
#' Once a dictionary D is found, the sparse coding model may be used to encode
#' other matrices, and saved for future usage.
#' 
#' To run this program, either an input matrix or an already-saved sparse coding
#' model must be specified.  An input matrix may be specified with the
#' "training" option, along with the number of atoms in the dictionary
#' (specified with the "atoms" parameter).  It is also possible to specify an
#' initial dictionary for the optimization, with the "initial_dictionary"
#' parameter.  An input model may be specified with the "input_model" parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # As an example, to build a sparse coding model on the dataset "data" using
#' # 200 atoms and an l1-regularization parameter of 0.1, saving the model into
#' # "model", use 
#' 
#' \donttest{
#' output <- sparse_coding(training=data, atoms=200, lambda1=0.1)
#' model <- output$output_model
#' }
#' 
#' # Then, this model could be used to encode a new matrix, "otherdata", and
#' # save the output codes to "codes": 
#' 
#' \donttest{
#' output <- sparse_coding(input_model=model, test=otherdata)
#' codes <- output$codes
#' }
sparse_coding <- function(atoms=NA,
                          initial_dictionary=NA,
                          input_model=NA,
                          lambda1=NA,
                          lambda2=NA,
                          max_iterations=NA,
                          newton_tolerance=NA,
                          normalize=FALSE,
                          objective_tolerance=NA,
                          seed=NA,
                          test=NA,
                          training=NA,
                          verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Sparse Coding")

  # Process each input argument before calling mlpackMain().
  if (!identical(atoms, NA)) {
    IO_SetParamInt("atoms", atoms)
  }

  if (!identical(initial_dictionary, NA)) {
    IO_SetParamMat("initial_dictionary", to_matrix(initial_dictionary))
  }

  if (!identical(input_model, NA)) {
    IO_SetParamSparseCodingPtr("input_model", input_model)
  }

  if (!identical(lambda1, NA)) {
    IO_SetParamDouble("lambda1", lambda1)
  }

  if (!identical(lambda2, NA)) {
    IO_SetParamDouble("lambda2", lambda2)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(newton_tolerance, NA)) {
    IO_SetParamDouble("newton_tolerance", newton_tolerance)
  }

  if (!identical(normalize, FALSE)) {
    IO_SetParamBool("normalize", normalize)
  }

  if (!identical(objective_tolerance, NA)) {
    IO_SetParamDouble("objective_tolerance", objective_tolerance)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (!identical(training, NA)) {
    IO_SetParamMat("training", to_matrix(training))
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("codes")
  IO_SetPassed("dictionary")
  IO_SetPassed("output_model")

  # Call the program.
  sparse_coding_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamSparseCodingPtr("output_model")
  attr(output_model, "type") <- "SparseCoding"

  # Extract the results in order.
  out <- list(
      "codes" = IO_GetParamMat("codes"),
      "dictionary" = IO_GetParamMat("dictionary"),
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
