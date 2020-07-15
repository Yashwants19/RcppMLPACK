#' @title Local Coordinate Coding
#'
#' @description
#' An implementation of Local Coordinate Coding (LCC), a data transformation
#' technique.  Given input data, this transforms each point to be expressed as a
#' linear combination of a few points in the dataset; once an LCC model is
#' trained, it can be used to transform points later also.
#'
#' @param atoms Number of atoms in the dictionary.  Default value "0".
#' @param initial_dictionary Optional initial dictionary.
#' @param input_model Input LCC model.
#' @param lambda Weighted l1-norm regularization parameter.  Default value "0".
#' @param max_iterations Maximum number of iterations for LCC (0 indicates no limit).
#'    Default value "0".
#' @param normalize If set, the input data matrix will be normalized before coding. 
#'   Default value "FALSE".
#' @param seed Random seed.  If 0, 'std::time(NULL)' is used.  Default value "0".
#' @param test Test points to encode.
#' @param tolerance Tolerance for objective function.  Default value "0.01".
#' @param training Matrix of training data (X).
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{codes}{Output codes matrix.}
#' \item{dictionary}{Output dictionary matrix.}
#' \item{output_model}{Output for trained LCC model.}
#'
#' @details
#' An implementation of Local Coordinate Coding (LCC), which codes data that
#' approximately lives on a manifold using a variation of l1-norm regularized
#' sparse coding.  Given a dense data matrix X with n points and d dimensions,
#' LCC seeks to find a dense dictionary matrix D with k atoms in d dimensions,
#' and a coding matrix Z with n points in k dimensions.  Because of the
#' regularization method used, the atoms in D should lie close to the manifold
#' on which the data points lie.
#' 
#' The original data matrix X can then be reconstructed as D * Z.  Therefore,
#' this program finds a representation of each point in X as a sparse linear
#' combination of atoms in the dictionary D.
#' 
#' The coding is found with an algorithm which alternates between a dictionary
#' step, which updates the dictionary D, and a coding step, which updates the
#' coding matrix Z.
#' 
#' To run this program, the input matrix X must be specified (with -i), along
#' with the number of atoms in the dictionary (-k).  An initial dictionary may
#' also be specified with the "initial_dictionary" parameter.  The l1-norm
#' regularization parameter is specified with the "lambda" parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # For example, to run LCC on the dataset "data" using 200 atoms and an
#' # l1-regularization parameter of 0.1, saving the dictionary "dictionary" and
#' # the codes into "codes", use
#' 
#' \donttest{
#' output <- local_coordinate_coding(training=data, atoms=200, lambda=0.1)
#' dict <- output$dictionary
#' codes <- output$codes
#' }
#' 
#' # The maximum number of iterations may be specified with the "max_iterations"
#' # parameter. Optionally, the input data matrix X can be normalized before
#' # coding with the "normalize" parameter.
#' # 
#' # An LCC model may be saved using the "output_model" output parameter.  Then,
#' # to encode new points from the dataset "points" with the previously saved
#' # model "lcc_model", saving the new codes to "new_codes", the following
#' # command can be used:
#' 
#' \donttest{
#' output <- local_coordinate_coding(input_model=lcc_model, test=points)
#' new_codes <- output$codes
#' }
local_coordinate_coding <- function(atoms=NA,
                                    initial_dictionary=NA,
                                    input_model=NA,
                                    lambda=NA,
                                    max_iterations=NA,
                                    normalize=FALSE,
                                    seed=NA,
                                    test=NA,
                                    tolerance=NA,
                                    training=NA,
                                    verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Local Coordinate Coding")

  # Process each input argument before calling mlpackMain().
  if (!identical(atoms, NA)) {
    IO_SetParamInt("atoms", atoms)
  }

  if (!identical(initial_dictionary, NA)) {
    IO_SetParamMat("initial_dictionary", to_matrix(initial_dictionary))
  }

  if (!identical(input_model, NA)) {
    IO_SetParamLocalCoordinateCodingPtr("input_model", input_model)
  }

  if (!identical(lambda, NA)) {
    IO_SetParamDouble("lambda", lambda)
  }

  if (!identical(max_iterations, NA)) {
    IO_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(normalize, FALSE)) {
    IO_SetParamBool("normalize", normalize)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (!identical(test, NA)) {
    IO_SetParamMat("test", to_matrix(test))
  }

  if (!identical(tolerance, NA)) {
    IO_SetParamDouble("tolerance", tolerance)
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
  local_coordinate_coding_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamLocalCoordinateCodingPtr("output_model")
  attr(output_model, "type") <- "LocalCoordinateCoding"

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
