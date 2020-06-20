#' @title Least Angle Regression (Stagewise/laSso).
#'
#' @description
#' An implementation of LARS: Least Angle Regression (Stagewise/laSso).  This is a
#' stage-wise homotopy-based algorithm for L1-regularized linear regression (LASSO)
#' and L1+L2-regularized linear regression (Elastic Net).
#'
#' This program is able to train a LARS/LASSO/Elastic Net model or load a model
#' from file, output regression predictions for a test set, and save the trained
#' model to a file.  The LARS algorithm is described in more detail below:
#'
#'   Let X be a matrix where each row is a point and each column is a dimension, and
#' let y be a vector of targets.
#'
#' The Elastic Net problem is to solve
#'
#' min_beta 0.5 || X * beta - y ||_2^2 + lambda_1 ||beta||_1 +
#'   0.5 lambda_2 ||beta||_2^2
#'
#' If lambda1 > 0 and lambda2 = 0, the problem is the LASSO.
#' If lambda1 > 0 and lambda2 > 0, the problem is the Elastic Net.
#' If lambda1 = 0 and lambda2 > 0, the problem is ridge regression.
#' If lambda1 = 0 and lambda2 = 0, the problem is unregularized linear regression.
#'
#' For efficiency reasons, it is not recommended to use this algorithm with
#' `lambda1` = 0.  In that case, use the 'linear_regression' program, which
#' implements both unregularized linear regression and ridge regression.
#'
#' To train a LARS/LASSO/Elastic Net model, the `input` and `responses` parameters
#' must be given.  The `lambda1`, `lambda2`, and `use_cholesky` parameters control
#' the training options.  A trained model can be saved with the `output_model`.  If
#' no training is desired at all, a model can be passed via the `input_model`
#' parameter.
#'
#' The program can also provide predictions for test data using either the trained
#' model or the given input model.  Test points can be specified with the `Test`
#' parameter.  Predicted responses to the test points can be saved with the
#' `output_predictions` output parameter.
#'
#' @examples 
#' \dontrun{
#' For example, the following command trains a model on the data `Data` and
#' responses `responses` with lambda1 set to 0.4 and lambda2 set to 0 (so, LASSO is
#' being solved), and then the model is saved to `out1$output_model`:
#' 
#' out1 <- lars(input=Data, lambda1=0.4, lambda2=0, responses=responses)
#'
#' The following command uses the `out1$output_model` to provide predicted responses for
#' the data `test` and save those responses to `out2$output_predictions`: 
#'
#' out2 <- lars(input_model=out1$outputModel, test=test)
#' }
#' @param input Matrix of covariates (X).
#' @param input_model Trained LARS model to use.
#' @param lambda1 Regularization parameter for l1-norm penalty. Default value `0`.
#' @param lambda2 Regularization parameter for l2-norm penalty. Default value `0`.
#' @param responses Matrix of responses/observations (y).
#' @param test Matrix containing points to regress on (test points).
#' @param use_cholesky Use Cholesky decomposition during computation
#'  rather than explicitly computing the full Gram matrix.  Default value `FALSE`.
#' @param verbose Display informational messages and the full list of
#'  parameters and timers at the end of execution.  Default value `FALSE`.
#'
#'
#' @return A list with several components:
#'
#' \item{output_model}{Output LARS model.}
#' \item{output_predictions}{ If --test_file is specified,
#'   this file is where the predicted responses will be saved.}


lars <- function(input = matrix(NA),
                 input_model = NULL,
                 lambda1 = 0.0,
                 lambda2 = 0.0,
                 responses = matrix(NA),
                 test = matrix(NA),
                 use_cholesky = FALSE,
                 verbose = FALSE) {
  CLI_RestoreSettings("LARS")

  if (!identical(input, matrix(NA))) {
    CLI_SetParamMat("input", to_matrix(input))
  }

  if (!identical(input_model, NULL)) {
    CLI_SetParamLARSPtr("input_model", input_model)
  }

  if (lambda1 != 0) {
    CLI_SetParamDouble("lambda1", lambda1)
  }

  if (lambda2 != 0) {
    CLI_SetParamDouble("lambda2", lambda2)
  }

  if (!identical(responses, matrix(NA))) {
    CLI_SetParamMat("responses", to_matrix(responses))
  }

  if (!identical(test, matrix(NA))) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (use_cholesky != FALSE) {
    CLI_SetParamBool("use_cholesky", use_cholesky)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("output_predictions")

  lars_mlpackMain()
  
  output_model <- CLI_GetParamLARSPtr("output_model")
  attr(output_model, "type") <- "LARS"

  out <- list(
    "output_predictions" = CLI_GetParamMat("output_predictions"),
    "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}

#' Extract serialized information for model.
#'
#' @title Serialize LARS to xml
#' @param model_in Input model.
#' @return transformed_model
serialize_lars_to_xml <- function(model_in = NULL) {
  if (!identical(model_in, NULL)) {
    return(transform_model(SerializeLARSToXML(model_in)))
  }
}
