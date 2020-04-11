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
#' `Lambda1` = 0.  In that case, use the 'linear_regression' program, which
#' implements both unregularized linear regression and ridge regression.
#'
#' To train a LARS/LASSO/Elastic Net model, the `Input` and `Responses` parameters
#' must be given.  The `Lambda1`, `Lambda2`, and `UseCholesky` parameters control
#' the training options.  A trained model can be saved with the `OutputModel`.  If
#' no training is desired at all, a model can be passed via the `InputModel`
#' parameter.
#'
#' The program can also provide predictions for test data using either the trained
#' model or the given input model.  Test points can be specified with the `Test`
#' parameter.  Predicted responses to the test points can be saved with the
#' `OutputPredictions` output parameter.
#'
#' @examples 
#' \dontrun{
#' For example, the following command trains a model on the data `Data` and
#' responses `Responses` with lambda1 set to 0.4 and lambda2 set to 0 (so, LASSO is
#' being solved), and then the model is saved to `out1$outputModel`:
#' 
#' out1 <- Lars(Input=Data, Lambda1=0.4, Lambda2=0, Responses=Responses)
#'
#' The following command uses the `out1$OutputModel` to provide predicted responses for
#' the data `Test` and save those responses to `out2$outputPredictions`: 
#'
#' out2 <- Lars(InputModel=out1$outputModel, Test=test)
#' }
#' @param Input Matrix of covariates (X).
#' @param InputModel Trained LARS model to use.
#' @param Lambda1 Regularization parameter for l1-norm penalty. Default value `0`.
#' @param Lambda2 Regularization parameter for l2-norm penalty. Default value `0`.
#' @param Responses Matrix of responses/observations (y).
#' @param Test Matrix containing points to regress on (test points).
#' @param UseCholesky Use Cholesky decomposition during computation
#'  rather than explicitly computing the full Gram matrix.  Default value `FALSE`.
#' @param Verbose Display informational messages and the full list of
#'  parameters and timers at the end of execution.  Default value `FALSE`.
#'
#'
#' @return A list with several components:
#'
#' \item{outputModel}{Output LARS model.}
#' \item{outputPredictions}{ If --test_file is specified,
#'   this file is where the predicted responses will be saved.}


Lars <- function(
    Input = matrix(NA),
    InputModel = NULL,
    Lambda1 = 0.0,
    Lambda2 = 0.0,
    Responses = matrix(NA),
    Test = matrix(NA),
    UseCholesky = FALSE,
    Verbose = FALSE)
{
  CLI_RestoreSettings("LARS")
  
  if (!identical(Input, matrix(NA)))
  {
    CLI_SetParamMat("input", Input)
  }

  if (!identical(InputModel, NULL))
  {
    CLI_SetParamLARSPtr("input_model", InputModel)
  }

  if (!identical(InputModel, NULL))
  {
  outputXml = SerializeLARSToXML("LARS", InputModel)    
  }

  if (Lambda1 != 0) 
  {
    CLI_SetParamDouble("lambda1", Lambda1)
  }

  if (Lambda2 != 0)
  {
    CLI_SetParamDouble("lambda2", Lambda2)
  }

  if (!identical(Responses, matrix(NA)))
  {
    CLI_SetParamMat("responses", Responses)
  }

  if (!identical(Test, matrix(NA)))
  {
    CLI_SetParamMat("test", Test)
  }

  if (UseCholesky != FALSE) 
  {
    CLI_SetParamBool("use_cholesky", UseCholesky)
  }
  
  if (Verbose != FALSE || Verbose == TRUE)
    CLI_EnableVerbose()
  else
    CLI_DisableVerbose()

  CLI_SetPassed("output_model")
  CLI_SetPassed("output_predictions")

  lars_mlpackMain()

  outputPredictions = CLIGetParamMat("output_predictions")
  outputModel = CLI_GetParamLARSPtr("output_model")

  if (identical(InputModel, NULL))
  {
    outputXml = NULL
  }

  CLI_ClearSettings()

  out <- list("outputModel" = outputModel, "outputPredictions" = outputPredictions, "outputXml" = outputXml)
  
  return (out)
}
