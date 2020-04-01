#' A simple program to test R binding functionality.
#'
#' @title Run to test R binding functionality
#' @param doubleIn Input double, must be 4.0.
#' @param intIn Input int, must be 12.
#' @param stringIn Input string, must be 'hello'.
#' @param matrixIn Input matrix.
#' @param buildModel If true, a model will be returned.
#' @param flag1 Input flag, must be specified.
#' @param flag2 Input flag, must not be specified.
#' @param modelIn Input model.
#' @return A list with several components: Output double, will be 5.0,
#' Output int, will be 13, Output string, will be 'hello2', Output matrix, 
#' Output model, with twice the bandwidth, The bandwidth of the model.
#' @examples
#' ## testRBinding:
#' x <- matrix ( c( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), nrow = 5)
#' out1 <- testRBinding(4.0, 12, 'hello', flag1 = TRUE, buildModel = TRUE)
#' out2 <- testRBinding(4.0, 12 , 'hello', x, TRUE, TRUE, modelIn = out1$modelOut)
testRBinding <- function(doubleIn, intIn, stringIn, matrixIn = matrix(NA), buildModel = FALSE, flag1 = FALSE, flag2 = FALSE, modelIn = NULL)
{
  CLI_RestoreSettings("R binding test")

  CLI_SetParamDouble("double_in", doubleIn)

  CLI_SetParamInt("int_in", intIn)

  CLI_SetParamString("string_in", stringIn)

  if (buildModel != FALSE)
  {
    CLI_SetParamBool("build_model", buildModel)
  }

  if (!identical(modelIn,NULL))
  {
    CLI_SetParamGaussianKernelPtr("model_in", modelIn)
  }

  if (!identical(matrixIn,matrix(NA)))
  {
    CLI_SetParamMat("matrix_in", matrixIn)
  }

  if (flag1 != FALSE)
  {
    CLI_SetParamBool("flag1", flag1)
  }

  if (flag2 != FALSE)
  {
    CLI_SetParamBool("flag2", flag2)
  }

  CLI_SetPassed("double_out")
  CLI_SetPassed("int_out")
  CLI_SetPassed("string_out")
  CLI_SetPassed("matrix_out")
  CLI_SetPassed("model_out")
  CLI_SetPassed("model_bw_out")

  test_r_binding_mlpackMain()

  doubleOut = CLI_GetParamDouble("double_out")
  intOut = CLI_GetParamInt("int_out")
  stringOut = CLI_GetParamString("string_out")
  matrixOut = CLIGetParamMat("matrix_out")
  modelOut = CLI_GetParamGaussianKernelPtr("model_out")
  modelBwout = CLI_GetParamDouble("model_bw_out")

  CLI_ClearSettings()

  my_list <- list("doubleOut" = doubleOut, "intOut" = intOut, "stringOut" = stringOut, "matrixOut" = matrixOut, "modelOut" = modelOut, "modelBwout" = modelBwout)

  return (my_list)
}
