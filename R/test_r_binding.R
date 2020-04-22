#' A simple program to test R binding functionality.
#'
#' @title Run to test R binding functionality
#' @param double_in Input double, must be 4.0.
#' @param int_in Input int, must be 12.
#' @param string_in Input string, must be 'hello'.
#' @param matrix_in Input matrix.
#' @param build_model If true, a model will be returned.
#' @param flag1 Input flag, must be specified.
#' @param flag2 Input flag, must not be specified.
#' @param model_in Input model.
#' @param copy_all_inputs If specified, all input parameters will be
#' deep copied before the method is run.  This is useful for debugging
#' problems where the input parameters are being modified by the algorithm,
#' but can slow down the code.
#' @param verbose Display informational messages and the full list of
#' parameters and timers at the end of execution.  Default value `FALSE`.
#'
#' @return A list with several components:
#' \item{double_out}{ Output double, will be 5.0.}
#' \item{int_out}{ Output int, will be 13.}
#' \item{string_out}{ Output string, will be 'hello2'.}
#' \item{matrix_out}{ Output matrix.}
#' \item{model_out}{ Output model, with twice the bandwidth.}
#' \item{model_bw_out}{ The bandwidth of the model.}
#' @examples
#' ## test_r_binding:
#' x <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), nrow = 5)
#' out1 <- test_r_binding(4.0, 12, "hello", flag1 = TRUE, build_model = TRUE)
#' out2 <- test_r_binding(4.0, 12, "hello", x, TRUE, TRUE, model_in = out1$model_out)
test_r_binding <- function(double_in,
                           int_in,
                           string_in,
                           matrix_in = matrix(NA),
                           build_model = FALSE,
                           flag1 = FALSE,
                           flag2 = FALSE,
                           model_in = NULL,
                           copy_all_inputs = FALSE,
                           verbose = FALSE) {
  CLI_RestoreSettings("R binding test")

  CLI_SetParamDouble("double_in", double_in)

  CLI_SetParamInt("int_in", int_in)

  CLI_SetParamString("string_in", string_in)

  if (build_model != FALSE) {
    CLI_SetParamBool("build_model", build_model)
  }

  if (!identical(model_in, NULL)) {
    CLI_SetParamGaussianKernelPtr("model_in", model_in)
  }

  if (!identical(matrix_in, matrix(NA))) {
    CLI_SetParamMat("matrix_in", matrix_in, copy_all_inputs)
  }

  if (flag1 != FALSE) {
    CLI_SetParamBool("flag1", flag1)
  }

  if (flag2 != FALSE) {
    CLI_SetParamBool("flag2", flag2)
  }

  if (verbose != FALSE || verbose == TRUE) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("double_out")
  CLI_SetPassed("int_out")
  CLI_SetPassed("string_out")
  CLI_SetPassed("matrix_out")
  CLI_SetPassed("model_out")
  CLI_SetPassed("model_bw_out")

  test_r_binding_mlpackMain()

  out <- list(
    "double_out" = CLI_GetParamDouble("double_out"),
    "int_out" = CLI_GetParamInt("int_out"),
    "string_out" = CLI_GetParamString("string_out"),
    "matrix_out" = CLIGetParamMat("matrix_out"),
    "model_out" = CLI_GetParamGaussianKernelPtr("model_out"),
    "model_bw_out" = CLI_GetParamDouble("model_bw_out")
  )

  CLI_ClearSettings()
  
  return(out)
}

#' Extract serialized information for model.
#'
#' @title Serialize GaussianKernel to xml
#' @param model_in Input model.
#' @return transformed_model
serialize_gaussian_kernel_to_xml <- function(model_in = NULL) {
  if (!identical(model_in, NULL)) {
    return(transform_model(SerializeGaussianKernelToXML(model_in)))
  }
}

#' Serialize a model to the given filename.
#'
#' @title Serialize GaussianKernel.
#' @param filename Input filename.
#' @param model_in Input model.
serialize_gaussian_kernel <- function(filename, model_in = NULL) {
  if (!identical(model_in, NULL)) {
    con <- file(as.character(filename), "wb")
    serialize(SerializeGaussianKernelPtr(model_in), con)
    close(con)
  }
}

#' Unserialize a model to the given filename.
#'
#' @title Unserialize GaussianKernel.
#' @param filename Input filename.
#' @return model_ptr Output model.
unserialize_gaussian_kernel <- function(filename) {
  con <- file(as.character(filename), "rb")
  model_ptr <- UnserializeGaussianKernelPtr(unserialize(con))
  close(con)
  return(model_ptr)
}