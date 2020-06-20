#' A simple program to test R binding functionality.
#'
#' @title Run to test R binding functionality
#' @param double_in Input double, must be 4.0.
#' @param int_in Input int, must be 12.
#' @param string_in Input string, must be 'hello'.
#' @param build_model If true, a model will be returned.
#' @param col_in Input column.
#' @param flag1 Input flag, must be specified.
#' @param flag2 Input flag, must not be specified.
#' @param matrix_and_info_in Input matrix and info.
#' @param matrix_in Input matrix.
#' @param model_in Input model.
#' @param row_in Input row.
#' @param str_vector_in Input vector of strings.
#' @param ucol_in Input unsigned column.
#' @param umatrix_in Input unsigned matrix.
#' @param urow_in Input unsigned row.
#' @param vector_in Input unsigned vector.
#' @param verbose Display informational messages and the full list of
#' parameters and timers at the end of execution.  Default value `FALSE`.
#'
#' @return A list with several components:
#' \item{col_out}{ Output column. 2x input column}
#' \item{double_out}{ Output double, will be 5.0.}
#' \item{int_out}{ Output int, will be 13.}
#' \item{matrix_and_info_out}{ Output matrix and info; all
#'       numeric elements multiplied by 3. }
#' \item{matrix_out}{ Output matrix.}
#' \item{model_bw_out}{ The bandwidth of the model.}
#' \item{model_out}{ Output model, with twice the bandwidth.}
#' \item{row_out}{ Output row.  2x input row.}
#' \item{str_vector_out}{ Output string vector.}
#' \item{string_out}{ Output string, will be 'hello2'.}
#' \item{ucol_out}{ Output unsigned column. 2x input column.}
#' \item{umatrix_out}{ Output unsigned matrix.}
#' \item{urow_out}{ Output unsigned row.  2x input row.}
#' \item{vector_out}{ Output vector.}
#' @examples
#' ## test_r_binding:
#' x <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), nrow = 3)
#' out1 <- test_r_binding(4.0, 12, "hello", flag1 = TRUE, build_model = TRUE)
#' out2 <- test_r_binding(4.0, 12, "hello", matrix_in=x, model_in = out1$model_out)
test_r_binding <- function(double_in,
                           int_in,
                           string_in,
                           build_model = FALSE,
                           col_in = matrix(NA),
                           flag1 = FALSE,
                           flag2 = FALSE,
                           matrix_and_info_in = data.frame(NA),
                           matrix_in = matrix(NA),
                           model_in = NULL,
                           row_in = matrix(NA),
                           str_vector_in = NA,
                           ucol_in = matrix(NA),
                           umatrix_in = matrix(NA),
                           urow_in = matrix(NA),
                           vector_in = NA,
                           verbose = FALSE) {
  CLI_RestoreSettings("R binding test")

  CLI_SetParamDouble("double_in", double_in)

  CLI_SetParamInt("int_in", int_in)

  CLI_SetParamString("string_in", string_in)

  if (build_model != FALSE) {
    CLI_SetParamBool("build_model", build_model)
  }

  if (!identical(col_in, matrix(NA))) {
    CLI_SetParamCol("col_in", to_matrix(col_in))
  }

  if (flag1 != FALSE) {
    CLI_SetParamBool("flag1", flag1)
  }

  if (flag2 != FALSE) {
    CLI_SetParamBool("flag2", flag2)
  }

  if (!identical(matrix_and_info_in, data.frame(NA))) {
    mat_and_info <- to_matrix_with_info(matrix_and_info_in)
    CLI_SetParamMatWithInfo("matrix_and_info_in", mat_and_info$info, mat_and_info$data)
  }

  if (!identical(matrix_in, matrix(NA))) {
    CLI_SetParamMat("matrix_in", to_matrix(matrix_in))
  }

  if (!identical(model_in, NULL)) {
    CLI_SetParamGaussianKernelPtr("model_in", model_in)
  }

  if (!identical(row_in, NA)) {
    CLI_SetParamRow("row_in", to_matrix(row_in))
  }

  if (!identical(str_vector_in, NA)) {
    CLI_SetParamVectorStr("str_vector_in", str_vector_in)
  }

  if (!identical(ucol_in, matrix(NA))) {
    CLI_SetParamUCol("ucol_in", to_matrix(ucol_in))
  }

  if (!identical(umatrix_in, matrix(NA))) {
    CLI_SetParamUMat("umatrix_in", to_matrix(umatrix_in))
  }

  if (!identical(urow_in, matrix(NA))) {
    CLI_SetParamURow("urow_in", to_matrix(urow_in))
  }

  if (!identical(vector_in, NA)) {
    CLI_SetParamVectorInt("vector_in", vector_in)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("col_out")
  CLI_SetPassed("double_out")
  CLI_SetPassed("int_out")
  CLI_SetPassed("matrix_and_info_out")
  CLI_SetPassed("matrix_out")
  CLI_SetPassed("model_bw_out")
  CLI_SetPassed("model_out")
  CLI_SetPassed("row_out")
  CLI_SetPassed("str_vector_out")
  CLI_SetPassed("string_out")
  CLI_SetPassed("ucol_out")
  CLI_SetPassed("umatrix_out")
  CLI_SetPassed("urow_out")
  CLI_SetPassed("vector_out")

  test_r_binding_mlpackMain()

  output_model <- CLI_GetParamGaussianKernelPtr("model_out")
  attr(output_model, "type") <- "GaussianKernel"

  out <- list(
    "col_out" = CLI_GetParamCol("col_out"),
    "double_out" = CLI_GetParamDouble("double_out"),
    "int_out" = CLI_GetParamInt("int_out"),
    "matrix_and_info_out" = CLI_GetParamMat("matrix_and_info_out"),
    "matrix_out" = CLI_GetParamMat("matrix_out"),
    "model_bw_out" = CLI_GetParamDouble("model_bw_out"),
    "model_out" = output_model,
    "row_out" = CLI_GetParamRow("row_out"),
    "str_vector_out" = CLI_GetParamVectorStr("str_vector_out"),
    "string_out" = CLI_GetParamString("string_out"),
    "ucol_out" = CLI_GetParamUCol("ucol_out"),
    "umatrix_out" = CLI_GetParamUMat("umatrix_out"),
    "urow_out" = CLI_GetParamURow("urow_out"),
    "vector_out" = CLI_GetParamVectorInt("vector_out")
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
