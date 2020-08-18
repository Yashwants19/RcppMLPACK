#' @title R binding test
#'
#' @description
#' A simple program to test R binding functionality.
#'
#' @param double_in Input double, must be 4.0 (numeric).
#' @param int_in Input int, must be 12 (integer).
#' @param string_in Input string, must be 'hello' (character).
#' @param build_model If true, a model will be returned.  Default value
#'   "FALSE" (logical).
#' @param col_in Input column (numeric column).
#' @param flag1 Input flag, must be specified.  Default value "FALSE"
#'   (logical).
#' @param flag2 Input flag, must not be specified.  Default value "FALSE"
#'   (logical).
#' @param matrix_and_info_in Input matrix and info (numeric
#'   matrix/data.frame with info).
#' @param matrix_in Input matrix (numeric matrix).
#' @param model_in Input model (GaussianKernel).
#' @param row_in Input row (numeric row).
#' @param str_vector_in Input vector of strings (character vector).
#' @param ucol_in Input unsigned column (integer column).
#' @param umatrix_in Input unsigned matrix (integer matrix).
#' @param urow_in Input unsigned row (integer row).
#' @param vector_in Input vector of numbers (integer vector).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{col_out}{Output column. 2x input colum (numeric column).}
#' \item{double_out}{Output double, will be 5.0.  Default value "0"
#'   (numeric).}
#' \item{int_out}{Output int, will be 13.  Default value "0" (integer).}
#' \item{matrix_and_info_out}{Output matrix and info; all numeric elements
#'   multiplied by 3 (numeric matrix).}
#' \item{matrix_out}{Output matrix (numeric matrix).}
#' \item{model_bw_out}{The bandwidth of the model.  Default value "0"
#'   (numeric).}
#' \item{model_out}{Output model, with twice the bandwidth
#'   (GaussianKernel).}
#' \item{row_out}{Output row.  2x input row (numeric row).}
#' \item{str_vector_out}{Output string vector (character vector).}
#' \item{string_out}{Output string, will be 'hello2'.  Default value ""
#'   (character).}
#' \item{ucol_out}{Output unsigned column. 2x input column (integer
#'   column).}
#' \item{umatrix_out}{Output unsigned matrix (integer matrix).}
#' \item{urow_out}{Output unsigned row.  2x input row (integer row).}
#' \item{vector_out}{Output vector (integer vector).}
#'
#' @details
#' A simple program to test R binding functionality.  You can build mlpack with
#' the BUILD_TESTS option set to off, and this binding will no longer be built.
#'
#' @author
#' mlpack developers
#'
#' @export

test_r_binding <- function(double_in,
                           int_in,
                           string_in,
                           build_model=FALSE,
                           col_in=NA,
                           flag1=FALSE,
                           flag2=FALSE,
                           matrix_and_info_in=NA,
                           matrix_in=NA,
                           model_in=NA,
                           row_in=NA,
                           str_vector_in=NA,
                           ucol_in=NA,
                           umatrix_in=NA,
                           urow_in=NA,
                           vector_in=NA,
                           verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("R binding test")

  # Process each input argument before calling mlpackMain().
  IO_SetParamDouble("double_in", double_in)

  IO_SetParamInt("int_in", int_in)

  IO_SetParamString("string_in", string_in)

  if (!identical(build_model, FALSE)) {
    IO_SetParamBool("build_model", build_model)
  }

  if (!identical(col_in, NA)) {
    IO_SetParamCol("col_in", to_matrix(col_in))
  }

  if (!identical(flag1, FALSE)) {
    IO_SetParamBool("flag1", flag1)
  }

  if (!identical(flag2, FALSE)) {
    IO_SetParamBool("flag2", flag2)
  }

  if (!identical(matrix_and_info_in, NA)) {
    matrix_and_info_in <- to_matrix_with_info(matrix_and_info_in)
    IO_SetParamMatWithInfo("matrix_and_info_in", matrix_and_info_in$info, matrix_and_info_in$data)
  }

  if (!identical(matrix_in, NA)) {
    IO_SetParamMat("matrix_in", to_matrix(matrix_in))
  }

  if (!identical(model_in, NA)) {
    IO_SetParamGaussianKernelPtr("model_in", model_in)
  }

  if (!identical(row_in, NA)) {
    IO_SetParamRow("row_in", to_matrix(row_in))
  }

  if (!identical(str_vector_in, NA)) {
    IO_SetParamVecString("str_vector_in", str_vector_in)
  }

  if (!identical(ucol_in, NA)) {
    IO_SetParamUCol("ucol_in", to_matrix(ucol_in))
  }

  if (!identical(umatrix_in, NA)) {
    IO_SetParamUMat("umatrix_in", to_matrix(umatrix_in))
  }

  if (!identical(urow_in, NA)) {
    IO_SetParamURow("urow_in", to_matrix(urow_in))
  }

  if (!identical(vector_in, NA)) {
    IO_SetParamVecInt("vector_in", vector_in)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("col_out")
  IO_SetPassed("double_out")
  IO_SetPassed("int_out")
  IO_SetPassed("matrix_and_info_out")
  IO_SetPassed("matrix_out")
  IO_SetPassed("model_bw_out")
  IO_SetPassed("model_out")
  IO_SetPassed("row_out")
  IO_SetPassed("str_vector_out")
  IO_SetPassed("string_out")
  IO_SetPassed("ucol_out")
  IO_SetPassed("umatrix_out")
  IO_SetPassed("urow_out")
  IO_SetPassed("vector_out")

  # Call the program.
  test_r_binding_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  model_out <- IO_GetParamGaussianKernelPtr("model_out")
  attr(model_out, "type") <- "GaussianKernel"

  # Extract the results in order.
  out <- list(
      "col_out" = IO_GetParamCol("col_out"),
      "double_out" = IO_GetParamDouble("double_out"),
      "int_out" = IO_GetParamInt("int_out"),
      "matrix_and_info_out" = IO_GetParamMat("matrix_and_info_out"),
      "matrix_out" = IO_GetParamMat("matrix_out"),
      "model_bw_out" = IO_GetParamDouble("model_bw_out"),
      "model_out" = model_out,
      "row_out" = IO_GetParamRow("row_out"),
      "str_vector_out" = IO_GetParamVecString("str_vector_out"),
      "string_out" = IO_GetParamString("string_out"),
      "ucol_out" = IO_GetParamUCol("ucol_out"),
      "umatrix_out" = IO_GetParamUMat("umatrix_out"),
      "urow_out" = IO_GetParamURow("urow_out"),
      "vector_out" = IO_GetParamVecInt("vector_out")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
