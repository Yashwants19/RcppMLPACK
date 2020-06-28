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

  CLI_RestoreSettings("R binding test")

  CLI_SetParamDouble("double_in", double_in)

  CLI_SetParamInt("int_in", int_in)

  CLI_SetParamString("string_in", string_in)

  if (!identical(build_model, FALSE)) {
    CLI_SetParamBool("build_model", build_model)
  }

  if (!identical(col_in, NA)) {
    CLI_SetParamCol("col_in", to_matrix(col_in))
  }

  if (!identical(flag1, FALSE)) {
    CLI_SetParamBool("flag1", flag1)
  }

  if (!identical(flag2, FALSE)) {
    CLI_SetParamBool("flag2", flag2)
  }

  if (!identical(matrix_and_info_in, NA)) {
    matrix_and_info_in <- to_matrix_with_info(matrix_and_info_in)
    CLI_SetParamMatWithInfo("matrix_and_info_in", matrix_and_info_in$info, matrix_and_info_in$data)
  }

  if (!identical(matrix_in, NA)) {
    CLI_SetParamMat("matrix_in", to_matrix(matrix_in))
  }

  if (!identical(model_in, NA)) {
    CLI_SetParamGaussianKernelPtr("model_in", model_in)
  }

  if (!identical(row_in, NA)) {
    CLI_SetParamRow("row_in", to_matrix(row_in))
  }

  if (!identical(str_vector_in, NA)) {
    CLI_SetParamVecString("str_vector_in", str_vector_in)
  }

  if (!identical(ucol_in, NA)) {
    CLI_SetParamUCol("ucol_in", to_matrix(ucol_in))
  }

  if (!identical(umatrix_in, NA)) {
    CLI_SetParamUMat("umatrix_in", to_matrix(umatrix_in))
  }

  if (!identical(urow_in, NA)) {
    CLI_SetParamURow("urow_in", to_matrix(urow_in))
  }

  if (!identical(vector_in, NA)) {
    CLI_SetParamVecInt("vector_in", vector_in)
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

  model_out <- CLI_GetParamGaussianKernelPtr("model_out")
  attr(model_out, "type") <- "GaussianKernel"

  out <- list(
      "col_out" = CLI_GetParamCol("col_out"),
      "double_out" = CLI_GetParamDouble("double_out"),
      "int_out" = CLI_GetParamInt("int_out"),
      "matrix_and_info_out" = CLI_GetParamMat("matrix_and_info_out"),
      "matrix_out" = CLI_GetParamMat("matrix_out"),
      "model_bw_out" = CLI_GetParamDouble("model_bw_out"),
      "model_out" = model_out,
      "row_out" = CLI_GetParamRow("row_out"),
      "str_vector_out" = CLI_GetParamVecString("str_vector_out"),
      "string_out" = CLI_GetParamString("string_out"),
      "ucol_out" = CLI_GetParamUCol("ucol_out"),
      "umatrix_out" = CLI_GetParamUMat("umatrix_out"),
      "urow_out" = CLI_GetParamURow("urow_out"),
      "vector_out" = CLI_GetParamVecInt("vector_out")
  )

  CLI_ClearSettings()

  return(out)
}
