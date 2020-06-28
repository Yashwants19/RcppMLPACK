#' @export
fastmks <- function(bandwidth=NA,
                    base=NA,
                    degree=NA,
                    input_model=NA,
                    k=NA,
                    kernel=NA,
                    naive=FALSE,
                    offset=NA,
                    query=NA,
                    reference=NA,
                    scale=NA,
                    single=FALSE,
                    verbose=FALSE) {

  CLI_RestoreSettings("FastMKS (Fast Max-Kernel Search)")

  if (!identical(bandwidth, NA)) {
    CLI_SetParamDouble("bandwidth", bandwidth)
  }

  if (!identical(base, NA)) {
    CLI_SetParamDouble("base", base)
  }

  if (!identical(degree, NA)) {
    CLI_SetParamDouble("degree", degree)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamFastMKSModelPtr("input_model", input_model)
  }

  if (!identical(k, NA)) {
    CLI_SetParamInt("k", k)
  }

  if (!identical(kernel, NA)) {
    CLI_SetParamString("kernel", kernel)
  }

  if (!identical(naive, FALSE)) {
    CLI_SetParamBool("naive", naive)
  }

  if (!identical(offset, NA)) {
    CLI_SetParamDouble("offset", offset)
  }

  if (!identical(query, NA)) {
    CLI_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    CLI_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(scale, NA)) {
    CLI_SetParamDouble("scale", scale)
  }

  if (!identical(single, FALSE)) {
    CLI_SetParamBool("single", single)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("indices")
  CLI_SetPassed("kernels")
  CLI_SetPassed("output_model")

  fastmks_mlpackMain()

  output_model <- CLI_GetParamFastMKSModelPtr("output_model")
  attr(output_model, "type") <- "FastMKSModel"

  out <- list(
      "indices" = CLI_GetParamUMat("indices"),
      "kernels" = CLI_GetParamMat("kernels"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
