#' @export
kernel_pca <- function(input,
                       kernel,
                       bandwidth=NA,
                       center=FALSE,
                       degree=NA,
                       kernel_scale=NA,
                       new_dimensionality=NA,
                       nystroem_method=FALSE,
                       offset=NA,
                       sampling=NA,
                       verbose=FALSE) {

  CLI_RestoreSettings("Kernel Principal Components Analysis")

  CLI_SetParamMat("input", to_matrix(input))

  CLI_SetParamString("kernel", kernel)

  if (!identical(bandwidth, NA)) {
    CLI_SetParamDouble("bandwidth", bandwidth)
  }

  if (!identical(center, FALSE)) {
    CLI_SetParamBool("center", center)
  }

  if (!identical(degree, NA)) {
    CLI_SetParamDouble("degree", degree)
  }

  if (!identical(kernel_scale, NA)) {
    CLI_SetParamDouble("kernel_scale", kernel_scale)
  }

  if (!identical(new_dimensionality, NA)) {
    CLI_SetParamInt("new_dimensionality", new_dimensionality)
  }

  if (!identical(nystroem_method, FALSE)) {
    CLI_SetParamBool("nystroem_method", nystroem_method)
  }

  if (!identical(offset, NA)) {
    CLI_SetParamDouble("offset", offset)
  }

  if (!identical(sampling, NA)) {
    CLI_SetParamString("sampling", sampling)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  kernel_pca_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
