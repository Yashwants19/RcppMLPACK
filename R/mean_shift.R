#' @export
mean_shift <- function(input,
                       force_convergence=FALSE,
                       in_place=FALSE,
                       labels_only=FALSE,
                       max_iterations=NA,
                       radius=NA,
                       verbose=FALSE) {

  CLI_RestoreSettings("Mean Shift Clustering")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(force_convergence, FALSE)) {
    CLI_SetParamBool("force_convergence", force_convergence)
  }

  if (!identical(in_place, FALSE)) {
    CLI_SetParamBool("in_place", in_place)
  }

  if (!identical(labels_only, FALSE)) {
    CLI_SetParamBool("labels_only", labels_only)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(radius, NA)) {
    CLI_SetParamDouble("radius", radius)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("centroid")
  CLI_SetPassed("output")

  mean_shift_mlpackMain()

  out <- list(
      "centroid" = CLI_GetParamMat("centroid"),
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
