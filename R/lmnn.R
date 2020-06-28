#' @export
lmnn <- function(input,
                 batch_size=NA,
                 center=FALSE,
                 distance=NA,
                 k=NA,
                 labels=NA,
                 linear_scan=FALSE,
                 max_iterations=NA,
                 normalize=FALSE,
                 optimizer=NA,
                 passes=NA,
                 print_accuracy=FALSE,
                 range=NA,
                 rank=NA,
                 regularization=NA,
                 seed=NA,
                 step_size=NA,
                 tolerance=NA,
                 verbose=FALSE) {

  CLI_RestoreSettings("Large Margin Nearest Neighbors (LMNN)")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(batch_size, NA)) {
    CLI_SetParamInt("batch_size", batch_size)
  }

  if (!identical(center, FALSE)) {
    CLI_SetParamBool("center", center)
  }

  if (!identical(distance, NA)) {
    CLI_SetParamMat("distance", to_matrix(distance))
  }

  if (!identical(k, NA)) {
    CLI_SetParamInt("k", k)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(linear_scan, FALSE)) {
    CLI_SetParamBool("linear_scan", linear_scan)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(normalize, FALSE)) {
    CLI_SetParamBool("normalize", normalize)
  }

  if (!identical(optimizer, NA)) {
    CLI_SetParamString("optimizer", optimizer)
  }

  if (!identical(passes, NA)) {
    CLI_SetParamInt("passes", passes)
  }

  if (!identical(print_accuracy, FALSE)) {
    CLI_SetParamBool("print_accuracy", print_accuracy)
  }

  if (!identical(range, NA)) {
    CLI_SetParamInt("range", range)
  }

  if (!identical(rank, NA)) {
    CLI_SetParamInt("rank", rank)
  }

  if (!identical(regularization, NA)) {
    CLI_SetParamDouble("regularization", regularization)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(step_size, NA)) {
    CLI_SetParamDouble("step_size", step_size)
  }

  if (!identical(tolerance, NA)) {
    CLI_SetParamDouble("tolerance", tolerance)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("centered_data")
  CLI_SetPassed("output")
  CLI_SetPassed("transformed_data")

  lmnn_mlpackMain()

  out <- list(
      "centered_data" = CLI_GetParamMat("centered_data"),
      "output" = CLI_GetParamMat("output"),
      "transformed_data" = CLI_GetParamMat("transformed_data")
  )

  CLI_ClearSettings()

  return(out)
}
