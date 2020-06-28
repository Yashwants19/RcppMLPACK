#' @export
nca <- function(input,
                armijo_constant=NA,
                batch_size=NA,
                labels=NA,
                linear_scan=FALSE,
                max_iterations=NA,
                max_line_search_trials=NA,
                max_step=NA,
                min_step=NA,
                normalize=FALSE,
                num_basis=NA,
                optimizer=NA,
                seed=NA,
                step_size=NA,
                tolerance=NA,
                verbose=FALSE,
                wolfe=NA) {

  CLI_RestoreSettings("Neighborhood Components Analysis (NCA)")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(armijo_constant, NA)) {
    CLI_SetParamDouble("armijo_constant", armijo_constant)
  }

  if (!identical(batch_size, NA)) {
    CLI_SetParamInt("batch_size", batch_size)
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

  if (!identical(max_line_search_trials, NA)) {
    CLI_SetParamInt("max_line_search_trials", max_line_search_trials)
  }

  if (!identical(max_step, NA)) {
    CLI_SetParamDouble("max_step", max_step)
  }

  if (!identical(min_step, NA)) {
    CLI_SetParamDouble("min_step", min_step)
  }

  if (!identical(normalize, FALSE)) {
    CLI_SetParamBool("normalize", normalize)
  }

  if (!identical(num_basis, NA)) {
    CLI_SetParamInt("num_basis", num_basis)
  }

  if (!identical(optimizer, NA)) {
    CLI_SetParamString("optimizer", optimizer)
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

  if (!identical(wolfe, NA)) {
    CLI_SetParamDouble("wolfe", wolfe)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  nca_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
