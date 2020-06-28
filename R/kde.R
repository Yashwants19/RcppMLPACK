#' @export
kde <- function(abs_error=NA,
                algorithm=NA,
                bandwidth=NA,
                initial_sample_size=NA,
                input_model=NA,
                kernel=NA,
                mc_break_coef=NA,
                mc_entry_coef=NA,
                mc_probability=NA,
                monte_carlo=FALSE,
                query=NA,
                reference=NA,
                rel_error=NA,
                tree=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("Kernel Density Estimation")

  if (!identical(abs_error, NA)) {
    CLI_SetParamDouble("abs_error", abs_error)
  }

  if (!identical(algorithm, NA)) {
    CLI_SetParamString("algorithm", algorithm)
  }

  if (!identical(bandwidth, NA)) {
    CLI_SetParamDouble("bandwidth", bandwidth)
  }

  if (!identical(initial_sample_size, NA)) {
    CLI_SetParamInt("initial_sample_size", initial_sample_size)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamKDEModelPtr("input_model", input_model)
  }

  if (!identical(kernel, NA)) {
    CLI_SetParamString("kernel", kernel)
  }

  if (!identical(mc_break_coef, NA)) {
    CLI_SetParamDouble("mc_break_coef", mc_break_coef)
  }

  if (!identical(mc_entry_coef, NA)) {
    CLI_SetParamDouble("mc_entry_coef", mc_entry_coef)
  }

  if (!identical(mc_probability, NA)) {
    CLI_SetParamDouble("mc_probability", mc_probability)
  }

  if (!identical(monte_carlo, FALSE)) {
    CLI_SetParamBool("monte_carlo", monte_carlo)
  }

  if (!identical(query, NA)) {
    CLI_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    CLI_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(rel_error, NA)) {
    CLI_SetParamDouble("rel_error", rel_error)
  }

  if (!identical(tree, NA)) {
    CLI_SetParamString("tree", tree)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")

  kde_mlpackMain()

  output_model <- CLI_GetParamKDEModelPtr("output_model")
  attr(output_model, "type") <- "KDEModel"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamCol("predictions")
  )

  CLI_ClearSettings()

  return(out)
}
