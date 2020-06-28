#' @export
preprocess_scale <- function(input,
                             epsilon=NA,
                             input_model=NA,
                             inverse_scaling=FALSE,
                             max_value=NA,
                             min_value=NA,
                             scaler_method=NA,
                             seed=NA,
                             verbose=FALSE) {

  CLI_RestoreSettings("Scale Data")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(epsilon, NA)) {
    CLI_SetParamDouble("epsilon", epsilon)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamScalingModelPtr("input_model", input_model)
  }

  if (!identical(inverse_scaling, FALSE)) {
    CLI_SetParamBool("inverse_scaling", inverse_scaling)
  }

  if (!identical(max_value, NA)) {
    CLI_SetParamInt("max_value", max_value)
  }

  if (!identical(min_value, NA)) {
    CLI_SetParamInt("min_value", min_value)
  }

  if (!identical(scaler_method, NA)) {
    CLI_SetParamString("scaler_method", scaler_method)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")
  CLI_SetPassed("output_model")

  preprocess_scale_mlpackMain()

  output_model <- CLI_GetParamScalingModelPtr("output_model")
  attr(output_model, "type") <- "ScalingModel"

  out <- list(
      "output" = CLI_GetParamMat("output"),
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
