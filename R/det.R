#' @export
det <- function(folds=NA,
                input_model=NA,
                max_leaf_size=NA,
                min_leaf_size=NA,
                path_format=NA,
                skip_pruning=FALSE,
                test=NA,
                training=NA,
                verbose=FALSE) {

  CLI_RestoreSettings("Density Estimation With Density Estimation Trees")

  if (!identical(folds, NA)) {
    CLI_SetParamInt("folds", folds)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamDTreePtr("input_model", input_model)
  }

  if (!identical(max_leaf_size, NA)) {
    CLI_SetParamInt("max_leaf_size", max_leaf_size)
  }

  if (!identical(min_leaf_size, NA)) {
    CLI_SetParamInt("min_leaf_size", min_leaf_size)
  }

  if (!identical(path_format, NA)) {
    CLI_SetParamString("path_format", path_format)
  }

  if (!identical(skip_pruning, FALSE)) {
    CLI_SetParamBool("skip_pruning", skip_pruning)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(training, NA)) {
    CLI_SetParamMat("training", to_matrix(training))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("tag_counters_file")
  CLI_SetPassed("tag_file")
  CLI_SetPassed("test_set_estimates")
  CLI_SetPassed("training_set_estimates")
  CLI_SetPassed("vi")

  det_mlpackMain()

  output_model <- CLI_GetParamDTreePtr("output_model")
  attr(output_model, "type") <- "DTree"

  out <- list(
      "output_model" = output_model,
      "tag_counters_file" = CLI_GetParamString("tag_counters_file"),
      "tag_file" = CLI_GetParamString("tag_file"),
      "test_set_estimates" = CLI_GetParamMat("test_set_estimates"),
      "training_set_estimates" = CLI_GetParamMat("training_set_estimates"),
      "vi" = CLI_GetParamMat("vi")
  )

  CLI_ClearSettings()

  return(out)
}
