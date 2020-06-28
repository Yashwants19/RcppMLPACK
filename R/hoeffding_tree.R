#' @export
hoeffding_tree <- function(batch_mode=FALSE,
                           bins=NA,
                           confidence=NA,
                           info_gain=FALSE,
                           input_model=NA,
                           labels=NA,
                           max_samples=NA,
                           min_samples=NA,
                           numeric_split_strategy=NA,
                           observations_before_binning=NA,
                           passes=NA,
                           test=NA,
                           test_labels=NA,
                           training=NA,
                           verbose=FALSE) {

  CLI_RestoreSettings("Hoeffding trees")

  if (!identical(batch_mode, FALSE)) {
    CLI_SetParamBool("batch_mode", batch_mode)
  }

  if (!identical(bins, NA)) {
    CLI_SetParamInt("bins", bins)
  }

  if (!identical(confidence, NA)) {
    CLI_SetParamDouble("confidence", confidence)
  }

  if (!identical(info_gain, FALSE)) {
    CLI_SetParamBool("info_gain", info_gain)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamHoeffdingTreeModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(max_samples, NA)) {
    CLI_SetParamInt("max_samples", max_samples)
  }

  if (!identical(min_samples, NA)) {
    CLI_SetParamInt("min_samples", min_samples)
  }

  if (!identical(numeric_split_strategy, NA)) {
    CLI_SetParamString("numeric_split_strategy", numeric_split_strategy)
  }

  if (!identical(observations_before_binning, NA)) {
    CLI_SetParamInt("observations_before_binning", observations_before_binning)
  }

  if (!identical(passes, NA)) {
    CLI_SetParamInt("passes", passes)
  }

  if (!identical(test, NA)) {
    test <- to_matrix_with_info(test)
    CLI_SetParamMatWithInfo("test", test$info, test$data)
  }

  if (!identical(test_labels, NA)) {
    CLI_SetParamURow("test_labels", to_matrix(test_labels))
  }

  if (!identical(training, NA)) {
    training <- to_matrix_with_info(training)
    CLI_SetParamMatWithInfo("training", training$info, training$data)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  hoeffding_tree_mlpackMain()

  output_model <- CLI_GetParamHoeffdingTreeModelPtr("output_model")
  attr(output_model, "type") <- "HoeffdingTreeModel"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
