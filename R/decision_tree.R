#' @export
decision_tree <- function(input_model=NA,
                          labels=NA,
                          maximum_depth=NA,
                          minimum_gain_split=NA,
                          minimum_leaf_size=NA,
                          print_training_accuracy=FALSE,
                          print_training_error=FALSE,
                          test=NA,
                          test_labels=NA,
                          training=NA,
                          verbose=FALSE,
                          weights=NA) {

  CLI_RestoreSettings("Decision tree")

  if (!identical(input_model, NA)) {
    CLI_SetParamDecisionTreeModelPtr("input_model", input_model)
  }

  if (!identical(labels, NA)) {
    CLI_SetParamURow("labels", to_matrix(labels))
  }

  if (!identical(maximum_depth, NA)) {
    CLI_SetParamInt("maximum_depth", maximum_depth)
  }

  if (!identical(minimum_gain_split, NA)) {
    CLI_SetParamDouble("minimum_gain_split", minimum_gain_split)
  }

  if (!identical(minimum_leaf_size, NA)) {
    CLI_SetParamInt("minimum_leaf_size", minimum_leaf_size)
  }

  if (!identical(print_training_accuracy, FALSE)) {
    CLI_SetParamBool("print_training_accuracy", print_training_accuracy)
  }

  if (!identical(print_training_error, FALSE)) {
    CLI_SetParamBool("print_training_error", print_training_error)
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

  if (!identical(weights, NA)) {
    CLI_SetParamMat("weights", to_matrix(weights))
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  decision_tree_mlpackMain()

  output_model <- CLI_GetParamDecisionTreeModelPtr("output_model")
  attr(output_model, "type") <- "DecisionTreeModel"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
