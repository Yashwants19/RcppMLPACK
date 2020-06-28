#' @export
random_forest <- function(input_model=NA,
                          labels=NA,
                          maximum_depth=NA,
                          minimum_gain_split=NA,
                          minimum_leaf_size=NA,
                          num_trees=NA,
                          print_training_accuracy=FALSE,
                          seed=NA,
                          subspace_dim=NA,
                          test=NA,
                          test_labels=NA,
                          training=NA,
                          verbose=FALSE) {

  CLI_RestoreSettings("Random forests")

  if (!identical(input_model, NA)) {
    CLI_SetParamRandomForestModelPtr("input_model", input_model)
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

  if (!identical(num_trees, NA)) {
    CLI_SetParamInt("num_trees", num_trees)
  }

  if (!identical(print_training_accuracy, FALSE)) {
    CLI_SetParamBool("print_training_accuracy", print_training_accuracy)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(subspace_dim, NA)) {
    CLI_SetParamInt("subspace_dim", subspace_dim)
  }

  if (!identical(test, NA)) {
    CLI_SetParamMat("test", to_matrix(test))
  }

  if (!identical(test_labels, NA)) {
    CLI_SetParamURow("test_labels", to_matrix(test_labels))
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
  CLI_SetPassed("predictions")
  CLI_SetPassed("probabilities")

  random_forest_mlpackMain()

  output_model <- CLI_GetParamRandomForestModelPtr("output_model")
  attr(output_model, "type") <- "RandomForestModel"

  out <- list(
      "output_model" = output_model,
      "predictions" = CLI_GetParamURow("predictions"),
      "probabilities" = CLI_GetParamMat("probabilities")
  )

  CLI_ClearSettings()

  return(out)
}
