#' @export
dbscan <- function(input,
                   epsilon=NA,
                   min_size=NA,
                   naive=FALSE,
                   selection_type=NA,
                   single_mode=FALSE,
                   tree_type=NA,
                   verbose=FALSE) {

  CLI_RestoreSettings("DBSCAN clustering")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(epsilon, NA)) {
    CLI_SetParamDouble("epsilon", epsilon)
  }

  if (!identical(min_size, NA)) {
    CLI_SetParamInt("min_size", min_size)
  }

  if (!identical(naive, FALSE)) {
    CLI_SetParamBool("naive", naive)
  }

  if (!identical(selection_type, NA)) {
    CLI_SetParamString("selection_type", selection_type)
  }

  if (!identical(single_mode, FALSE)) {
    CLI_SetParamBool("single_mode", single_mode)
  }

  if (!identical(tree_type, NA)) {
    CLI_SetParamString("tree_type", tree_type)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("assignments")
  CLI_SetPassed("centroids")

  dbscan_mlpackMain()

  out <- list(
      "assignments" = CLI_GetParamURow("assignments"),
      "centroids" = CLI_GetParamMat("centroids")
  )

  CLI_ClearSettings()

  return(out)
}
