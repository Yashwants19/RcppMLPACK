#' @export
preprocess_describe <- function(input,
                                dimension=NA,
                                population=FALSE,
                                precision=NA,
                                row_major=FALSE,
                                verbose=FALSE,
                                width=NA) {

  CLI_RestoreSettings("Descriptive Statistics")

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(dimension, NA)) {
    CLI_SetParamInt("dimension", dimension)
  }

  if (!identical(population, FALSE)) {
    CLI_SetParamBool("population", population)
  }

  if (!identical(precision, NA)) {
    CLI_SetParamInt("precision", precision)
  }

  if (!identical(row_major, FALSE)) {
    CLI_SetParamBool("row_major", row_major)
  }

  if (!identical(width, NA)) {
    CLI_SetParamInt("width", width)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }


  preprocess_describe_mlpackMain()

  out <- list(

  )

  CLI_ClearSettings()

  return(out)
}
