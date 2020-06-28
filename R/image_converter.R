#' @export
image_converter <- function(input,
                            channels=NA,
                            dataset=NA,
                            height=NA,
                            quality=NA,
                            save=FALSE,
                            verbose=FALSE,
                            width=NA) {

  CLI_RestoreSettings("Image Converter")

  CLI_SetParamVecString("input", input)

  if (!identical(channels, NA)) {
    CLI_SetParamInt("channels", channels)
  }

  if (!identical(dataset, NA)) {
    CLI_SetParamMat("dataset", to_matrix(dataset))
  }

  if (!identical(height, NA)) {
    CLI_SetParamInt("height", height)
  }

  if (!identical(quality, NA)) {
    CLI_SetParamInt("quality", quality)
  }

  if (!identical(save, FALSE)) {
    CLI_SetParamBool("save", save)
  }

  if (!identical(width, NA)) {
    CLI_SetParamInt("width", width)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output")

  image_converter_mlpackMain()

  out <- list(
      "output" = CLI_GetParamMat("output")
  )

  CLI_ClearSettings()

  return(out)
}
