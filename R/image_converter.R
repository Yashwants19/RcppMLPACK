#' @title Image Converter
#'
#' @description
#' A utility to load an image or set of images into a single dataset that can
#' then be used by other mlpack methods and utilities. This can also unpack an
#' image dataset into individual files, for instance after mlpack methods have
#' been used.
#'
#' @param input Image filenames which have to be loaded/saved.
#' @param channels Number of channels in the image.  Default value "0".
#' @param dataset Input matrix to save as images.
#' @param height Height of the images.  Default value "0".
#' @param quality Compression of the image if saved as jpg (0-100).  Default value
#'   "90".
#' @param save Save a dataset as images.  Default value "FALSE".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#' @param width Width of the image.  Default value "0".
#'
#' @return A list with several components:
#' \item{output}{Matrix to save images data to, Onlyneeded if you are specifying
#'   'save' option.}
#'
#' @details
#' This utility takes an image or an array of images and loads them to a matrix.
#' You can optionally specify the height "height" width "width" and channel
#' "channels" of the images that needs to be loaded; otherwise, these parameters
#' will be automatically detected from the image.
#' There are other options too, that can be specified such as "quality".
#' 
#' You can also provide a dataset and save them as images using "dataset" and
#' "save" as an parameter.
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # An example to load an image : 
#' 
#' \donttest{
#' output <- image_converter(input=X, height=256, width=256, channels=3)
#' Y <- output$output
#' }
#' 
#' # An example to save an image is :
#' 
#' \donttest{
#' output <- image_converter(input=X, height=256, width=256, channels=3,
#'   dataset=Y, save=TRUE)
#' }
image_converter <- function(input,
                            channels=NA,
                            dataset=NA,
                            height=NA,
                            quality=NA,
                            save=FALSE,
                            verbose=FALSE,
                            width=NA) {
  # Restore IO settings.
  IO_RestoreSettings("Image Converter")

  # Process each input argument before calling mlpackMain().
  IO_SetParamVecString("input", input)

  if (!identical(channels, NA)) {
    IO_SetParamInt("channels", channels)
  }

  if (!identical(dataset, NA)) {
    IO_SetParamMat("dataset", to_matrix(dataset))
  }

  if (!identical(height, NA)) {
    IO_SetParamInt("height", height)
  }

  if (!identical(quality, NA)) {
    IO_SetParamInt("quality", quality)
  }

  if (!identical(save, FALSE)) {
    IO_SetParamBool("save", save)
  }

  if (!identical(width, NA)) {
    IO_SetParamInt("width", width)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")

  # Call the program.
  image_converter_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
