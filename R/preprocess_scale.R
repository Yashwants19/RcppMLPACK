#' @title Scale Data
#'
#' @description
#' A utility to perform feature scaling on datasets using one of sixtechniques. 
#' Both scaling and inverse scaling are supported, andscalers can be saved and
#' then applied to other datasets.
#'
#' @param input Matrix containing data.
#' @param epsilon regularization Parameter for pcawhitening, or zcawhitening, should
#'   be between -1 to 1.  Default value "1e-06".
#' @param input_model Input Scaling model.
#' @param inverse_scaling Inverse Scaling to get original dataset  Default value
#'   "FALSE".
#' @param max_value Ending value of range for min_max_scaler.  Default value "1".
#' @param min_value Starting value of range for min_max_scaler.  Default value "0".
#' @param scaler_method method to use for scaling, the default is standard_scaler. 
#'   Default value "standard_scaler".
#' @param seed Random seed (0 for std::time(NULL)).  Default value "0".
#' @param verbose Display informational messages and the full list of parameters and
#'   timers at the end of execution.  Default value "FALSE".
#'
#' @return A list with several components:
#' \item{output}{Matrix to save scaled data to.}
#' \item{output_model}{Output scaling model.}
#'
#' @details
#' This utility takes a dataset and performs feature scaling using one of the
#' six scaler methods namely: 'max_abs_scaler', 'mean_normalization',
#' 'min_max_scaler' ,'standard_scaler', 'pca_whitening' and 'zca_whitening'. The
#' function takes a matrix as "input" and a scaling method type which you can
#' specify using "scaler_method" parameter; the default is standard scaler, and
#' outputs a matrix with scaled feature.
#' 
#' The output scaled feature matrix may be saved with the "output" output
#' parameters.
#' 
#' The model to scale features can be saved using "output_model" and later can
#' be loaded back using"input_model".
#' 
#' 
#' @author
#' MLPACK Developers
#'
#' @export
#' @examples
#' # So, a simple example where we want to scale the dataset "X" into "X_scaled"
#' # with  standard_scaler as scaler_method, we could run 
#' 
#' \donttest{
#' output <- preprocess_scale(input=X, scaler_method="standard_scaler")
#' X_scaled <- output$output
#' }
#' 
#' # A simple example where we want to whiten the dataset "X" into "X_whitened"
#' # with  PCA as whitening_method and use 0.01 as regularization parameter, we
#' # could run 
#' 
#' \donttest{
#' output <- preprocess_scale(input=X, scaler_method="pca_whitening",
#'   epsilon=0.01)
#' X_scaled <- output$output
#' }
#' 
#' # You can also retransform the scaled dataset back using"inverse_scaling". An
#' # example to rescale : "X_scaled" into "X"using the saved model "input_model"
#' # is:
#' 
#' \donttest{
#' output <- preprocess_scale(input=X_scaled, inverse_scaling=TRUE,
#'   input_model=saved)
#' X <- output$output
#' }
#' 
#' # Another simple example where we want to scale the dataset "X" into
#' # "X_scaled" with  min_max_scaler as scaler method, where scaling range is 1
#' # to 3 instead of default 0 to 1. We could run 
#' 
#' \donttest{
#' output <- preprocess_scale(input=X, scaler_method="min_max_scaler",
#'   min_value=1, max_value=3)
#' X_scaled <- output$output
#' }
preprocess_scale <- function(input,
                             epsilon=NA,
                             input_model=NA,
                             inverse_scaling=FALSE,
                             max_value=NA,
                             min_value=NA,
                             scaler_method=NA,
                             seed=NA,
                             verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Scale Data")

  # Process each input argument before calling mlpackMain().
  IO_SetParamMat("input", to_matrix(input))

  if (!identical(epsilon, NA)) {
    IO_SetParamDouble("epsilon", epsilon)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamScalingModelPtr("input_model", input_model)
  }

  if (!identical(inverse_scaling, FALSE)) {
    IO_SetParamBool("inverse_scaling", inverse_scaling)
  }

  if (!identical(max_value, NA)) {
    IO_SetParamInt("max_value", max_value)
  }

  if (!identical(min_value, NA)) {
    IO_SetParamInt("min_value", min_value)
  }

  if (!identical(scaler_method, NA)) {
    IO_SetParamString("scaler_method", scaler_method)
  }

  if (!identical(seed, NA)) {
    IO_SetParamInt("seed", seed)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output")
  IO_SetPassed("output_model")

  # Call the program.
  preprocess_scale_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamScalingModelPtr("output_model")
  attr(output_model, "type") <- "ScalingModel"

  # Extract the results in order.
  out <- list(
      "output" = IO_GetParamMat("output"),
      "output_model" = output_model
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
