#' @export
gmm_train <- function(gaussians,
                      input,
                      diagonal_covariance=FALSE,
                      input_model=NA,
                      kmeans_max_iterations=NA,
                      max_iterations=NA,
                      no_force_positive=FALSE,
                      noise=NA,
                      percentage=NA,
                      refined_start=FALSE,
                      samplings=NA,
                      seed=NA,
                      tolerance=NA,
                      trials=NA,
                      verbose=FALSE) {

  CLI_RestoreSettings("Gaussian Mixture Model (GMM) Training")

  CLI_SetParamInt("gaussians", gaussians)

  CLI_SetParamMat("input", to_matrix(input))

  if (!identical(diagonal_covariance, FALSE)) {
    CLI_SetParamBool("diagonal_covariance", diagonal_covariance)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamGMMPtr("input_model", input_model)
  }

  if (!identical(kmeans_max_iterations, NA)) {
    CLI_SetParamInt("kmeans_max_iterations", kmeans_max_iterations)
  }

  if (!identical(max_iterations, NA)) {
    CLI_SetParamInt("max_iterations", max_iterations)
  }

  if (!identical(no_force_positive, FALSE)) {
    CLI_SetParamBool("no_force_positive", no_force_positive)
  }

  if (!identical(noise, NA)) {
    CLI_SetParamDouble("noise", noise)
  }

  if (!identical(percentage, NA)) {
    CLI_SetParamDouble("percentage", percentage)
  }

  if (!identical(refined_start, FALSE)) {
    CLI_SetParamBool("refined_start", refined_start)
  }

  if (!identical(samplings, NA)) {
    CLI_SetParamInt("samplings", samplings)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(tolerance, NA)) {
    CLI_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(trials, NA)) {
    CLI_SetParamInt("trials", trials)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")

  gmm_train_mlpackMain()

  output_model <- CLI_GetParamGMMPtr("output_model")
  attr(output_model, "type") <- "GMM"

  out <- list(
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
