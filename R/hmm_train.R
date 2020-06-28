#' @export
hmm_train <- function(input_file,
                      batch=FALSE,
                      gaussians=NA,
                      input_model=NA,
                      labels_file=NA,
                      seed=NA,
                      states=NA,
                      tolerance=NA,
                      type=NA,
                      verbose=FALSE) {

  CLI_RestoreSettings("Hidden Markov Model (HMM) Training")

  CLI_SetParamString("input_file", input_file)

  if (!identical(batch, FALSE)) {
    CLI_SetParamBool("batch", batch)
  }

  if (!identical(gaussians, NA)) {
    CLI_SetParamInt("gaussians", gaussians)
  }

  if (!identical(input_model, NA)) {
    CLI_SetParamHMMModelPtr("input_model", input_model)
  }

  if (!identical(labels_file, NA)) {
    CLI_SetParamString("labels_file", labels_file)
  }

  if (!identical(seed, NA)) {
    CLI_SetParamInt("seed", seed)
  }

  if (!identical(states, NA)) {
    CLI_SetParamInt("states", states)
  }

  if (!identical(tolerance, NA)) {
    CLI_SetParamDouble("tolerance", tolerance)
  }

  if (!identical(type, NA)) {
    CLI_SetParamString("type", type)
  }

  if (verbose) {
    CLI_EnableVerbose()
  } else {
    CLI_DisableVerbose()
  }

  CLI_SetPassed("output_model")

  hmm_train_mlpackMain()

  output_model <- CLI_GetParamHMMModelPtr("output_model")
  attr(output_model, "type") <- "HMMModel"

  out <- list(
      "output_model" = output_model
  )

  CLI_ClearSettings()

  return(out)
}
