#' @title Kernel Density Estimation
#'
#' @description
#' An implementation of kernel density estimation with dual-tree algorithms.
#' Given a set of reference points and query points and a kernel function, this
#' can estimate the density function at the location of each query point using
#' trees; trees that are built can be saved for later use.
#'
#' @param abs_error Relative error tolerance for the prediction.  Default
#'   value "0" (numeric).
#' @param algorithm Algorithm to use for the prediction.('dual-tree',
#'   'single-tree').  Default value "dual-tree" (character).
#' @param bandwidth Bandwidth of the kernel.  Default value "1" (numeric).
#' @param initial_sample_size Initial sample size for Monte Carlo
#'   estimations.  Default value "100" (integer).
#' @param input_model Contains pre-trained KDE model (KDEModel).
#' @param kernel Kernel to use for the prediction.('gaussian',
#'   'epanechnikov', 'laplacian', 'spherical', 'triangular').  Default value
#'   "gaussian" (character).
#' @param mc_break_coef Controls what fraction of the amount of node's
#'   descendants is the limit for the sample size before it recurses.  Default
#'   value "0.4" (numeric).
#' @param mc_entry_coef Controls how much larger does the amount of node
#'   descendants has to be compared to the initial sample size in order to be a
#'   candidate for Monte Carlo estimations.  Default value "3" (numeric).
#' @param mc_probability Probability of the estimation being bounded by
#'   relative error when using Monte Carlo estimations.  Default value "0.95"
#'   (numeric).
#' @param monte_carlo Whether to use Monte Carlo estimations when possible.
#'    Default value "FALSE" (logical).
#' @param query Query dataset to KDE on (numeric matrix).
#' @param reference Input reference dataset use for KDE (numeric matrix).
#' @param rel_error Relative error tolerance for the prediction.  Default
#'   value "0.05" (numeric).
#' @param tree Tree to use for the prediction.('kd-tree', 'ball-tree',
#'   'cover-tree', 'octree', 'r-tree').  Default value "kd-tree" (character).
#' @param verbose Display informational messages and the full list of
#'   parameters and timers at the end of execution.  Default value "FALSE"
#'   (logical).
#'
#' @return A list with several components:
#' \item{output_model}{If specified, the KDE model will be saved here
#'   (KDEModel).}
#' \item{predictions}{Vector to store density predictions (numeric
#'   column).}
#'
#' @details
#' This program performs a Kernel Density Estimation. KDE is a non-parametric
#' way of estimating probability density function. For each query point the
#' program will estimate its probability density by applying a kernel function
#' to each reference point. The computational complexity of this is O(N^2) where
#' there are N query points and N reference points, but this implementation will
#' typically see better performance as it uses an approximate dual or single
#' tree algorithm for acceleration.
#' 
#' Dual or single tree optimization avoids many barely relevant calculations (as
#' kernel function values decrease with distance), so it is an approximate
#' computation. You can specify the maximum relative error tolerance for each
#' query value with "rel_error" as well as the maximum absolute error tolerance
#' with the parameter "abs_error". This program runs using an Euclidean metric.
#' Kernel function can be selected using the "kernel" option. You can also
#' choose what which type of tree to use for the dual-tree algorithm with
#' "tree". It is also possible to select whether to use dual-tree algorithm or
#' single-tree algorithm using the "algorithm" option.
#' 
#' Monte Carlo estimations can be used to accelerate the KDE estimate when the
#' Gaussian Kernel is used. This provides a probabilistic guarantee on the the
#' error of the resulting KDE instead of an absolute guarantee.To enable Monte
#' Carlo estimations, the "monte_carlo" flag can be used, and success
#' probability can be set with the "mc_probability" option. It is possible to
#' set the initial sample size for the Monte Carlo estimation using
#' "initial_sample_size". This implementation will only consider a node, as a
#' candidate for the Monte Carlo estimation, if its number of descendant nodes
#' is bigger than the initial sample size. This can be controlled using a
#' coefficient that will multiply the initial sample size and can be set using
#' "mc_entry_coef". To avoid using the same amount of computations an exact
#' approach would take, this program recurses the tree whenever a fraction of
#' the amount of the node's descendant points have already been computed. This
#' fraction is set using "mc_break_coef".
#'
#' @author
#' mlpack developers
#'
#' @export
#' @examples
#' # For example, the following will run KDE using the data in "ref_data" for
#' # training and the data in "qu_data" as query data. It will apply an
#' # Epanechnikov kernel with a 0.2 bandwidth to each reference point and use a
#' # KD-Tree for the dual-tree optimization. The returned predictions will be
#' # within 5% of the real KDE value for each query point.
#' 
#' \donttest{
#' output <- kde(reference=ref_data, query=qu_data, bandwidth=0.2,
#'   kernel="epanechnikov", tree="kd-tree", rel_error=0.05)
#' out_data <- output$predictions
#' }
#' 
#' # the predicted density estimations will be stored in "out_data".
#' # If no "query" is provided, then KDE will be computed on the "reference"
#' # dataset.
#' # It is possible to select either a reference dataset or an input model but
#' # not both at the same time. If an input model is selected and parameter
#' # values are not set (e.g. "bandwidth") then default parameter values will be
#' # used.
#' # 
#' # In addition to the last program call, it is also possible to activate Monte
#' # Carlo estimations if a Gaussian kernel is used. This can provide faster
#' # results, but the KDE will only have a probabilistic guarantee of meeting
#' # the desired error bound (instead of an absolute guarantee). The following
#' # example will run KDE using a Monte Carlo estimation when possible. The
#' # results will be within a 5% of the real KDE value with a 95% probability.
#' # Initial sample size for the Monte Carlo estimation will be 200 points and a
#' # node will be a candidate for the estimation only when it contains 700 (i.e.
#' # 3.5*200) points. If a node contains 700 points and 420 (i.e. 0.6*700) have
#' # already been sampled, then the algorithm will recurse instead of keep
#' # sampling.
#' 
#' \donttest{
#' output <- kde(reference=ref_data, query=qu_data, bandwidth=0.2,
#'   kernel="gaussian", tree="kd-tree", rel_error=0.05, monte_carlo=,
#'   mc_probability=0.95, initial_sample_size=200, mc_entry_coef=3.5,
#'   mc_break_coef=0.6)
#' out_data <- output$predictions
#' }
kde <- function(abs_error=NA,
                algorithm=NA,
                bandwidth=NA,
                initial_sample_size=NA,
                input_model=NA,
                kernel=NA,
                mc_break_coef=NA,
                mc_entry_coef=NA,
                mc_probability=NA,
                monte_carlo=FALSE,
                query=NA,
                reference=NA,
                rel_error=NA,
                tree=NA,
                verbose=FALSE) {
  # Restore IO settings.
  IO_RestoreSettings("Kernel Density Estimation")

  # Process each input argument before calling mlpackMain().
  if (!identical(abs_error, NA)) {
    IO_SetParamDouble("abs_error", abs_error)
  }

  if (!identical(algorithm, NA)) {
    IO_SetParamString("algorithm", algorithm)
  }

  if (!identical(bandwidth, NA)) {
    IO_SetParamDouble("bandwidth", bandwidth)
  }

  if (!identical(initial_sample_size, NA)) {
    IO_SetParamInt("initial_sample_size", initial_sample_size)
  }

  if (!identical(input_model, NA)) {
    IO_SetParamKDEModelPtr("input_model", input_model)
  }

  if (!identical(kernel, NA)) {
    IO_SetParamString("kernel", kernel)
  }

  if (!identical(mc_break_coef, NA)) {
    IO_SetParamDouble("mc_break_coef", mc_break_coef)
  }

  if (!identical(mc_entry_coef, NA)) {
    IO_SetParamDouble("mc_entry_coef", mc_entry_coef)
  }

  if (!identical(mc_probability, NA)) {
    IO_SetParamDouble("mc_probability", mc_probability)
  }

  if (!identical(monte_carlo, FALSE)) {
    IO_SetParamBool("monte_carlo", monte_carlo)
  }

  if (!identical(query, NA)) {
    IO_SetParamMat("query", to_matrix(query))
  }

  if (!identical(reference, NA)) {
    IO_SetParamMat("reference", to_matrix(reference))
  }

  if (!identical(rel_error, NA)) {
    IO_SetParamDouble("rel_error", rel_error)
  }

  if (!identical(tree, NA)) {
    IO_SetParamString("tree", tree)
  }

  if (verbose) {
    IO_EnableVerbose()
  } else {
    IO_DisableVerbose()
  }

  # Mark all output options as passed.
  IO_SetPassed("output_model")
  IO_SetPassed("predictions")

  # Call the program.
  kde_mlpackMain()

  # Add ModelType as attribute to the model pointer, if needed.
  output_model <- IO_GetParamKDEModelPtr("output_model")
  attr(output_model, "type") <- "KDEModel"

  # Extract the results in order.
  out <- list(
      "output_model" = output_model,
      "predictions" = IO_GetParamCol("predictions")
  )

  # Clear the parameters.
  IO_ClearSettings()

  return(out)
}
