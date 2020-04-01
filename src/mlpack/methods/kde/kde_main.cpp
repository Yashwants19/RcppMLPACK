/**
 * @file kde_main.cpp
 * @author Roberto Hueso
 *
 * Executable for running Kernel Density Estimation.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */

#include <mlpack/core/util/mlpack_main.hpp>

#include "kde.hpp"
#include "kde_model.hpp"

using namespace mlpack;
using namespace mlpack::kde;
using namespace mlpack::util;
using namespace std;

// Define parameters for the executable.
PROGRAM_INFO("Kernel Density Estimation",
    // Short description.
    "An implementation of kernel density estimation with dual-tree algorithms. "
    "Given a set of reference points and query points and a kernel function, "
    "this can estimate the density function at the location of each query point"
    " using trees; trees that are built can be saved for later use.",
    // Long description.
    "This program performs a Kernel Density Estimation. KDE is a "
    "non-parametric way of estimating probability density function. "
    "For each query point the program will estimate its probability density "
    "by applying a kernel function to each reference point. The computational "
    "complexity of this is O(N^2) where there are N query points and N "
    "reference points, but this implementation will typically see better "
    "performance as it uses an approximate dual or single tree algorithm for "
    "acceleration."
    "\n\n"
    "Dual or single tree optimization avoids many barely relevant "
    "calculations (as kernel function values decrease with distance), so it is "
    "an approximate computation. You can specify the maximum relative error "
    "tolerance for each query value with " + PRINT_PARAM_STRING("rel_error") +
    " as well as the maximum absolute error tolerance with the parameter " +
    PRINT_PARAM_STRING("abs_error") + ". This program runs using an Euclidean "
    "metric. Kernel function can be selected using the " +
    PRINT_PARAM_STRING("kernel") + " option. You can also choose what which "
    "type of tree to use for the dual-tree algorithm with " +
    PRINT_PARAM_STRING("tree") + ". It is also possible to select whether to "
    "use dual-tree algorithm or single-tree algorithm using the " +
    PRINT_PARAM_STRING("algorithm") + " option."
    "\n\n"
    "Monte Carlo estimations can be used to accelerate the KDE estimate when "
    "the Gaussian Kernel is used. This provides a probabilistic guarantee on "
    "the the error of the resulting KDE instead of an absolute guarantee."
    "To enable Monte Carlo estimations, the " +
    PRINT_PARAM_STRING("monte_carlo") + " flag can be used, and success "
    "probability can be set with the " + PRINT_PARAM_STRING("mc_probability") +
    " option. It is possible to set the initial sample size for the Monte "
    "Carlo estimation using " + PRINT_PARAM_STRING("initial_sample_size") +
    ". This implementation will only consider a node, as a candidate for the "
    "Monte Carlo estimation, if its number of descendant nodes is bigger than "
    "the initial sample size. This can be controlled using a coefficient that "
    "will multiply the initial sample size and can be set using " +
    PRINT_PARAM_STRING("mc_entry_coef") + ". To avoid using the same amount of "
    "computations an exact approach would take, this program recurses the tree "
    "whenever a fraction of the amount of the node's descendant points have "
    "already been computed. This fraction is set using " +
    PRINT_PARAM_STRING("mc_break_coef") + "."
    "\n\n"
    "For example, the following will run KDE using the data in " +
    PRINT_DATASET("ref_data") + " for training and the data in " +
    PRINT_DATASET("qu_data") + " as query data. It will apply an Epanechnikov "
    "kernel with a 0.2 bandwidth to each reference point and use a KD-Tree for "
    "the dual-tree optimization. The returned predictions will be within 5% of "
    "the real KDE value for each query point."
    "\n\n" +
    PRINT_CALL("kde", "reference", "ref_data", "query", "qu_data", "bandwidth",
        0.2, "kernel", "epanechnikov", "tree", "kd-tree", "rel_error",
        0.05, "predictions", "out_data") +
    "\n\n"
    "the predicted density estimations will be stored in " +
    PRINT_DATASET("out_data") + "."
    "\n"
    "If no " + PRINT_PARAM_STRING("query") + " is provided, then KDE will be "
    "computed on the " + PRINT_PARAM_STRING("reference") + " dataset."
    "\n"
    "It is possible to select either a reference dataset or an input model "
    "but not both at the same time. If an input model is selected and "
    "parameter values are not set (e.g. " + PRINT_PARAM_STRING("bandwidth") +
    ") then default parameter values will be used."
    "\n\n"
    "In addition to the last program call, it is also possible to activate "
    "Monte Carlo estimations if a Gaussian kernel is used. This can provide "
    "faster results, but the KDE will only have a probabilistic guarantee of "
    "meeting the desired error bound (instead of an absolute guarantee). The "
    "following example will run KDE using a Monte Carlo estimation when "
    "possible. The results will be within a 5% of the real KDE value with a "
    "95% probability. Initial sample size for the Monte Carlo estimation will "
    "be 200 points and a node will be a candidate for the estimation only when "
    "it contains 700 (i.e. 3.5*200) points. If a node contains 700 points and "
    "420 (i.e. 0.6*700) have already been sampled, then the algorithm will "
    "recurse instead of keep sampling."
    "\n\n" +
    PRINT_CALL("kde", "reference", "ref_data", "query", "qu_data", "bandwidth",
        0.2, "kernel", "gaussian", "tree", "kd-tree", "rel_error",
        0.05, "predictions", "out_data", "monte_carlo", "", "mc_probability",
        0.95, "initial_sample_size", 200, "mc_entry_coef", 3.5, "mc_break_coef",
        0.6) +
    "\n\n",
    SEE_ALSO("@knn", "#knn"),
    SEE_ALSO("Kernel density estimation on Wikipedia",
        "https://en.wikipedia.org/wiki/Kernel_density_estimation"),
    SEE_ALSO("Tree-Independent Dual-Tree Algorithms",
             "https://arxiv.org/pdf/1304.4327.pdf"),
    SEE_ALSO("Fast High-dimensional Kernel Summations Using the Monte Carlo "
        "Multipole Method", "http://papers.nips.cc/paper/3539-fast-high-"
        "dimensional-kernel-summations-using-the-monte-carlo-multipole-method."
        "pdf"),
    SEE_ALSO("mlpack::kde::KDE C++ class documentation",
        "@doxygen/classmlpack_1_1kde_1_1KDE.html"));

// Required options.
PARAM_MATRIX_IN("reference", "Input reference dataset use for KDE.", "r");
PARAM_MATRIX_IN("query", "Query dataset to KDE on.", "q");
PARAM_DOUBLE_IN("bandwidth", "Bandwidth of the kernel.", "b", 1.0);

// Load or save models.
PARAM_MODEL_IN(KDEModel,
               "input_model",
               "Contains pre-trained KDE model.",
               "m");
PARAM_MODEL_OUT(KDEModel,
                "output_model",
                "If specified, the KDE model will be saved here.",
                "M");

// Configuration options.
PARAM_STRING_IN("kernel", "Kernel to use for the prediction."
    "('gaussian', 'epanechnikov', 'laplacian', 'spherical', 'triangular').",
    "k", "gaussian");
PARAM_STRING_IN("tree", "Tree to use for the prediction."
    "('kd-tree', 'ball-tree', 'cover-tree', 'octree', 'r-tree').",
    "t", "kd-tree");
PARAM_STRING_IN("algorithm", "Algorithm to use for the prediction."
    "('dual-tree', 'single-tree').",
    "a", "dual-tree");
PARAM_DOUBLE_IN("rel_error",
                "Relative error tolerance for the prediction.",
                "e",
                KDEDefaultParams::relError);
PARAM_DOUBLE_IN("abs_error",
                "Relative error tolerance for the prediction.",
                "E",
                KDEDefaultParams::absError);
PARAM_FLAG("monte_carlo",
           "Whether to use Monte Carlo estimations when possible.",
           "S");
PARAM_DOUBLE_IN("mc_probability",
                "Probability of the estimation being bounded by relative error "
                "when using Monte Carlo estimations.",
                "P",
                KDEDefaultParams::mcProb);
PARAM_INT_IN("initial_sample_size",
             "Initial sample size for Monte Carlo estimations.",
             "s",
             KDEDefaultParams::initialSampleSize);
PARAM_DOUBLE_IN("mc_entry_coef",
                "Controls how much larger does the amount of node descendants "
                "has to be compared to the initial sample size in order to be "
                "a candidate for Monte Carlo estimations.",
                "C",
                KDEDefaultParams::mcEntryCoef);
PARAM_DOUBLE_IN("mc_break_coef",
                "Controls what fraction of the amount of node's descendants is "
                "the limit for the sample size before it recurses.",
                "c",
                KDEDefaultParams::mcBreakCoef);

// Output predictions options.
PARAM_COL_OUT("predictions", "Vector to store density predictions.",
    "p");

// Maybe, in the future, it could be interesting to implement different metrics.

static void mlpackMain()
{
  // Get some parameters.
  const double bandwidth = CLI::GetParam<double>("bandwidth");
  const std::string kernelStr = CLI::GetParam<std::string>("kernel");
  const std::string treeStr = CLI::GetParam<std::string>("tree");
  const std::string modeStr = CLI::GetParam<std::string>("algorithm");
  const double relError = CLI::GetParam<double>("rel_error");
  const double absError = CLI::GetParam<double>("abs_error");
  const bool monteCarlo = CLI::GetParam<bool>("monte_carlo");
  const double mcProb = CLI::GetParam<double>("mc_probability");
  const int initialSampleSize = CLI::GetParam<int>("initial_sample_size");
  const double mcEntryCoef = CLI::GetParam<double>("mc_entry_coef");
  const double mcBreakCoef = CLI::GetParam<double>("mc_break_coef");

  // Initialize results vector.
  arma::vec estimations;

  // You can only specify reference data or a pre-trained model.
  RequireOnlyOnePassed({ "reference", "input_model" }, true);
  ReportIgnoredParam({{ "input_model", true }}, "tree");
  ReportIgnoredParam({{ "input_model", true }}, "kernel");

  // Monte Carlo parameters only make sense if it is activated.
  ReportIgnoredParam({{ "monte_carlo", false }}, "mc_probability");
  ReportIgnoredParam({{ "monte_carlo", false }}, "initial_sample_size");
  ReportIgnoredParam({{ "monte_carlo", false }}, "mc_entry_coef");
  ReportIgnoredParam({{ "monte_carlo", false }}, "mc_break_coef");
  if (monteCarlo && kernelStr != "gaussian")
  {
    ReportIgnoredParam("monte_carlo",
                       "Monte Carlo only works with Gaussian kernel");
  }

  // Requirements for parameter values.
  RequireParamInSet<string>("kernel", { "gaussian", "epanechnikov",
      "laplacian", "spherical", "triangular" }, true, "unknown kernel type");
  RequireParamInSet<string>("tree", { "kd-tree", "ball-tree", "cover-tree",
      "octree", "r-tree"}, true, "unknown tree type");
  RequireParamInSet<string>("algorithm", { "dual-tree", "single-tree"},
      true, "unknown algorithm");
  RequireParamValue<double>("rel_error", [](double x){return x >= 0 && x <= 1;},
      true, "relative error must be between 0 and 1");
  RequireParamValue<double>("abs_error", [](double x){return x >= 0;},
      true, "absolute error must be equal to or greater than 0");
  RequireParamValue<double>("mc_probability",
      [](double x){return x >= 0 && x < 1;}, true,
      "Monte Carlo probability must be greater than or equal to 0 or less "
      "than 1");
  RequireParamValue<int>("initial_sample_size", [](int x){return x > 0;},
      true, "initial sample size must be greater than 0");
  RequireParamValue<double>("mc_entry_coef", [](double x){return x >= 1;},
      true, "Monte Carlo entry coefficient must be greater than or equal to 1");
  RequireParamValue<double>("mc_break_coef",
      [](double x){return x > 0 && x <= 1;}, true,
      "Monte Carlo break coefficient must be greater than 0 and less than "
      "or equal to 1");

  KDEModel* kde;

  if (CLI::HasParam("reference"))
  {
    arma::mat reference = std::move(CLI::GetParam<arma::mat>("reference"));

    kde = new KDEModel();

    // Set KernelType.
    if (kernelStr == "gaussian")
      kde->KernelType() = KDEModel::GAUSSIAN_KERNEL;
    else if (kernelStr == "epanechnikov")
      kde->KernelType() = KDEModel::EPANECHNIKOV_KERNEL;
    else if (kernelStr == "laplacian")
      kde->KernelType() = KDEModel::LAPLACIAN_KERNEL;
    else if (kernelStr == "spherical")
      kde->KernelType() = KDEModel::SPHERICAL_KERNEL;
    else if (kernelStr == "triangular")
      kde->KernelType() = KDEModel::TRIANGULAR_KERNEL;

    // Set TreeType.
    if (treeStr == "kd-tree")
      kde->TreeType() = KDEModel::KD_TREE;
    else if (treeStr == "ball-tree")
      kde->TreeType() = KDEModel::BALL_TREE;
    else if (treeStr == "cover-tree")
      kde->TreeType() = KDEModel::COVER_TREE;
    else if (treeStr == "octree")
      kde->TreeType() = KDEModel::OCTREE;
    else if (treeStr == "r-tree")
      kde->TreeType() = KDEModel::R_TREE;

    // Build model.
    kde->BuildModel(std::move(reference));

    // Set Mode.
    if (modeStr == "dual-tree")
      kde->Mode() = KDEMode::DUAL_TREE_MODE;
    else if (modeStr == "single-tree")
      kde->Mode() = KDEMode::SINGLE_TREE_MODE;
  }
  else
  {
    // Load model.
    kde = CLI::GetParam<KDEModel*>("input_model");
  }

  // Set model parameters.
  kde->Bandwidth(bandwidth);
  kde->RelativeError(relError);
  kde->AbsoluteError(absError);
  kde->MonteCarlo(monteCarlo);
  kde->MCProbability(mcProb);
  kde->MCInitialSampleSize(initialSampleSize);
  kde->MCEntryCoefficient(mcEntryCoef);
  kde->MCBreakCoefficient(mcBreakCoef);

  // Evaluation.
  if (CLI::HasParam("query"))
  {
    arma::mat query = std::move(CLI::GetParam<arma::mat>("query"));
    kde->Evaluate(std::move(query), estimations);
  }
  else
  {
    kde->Evaluate(estimations);
  }

  // Output predictions if needed.
  if (CLI::HasParam("predictions"))
    CLI::GetParam<arma::vec>("predictions") = std::move(estimations);

  // Save model.
  if (CLI::HasParam("output_model"))
    CLI::GetParam<KDEModel*>("output_model") = kde;
}
