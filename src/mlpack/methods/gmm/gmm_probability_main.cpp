/**
 * @file methods/gmm/gmm_probability_main.cpp
 * @author Ryan Curtin
 *
 * Given a GMM, calculate the probability of points coming from it.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/io.hpp>
#include <mlpack/core/util/mlpack_main.hpp>
#include "gmm.hpp"

using namespace std;
using namespace mlpack;
using namespace mlpack::gmm;
using namespace mlpack::util;

PROGRAM_INFO("GMM Probability Calculator",
    // Short description.
    "A probability calculator for GMMs.  Given a pre-trained GMM and a set of "
    "points, this can compute the probability that each point is from the given"
    " GMM.",
    // Long description.
    "This program calculates the probability that given points came from a "
    "given GMM (that is, P(X | gmm)).  The GMM is specified with the " +
    PRINT_PARAM_STRING("input_model") + " parameter, and the points are "
    "specified with the " + PRINT_PARAM_STRING("input") + " parameter.  The "
    "output probabilities may be saved via the " +
    PRINT_PARAM_STRING("output") + " output parameter.",
    // Example.
    "So, for example, to calculate the probabilities of each point in " +
    PRINT_DATASET("points") + " coming from the pre-trained GMM " +
    PRINT_MODEL("gmm") + ", while storing those probabilities in " +
    PRINT_DATASET("probs") + ", the following command could be used:"
    "\n\n" +
    PRINT_CALL("gmm_probability", "input_model", "gmm", "input", "points",
        "output", "probs"),
    SEE_ALSO("@gmm_train", "#gmm_train"),
    SEE_ALSO("@gmm_generate", "#gmm_generate"),
    SEE_ALSO("Gaussian Mixture Models on Wikipedia",
        "https://en.wikipedia.org/wiki/Mixture_model#Gaussian_mixture_model"),
    SEE_ALSO("mlpack::gmm::GMM class documentation",
        "@doxygen/classmlpack_1_1gmm_1_1GMM.html"));

PARAM_MODEL_IN_REQ(GMM, "input_model", "Input GMM to use as model.", "m");
PARAM_MATRIX_IN_REQ("input", "Input matrix to calculate probabilities of.",
    "i");

PARAM_MATRIX_OUT("output", "Matrix to store calculated probabilities in.", "o");

static void mlpackMain()
{
  RequireAtLeastOnePassed({ "output" }, false, "no results will be saved");

  // Get the GMM and the points.
  GMM* gmm = IO::GetParam<GMM*>("input_model");

  arma::mat dataset = std::move(IO::GetParam<arma::mat>("input"));

  // Now calculate the probabilities.
  arma::rowvec probabilities(dataset.n_cols);
  for (size_t i = 0; i < dataset.n_cols; ++i)
    probabilities[i] = gmm->Probability(dataset.unsafe_col(i));

  // And save the result.
  IO::GetParam<arma::mat>("output") = std::move(probabilities);
}
