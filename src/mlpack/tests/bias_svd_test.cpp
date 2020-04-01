/**
 * @file bias_svd_test.cpp
 * @author Siddharth Agrawal
 * @author Wenhao Huang
 *
 * Test the BiasSVDFunction class.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/core.hpp>
#include <mlpack/methods/bias_svd/bias_svd.hpp>

#include <ensmallen.hpp>

#include <boost/test/unit_test.hpp>
#include "test_tools.hpp"

using namespace mlpack;
using namespace mlpack::svd;

BOOST_AUTO_TEST_SUITE(BiasSVDTest);

BOOST_AUTO_TEST_CASE(BiasSVDFunctionRandomEvaluate)
{
  // Define useful constants.
  const size_t numUsers = 100;
  const size_t numItems = 100;
  const size_t numRatings = 1000;
  const size_t maxRating = 5;
  const size_t rank = 10;
  const size_t numTrials = 50;

  // Make a random rating dataset.
  arma::mat data = arma::randu(3, numRatings);
  data.row(0) = floor(data.row(0) * numUsers);
  data.row(1) = floor(data.row(1) * numItems);
  data.row(2) = floor(data.row(2) * maxRating + 0.5);

  // Manually set last row to maximum user and maximum item.
  data(0, numRatings - 1) = numUsers - 1;
  data(1, numRatings - 1) = numItems - 1;

  // Make a BiasSVDFunction with zero regularization.
  BiasSVDFunction<arma::mat> biasSVDFunc(data, rank, 0);

  for (size_t i = 0; i < numTrials; i++)
  {
    arma::mat parameters = arma::randu(rank + 1, numUsers + numItems);

    // Calculate cost by summing up cost of each example.
    double cost = 0;
    for (size_t j = 0; j < numRatings; j++)
    {
      const size_t user = data(0, j);
      const size_t item = data(1, j) + numUsers;

      const double rating = data(2, j);
      const double userBias = parameters(rank, user);
      const double itemBias = parameters(rank, item);
      const double ratingError = rating - userBias - itemBias -
          arma::dot(parameters.col(user).subvec(0, rank - 1),
                    parameters.col(item).subvec(0, rank - 1));
      const double ratingErrorSquared = ratingError * ratingError;

      cost += ratingErrorSquared;
    }

    // Compare calculated cost and value obtained using Evaluate().
    BOOST_REQUIRE_CLOSE(cost, biasSVDFunc.Evaluate(parameters), 1e-5);
  }
}

BOOST_AUTO_TEST_CASE(BiasSVDFunctionRegularizationEvaluate)
{
  // Define useful constants.
  const size_t numUsers = 100;
  const size_t numItems = 100;
  const size_t numRatings = 1000;
  const size_t maxRating = 5;
  const size_t rank = 10;
  const size_t numTrials = 50;

  // Make a random rating dataset.
  arma::mat data = arma::randu(3, numRatings);
  data.row(0) = floor(data.row(0) * numUsers);
  data.row(1) = floor(data.row(1) * numItems);
  data.row(2) = floor(data.row(2) * maxRating + 0.5);

  // Manually set last row to maximum user and maximum item.
  data(0, numRatings - 1) = numUsers - 1;
  data(1, numRatings - 1) = numItems - 1;

  // Make three BiasSVDFunction objects with different amounts of
  // regularization.
  BiasSVDFunction<arma::mat> biasSVDFuncNoReg(data, rank, 0);
  BiasSVDFunction<arma::mat> biasSVDFuncSmallReg(data, rank, 0.5);
  BiasSVDFunction<arma::mat> biasSVDFuncBigReg(data, rank, 20);

  for (size_t i = 0; i < numTrials; i++)
  {
    arma::mat parameters = arma::randu(rank + 1, numUsers + numItems);

    // Calculate the regularization contributions of parameters corresponding to
    // each rating and sum them up.
    double smallRegTerm = 0;
    double bigRegTerm = 0;
    for (size_t j = 0; j < numRatings; j++)
    {
      const size_t user = data(0, j);
      const size_t item = data(1, j) + numUsers;

      const double userVecNorm = arma::norm(parameters.col(user), 2);
      const double itemVecNorm = arma::norm(parameters.col(item), 2);

      smallRegTerm += 0.5 * (userVecNorm * userVecNorm +
                             itemVecNorm * itemVecNorm);
      bigRegTerm += 20 * (userVecNorm * userVecNorm +
                          itemVecNorm * itemVecNorm);
    }

    // Cost with regularization should be close to the sum of cost without
    // regularization and the regularization terms.
    BOOST_REQUIRE_CLOSE(biasSVDFuncNoReg.Evaluate(parameters) + smallRegTerm,
        biasSVDFuncSmallReg.Evaluate(parameters), 1e-5);
    BOOST_REQUIRE_CLOSE(biasSVDFuncNoReg.Evaluate(parameters) + bigRegTerm,
        biasSVDFuncBigReg.Evaluate(parameters), 1e-5);
  }
}

BOOST_AUTO_TEST_CASE(BiasSVDFunctionGradient)
{
  // Define useful constants.
  const size_t numUsers = 50;
  const size_t numItems = 50;
  const size_t numRatings = 100;
  const size_t maxRating = 5;
  const size_t rank = 10;

  // Make a random rating dataset.
  arma::mat data = arma::randu(3, numRatings);
  data.row(0) = floor(data.row(0) * numUsers);
  data.row(1) = floor(data.row(1) * numItems);
  data.row(2) = floor(data.row(2) * maxRating + 0.5);

  // Manually set last row to maximum user and maximum item.
  data(0, numRatings - 1) = numUsers - 1;
  data(1, numRatings - 1) = numItems - 1;

  arma::mat parameters = arma::randu(rank + 1, numUsers + numItems);

  // Make two BiasSVDFunction objects, one with regularization and one
  // without.
  BiasSVDFunction<arma::mat> biasSVDFunc1(data, rank, 0);
  BiasSVDFunction<arma::mat> biasSVDFunc2(data, rank, 0.5);

  // Calculate gradients for both the objects.
  arma::mat gradient1, gradient2;
  biasSVDFunc1.Gradient(parameters, gradient1);
  biasSVDFunc2.Gradient(parameters, gradient2);

  // Perturbation constant.
  const double epsilon = 0.0001;
  double costPlus1, costMinus1, numGradient1;
  double costPlus2, costMinus2, numGradient2;

  for (size_t i = 0; i < rank; i++)
  {
    for (size_t j = 0; j < numUsers + numItems; j++)
    {
      // Perturb parameter with a positive constant and get costs.
      parameters(i, j) += epsilon;
      costPlus1 = biasSVDFunc1.Evaluate(parameters);
      costPlus2 = biasSVDFunc2.Evaluate(parameters);

      // Perturb parameter with a negative constant and get costs.
      parameters(i, j) -= 2 * epsilon;
      costMinus1 = biasSVDFunc1.Evaluate(parameters);
      costMinus2 = biasSVDFunc2.Evaluate(parameters);

      // Compute numerical gradients using the costs calculated above.
      numGradient1 = (costPlus1 - costMinus1) / (2 * epsilon);
      numGradient2 = (costPlus2 - costMinus2) / (2 * epsilon);

      // Restore the parameter value.
      parameters(i, j) += epsilon;

      // Compare numerical and backpropagation gradient values.
      if (std::abs(gradient1(i, j)) <= 1e-6)
        BOOST_REQUIRE_SMALL(numGradient1, 1e-5);
      else
        BOOST_REQUIRE_CLOSE(numGradient1, gradient1(i, j), 0.02);

      if (std::abs(gradient2(i, j)) <= 1e-6)
        BOOST_REQUIRE_SMALL(numGradient2, 1e-5);
      else
        BOOST_REQUIRE_CLOSE(numGradient2, gradient2(i, j), 0.02);
    }
  }
}

BOOST_AUTO_TEST_CASE(BiasSVDOutputSizeTest)
{
  // Define useful constants.
  const size_t numUsers = 100;
  const size_t numItems = 50;
  const size_t numRatings = 500;
  const size_t maxRating = 5;
  const size_t rank = 5;
  const size_t iterations = 10;

  // Make a random rating dataset.
  arma::mat data = arma::randu(3, numRatings);
  data.row(0) = floor(data.row(0) * numUsers);
  data.row(1) = floor(data.row(1) * numItems);
  data.row(2) = floor(data.row(2) * maxRating + 0.5);

  // Manually set last row to maximum user and maximum item.
  data(0, numRatings - 1) = numUsers - 1;
  data(1, numRatings - 1) = numItems - 1;

  // Resulting user/item matrices/bias.
  arma::mat userLatent, itemLatent;
  arma::vec userBias, itemBias;

  // Apply Bias SVD.
  BiasSVD<> biasSVD(iterations);
  biasSVD.Apply(data, rank, itemLatent, userLatent, itemBias, userBias);

  // Check the size of outputs.
  BOOST_REQUIRE_EQUAL(itemLatent.n_rows, numItems);
  BOOST_REQUIRE_EQUAL(itemLatent.n_cols, rank);
  BOOST_REQUIRE_EQUAL(userLatent.n_rows, rank);
  BOOST_REQUIRE_EQUAL(userLatent.n_cols, numUsers);
  BOOST_REQUIRE_EQUAL(itemBias.n_elem, numItems);
  BOOST_REQUIRE_EQUAL(userBias.n_elem, numUsers);
}

BOOST_AUTO_TEST_CASE(BiasSVDFunctionOptimize)
{
  // Define useful constants.
  const size_t numUsers = 50;
  const size_t numItems = 50;
  const size_t numRatings = 100;
  const size_t iterations = 30;
  const size_t rank = 10;
  const double alpha = 0.01;
  const double lambda = 0.01;

  // Initiate random parameters.
  arma::mat parameters = arma::randu(rank + 1, numUsers + numItems);

  // Make a random rating dataset.
  arma::mat data = arma::randu(3, numRatings);
  data.row(0) = floor(data.row(0) * numUsers);
  data.row(1) = floor(data.row(1) * numItems);

  // Manually set last row to maximum user and maximum item.
  data(0, numRatings - 1) = numUsers - 1;
  data(1, numRatings - 1) = numItems - 1;

  // Make rating entries based on the parameters.
  for (size_t i = 0; i < numRatings; i++)
  {
    const size_t user = data(0, i);
    const size_t item = data(1, i) + numUsers;
    const double userBias = parameters(rank, user);
    const double itemBias = parameters(rank, item);
    data(2, i) = userBias + itemBias +
        arma::dot(parameters.col(user).subvec(0, rank - 1),
                  parameters.col(item).subvec(0, rank - 1));
  }

  // Make the Bias SVD function and the optimizer.
  BiasSVDFunction<arma::mat> biasSVDFunc(data, rank, lambda);
  ens::StandardSGD optimizer(alpha, iterations * numRatings);

  // Obtain optimized parameters after training.
  arma::mat optParameters = arma::randu(rank + 1, numUsers + numItems);
  optimizer.Optimize(biasSVDFunc, optParameters);

  // Get predicted ratings from optimized parameters.
  arma::mat predictedData(1, numRatings);
  for (size_t i = 0; i < numRatings; i++)
  {
    const size_t user = data(0, i);
    const size_t item = data(1, i) + numUsers;
    const double userBias = optParameters(rank, user);
    const double itemBias = optParameters(rank, item);
    predictedData(0, i) = userBias + itemBias +
        arma::dot(optParameters.col(user).subvec(0, rank - 1),
                  optParameters.col(item).subvec(0, rank - 1));
  }

  // Calculate relative error.
  const double relativeError = arma::norm(data.row(2) - predictedData, "frob") /
                               arma::norm(data, "frob");

  // Relative error should be small.
  BOOST_REQUIRE_SMALL(relativeError, 1e-2);
}

// The test is only compiled if the user has specified OpenMP to be
// used.
#ifdef HAS_OPENMP

// Test Bias SVD with parallel SGD.
BOOST_AUTO_TEST_CASE(BiasSVDFunctionParallelOptimize)
{
  // Define useful constants.
  const size_t numUsers = 50;
  const size_t numItems = 50;
  const size_t numRatings = 100;
  const size_t rank = 10;
  const double alpha = 0.01;
  const double lambda = 0.01;

  // Initiate random parameters.
  arma::mat parameters = arma::randu(rank + 1, numUsers + numItems);

  // Make a random rating dataset.
  arma::mat data = arma::randu(3, numRatings);
  data.row(0) = floor(data.row(0) * numUsers);
  data.row(1) = floor(data.row(1) * numItems);

  // Manually set last row to maximum user and maximum item.
  data(0, numRatings - 1) = numUsers - 1;
  data(1, numRatings - 1) = numItems - 1;

  // Make rating entries based on the parameters.
  for (size_t i = 0; i < numRatings; i++)
  {
    const size_t user = data(0, i);
    const size_t item = data(1, i) + numUsers;
    const double userBias = parameters(rank, user);
    const double itemBias = parameters(rank, item);
    data(2, i) = userBias + itemBias +
        arma::dot(parameters.col(user).subvec(0, rank - 1),
                  parameters.col(item).subvec(0, rank - 1));
  }

  // Make the Bias SVD function and the optimizer.
  BiasSVDFunction<arma::mat> biasSVDFunc(data, rank, lambda);

  ens::ConstantStep decayPolicy(alpha);

  // Iterate till convergence.
  // The threadShareSize is chosen such that each function gets optimized.
  ens::ParallelSGD<ens::ConstantStep> optimizer(0,
      std::ceil((float) biasSVDFunc.NumFunctions() / omp_get_max_threads()),
      1e-5, true, decayPolicy);

  // Obtain optimized parameters after training.
  arma::mat optParameters = arma::randu(rank + 1, numUsers + numItems);
  optimizer.Optimize(biasSVDFunc, optParameters);

  // Get predicted ratings from optimized parameters.
  arma::mat predictedData(1, numRatings);
  for (size_t i = 0; i < numRatings; i++)
  {
    const size_t user = data(0, i);
    const size_t item = data(1, i) + numUsers;
    const double userBias = optParameters(rank, user);
    const double itemBias = optParameters(rank, item);
    predictedData(0, i) = userBias + itemBias +
        arma::dot(optParameters.col(user).subvec(0, rank - 1),
                  optParameters.col(item).subvec(0, rank - 1));
  }

  // Calculate relative error.
  const double relativeError = arma::norm(data.row(2) - predictedData, "frob") /
                               arma::norm(data, "frob");

  // Relative error should be small.
  BOOST_REQUIRE_SMALL(relativeError, 1e-2);
}

#endif

BOOST_AUTO_TEST_SUITE_END();
