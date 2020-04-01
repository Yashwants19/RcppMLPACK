/**
 * @file decision_stump_test.cpp
 * @author Udit Saxena
 *
 * Tests for DecisionStump class.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/core.hpp>
#include <mlpack/methods/decision_stump/decision_stump.hpp>

#include <boost/test/unit_test.hpp>
#include "test_tools.hpp"

using namespace mlpack;
using namespace mlpack::decision_stump;
using namespace arma;
using namespace mlpack::distribution;

BOOST_AUTO_TEST_SUITE(DecisionStumpTest);

/**
 * This tests handles the case wherein only one class exists in the input
 * labels.  It checks whether the only class supplied was the only class
 * predicted.
 */
BOOST_AUTO_TEST_CASE(OneClass)
{
  const size_t numClasses = 2;
  const size_t inpBucketSize = 6;

  mat trainingData;
  trainingData << 2.4 << 3.8 << 3.8 << endr
               << 1   << 1   << 2   << endr
               << 1.3 << 1.9 << 1.3 << endr;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 1 << 1 << 1;

  mat testingData;
  testingData << 2.4 << 2.5 << 2.6;

  DecisionStump<> ds(trainingData, labelsIn.row(0), numClasses, inpBucketSize);

  Row<size_t> predictedLabels;
  ds.Classify(testingData, predictedLabels);

  for (size_t i = 0; i < predictedLabels.size(); i++)
    BOOST_CHECK_EQUAL(predictedLabels(i), 1);
}

/**
 * This tests whether the entropy is being correctly calculated by checking the
 * correct value of the splitting column value.  This test is for an
 * inpBucketSize of 4 and the correct value of the splitting dimension is 0.
 */
BOOST_AUTO_TEST_CASE(CorrectDimensionChosen)
{
  const size_t numClasses = 2;
  const size_t inpBucketSize = 4;

  // This dataset comes from Chapter 6 of the book "Data Mining: Concepts,
  // Models, Methods, and Algorithms" (2nd Edition) by Mehmed Kantardzic.  It is
  // found on page 176 (and a description of the correct splitting dimension is
  // given below that).
  mat trainingData;
  trainingData << 0  << 0  << 0  << 0  << 0  << 1  << 1  << 1  << 1
               << 2  << 2  << 2  << 2  << 2  << endr
               << 70 << 90 << 85 << 95 << 70 << 90 << 78 << 65 << 75
               << 80 << 70 << 80 << 80 << 96 << endr
               << 1  << 1  << 0  << 0  << 0  << 1  << 0  << 1  << 0
               << 1  << 1  << 0  << 0  << 0  << endr;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 1 << 1 << 1 << 0 << 0 << 0 << 0
           << 0 << 1 << 1 << 0 << 0 << 0;

  DecisionStump<> ds(trainingData, labelsIn.row(0), numClasses, inpBucketSize);

  // Only need to check the value of the splitting column, no need of
  // classification.
  BOOST_CHECK_EQUAL(ds.SplitDimension(), 0);
}

/**
 * This tests for the classification:
 *   if testinput < 0 - class 0
 *   if testinput > 0 - class 1
 * An almost perfect split on zero.
 */
BOOST_AUTO_TEST_CASE(PerfectSplitOnZero)
{
  const size_t numClasses = 2;
  const size_t inpBucketSize = 2;

  mat trainingData;
  trainingData << -1 << 1 << -2 << 2 << -3 << 3;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 1 << 0 << 1 << 0 << 1;

  mat testingData;
  testingData << -4 << 7 << -7 << -5 << 6;

  DecisionStump<> ds(trainingData, labelsIn.row(0), numClasses, inpBucketSize);

  Row<size_t> predictedLabels;
  ds.Classify(testingData, predictedLabels);

  BOOST_CHECK_EQUAL(predictedLabels(0, 0), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 1), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 2), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 3), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 4), 1);
}

/**
 * This tests the binning function for the case when a dataset with cardinality
 * of input < inpBucketSize is provided.
 */
BOOST_AUTO_TEST_CASE(BinningTesting)
{
  const size_t numClasses = 2;
  const size_t inpBucketSize = 10;

  mat trainingData;
  trainingData << -1 << 1 << -2 << 2 << -3 << 3 << -4;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 1 << 0 << 1 << 0 << 1 << 0;

  mat testingData;
  testingData << 5;

  DecisionStump<> ds(trainingData, labelsIn.row(0), numClasses, inpBucketSize);

  Row<size_t> predictedLabels;
  ds.Classify(testingData, predictedLabels);

  BOOST_CHECK_EQUAL(predictedLabels(0, 0), 0);
}

/**
 * This is a test for the case when non-overlapping, multiple classes are
 * provided. It tests for a perfect split due to the non-overlapping nature of
 * the input classes.
 */
BOOST_AUTO_TEST_CASE(PerfectMultiClassSplit)
{
  const size_t numClasses = 4;
  const size_t inpBucketSize = 3;

  mat trainingData;
  trainingData << -8 << -7 << -6 << -5 << -4 << -3 << -2 << -1
               << 0  << 1  << 2  << 3  << 4  << 5  << 6  << 7;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 0 << 0 << 0 << 1 << 1 << 1 << 1
           << 2 << 2 << 2 << 2 << 3 << 3 << 3 << 3;

  mat testingData;
  testingData << -6.1 << -2.1 << 1.1 << 5.1;

  DecisionStump<> ds(trainingData, labelsIn.row(0), numClasses, inpBucketSize);

  Row<size_t> predictedLabels;
  ds.Classify(testingData, predictedLabels);

  BOOST_CHECK_EQUAL(predictedLabels(0, 0), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 1), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 2), 2);
  BOOST_CHECK_EQUAL(predictedLabels(0, 3), 3);
}

/**
 * This test is for the case when reasonably overlapping, multiple classes are
 * provided in the input label set. It tests whether classification takes place
 * with a reasonable amount of error due to the overlapping nature of input
 * classes.
 */
BOOST_AUTO_TEST_CASE(MultiClassSplit)
{
  const size_t numClasses = 3;
  const size_t inpBucketSize = 3;

  mat trainingData;
  trainingData << -7 << -6 << -5 << -4 << -3 << -2 << -1 << 0 << 1
               << 2  << 3  << 4  << 5  << 6  << 7  << 8  << 9 << 10;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 0 << 0 << 0 << 1 << 1 << 0 << 0
           << 1 << 1 << 1 << 2 << 1 << 2 << 2 << 2 << 2 << 2;


  mat testingData;
  testingData << -6.1 << -5.9 << -2.1 << -0.7 << 2.5 << 4.7 << 7.2 << 9.1;

  DecisionStump<> ds(trainingData, labelsIn.row(0), numClasses, inpBucketSize);

  Row<size_t> predictedLabels;
  ds.Classify(testingData, predictedLabels);

  BOOST_CHECK_EQUAL(predictedLabels(0, 0), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 1), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 2), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 3), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 4), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 5), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 6), 2);
  BOOST_CHECK_EQUAL(predictedLabels(0, 7), 2);
}

/**
 * This tests that the decision stump can learn a good split on a dataset with
 * four dimensions that have progressing levels of separation.
 */
BOOST_AUTO_TEST_CASE(DimensionSelectionTest)
{
  const size_t numClasses = 2;
  const size_t inpBucketSize = 2500;

  arma::mat dataset(4, 5000);

  // The most separable dimension.
  GaussianDistribution g1("-5", "1");
  GaussianDistribution g2("5", "1");

  for (size_t i = 0; i < 2500; ++i)
  {
    arma::vec tmp = g1.Random();
    dataset(1, i) = tmp[0];
  }
  for (size_t i = 2500; i < 5000; ++i)
  {
    arma::vec tmp = g2.Random();
    dataset(1, i) = tmp[0];
  }

  g1 = GaussianDistribution("-3", "1");
  g2 = GaussianDistribution("3", "1");

  for (size_t i = 0; i < 2500; ++i)
  {
    arma::vec tmp = g1.Random();
    dataset(3, i) = tmp[0];
  }
  for (size_t i = 2500; i < 5000; ++i)
  {
    arma::vec tmp = g2.Random();
    dataset(3, i) = tmp[0];
  }

  g1 = GaussianDistribution("-1", "1");
  g2 = GaussianDistribution("1", "1");

  for (size_t i = 0; i < 2500; ++i)
  {
    arma::vec tmp = g1.Random();
    dataset(0, i) = tmp[0];
  }
  for (size_t i = 2500; i < 5000; ++i)
  {
    arma::vec tmp = g2.Random();
    dataset(0, i) = tmp[0];
  }

  // Not separable at all.
  g1 = GaussianDistribution("0", "1");
  g2 = GaussianDistribution("0", "1");

  for (size_t i = 0; i < 2500; ++i)
  {
    arma::vec tmp = g1.Random();
    dataset(2, i) = tmp[0];
  }
  for (size_t i = 2500; i < 5000; ++i)
  {
    arma::vec tmp = g2.Random();
    dataset(2, i) = tmp[0];
  }

  // Generate the labels.
  arma::Row<size_t> labels(5000);
  for (size_t i = 0; i < 2500; ++i)
    labels[i] = 0;
  for (size_t i = 2500; i < 5000; ++i)
    labels[i] = 1;

  // Now create a decision stump.
  DecisionStump<> ds(dataset, labels, numClasses, inpBucketSize);

  // Make sure it split on the dimension that is most separable.
  BOOST_CHECK_EQUAL(ds.SplitDimension(), 1);

  // Make sure every bin below -1 classifies as label 0, and every bin above 1
  // classifies as label 1 (What happens in [-1, 1] isn't that big a deal.).
  for (size_t i = 0; i < ds.Split().n_elem; ++i)
  {
    if (ds.Split()[i] <= -3.0)
      BOOST_CHECK_EQUAL(ds.BinLabels()[i], 0);
    else if (ds.Split()[i] >= 3.0)
      BOOST_CHECK_EQUAL(ds.BinLabels()[i], 1);
  }
}

/**
 * Ensure that the default constructor works and that it classifies things as 0
 * always.
 */
BOOST_AUTO_TEST_CASE(EmptyConstructorTest)
{
  DecisionStump<> d;

  arma::mat data = arma::randu<arma::mat>(3, 10);
  arma::Row<size_t> labels;

  d.Classify(data, labels);

  for (size_t i = 0; i < 10; ++i)
    BOOST_REQUIRE_EQUAL(labels[i], 0);

  // Now train on another dataset and make sure something kind of makes sense.
  mat trainingData;
  trainingData << -7 << -6 << -5 << -4 << -3 << -2 << -1 << 0 << 1
               << 2  << 3  << 4  << 5  << 6  << 7  << 8  << 9 << 10;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 0 << 0 << 0 << 1 << 1 << 0 << 0
           << 1 << 1 << 1 << 2 << 1 << 2 << 2 << 2 << 2 << 2;


  mat testingData;
  testingData << -6.1 << -5.9 << -2.1 << -0.7 << 2.5 << 4.7 << 7.2 << 9.1;

  DecisionStump<> ds(trainingData, labelsIn.row(0), 4, 3);

  Row<size_t> predictedLabels(testingData.n_cols);
  ds.Classify(testingData, predictedLabels);

  BOOST_CHECK_EQUAL(predictedLabels(0, 0), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 1), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 2), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 3), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 4), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 5), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 6), 2);
  BOOST_CHECK_EQUAL(predictedLabels(0, 7), 2);
}

/**
 * Ensure that a matrix holding ints can be trained.  The bigger issue here is
 * just compilation.
 */
BOOST_AUTO_TEST_CASE(IntTest)
{
  // Train on a dataset and make sure something kind of makes sense.
  imat trainingData;
  trainingData << -7 << -6 << -5 << -4 << -3 << -2 << -1 << 0 << 1
               << 2  << 3  << 4  << 5  << 6  << 7  << 8  << 9 << 10;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 0 << 0 << 0 << 1 << 1 << 0 << 0
           << 1 << 1 << 1 << 2 << 1 << 2 << 2 << 2 << 2 << 2;

  DecisionStump<arma::imat> ds(trainingData, labelsIn.row(0), 4, 3);

  imat testingData;
  testingData << -6 << -6 << -2 << -1 << 3 << 5 << 7 << 9;

  arma::Row<size_t> predictedLabels;
  ds.Classify(testingData, predictedLabels);

  BOOST_CHECK_EQUAL(predictedLabels(0, 0), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 1), 0);
  BOOST_CHECK_EQUAL(predictedLabels(0, 2), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 3), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 4), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 5), 1);
  BOOST_CHECK_EQUAL(predictedLabels(0, 6), 2);
  BOOST_CHECK_EQUAL(predictedLabels(0, 7), 2);
}

/**
 * Test that DecisionStump::Train() returns finite gain.
 */
BOOST_AUTO_TEST_CASE(DecisionStumpTrainReturnEntropy)
{
  const size_t numClasses = 2;
  const size_t inpBucketSize = 2;

  mat trainingData;
  trainingData << -1 << 1 << -2 << 2 << -3 << 3;

  // No need to normalize labels here.
  Mat<size_t> labelsIn;
  labelsIn << 0 << 1 << 0 << 1 << 0 << 1;

  arma::Row<double> weights = arma::ones<arma::Row<double>>(labelsIn.n_elem);

  // Train a simple decision stump without weights.
  DecisionStump<> ds;
  double gain = ds.Train(trainingData, labelsIn.row(0), numClasses,
      inpBucketSize);

  BOOST_REQUIRE_EQUAL(std::isfinite(gain), true);

  // Train decision stump with weights.
  DecisionStump<> wds;
  gain = wds.Train(trainingData, labelsIn.row(0), weights, numClasses,
      inpBucketSize);

  BOOST_REQUIRE_EQUAL(std::isfinite(gain), true);
}

BOOST_AUTO_TEST_SUITE_END();
