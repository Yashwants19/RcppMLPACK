/**
 * @file AdaBoost_test.cpp
 * @author Udit Saxena
 *
 * Tests for AdaBoost class.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/core.hpp>
#include <mlpack/methods/adaboost/adaboost.hpp>

#include <boost/test/unit_test.hpp>
#include "test_tools.hpp"
#include "serialization.hpp"

using namespace arma;
using namespace mlpack;
using namespace mlpack::adaboost;
using namespace mlpack::tree;
using namespace mlpack::perceptron;

BOOST_AUTO_TEST_SUITE(AdaBoostTest);

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Iris dataset.  It
 * checks whether the hamming loss breaches the upperbound, which is provided by
 * ztAccumulator.
 */
BOOST_AUTO_TEST_CASE(HammingLossBoundIris)
{
  arma::mat inputData;

  if (!data::Load("iris.csv", inputData))
    BOOST_FAIL("Cannot load test dataset iris.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("iris_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for iris iris_labels.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  int perceptronIter = 400;

  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);

  // Define parameters for AdaBoost.
  size_t iterations = 100;
  double tolerance = 1e-10;
  AdaBoost<> a(tolerance);
  double ztProduct = a.Train(inputData, labels.row(0), numClasses, p,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double hammingLoss = (double) countError / labels.n_cols;

  // Check that ztProduct is finite.
  BOOST_REQUIRE_EQUAL(std::isfinite(ztProduct), true);
  BOOST_REQUIRE_LE(hammingLoss, ztProduct);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Iris dataset.  It
 * checks if the error returned by running a single instance of the weak learner
 * is worse than running the boosted weak learner using adaboost.
 */
BOOST_AUTO_TEST_CASE(WeakLearnerErrorIris)
{
  arma::mat inputData;

  if (!data::Load("iris.csv", inputData))
    BOOST_FAIL("Cannot load test dataset iris.csv!");

  arma::Mat<size_t> labels;

  if (!data::Load("iris_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for iris iris_labels.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  int perceptronIter = 400;

  arma::Row<size_t> perceptronPrediction(labels.n_cols);
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);
  p.Classify(inputData, perceptronPrediction);

  size_t countWeakLearnerError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != perceptronPrediction(i))
      countWeakLearnerError++;
  double weakLearnerErrorRate = (double) countWeakLearnerError / labels.n_cols;

  // Define parameters for AdaBoost.
  size_t iterations = 100;
  double tolerance = 1e-10;
  AdaBoost<> a(inputData, labels.row(0), numClasses, p, iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double error = (double) countError / labels.n_cols;

  BOOST_REQUIRE_LE(error, weakLearnerErrorRate);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Vertebral Column
 * dataset.  It checks whether the hamming loss breaches the upperbound, which
 * is provided by ztAccumulator.
 */
BOOST_AUTO_TEST_CASE(HammingLossBoundVertebralColumn)
{
  arma::mat inputData;
  if (!data::Load("vc2.csv", inputData))
    BOOST_FAIL("Cannot load test dataset vc2.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("vc2_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for vc2_labels.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  size_t perceptronIter = 800;
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<> a(tolerance);
  double ztProduct = a.Train(inputData, labels.row(0), numClasses, p,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double hammingLoss = (double) countError / labels.n_cols;

  // Check that ztProduct is finite.
  BOOST_REQUIRE_EQUAL(std::isfinite(ztProduct), true);
  BOOST_REQUIRE_LE(hammingLoss, ztProduct);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Vertebral Column
 * dataset.  It checks if the error returned by running a single instance of the
 * weak learner is worse than running the boosted weak learner using adaboost.
 */
BOOST_AUTO_TEST_CASE(WeakLearnerErrorVertebralColumn)
{
  arma::mat inputData;
  if (!data::Load("vc2.csv", inputData))
    BOOST_FAIL("Cannot load test dataset vc2.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("vc2_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for vc2_labels.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  size_t perceptronIter = 800;

  Row<size_t> perceptronPrediction(labels.n_cols);
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);
  p.Classify(inputData, perceptronPrediction);

  size_t countWeakLearnerError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != perceptronPrediction(i))
      countWeakLearnerError++;
  double weakLearnerErrorRate = (double) countWeakLearnerError / labels.n_cols;

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<> a(inputData, labels.row(0), numClasses, p, iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double error = (double) countError / labels.n_cols;

  BOOST_REQUIRE_LE(error, weakLearnerErrorRate);
}

/**
 * This test case runs the AdaBoost.mh algorithm on non-linearly separable
 * dataset.  It checks whether the hamming loss breaches the upperbound, which
 * is provided by ztAccumulator.
 */
BOOST_AUTO_TEST_CASE(HammingLossBoundNonLinearSepData)
{
  arma::mat inputData;
  if (!data::Load("train_nonlinsep.txt", inputData))
    BOOST_FAIL("Cannot load test dataset train_nonlinsep.txt!");

  arma::Mat<size_t> labels;
  if (!data::Load("train_labels_nonlinsep.txt", labels))
    BOOST_FAIL("Cannot load labels for train_labels_nonlinsep.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  size_t perceptronIter = 800;
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<> a(tolerance);
  double ztProduct = a.Train(inputData, labels.row(0), numClasses, p,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double hammingLoss = (double) countError / labels.n_cols;

  // Check that ztProduct is finite.
  BOOST_REQUIRE_EQUAL(std::isfinite(ztProduct), true);
  BOOST_REQUIRE_LE(hammingLoss, ztProduct);
}

/**
 * This test case runs the AdaBoost.mh algorithm on a non-linearly separable
 * dataset.  It checks if the error returned by running a single instance of the
 * weak learner is worse than running the boosted weak learner using AdaBoost.
 */
BOOST_AUTO_TEST_CASE(WeakLearnerErrorNonLinearSepData)
{
  arma::mat inputData;
  if (!data::Load("train_nonlinsep.txt", inputData))
    BOOST_FAIL("Cannot load test dataset train_nonlinsep.txt!");

  arma::Mat<size_t> labels;
  if (!data::Load("train_labels_nonlinsep.txt", labels))
    BOOST_FAIL("Cannot load labels for train_labels_nonlinsep.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  size_t perceptronIter = 800;

  Row<size_t> perceptronPrediction(labels.n_cols);
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);
  p.Classify(inputData, perceptronPrediction);

  size_t countWeakLearnerError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != perceptronPrediction(i))
      countWeakLearnerError++;
  double weakLearnerErrorRate = (double) countWeakLearnerError / labels.n_cols;

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<> a(inputData, labels.row(0), numClasses, p, iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double error = (double) countError / labels.n_cols;

  BOOST_REQUIRE_LE(error, weakLearnerErrorRate);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Iris dataset.  It
 * checks whether the Hamming loss breaches the upper bound, which is provided
 * by ztAccumulator.  This uses decision stumps as the weak learner.
 */
BOOST_AUTO_TEST_CASE(HammingLossIris_DS)
{
  arma::mat inputData;
  if (!data::Load("iris.csv", inputData))
    BOOST_FAIL("Cannot load test dataset iris.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("iris_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for iris_labels.txt");

  // Define your own weak learner, decision stumps in this case.
  const size_t numClasses = 3;
  const size_t inpBucketSize = 6;
  arma::Row<size_t> labelsvec = labels.row(0);
  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<ID3DecisionStump> a(tolerance);
  double ztProduct = a.Train(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double hammingLoss = (double) countError / labels.n_cols;

  // Check that ztProduct is finite.
  BOOST_REQUIRE_EQUAL(std::isfinite(ztProduct), true);
  BOOST_REQUIRE_LE(hammingLoss, ztProduct);
}

/**
 * This test case runs the AdaBoost.mh algorithm on a non-linearly separable
 * dataset.  It checks if the error returned by running a single instance of the
 * weak learner is worse than running the boosted weak learner using adaboost.
 * This is for the weak learner: decision stumps.
 */
BOOST_AUTO_TEST_CASE(WeakLearnerErrorIris_DS)
{
  arma::mat inputData;
  if (!data::Load("iris.csv", inputData))
    BOOST_FAIL("Cannot load test dataset iris.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("iris_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for iris_labels.txt");

  // no need to map the labels here

  // Define your own weak learner, decision stumps in this case.
  const size_t numClasses = 3;
  const size_t inpBucketSize = 6;
  arma::Row<size_t> labelsvec = labels.row(0);

  arma::Row<size_t> dsPrediction(labels.n_cols);

  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);
  ds.Classify(inputData, dsPrediction);

  size_t countWeakLearnerError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != dsPrediction(i))
      countWeakLearnerError++;
  double weakLearnerErrorRate = (double) countWeakLearnerError / labels.n_cols;

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;

  AdaBoost<ID3DecisionStump> a(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double error = (double) countError / labels.n_cols;

  BOOST_REQUIRE_LE(error, weakLearnerErrorRate);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Vertebral Column
 * dataset.  It checks if the error returned by running a single instance of the
 * weak learner is worse than running the boosted weak learner using adaboost.
 * This is for the weak learner: decision stumps.
 */
BOOST_AUTO_TEST_CASE(HammingLossBoundVertebralColumn_DS)
{
  arma::mat inputData;
  if (!data::Load("vc2.csv", inputData))
    BOOST_FAIL("Cannot load test dataset vc2.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("vc2_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for vc2_labels.txt");

  // Define your own weak learner, decision stumps in this case.
  const size_t numClasses = 3;
  const size_t inpBucketSize = 6;
  arma::Row<size_t> labelsvec = labels.row(0);

  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;

  AdaBoost<ID3DecisionStump> a(tolerance);
  double ztProduct = a.Train(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double hammingLoss = (double) countError / labels.n_cols;

  // Check that ztProduct is finite.
  BOOST_REQUIRE_EQUAL(std::isfinite(ztProduct), true);
  BOOST_REQUIRE_LE(hammingLoss, ztProduct);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Vertebral Column
 * dataset.  It checks if the error returned by running a single instance of the
 * weak learner is worse than running the boosted weak learner using adaboost.
 * This is for the weak learner: decision stumps.
 */
BOOST_AUTO_TEST_CASE(WeakLearnerErrorVertebralColumn_DS)
{
  arma::mat inputData;
  if (!data::Load("vc2.csv", inputData))
    BOOST_FAIL("Cannot load test dataset vc2.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("vc2_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for vc2_labels.txt");

  // Define your own weak learner, decision stumps in this case.
  const size_t numClasses = 3;
  const size_t inpBucketSize = 6;
  arma::Row<size_t> dsPrediction(labels.n_cols);
  arma::Row<size_t> labelsvec = labels.row(0);

  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);
  ds.Classify(inputData, dsPrediction);

  size_t countWeakLearnerError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != dsPrediction(i))
      countWeakLearnerError++;

  double weakLearnerErrorRate = (double) countWeakLearnerError / labels.n_cols;

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<ID3DecisionStump> a(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double error = (double) countError / labels.n_cols;

  BOOST_REQUIRE_LE(error, weakLearnerErrorRate);
}

/**
 * This test case runs the AdaBoost.mh algorithm on non-linearly separable
 * dataset.  It checks whether the hamming loss breaches the upperbound, which
 * is provided by ztAccumulator.  This is for the weak learner: decision stumps.
 */
BOOST_AUTO_TEST_CASE(HammingLossBoundNonLinearSepData_DS)
{
  arma::mat inputData;
  if (!data::Load("train_nonlinsep.txt", inputData))
    BOOST_FAIL("Cannot load test dataset train_nonlinsep.txt!");

  arma::Mat<size_t> labels;
  if (!data::Load("train_labels_nonlinsep.txt", labels))
    BOOST_FAIL("Cannot load labels for train_labels_nonlinsep.txt");

  // Define your own weak learner, decision stumps in this case.
  const size_t numClasses = 2;
  const size_t inpBucketSize = 6;
  arma::Row<size_t> labelsvec = labels.row(0);

  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);

  // Define parameters for Adaboost.
  size_t iterations = 50;
  double tolerance = 1e-10;

  AdaBoost<ID3DecisionStump> a(tolerance);
  double ztProduct = a.Train(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double hammingLoss = (double) countError / labels.n_cols;

  // Check that ztProduct is finite.
  BOOST_REQUIRE_EQUAL(std::isfinite(ztProduct), true);
  BOOST_REQUIRE_LE(hammingLoss, ztProduct);
}

/**
 * This test case runs the AdaBoost.mh algorithm on a non-linearly separable
 * dataset.  It checks if the error returned by running a single instance of the
 * weak learner is worse than running the boosted weak learner using adaboost.
 * This for the weak learner: decision stumps.
 */
BOOST_AUTO_TEST_CASE(WeakLearnerErrorNonLinearSepData_DS)
{
  arma::mat inputData;
  if (!data::Load("train_nonlinsep.txt", inputData))
    BOOST_FAIL("Cannot load test dataset train_nonlinsep.txt!");

  arma::Mat<size_t> labels;
  if (!data::Load("train_labels_nonlinsep.txt", labels))
    BOOST_FAIL("Cannot load labels for train_labels_nonlinsep.txt");

  // Define your own weak learner, decision stumps in this case.
  const size_t numClasses = 2;
  const size_t inpBucketSize = 3;
  arma::Row<size_t> labelsvec = labels.row(0);

  arma::Row<size_t> dsPrediction(labels.n_cols);

  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);
  ds.Classify(inputData, dsPrediction);

  size_t countWeakLearnerError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != dsPrediction(i))
      countWeakLearnerError++;
  double weakLearnerErrorRate = (double) countWeakLearnerError / labels.n_cols;

  // Define parameters for AdaBoost.
  size_t iterations = 500;
  double tolerance = 1e-23;

  AdaBoost<ID3DecisionStump > a(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels;
  a.Classify(inputData, predictedLabels);

  size_t countError = 0;
  for (size_t i = 0; i < labels.n_cols; i++)
    if (labels(i) != predictedLabels(i))
      countError++;
  double error = (double) countError / labels.n_cols;

  BOOST_REQUIRE_LE(error, weakLearnerErrorRate);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Vertebral Column
 * dataset.  It tests the Classify function and checks for a satisfactory error
 * rate.
 */
BOOST_AUTO_TEST_CASE(ClassifyTest_VERTEBRALCOL)
{
  arma::mat inputData;
  if (!data::Load("vc2.csv", inputData))
    BOOST_FAIL("Cannot load test dataset vc2.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("vc2_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for vc2_labels.txt");

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  size_t perceptronIter = 1000;

  arma::mat testData;

  if (!data::Load("vc2_test.csv", testData))
    BOOST_FAIL("Cannot load test dataset vc2_test.csv!");

  arma::Mat<size_t> trueTestLabels;

  if (!data::Load("vc2_test_labels.txt", trueTestLabels))
    BOOST_FAIL("Cannot load labels for vc2_test_labels.txt");

  const size_t numClasses = max(labels.row(0)) + 1;

  Row<size_t> perceptronPrediction(labels.n_cols);
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);
  p.Classify(inputData, perceptronPrediction);

  // Define parameters for AdaBoost.
  size_t iterations = 100;
  double tolerance = 1e-10;
  AdaBoost<> a(inputData, labels.row(0), numClasses, p, iterations, tolerance);

  arma::Row<size_t> predictedLabels1(testData.n_cols),
                    predictedLabels2(testData.n_cols);
  arma::mat probabilities;

  a.Classify(testData, predictedLabels1);
  a.Classify(testData, predictedLabels2, probabilities);

  BOOST_REQUIRE_EQUAL(probabilities.n_cols, testData.n_cols);
  BOOST_REQUIRE_EQUAL(probabilities.n_rows, numClasses);

  for (size_t i = 0; i < predictedLabels1.n_cols; ++i)
    BOOST_REQUIRE_EQUAL(predictedLabels1[i], predictedLabels2[i]);

  arma::colvec pRow;
  arma::uword maxIndex = 0;

  for (size_t i = 0; i < predictedLabels1.n_cols; i++)
  {
    pRow = probabilities.unsafe_col(i);
    pRow.max(maxIndex);
    BOOST_REQUIRE_EQUAL(predictedLabels1(i), maxIndex);
    BOOST_REQUIRE_CLOSE(arma::accu(probabilities.col(i)), 1, 1e-5);
  }

  size_t localError = 0;
  for (size_t i = 0; i < trueTestLabels.n_cols; i++)
    if (trueTestLabels(i) != predictedLabels1(i))
      localError++;

  double lError = (double) localError / trueTestLabels.n_cols;
  BOOST_REQUIRE_LE(lError, 0.30);
}

/**
 * This test case runs the AdaBoost.mh algorithm on a non linearly separable
 * dataset.  It tests the Classify function and checks for a satisfactory error
 * rate.
 */
BOOST_AUTO_TEST_CASE(ClassifyTest_NONLINSEP)
{
  arma::mat inputData;
  if (!data::Load("train_nonlinsep.txt", inputData))
    BOOST_FAIL("Cannot load test dataset train_nonlinsep.txt!");

  arma::Mat<size_t> labels;
  if (!data::Load("train_labels_nonlinsep.txt", labels))
    BOOST_FAIL("Cannot load labels for train_labels_nonlinsep.txt");

  // Define your own weak learner; in this test decision stumps are used.
  const size_t numClasses = 2;
  const size_t inpBucketSize = 3;
  arma::Row<size_t> labelsvec = labels.row(0);

  arma::mat testData;

  if (!data::Load("test_nonlinsep.txt", testData))
    BOOST_FAIL("Cannot load test dataset test_nonlinsep.txt!");

  arma::Mat<size_t> trueTestLabels;

  if (!data::Load("test_labels_nonlinsep.txt", trueTestLabels))
    BOOST_FAIL("Cannot load labels for test_labels_nonlinsep.txt");

  arma::Row<size_t> dsPrediction(labels.n_cols);

  ID3DecisionStump ds(inputData, labelsvec, numClasses, inpBucketSize);

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<ID3DecisionStump > a(inputData, labelsvec, numClasses, ds,
      iterations, tolerance);

  arma::Row<size_t> predictedLabels1(testData.n_cols),
                    predictedLabels2(testData.n_cols);
  arma::mat probabilities;

  a.Classify(testData, predictedLabels1);
  a.Classify(testData, predictedLabels2, probabilities);

  BOOST_REQUIRE_EQUAL(probabilities.n_cols, testData.n_cols);

  for (size_t i = 0; i < predictedLabels1.n_cols; ++i)
    BOOST_REQUIRE_EQUAL(predictedLabels1[i], predictedLabels2[i]);

  arma::colvec pRow;
  arma::uword maxIndex = 0;

  for (size_t i = 0; i < predictedLabels1.n_cols; i++)
  {
    pRow = probabilities.unsafe_col(i);
    pRow.max(maxIndex);
    BOOST_REQUIRE_EQUAL(predictedLabels1(i), maxIndex);
    BOOST_REQUIRE_CLOSE(arma::accu(probabilities.col(i)), 1, 1e-5);
  }

  size_t localError = 0;
  for (size_t i = 0; i < trueTestLabels.n_cols; i++)
    if (trueTestLabels(i) != predictedLabels1(i))
      localError++;

  double lError = (double) localError / trueTestLabels.n_cols;
  BOOST_REQUIRE_LE(lError, 0.30);
}

/**
 * This test case runs the AdaBoost.mh algorithm on the UCI Iris Dataset.  It
 * trains it on two thirds of the Iris dataset (iris_train.csv), and tests on
 * the remaining third of the dataset (iris_test.csv).  It tests the Classify()
 * function and checks for a satisfactory error rate.
 */
BOOST_AUTO_TEST_CASE(ClassifyTest_IRIS)
{
  arma::mat inputData;
  if (!data::Load("iris_train.csv", inputData))
    BOOST_FAIL("Cannot load test dataset iris_train.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("iris_train_labels.csv", labels))
    BOOST_FAIL("Cannot load labels for iris_train_labels.csv");
  const size_t numClasses = max(labels.row(0)) + 1;

  // Define your own weak learner, perceptron in this case.
  // Run the perceptron for perceptronIter iterations.
  size_t perceptronIter = 800;

  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);

  // Define parameters for AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<> a(inputData, labels.row(0), numClasses, p, iterations, tolerance);

  arma::mat testData;
  if (!data::Load("iris_test.csv", testData))
    BOOST_FAIL("Cannot load test dataset iris_test.csv!");

  arma::Row<size_t> predictedLabels(testData.n_cols);
  a.Classify(testData, predictedLabels);

  arma::Mat<size_t> trueTestLabels;
  if (!data::Load("iris_test_labels.csv", trueTestLabels))
    BOOST_FAIL("Cannot load test dataset iris_test_labels.csv!");

  arma::Row<size_t> predictedLabels1(testData.n_cols),
                    predictedLabels2(testData.n_cols);
  arma::mat probabilities;

  a.Classify(testData, predictedLabels1);
  a.Classify(testData, predictedLabels2, probabilities);

  BOOST_REQUIRE_EQUAL(probabilities.n_cols, testData.n_cols);

  for (size_t i = 0; i < predictedLabels1.n_cols; ++i)
    BOOST_REQUIRE_EQUAL(predictedLabels1[i], predictedLabels2[i]);

  arma::colvec pRow;
  arma::uword maxIndex = 0;

  for (size_t i = 0; i < predictedLabels1.n_cols; i++)
  {
    pRow = probabilities.unsafe_col(i);
    pRow.max(maxIndex);
    BOOST_REQUIRE_EQUAL(predictedLabels1(i), maxIndex);
    BOOST_REQUIRE_CLOSE(arma::accu(probabilities.col(i)), 1, 1e-5);
  }

  size_t localError = 0;
  for (size_t i = 0; i < trueTestLabels.n_cols; i++)
    if (trueTestLabels(i) != predictedLabels1(i))
      localError++;
  double lError = (double) localError / labels.n_cols;
  BOOST_REQUIRE_LE(lError, 0.30);
}

/**
 * Ensure that the Train() function works like it is supposed to, by building
 * AdaBoost on one dataset and then re-training on another dataset.
 */
BOOST_AUTO_TEST_CASE(TrainTest)
{
  // First train on the iris dataset.
  arma::mat inputData;
  if (!data::Load("iris_train.csv", inputData))
    BOOST_FAIL("Cannot load test dataset iris_train.csv!");

  arma::Mat<size_t> labels;
  if (!data::Load("iris_train_labels.csv", labels))
    BOOST_FAIL("Cannot load labels for iris_train_labels.csv");

  const size_t numClasses = max(labels.row(0)) + 1;

  size_t perceptronIter = 800;
  Perceptron<> p(inputData, labels.row(0), numClasses, perceptronIter);

  // Now train AdaBoost.
  size_t iterations = 50;
  double tolerance = 1e-10;
  AdaBoost<> a(inputData, labels.row(0), numClasses, p, iterations, tolerance);

  // Now load another dataset...
  if (!data::Load("vc2.csv", inputData))
    BOOST_FAIL("Cannot load test dataset vc2.csv!");
  if (!data::Load("vc2_labels.txt", labels))
    BOOST_FAIL("Cannot load labels for vc2_labels.txt");

  const size_t newNumClasses = max(labels.row(0)) + 1;

  Perceptron<> p2(inputData, labels.row(0), newNumClasses, perceptronIter);

  a.Train(inputData, labels.row(0), newNumClasses, p2, iterations, tolerance);

  // Load test set to see if it trained on vc2 correctly.
  arma::mat testData;
  if (!data::Load("vc2_test.csv", testData))
    BOOST_FAIL("Cannot load test dataset vc2_test.csv!");

  arma::Mat<size_t> trueTestLabels;
  if (!data::Load("vc2_test_labels.txt", trueTestLabels))
    BOOST_FAIL("Cannot load labels for vc2_test_labels.txt");

  // Define parameters for AdaBoost.
  arma::Row<size_t> predictedLabels(testData.n_cols);
  a.Classify(testData, predictedLabels);

  int localError = 0;
  for (size_t i = 0; i < trueTestLabels.n_cols; i++)
    if (trueTestLabels(i) != predictedLabels(i))
      localError++;

  double lError = (double) localError / trueTestLabels.n_cols;

  BOOST_REQUIRE_LE(lError, 0.30);
}

BOOST_AUTO_TEST_CASE(PerceptronSerializationTest)
{
  // Build an AdaBoost object.
  mat data = randu<mat>(10, 500);
  Row<size_t> labels(500);
  for (size_t i = 0; i < 250; ++i)
    labels[i] = 0;
  for (size_t i = 250; i < 500; ++i)
    labels[i] = 1;

  Perceptron<> p(data, labels, 2, 800);
  AdaBoost<> ab(data, labels, 2, p, 50, 1e-10);

  // Now create another dataset to train with.
  mat otherData = randu<mat>(5, 200);
  Row<size_t> otherLabels(200);
  for (size_t i = 0; i < 100; ++i)
    otherLabels[i] = 1;
  for (size_t i = 100; i < 150; ++i)
    otherLabels[i] = 0;
  for (size_t i = 150; i < 200; ++i)
    otherLabels[i] = 2;

  Perceptron<> p2(otherData, otherLabels, 3, 500);
  AdaBoost<> abText(otherData, otherLabels, 3, p2, 50, 1e-10);

  AdaBoost<> abXml, abBinary;

  SerializeObjectAll(ab, abXml, abText, abBinary);

  // Now check that the objects are the same.
  BOOST_REQUIRE_CLOSE(ab.Tolerance(), abXml.Tolerance(), 1e-5);
  BOOST_REQUIRE_CLOSE(ab.Tolerance(), abText.Tolerance(), 1e-5);
  BOOST_REQUIRE_CLOSE(ab.Tolerance(), abBinary.Tolerance(), 1e-5);

  BOOST_REQUIRE_EQUAL(ab.WeakLearners(), abXml.WeakLearners());
  BOOST_REQUIRE_EQUAL(ab.WeakLearners(), abText.WeakLearners());
  BOOST_REQUIRE_EQUAL(ab.WeakLearners(), abBinary.WeakLearners());

  for (size_t i = 0; i < ab.WeakLearners(); ++i)
  {
    CheckMatrices(ab.WeakLearner(i).Weights(),
                  abXml.WeakLearner(i).Weights(),
                  abText.WeakLearner(i).Weights(),
                  abBinary.WeakLearner(i).Weights());

    CheckMatrices(ab.WeakLearner(i).Biases(),
                  abXml.WeakLearner(i).Biases(),
                  abText.WeakLearner(i).Biases(),
                  abBinary.WeakLearner(i).Biases());
  }
}

BOOST_AUTO_TEST_CASE(ID3DecisionStumpSerializationTest)
{
  // Build an AdaBoost object.
  mat data = randu<mat>(10, 500);
  Row<size_t> labels(500);
  for (size_t i = 0; i < 250; ++i)
    labels[i] = 0;
  for (size_t i = 250; i < 500; ++i)
    labels[i] = 1;

  ID3DecisionStump p(data, labels, 2, 800);
  AdaBoost<ID3DecisionStump> ab(data, labels, 2, p, 50, 1e-10);

  // Now create another dataset to train with.
  mat otherData = randu<mat>(5, 200);
  Row<size_t> otherLabels(200);
  for (size_t i = 0; i < 100; ++i)
    otherLabels[i] = 1;
  for (size_t i = 100; i < 150; ++i)
    otherLabels[i] = 0;
  for (size_t i = 150; i < 200; ++i)
    otherLabels[i] = 2;

  ID3DecisionStump p2(otherData, otherLabels, 3, 500);
  AdaBoost<ID3DecisionStump> abText(otherData, otherLabels, 3, p2, 50, 1e-10);

  AdaBoost<ID3DecisionStump> abXml, abBinary;

  SerializeObjectAll(ab, abXml, abText, abBinary);

  // Now check that the objects are the same.
  BOOST_REQUIRE_CLOSE(ab.Tolerance(), abXml.Tolerance(), 1e-5);
  BOOST_REQUIRE_CLOSE(ab.Tolerance(), abText.Tolerance(), 1e-5);
  BOOST_REQUIRE_CLOSE(ab.Tolerance(), abBinary.Tolerance(), 1e-5);

  BOOST_REQUIRE_EQUAL(ab.WeakLearners(), abXml.WeakLearners());
  BOOST_REQUIRE_EQUAL(ab.WeakLearners(), abText.WeakLearners());
  BOOST_REQUIRE_EQUAL(ab.WeakLearners(), abBinary.WeakLearners());

  for (size_t i = 0; i < ab.WeakLearners(); ++i)
  {
    BOOST_REQUIRE_EQUAL(ab.WeakLearner(i).SplitDimension(),
                        abXml.WeakLearner(i).SplitDimension());
    BOOST_REQUIRE_EQUAL(ab.WeakLearner(i).SplitDimension(),
                        abText.WeakLearner(i).SplitDimension());
    BOOST_REQUIRE_EQUAL(ab.WeakLearner(i).SplitDimension(),
                        abBinary.WeakLearner(i).SplitDimension());
  }
}

BOOST_AUTO_TEST_SUITE_END();
