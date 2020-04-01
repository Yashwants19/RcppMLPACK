/**
 * @file det_test.cpp
 * @author Parikshit Ram (pram@cc.gatech.edu)
 *
 * Unit tests for the functions of the class DTree and the utility functions
 * using this class.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/core.hpp>
#include <boost/test/unit_test.hpp>
#include "test_tools.hpp"

// This trick does not work on Windows.  We will have to comment out the tests
// that depend on it.
#ifndef _WIN32
  #define protected public
  #define private public
#endif

#include <mlpack/methods/det/dtree.hpp>
#include <mlpack/methods/det/dt_utils.hpp>

#ifndef _WIN32
  #undef protected
  #undef private
#endif

using namespace mlpack;
using namespace mlpack::det;
using namespace std;

BOOST_AUTO_TEST_SUITE(DETTest);

// Tests for the private functions.  We cannot perform these if we are on
// Windows because we cannot make private functions accessible using the macro
// trick above.
#ifndef _WIN32
BOOST_AUTO_TEST_CASE(TestGetMaxMinVals)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  DTree<arma::mat> tree(testData);

  BOOST_REQUIRE_EQUAL(tree.MaxVals()[0], 7);
  BOOST_REQUIRE_EQUAL(tree.MinVals()[0], 3);
  BOOST_REQUIRE_EQUAL(tree.MaxVals()[1], 7);
  BOOST_REQUIRE_EQUAL(tree.MinVals()[1], 0);
  BOOST_REQUIRE_EQUAL(tree.MaxVals()[2], 8);
  BOOST_REQUIRE_EQUAL(tree.MinVals()[2], 1);
}

BOOST_AUTO_TEST_CASE(TestComputeNodeError)
{
  arma::vec maxVals("7 7 8");
  arma::vec minVals("3 0 1");

  DTree<arma::mat> testDTree(maxVals, minVals, 5);
  double trueNodeError = -log(4.0) - log(7.0) - log(7.0);

  BOOST_REQUIRE_CLOSE((double) testDTree.logNegError, trueNodeError, 1e-10);

  testDTree.start = 3;
  testDTree.end = 5;

  double nodeError = testDTree.LogNegativeError(5);
  trueNodeError = 2 * log(2.0 / 5.0) - log(4.0) - log(7.0) - log(7.0);
  BOOST_REQUIRE_CLOSE(nodeError, trueNodeError, 1e-10);
}

BOOST_AUTO_TEST_CASE(TestWithinRange)
{
  arma::vec maxVals("7 7 8");
  arma::vec minVals("3 0 1");

  DTree<arma::mat> testDTree(maxVals, minVals, 5);

  arma::vec testQuery(3);
  testQuery << 4.5 << 2.5 << 2;

  BOOST_REQUIRE_EQUAL(testDTree.WithinRange(testQuery), true);

  testQuery << 8.5 << 2.5 << 2;

  BOOST_REQUIRE_EQUAL(testDTree.WithinRange(testQuery), false);
}

BOOST_AUTO_TEST_CASE(TestFindSplit)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  DTree<arma::mat> testDTree(testData);

  size_t obDim;
  double obLeftError, obRightError, obSplit;

  size_t trueDim = 2;
  double trueSplit = 5.5;
  double trueLeftError = 2 * log(2.0 / 5.0) - (log(7.0) + log(4.0) + log(4.5));
  double trueRightError = 2 * log(3.0 / 5.0) - (log(7.0) + log(4.0) + log(2.5));

  testDTree.logVolume = log(7.0) + log(4.0) + log(7.0);
  BOOST_REQUIRE(testDTree.FindSplit(
      testData, obDim, obSplit, obLeftError, obRightError, 1));

  BOOST_REQUIRE(trueDim == obDim);
  BOOST_REQUIRE_CLOSE(trueSplit, obSplit, 1e-10);

  BOOST_REQUIRE_CLOSE(trueLeftError, obLeftError, 1e-10);
  BOOST_REQUIRE_CLOSE(trueRightError, obRightError, 1e-10);
}

BOOST_AUTO_TEST_CASE(TestSplitData)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  DTree<arma::mat> testDTree(testData);

  arma::Col<size_t> oTest(5);
  oTest << 1 << 2 << 3 << 4 << 5;

  size_t splitDim = 2;
  double trueSplitVal = 5.5;

  size_t splitInd = testDTree.SplitData(
      testData, splitDim, trueSplitVal, oTest);

  BOOST_REQUIRE_EQUAL(splitInd, 2); // 2 points on left side.

  BOOST_REQUIRE_EQUAL(oTest[0], 1);
  BOOST_REQUIRE_EQUAL(oTest[1], 4);
  BOOST_REQUIRE_EQUAL(oTest[2], 3);
  BOOST_REQUIRE_EQUAL(oTest[3], 2);
  BOOST_REQUIRE_EQUAL(oTest[4], 5);
}

BOOST_AUTO_TEST_CASE(TestSparseFindSplit)
{
  arma::mat realData(4, 7);

  realData << .0 << 4 << 5 << 7 << 0 << 5 << 0 << arma::endr
           << .0 << 5 << 0 << 0 << 1 << 7 << 1 << arma::endr
           << .0 << 5 << 6 << 7 << 1 << 0 << 8 << arma::endr
           << -1 << 2 << 5 << 0 << 0 << 0 << 0 << arma::endr;

  arma::sp_mat testData(realData);

  DTree<arma::sp_mat> testDTree(testData);

  size_t obDim;
  double obLeftError, obRightError, obSplit;

  size_t trueDim = 1;
  double trueSplit = .5;
  double trueLeftError = 2 * log(3.0 / 7.0) -
      (log(7.0) + log(0.5) + log(8.0) + log(6.0));
  double trueRightError = 2 * log(4.0 / 7.0) -
      (log(7.0) + log(6.5) + log(8.0) + log(6.0));

  testDTree.logVolume = log(7.0) + log(7.0) + log(8.0) + log(6.0);
  BOOST_REQUIRE(testDTree.FindSplit(
      testData, obDim, obSplit, obLeftError, obRightError, 1));

  BOOST_REQUIRE(trueDim == obDim);
  BOOST_REQUIRE_CLOSE(trueSplit, obSplit, 1e-10);

  BOOST_REQUIRE_CLOSE(trueLeftError, obLeftError, 1e-10);
  BOOST_REQUIRE_CLOSE(trueRightError, obRightError, 1e-10);
}

BOOST_AUTO_TEST_CASE(TestSparseSplitData)
{
  arma::mat realData(4, 7);

  realData << .0 << 4 << 5 << 7 << 0 << 5 << 0 << arma::endr
           << .0 << 5 << 0 << 0 << 1 << 7 << 1 << arma::endr
           << .0 << 5 << 6 << 7 << 1 << 0 << 8 << arma::endr
           << -1 << 2 << 5 << 0 << 0 << 0 << 0 << arma::endr;

  arma::sp_mat testData(realData);

  DTree<arma::sp_mat> testDTree(testData);

  arma::Col<size_t> oTest(7);
  oTest << 1 << 2 << 3 << 4 << 5 << 6 << 7;

  size_t splitDim = 1;
  double trueSplitVal = .5;

  size_t splitInd = testDTree.SplitData(
      testData, splitDim, trueSplitVal, oTest);

  BOOST_REQUIRE_EQUAL(splitInd, 3); // 2 points on left side.

  BOOST_REQUIRE_EQUAL(oTest[0], 1);
  BOOST_REQUIRE_EQUAL(oTest[1], 4);
  BOOST_REQUIRE_EQUAL(oTest[2], 3);
  BOOST_REQUIRE_EQUAL(oTest[3], 2);
  BOOST_REQUIRE_EQUAL(oTest[4], 5);
  BOOST_REQUIRE_EQUAL(oTest[5], 6);
  BOOST_REQUIRE_EQUAL(oTest[6], 7);
}

#endif

// Tests for the public functions.

BOOST_AUTO_TEST_CASE(TestGrow)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  double rootError, lError, rError, rlError, rrError;

  rootError = -log(4.0) - log(7.0) - log(7.0);

  lError = 2 * log(2.0 / 5.0) - (log(7.0) + log(4.0) + log(4.5));
  rError =  2 * log(3.0 / 5.0) - (log(7.0) + log(4.0) + log(2.5));

  rlError = 2 * log(1.0 / 5.0) - (log(0.5) + log(4.0) + log(2.5));
  rrError = 2 * log(2.0 / 5.0) - (log(6.5) + log(4.0) + log(2.5));

  DTree<arma::mat> testDTree(testData);
  double alpha = testDTree.Grow(testData, oTest, false, 2, 1);

  BOOST_REQUIRE_EQUAL(oTest[0], 0);
  BOOST_REQUIRE_EQUAL(oTest[1], 3);
  BOOST_REQUIRE_EQUAL(oTest[2], 1);
  BOOST_REQUIRE_EQUAL(oTest[3], 2);
  BOOST_REQUIRE_EQUAL(oTest[4], 4);

  // Test the structure of the tree.
  BOOST_REQUIRE(testDTree.Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree.Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree.Right()->Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree.Right()->Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree.Right()->Right()->Left() == NULL);
  BOOST_REQUIRE(testDTree.Right()->Right()->Right() == NULL);

  BOOST_REQUIRE(testDTree.SubtreeLeaves() == 3);

  BOOST_REQUIRE(testDTree.SplitDim() == 2);
  BOOST_REQUIRE_CLOSE(testDTree.SplitValue(), 5.5, 1e-5);
  BOOST_REQUIRE(testDTree.Right()->SplitDim() == 1);
  BOOST_REQUIRE_CLOSE(testDTree.Right()->SplitValue(), 0.5, 1e-5);

  // Test node errors for every node (these are private functions).
#ifndef _WIN32
  BOOST_REQUIRE_CLOSE(testDTree.logNegError, rootError, 1e-10);
  BOOST_REQUIRE_CLOSE(testDTree.Left()->logNegError, lError, 1e-10);
  BOOST_REQUIRE_CLOSE(testDTree.Right()->logNegError, rError, 1e-10);
  BOOST_REQUIRE_CLOSE(testDTree.Right()->Left()->logNegError, rlError, 1e-10);
  BOOST_REQUIRE_CLOSE(testDTree.Right()->Right()->logNegError, rrError, 1e-10);
#endif

  // Test alpha.
  double rootAlpha, rAlpha;
  rootAlpha = std::log(-((std::exp(rootError) - (std::exp(lError) +
      std::exp(rlError) + std::exp(rrError))) / 2));
  rAlpha = std::log(-(std::exp(rError) - (std::exp(rlError) +
      std::exp(rrError))));

  BOOST_REQUIRE_CLOSE(alpha, min(rootAlpha, rAlpha), 1e-10);
}

BOOST_AUTO_TEST_CASE(TestPruneAndUpdate)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;
  DTree<arma::mat> testDTree(testData);
  double alpha = testDTree.Grow(testData, oTest, false, 2, 1);
  alpha = testDTree.PruneAndUpdate(alpha, testData.n_cols, false);

  BOOST_REQUIRE_CLOSE(alpha, numeric_limits<double>::max(), 1e-10);
  BOOST_REQUIRE(testDTree.SubtreeLeaves() == 1);

  double rootError = -log(4.0) - log(7.0) - log(7.0);

  BOOST_REQUIRE_CLOSE(testDTree.LogNegError(), rootError, 1e-10);
  BOOST_REQUIRE_CLOSE(testDTree.SubtreeLeavesLogNegError(), rootError, 1e-10);
  BOOST_REQUIRE(testDTree.Left() == NULL);
  BOOST_REQUIRE(testDTree.Right() == NULL);
}

BOOST_AUTO_TEST_CASE(TestComputeValue)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  arma::vec q1(3), q2(3), q3(3), q4(3);

  q1 << 4 << 2 << 2;
  q2 << 5 << 0.25 << 6;
  q3 << 5 << 3 << 7;
  q4 << 2 << 3 << 3;

  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::mat> testDTree(testData);
  double alpha = testDTree.Grow(testData, oTest, false, 2, 1);

  double d1 = (2.0 / 5.0) / exp(log(4.0) + log(7.0) + log(4.5));
  double d2 = (1.0 / 5.0) / exp(log(4.0) + log(0.5) + log(2.5));
  double d3 = (2.0 / 5.0) / exp(log(4.0) + log(6.5) + log(2.5));

  BOOST_REQUIRE_CLOSE(d1, testDTree.ComputeValue(q1), 1e-10);
  BOOST_REQUIRE_CLOSE(d2, testDTree.ComputeValue(q2), 1e-10);
  BOOST_REQUIRE_CLOSE(d3, testDTree.ComputeValue(q3), 1e-10);
  BOOST_REQUIRE_CLOSE(0.0, testDTree.ComputeValue(q4), 1e-10);

  alpha = testDTree.PruneAndUpdate(alpha, testData.n_cols, false);

  double d = 1.0 / exp(log(4.0) + log(7.0) + log(7.0));

  BOOST_REQUIRE_CLOSE(d, testDTree.ComputeValue(q1), 1e-10);
  BOOST_REQUIRE_CLOSE(d, testDTree.ComputeValue(q2), 1e-10);
  BOOST_REQUIRE_CLOSE(d, testDTree.ComputeValue(q3), 1e-10);
  BOOST_REQUIRE_CLOSE(0.0, testDTree.ComputeValue(q4), 1e-10);
}

BOOST_AUTO_TEST_CASE(TestVariableImportance)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  double rootError, lError, rError, rlError, rrError;

  rootError = -1.0 * exp(-log(4.0) - log(7.0) - log(7.0));

  lError = -1.0 * exp(2 * log(2.0 / 5.0) - (log(7.0) + log(4.0) + log(4.5)));
  rError =  -1.0 * exp(2 * log(3.0 / 5.0) - (log(7.0) + log(4.0) + log(2.5)));

  rlError = -1.0 * exp(2 * log(1.0 / 5.0) - (log(0.5) + log(4.0) + log(2.5)));
  rrError = -1.0 * exp(2 * log(2.0 / 5.0) - (log(6.5) + log(4.0) + log(2.5)));

  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::mat> testDTree(testData);
  testDTree.Grow(testData, oTest, false, 2, 1);

  arma::vec imps;

  testDTree.ComputeVariableImportance(imps);

  BOOST_REQUIRE_CLOSE((double) 0.0, imps[0], 1e-10);
  BOOST_REQUIRE_CLOSE((double) (rError - (rlError + rrError)), imps[1], 1e-10);
  BOOST_REQUIRE_CLOSE((double) (rootError - (lError + rError)), imps[2], 1e-10);
}

BOOST_AUTO_TEST_CASE(TestSparsePruneAndUpdate)
{
  arma::mat realData(3, 5);

  realData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  arma::sp_mat testData(realData);

  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::sp_mat> testDTree(testData);
  double alpha = testDTree.Grow(testData, oTest, false, 2, 1);
  alpha = testDTree.PruneAndUpdate(alpha, testData.n_cols, false);

  BOOST_REQUIRE_CLOSE(alpha, numeric_limits<double>::max(), 1e-10);
  BOOST_REQUIRE(testDTree.SubtreeLeaves() == 1);

  double rootError = -log(4.0) - log(7.0) - log(7.0);

  BOOST_REQUIRE_CLOSE(testDTree.LogNegError(), rootError, 1e-10);
  BOOST_REQUIRE_CLOSE(testDTree.SubtreeLeavesLogNegError(), rootError, 1e-10);
  BOOST_REQUIRE(testDTree.Left() == NULL);
  BOOST_REQUIRE(testDTree.Right() == NULL);
}

BOOST_AUTO_TEST_CASE(TestSparseComputeValue)
{
  arma::mat realData(3, 5);

  realData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  arma::vec q1d(3), q2d(3), q3d(3), q4d(3);

  q1d << 4 << 2 << 2;
  q2d << 5 << 0.25 << 6;
  q3d << 5 << 3 << 7;
  q4d << 2 << 3 << 3;

  arma::sp_mat testData(realData);
  arma::sp_vec q1(q1d), q2(q2d), q3(q3d), q4(q4d);

  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::sp_mat> testDTree(testData);
  double alpha = testDTree.Grow(testData, oTest, false, 2, 1);

  double d1 = (2.0 / 5.0) / exp(log(4.0) + log(7.0) + log(4.5));
  double d2 = (1.0 / 5.0) / exp(log(4.0) + log(0.5) + log(2.5));
  double d3 = (2.0 / 5.0) / exp(log(4.0) + log(6.5) + log(2.5));

  BOOST_REQUIRE_CLOSE(d1, testDTree.ComputeValue(q1), 1e-10);
  BOOST_REQUIRE_CLOSE(d2, testDTree.ComputeValue(q2), 1e-10);
  BOOST_REQUIRE_CLOSE(d3, testDTree.ComputeValue(q3), 1e-10);
  BOOST_REQUIRE_CLOSE(0.0, testDTree.ComputeValue(q4), 1e-10);

  alpha = testDTree.PruneAndUpdate(alpha, testData.n_cols, false);

  double d = 1.0 / exp(log(4.0) + log(7.0) + log(7.0));

  BOOST_REQUIRE_CLOSE(d, testDTree.ComputeValue(q1), 1e-10);
  BOOST_REQUIRE_CLOSE(d, testDTree.ComputeValue(q2), 1e-10);
  BOOST_REQUIRE_CLOSE(d, testDTree.ComputeValue(q3), 1e-10);
  BOOST_REQUIRE_CLOSE(0.0, testDTree.ComputeValue(q4), 1e-10);
}

/**
 * These are not yet implemented.
 *
BOOST_AUTO_TEST_CASE(TestTagTree)
{
  MatType testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
            << 5 << 0 << 1 << 7 << 1 << arma::endr
            << 5 << 6 << 7 << 1 << 8 << arma::endr;

  DTree<>* testDTree = new DTree<>(&testData);

  delete testDTree;
}

BOOST_AUTO_TEST_CASE(TestFindBucket)
{
  MatType testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
            << 5 << 0 << 1 << 7 << 1 << arma::endr
            << 5 << 6 << 7 << 1 << 8 << arma::endr;

  DTree<>* testDTree = new DTree<>(&testData);

  delete testDTree;
}

// Test functions in dt_utils.hpp

BOOST_AUTO_TEST_CASE(TestTrainer)
{

}

BOOST_AUTO_TEST_CASE(TestPrintVariableImportance)
{

}

BOOST_AUTO_TEST_CASE(TestPrintLeafMembership)
{

}
*/

// Test the copy constructor and the copy operator.
BOOST_AUTO_TEST_CASE(CopyConstructorAndOperatorTest)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  // Construct another DTree for testing the children.
  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::mat> *testDTree = new DTree<arma::mat>(testData);
  testDTree->Grow(testData, oTest, false, 2, 1);

  DTree<arma::mat> testDTree2(*testDTree);
  DTree<arma::mat> testDTree3 = *testDTree;

  double maxVals0 = testDTree->MaxVals()[0];
  double maxVals1 = testDTree->MaxVals()[1];
  double maxVals2 = testDTree->MaxVals()[2];
  double minVals0 = testDTree->MinVals()[0];
  double minVals1 = testDTree->MinVals()[1];
  double minVals2 = testDTree->MinVals()[2];

  double maxValsL0 = testDTree->Left()->MaxVals()[0];
  double maxValsL1 = testDTree->Left()->MaxVals()[1];
  double maxValsL2 = testDTree->Left()->MaxVals()[2];
  double minValsL0 = testDTree->Left()->MinVals()[0];
  double minValsL1 = testDTree->Left()->MinVals()[1];
  double minValsL2 = testDTree->Left()->MinVals()[2];

  double maxValsR0 = testDTree->Right()->MaxVals()[0];
  double maxValsR1 = testDTree->Right()->MaxVals()[1];
  double maxValsR2 = testDTree->Right()->MaxVals()[2];
  double minValsR0 = testDTree->Right()->MinVals()[0];
  double minValsR1 = testDTree->Right()->MinVals()[1];
  double minValsR2 = testDTree->Right()->MinVals()[2];

  // Delete the original tree.
  delete testDTree;

  // Test the data of copied tree (using copy constructor).
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[0], maxVals0);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[0], minVals0);
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[1], maxVals1);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[1], minVals1);
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[2], maxVals2);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[2], minVals2);

  // Test the data of the copied tree (using the copy operator).
  BOOST_REQUIRE_EQUAL(testDTree3.MaxVals()[0], maxVals0);
  BOOST_REQUIRE_EQUAL(testDTree3.MinVals()[0], minVals0);
  BOOST_REQUIRE_EQUAL(testDTree3.MaxVals()[1], maxVals1);
  BOOST_REQUIRE_EQUAL(testDTree3.MinVals()[1], minVals1);
  BOOST_REQUIRE_EQUAL(testDTree3.MaxVals()[2], maxVals2);
  BOOST_REQUIRE_EQUAL(testDTree3.MinVals()[2], minVals2);

  // Test the structure of the tree copied using the copy constructor.
  BOOST_REQUIRE(testDTree2.Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Right()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Right()->Right() == NULL);

  // Test the structure of the tree copied using the copy operator.
  BOOST_REQUIRE(testDTree3.Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree3.Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree3.Right()->Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree3.Right()->Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree3.Right()->Right()->Left() == NULL);
  BOOST_REQUIRE(testDTree3.Right()->Right()->Right() == NULL);

  // Test the data of the tree copied using the copy constructor.
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[0], maxValsL0);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[1], maxValsL1);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[2], maxValsL2);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[0], minValsL0);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[1], minValsL1);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[2], minValsL2);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[0], maxValsR0);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[1], maxValsR1);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[2], maxValsR2);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[0], minValsR0);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[1], minValsR1);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[2], minValsR2);
  BOOST_REQUIRE(testDTree2.SplitDim() == 2);
  BOOST_REQUIRE_CLOSE(testDTree2.SplitValue(), 5.5, 1e-5);
  BOOST_REQUIRE(testDTree2.Right()->SplitDim() == 1);
  BOOST_REQUIRE_CLOSE(testDTree2.Right()->SplitValue(), 0.5, 1e-5);

  // Test the data of the tree copied using the copy operator.
  BOOST_REQUIRE_EQUAL(testDTree3.Left()->MaxVals()[0], maxValsL0);
  BOOST_REQUIRE_EQUAL(testDTree3.Left()->MaxVals()[1], maxValsL1);
  BOOST_REQUIRE_EQUAL(testDTree3.Left()->MaxVals()[2], maxValsL2);
  BOOST_REQUIRE_EQUAL(testDTree3.Left()->MinVals()[0], minValsL0);
  BOOST_REQUIRE_EQUAL(testDTree3.Left()->MinVals()[1], minValsL1);
  BOOST_REQUIRE_EQUAL(testDTree3.Left()->MinVals()[2], minValsL2);
  BOOST_REQUIRE_EQUAL(testDTree3.Right()->MaxVals()[0], maxValsR0);
  BOOST_REQUIRE_EQUAL(testDTree3.Right()->MaxVals()[1], maxValsR1);
  BOOST_REQUIRE_EQUAL(testDTree3.Right()->MaxVals()[2], maxValsR2);
  BOOST_REQUIRE_EQUAL(testDTree3.Right()->MinVals()[0], minValsR0);
  BOOST_REQUIRE_EQUAL(testDTree3.Right()->MinVals()[1], minValsR1);
  BOOST_REQUIRE_EQUAL(testDTree3.Right()->MinVals()[2], minValsR2);
  BOOST_REQUIRE(testDTree3.SplitDim() == 2);
  BOOST_REQUIRE_CLOSE(testDTree3.SplitValue(), 5.5, 1e-5);
  BOOST_REQUIRE(testDTree3.Right()->SplitDim() == 1);
  BOOST_REQUIRE_CLOSE(testDTree3.Right()->SplitValue(), 0.5, 1e-5);
}

// Test the move constructor.
BOOST_AUTO_TEST_CASE(MoveConstructorTest)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  // Construct another DTree for testing the children.
  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::mat> *testDTree = new DTree<arma::mat>(testData);
  testDTree->Grow(testData, oTest, false, 2, 1);

  double maxVals0 = testDTree->MaxVals()[0];
  double maxVals1 = testDTree->MaxVals()[1];
  double maxVals2 = testDTree->MaxVals()[2];
  double minVals0 = testDTree->MinVals()[0];
  double minVals1 = testDTree->MinVals()[1];
  double minVals2 = testDTree->MinVals()[2];

  double maxValsL0 = testDTree->Left()->MaxVals()[0];
  double maxValsL1 = testDTree->Left()->MaxVals()[1];
  double maxValsL2 = testDTree->Left()->MaxVals()[2];
  double minValsL0 = testDTree->Left()->MinVals()[0];
  double minValsL1 = testDTree->Left()->MinVals()[1];
  double minValsL2 = testDTree->Left()->MinVals()[2];

  double maxValsR0 = testDTree->Right()->MaxVals()[0];
  double maxValsR1 = testDTree->Right()->MaxVals()[1];
  double maxValsR2 = testDTree->Right()->MaxVals()[2];
  double minValsR0 = testDTree->Right()->MinVals()[0];
  double minValsR1 = testDTree->Right()->MinVals()[1];
  double minValsR2 = testDTree->Right()->MinVals()[2];

  // Construct a new tree using the move constructor.
  DTree<arma::mat> testDTree2(std::move(*testDTree));

  // Check default values of the original tree.
  BOOST_REQUIRE_EQUAL(testDTree->LogNegError(), -DBL_MAX);
  BOOST_REQUIRE(testDTree->Left() == (DTree<arma::mat>*) NULL);
  BOOST_REQUIRE(testDTree->Right() == (DTree<arma::mat>*) NULL);

  // Delete the original tree.
  delete testDTree;

  // Test the data of the moved tree.
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[0], maxVals0);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[0], minVals0);
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[1], maxVals1);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[1], minVals1);
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[2], maxVals2);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[2], minVals2);

  // Test the structure of the moved tree.
  BOOST_REQUIRE(testDTree2.Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Right()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Right()->Right() == NULL);

  // Test the data of the moved tree.
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[0], maxValsL0);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[1], maxValsL1);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[2], maxValsL2);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[0], minValsL0);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[1], minValsL1);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[2], minValsL2);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[0], maxValsR0);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[1], maxValsR1);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[2], maxValsR2);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[0], minValsR0);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[1], minValsR1);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[2], minValsR2);
  BOOST_REQUIRE(testDTree2.SplitDim() == 2);
  BOOST_REQUIRE_CLOSE(testDTree2.SplitValue(), 5.5, 1e-5);
  BOOST_REQUIRE(testDTree2.Right()->SplitDim() == 1);
  BOOST_REQUIRE_CLOSE(testDTree2.Right()->SplitValue(), 0.5, 1e-5);
}

// Test the move operator.
BOOST_AUTO_TEST_CASE(MoveOperatorTest)
{
  arma::mat testData(3, 5);

  testData << 4 << 5 << 7 << 3 << 5 << arma::endr
           << 5 << 0 << 1 << 7 << 1 << arma::endr
           << 5 << 6 << 7 << 1 << 8 << arma::endr;

  // Construct another DTree for testing the children.
  arma::Col<size_t> oTest(5);
  oTest << 0 << 1 << 2 << 3 << 4;

  DTree<arma::mat> *testDTree = new DTree<arma::mat>(testData);
  testDTree->Grow(testData, oTest, false, 2, 1);

  double maxVals0 = testDTree->MaxVals()[0];
  double maxVals1 = testDTree->MaxVals()[1];
  double maxVals2 = testDTree->MaxVals()[2];
  double minVals0 = testDTree->MinVals()[0];
  double minVals1 = testDTree->MinVals()[1];
  double minVals2 = testDTree->MinVals()[2];

  double maxValsL0 = testDTree->Left()->MaxVals()[0];
  double maxValsL1 = testDTree->Left()->MaxVals()[1];
  double maxValsL2 = testDTree->Left()->MaxVals()[2];
  double minValsL0 = testDTree->Left()->MinVals()[0];
  double minValsL1 = testDTree->Left()->MinVals()[1];
  double minValsL2 = testDTree->Left()->MinVals()[2];

  double maxValsR0 = testDTree->Right()->MaxVals()[0];
  double maxValsR1 = testDTree->Right()->MaxVals()[1];
  double maxValsR2 = testDTree->Right()->MaxVals()[2];
  double minValsR0 = testDTree->Right()->MinVals()[0];
  double minValsR1 = testDTree->Right()->MinVals()[1];
  double minValsR2 = testDTree->Right()->MinVals()[2];

  // Construct a new tree using the move constructor.
  DTree<arma::mat> testDTree2 = std::move(*testDTree);

  // Check default values of the original tree.
  BOOST_REQUIRE_EQUAL(testDTree->LogNegError(), -DBL_MAX);
  BOOST_REQUIRE(testDTree->Left() == (DTree<arma::mat>*) NULL);
  BOOST_REQUIRE(testDTree->Right() == (DTree<arma::mat>*) NULL);

  // Delete the original tree.
  delete testDTree;

  // Test the data of the moved tree.
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[0], maxVals0);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[0], minVals0);
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[1], maxVals1);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[1], minVals1);
  BOOST_REQUIRE_EQUAL(testDTree2.MaxVals()[2], maxVals2);
  BOOST_REQUIRE_EQUAL(testDTree2.MinVals()[2], minVals2);

  // Test the structure of the moved tree.
  BOOST_REQUIRE(testDTree2.Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Left()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Left()->Right() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Right()->Left() == NULL);
  BOOST_REQUIRE(testDTree2.Right()->Right()->Right() == NULL);

  // Test the data of moved tree.
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[0], maxValsL0);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[1], maxValsL1);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MaxVals()[2], maxValsL2);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[0], minValsL0);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[1], minValsL1);
  BOOST_REQUIRE_EQUAL(testDTree2.Left()->MinVals()[2], minValsL2);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[0], maxValsR0);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[1], maxValsR1);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MaxVals()[2], maxValsR2);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[0], minValsR0);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[1], minValsR1);
  BOOST_REQUIRE_EQUAL(testDTree2.Right()->MinVals()[2], minValsR2);
  BOOST_REQUIRE(testDTree2.SplitDim() == 2);
  BOOST_REQUIRE_CLOSE(testDTree2.SplitValue(), 5.5, 1e-5);
  BOOST_REQUIRE(testDTree2.Right()->SplitDim() == 1);
  BOOST_REQUIRE_CLOSE(testDTree2.Right()->SplitValue(), 0.5, 1e-5);
}

BOOST_AUTO_TEST_SUITE_END();
