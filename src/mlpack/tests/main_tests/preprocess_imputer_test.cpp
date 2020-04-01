/**
 * @file preprocess_imputer_test.cpp
 * @author Manish Kumar
 *
 * Test mlpackMain() of preprocess_imputer_main.cpp.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#define BINDING_TYPE BINDING_TYPE_TEST

#include <mlpack/core.hpp>
static const std::string testName = "PreprocessImputer";

#include <mlpack/core/util/mlpack_main.hpp>
#include <mlpack/methods/preprocess/preprocess_imputer_main.cpp>

#include "test_helper.hpp"
#include <boost/test/unit_test.hpp>
#include "../test_tools.hpp"

#include <cmath>

using namespace mlpack;

struct PreprocessImputerTestFixture
{
 public:
  PreprocessImputerTestFixture()
  {
    // Cache in the options for this program.
    CLI::RestoreSettings(testName);
  }

  ~PreprocessImputerTestFixture()
  {
    // Clear the settings.
    bindings::tests::CleanMemory();
    CLI::ClearSettings();
  }
};

BOOST_FIXTURE_TEST_SUITE(PreprocessImputerMainTest,
                         PreprocessImputerTestFixture);

/**
 * Check that input and output have same dimensions
 * except for listwise_deletion strategy.
 */
BOOST_AUTO_TEST_CASE(PreprocessImputerDimensionTest)
{
  // Load synthetic dataset.
  arma::mat inputData;
  data::Load("preprocess_imputer_test.csv", inputData);

  arma::mat outputData;

  // Store size of input dataset.
  size_t inputSize = inputData.n_cols;

  // Input custom data points and labels.
  SetInputParam("input_file", (std::string) "preprocess_imputer_test.csv");
  SetInputParam("missing_value", (std::string) "nan");
  SetInputParam("output_file",
      (std::string) "preprocess_imputer_output_test.csv");

  // Check for mean strategy.
  SetInputParam("strategy", (std::string) "mean");

  mlpackMain();

  // Now check that the output has desired dimensions.
  data::Load(CLI::GetParam<std::string>("output_file"), outputData);
  BOOST_REQUIRE_EQUAL(outputData.n_cols, inputSize);
  BOOST_REQUIRE_EQUAL(outputData.n_rows, 3); // Input Dimension.

  // Reset passed strategy.
  CLI::GetSingleton().Parameters()["strategy"].wasPassed = false;

  // Check for median strategy.
  SetInputParam("strategy", (std::string) "median");

  mlpackMain();

  // Now check that the output has desired dimensions.
  data::Load(CLI::GetParam<std::string>("output_file"), outputData);
  BOOST_REQUIRE_EQUAL(outputData.n_cols, inputSize);
  BOOST_REQUIRE_EQUAL(outputData.n_rows, 3); // Input Dimension.

  // Reset passed strategy.
  CLI::GetSingleton().Parameters()["strategy"].wasPassed = false;

  // Check for custom strategy.
  SetInputParam("strategy", (std::string) "custom");
  SetInputParam("custom_value", (double) 75.12);

  mlpackMain();

  // Now check that the output has desired dimensions.
  data::Load(CLI::GetParam<std::string>("output_file"), outputData);
  BOOST_REQUIRE_EQUAL(outputData.n_cols, inputSize);
  BOOST_REQUIRE_EQUAL(outputData.n_rows, 3); // Input Dimension.
}

/**
 * Check that output has fewer points in case of listwise_deletion strategy.
 */
BOOST_AUTO_TEST_CASE(PreprocessImputerListwiseDimensionTest)
{
  // Load synthetic dataset.
  arma::mat inputData;
  data::Load("preprocess_imputer_test.csv", inputData);

  // Store size of input dataset.
  size_t inputSize  = inputData.n_cols;
  size_t countNaN = 0;

  // Count number of unavailable entries in all dimensions.
  for (size_t i = 0; i < inputSize; i++)
  {
    if (std::to_string(inputData(0, i)) == "nan" ||
        std::to_string(inputData(1, i)) == "nan" ||
        std::to_string(inputData(2, i)) == "nan")
    {
      countNaN++;
    }
  }

  // Input custom data points and labels.
  SetInputParam("input_file", (std::string) "preprocess_imputer_test.csv");
  SetInputParam("missing_value", (std::string) "nan");
  SetInputParam("strategy", (std::string) "listwise_deletion");
  SetInputParam("output_file",
      (std::string) "preprocess_imputer_output_test.csv");

  mlpackMain();

  // Now check that the output has desired dimensions.
  arma::mat outputData;
  data::Load(CLI::GetParam<std::string>("output_file"), outputData);
  BOOST_REQUIRE_EQUAL(outputData.n_cols + countNaN, inputSize);
  BOOST_REQUIRE_EQUAL(outputData.n_rows, 3); // Input Dimension.
}

/**
 * Check that invalid strategy can't be specified.
 */
BOOST_AUTO_TEST_CASE(PreprocessImputerStrategyTest)
{
  // Load synthetic dataset.
  arma::mat inputData;
  data::Load("preprocess_imputer_test.csv", inputData);

  // Input custom data points and labels.
  SetInputParam("input_file", (std::string) "preprocess_imputer_test.csv");
  SetInputParam("missing_value", (std::string) "nan");
  SetInputParam("strategy", (std::string) "notmean"); // Invalid.

  Log::Fatal.ignoreInput = true;
  BOOST_REQUIRE_THROW(mlpackMain(), std::runtime_error);
  Log::Fatal.ignoreInput = false;
}

BOOST_AUTO_TEST_SUITE_END();
