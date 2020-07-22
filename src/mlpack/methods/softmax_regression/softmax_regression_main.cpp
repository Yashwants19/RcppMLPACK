/**
 * @file methods/softmax_regression/softmax_regression_main.cpp
 *
 * Main program for softmax regression.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/io.hpp>
#include <mlpack/core/util/mlpack_main.hpp>

#include <mlpack/methods/softmax_regression/softmax_regression.hpp>
#include <ensmallen.hpp>

#include <memory>
#include <set>

using namespace std;
using namespace mlpack;
using namespace mlpack::regression;
using namespace mlpack::util;

// Define parameters for the executable.
PROGRAM_INFO("Softmax Regression",
    // Short description.
    "An implementation of softmax regression for classification, which is a "
    "multiclass generalization of logistic regression.  Given labeled data, a "
    "softmax regression model can be trained and saved for future use, or, a "
    "pre-trained softmax regression model can be used for classification of "
    "new points.",
    // Long description.
    "This program performs softmax regression, a generalization of logistic "
    "regression to the multiclass case, and has support for L2 regularization. "
    " The program is able to train a model, load  an existing model, and give "
    "predictions (and optionally their accuracy) for test data."
    "\n\n"
    "Training a softmax regression model is done by giving a file of training "
    "points with the " + PRINT_PARAM_STRING("training") + " parameter and their"
    " corresponding labels with the " + PRINT_PARAM_STRING("labels") +
    " parameter. The number of classes can be manually specified with the " +
    PRINT_PARAM_STRING("number_of_classes") + " parameter, and the maximum " +
    "number of iterations of the L-BFGS optimizer can be specified with the " +
    PRINT_PARAM_STRING("max_iterations") + " parameter.  The L2 regularization "
    "constant can be specified with the " + PRINT_PARAM_STRING("lambda") +
    " parameter and if an intercept term is not desired in the model, the " +
    PRINT_PARAM_STRING("no_intercept") + " parameter can be specified."
    "\n\n"
    "The trained model can be saved with the " +
    PRINT_PARAM_STRING("output_model") + " output parameter. If training is not"
    " desired, but only testing is, a model can be loaded with the " +
    PRINT_PARAM_STRING("input_model") + " parameter.  At the current time, a "
    "loaded model cannot be trained further, so specifying both " +
    PRINT_PARAM_STRING("input_model") + " and " +
    PRINT_PARAM_STRING("training") + " is not allowed."
    "\n\n"
    "The program is also able to evaluate a model on test data.  A test dataset"
    " can be specified with the " + PRINT_PARAM_STRING("test") + " parameter. "
    "Class predictions can be saved with the " +
    PRINT_PARAM_STRING("predictions") + " output parameter.  If labels are "
    "specified for the test data with the " +
    PRINT_PARAM_STRING("test_labels") + " parameter, then the program will "
    "print the accuracy of the predictions on the given test set and its "
    "corresponding labels.",
    // Example.
    "For example, to train a softmax regression model on the data " +
    PRINT_DATASET("dataset") + " with labels " + PRINT_DATASET("labels") +
    " with a maximum of 1000 iterations for training, saving the trained model "
    "to " + PRINT_MODEL("sr_model") + ", the following command can be used: "
    "\n\n" +
    PRINT_CALL("softmax_regression", "training", "dataset", "labels", "labels",
        "output_model", "sr_model") +
    "\n\n"
    "Then, to use " + PRINT_MODEL("sr_model") + " to classify the test points "
    "in " + PRINT_DATASET("test_points") + ", saving the output predictions to"
    " " + PRINT_DATASET("predictions") + ", the following command can be used:"
    "\n\n" +
    PRINT_CALL("softmax_regression", "input_model", "sr_model", "test",
        "test_points", "predictions", "predictions"),
    SEE_ALSO("@logistic_regression", "#logistic_regression"),
    SEE_ALSO("@random_forest", "#random_forest"),
    SEE_ALSO("Multinomial logistic regression (softmax regression) on "
        "Wikipedia",
        "https://en.wikipedia.org/wiki/Multinomial_logistic_regression"),
    SEE_ALSO("mlpack::regression::SoftmaxRegression C++ class documentation",
        "@doxygen/classmlpack_1_1regression_1_1SoftmaxRegression.html"));

// Required options.
PARAM_MATRIX_IN("training", "A matrix containing the training set (the matrix "
    "of predictors, X).", "t");
PARAM_UROW_IN("labels", "A matrix containing labels (0 or 1) for the points "
    "in the training set (y). The labels must order as a row.", "l");

// Model loading/saving.
PARAM_MODEL_IN(SoftmaxRegression, "input_model", "File containing existing "
    "model (parameters).", "m");
PARAM_MODEL_OUT(SoftmaxRegression, "output_model", "File to save trained "
    "softmax regression model to.", "M");

// Testing.
PARAM_MATRIX_IN("test", "Matrix containing test dataset.", "T");
PARAM_UROW_OUT("predictions", "Matrix to save predictions for test dataset "
    "into.", "p");
PARAM_UROW_IN("test_labels", "Matrix containing test labels.", "L");

// Softmax configuration options.
PARAM_INT_IN("max_iterations", "Maximum number of iterations before "
    "termination.", "n", 400);

PARAM_INT_IN("number_of_classes", "Number of classes for classification; if "
    "unspecified (or 0), the number of classes found in the labels will be "
    "used.", "c", 0);

PARAM_DOUBLE_IN("lambda", "L2-regularization constant", "r", 0.0001);

PARAM_FLAG("no_intercept", "Do not add the intercept term to the model.", "N");

// Count the number of classes in the given labels (if numClasses == 0).
size_t CalculateNumberOfClasses(const size_t numClasses,
                                const arma::Row<size_t>& trainLabels);

// Test the accuracy of the model.
template<typename Model>
void TestClassifyAcc(const size_t numClasses, const Model& model);

// Build the softmax model given the parameters.
template<typename Model>
Model* TrainSoftmax(const size_t maxIterations);

static void mlpackMain()
{
  const int maxIterations = IO::GetParam<int>("max_iterations");

  // One of inputFile and modelFile must be specified.
  RequireOnlyOnePassed({ "input_model", "training" }, true);
  if (IO::HasParam("training"))
  {
    RequireAtLeastOnePassed({ "labels" }, true, "if training data is specified,"
        " labels must also be specified");
  }
  ReportIgnoredParam({{ "training", false }}, "labels");
  ReportIgnoredParam({{ "training", false }}, "max_iterations");
  ReportIgnoredParam({{ "training", false }}, "number_of_classes");
  ReportIgnoredParam({{ "training", false }}, "lambda");
  ReportIgnoredParam({{ "training", false }}, "no_intercept");

  RequireParamValue<int>("max_iterations", [](int x) { return x >= 0; }, true,
      "maximum number of iterations must be greater than or equal to 0");
  RequireParamValue<double>("lambda", [](double x) { return x >= 0.0; }, true,
      "lambda penalty parameter must be greater than or equal to 0");
  RequireParamValue<int>("number_of_classes", [](int x) { return x >= 0; },
                         true, "number of classes must be greater than or "
                         "equal to 0 (equal to 0 in case of unspecified.)");

  // Make sure we have an output file of some sort.
  RequireAtLeastOnePassed({ "output_model", "predictions" }, false, "no results"
      " will be saved");

  SoftmaxRegression* sm = TrainSoftmax<SoftmaxRegression>(maxIterations);

  TestClassifyAcc(sm->NumClasses(), *sm);

  IO::GetParam<SoftmaxRegression*>("output_model") = sm;
}

size_t CalculateNumberOfClasses(const size_t numClasses,
                                const arma::Row<size_t>& trainLabels)
{
  if (numClasses == 0)
  {
    const set<size_t> unique_labels(begin(trainLabels),
                                    end(trainLabels));
    return unique_labels.size();
  }
  else
  {
    return numClasses;
  }
}

template<typename Model>
void TestClassifyAcc(size_t numClasses, const Model& model)
{
  using namespace mlpack;

  // If there is no test set, there is nothing to test on.
  if (!IO::HasParam("test"))
  {
    ReportIgnoredParam({{ "test", false }}, "test_labels");
    ReportIgnoredParam({{ "test", false }}, "predictions");

    return;
  }

  // Get the test dataset, and get predictions.
  arma::mat testData = std::move(IO::GetParam<arma::mat>("test"));

  arma::Row<size_t> predictLabels;
  model.Classify(testData, predictLabels);

  // Calculate accuracy, if desired.
  if (IO::HasParam("test_labels"))
  {
    arma::Row<size_t> testLabels =
      std::move(IO::GetParam<arma::Row<size_t>>("test_labels"));

    if (testData.n_cols != testLabels.n_elem)
    {
      Log::Fatal << "Test data given with " << PRINT_PARAM_STRING("test")
          << " has " << testData.n_cols << " points, but labels in "
          << PRINT_PARAM_STRING("test_labels") << " have " << testLabels.n_elem
          << " labels!" << endl;
    }

    vector<size_t> bingoLabels(numClasses, 0);
    vector<size_t> labelSize(numClasses, 0);
    for (arma::uword i = 0; i != predictLabels.n_elem; ++i)
    {
      if (predictLabels(i) == testLabels(i))
      {
        ++bingoLabels[testLabels(i)];
      }
      ++labelSize[testLabels(i)];
    }

    size_t totalBingo = 0;
    for (size_t i = 0; i != bingoLabels.size(); ++i)
    {
      Log::Info << "Accuracy for points with label " << i << " is "
          << (bingoLabels[i] / static_cast<double>(labelSize[i])) << " ("
          << bingoLabels[i] << " of " << labelSize[i] << ")." << endl;
      totalBingo += bingoLabels[i];
    }

    Log::Info << "Total accuracy for all points is "
        << (totalBingo) / static_cast<double>(predictLabels.n_elem) << " ("
        << totalBingo << " of " << predictLabels.n_elem << ")." << endl;
  }
  // Save predictions, if desired.
  if (IO::HasParam("predictions"))
    IO::GetParam<arma::Row<size_t>>("predictions") = std::move(predictLabels);
}

template<typename Model>
Model* TrainSoftmax(const size_t maxIterations)
{
  using namespace mlpack;

  Model* sm;
  if (IO::HasParam("input_model"))
  {
    sm = IO::GetParam<Model*>("input_model");
  }
  else
  {
    arma::mat trainData = std::move(IO::GetParam<arma::mat>("training"));
    arma::Row<size_t> trainLabels =
        std::move(IO::GetParam<arma::Row<size_t>>("labels"));

    if (trainData.n_cols != trainLabels.n_elem)
      Log::Fatal << "Samples of input_data should same as the size of "
          << "input_label." << endl;

    const size_t numClasses = CalculateNumberOfClasses(
        (size_t) IO::GetParam<int>("number_of_classes"), trainLabels);

    const bool intercept = IO::HasParam("no_intercept") ? false : true;

    const size_t numBasis = 5;
    ens::L_BFGS optimizer(numBasis, maxIterations);
    sm = new Model(trainData, trainLabels, numClasses,
        IO::GetParam<double>("lambda"), intercept, std::move(optimizer));
  }
  return sm;
}
