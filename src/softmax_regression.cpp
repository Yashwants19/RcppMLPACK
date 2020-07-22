/**
 * @file src/softmax_regression.cpp
 *
 * This is an autogenerated file containing implementations of C++ functions to
 * be called by the R softmax_regression binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/softmax_regression/softmax_regression_main.cpp>

// [[Rcpp::export]]
void softmax_regression_mlpackMain()
{
  mlpackMain();
}

// Any implementations of methods for dealing with model pointers will be put
// below this comment, if needed.

// Get the pointer to a SoftmaxRegression parameter.
// [[Rcpp::export]]
SEXP IO_GetParamSoftmaxRegressionPtr(const std::string& paramName)
{
  return std::move((Rcpp::XPtr<SoftmaxRegression>) IO::GetParam<SoftmaxRegression*>(paramName));
}

// Set the pointer to a SoftmaxRegression parameter.
// [[Rcpp::export]]
void IO_SetParamSoftmaxRegressionPtr(const std::string& paramName, SEXP ptr)
{
  IO::GetParam<SoftmaxRegression*>(paramName) =  Rcpp::as<Rcpp::XPtr<SoftmaxRegression>>(ptr);
  IO::SetPassed(paramName);
}

// Serialize a SoftmaxRegression pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeSoftmaxRegressionPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::binary_oarchive oa(oss);
    oa << boost::serialization::make_nvp("SoftmaxRegression",
          *Rcpp::as<Rcpp::XPtr<SoftmaxRegression>>(ptr));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "SoftmaxRegression";
  return raw_vec;
}

// Deserialize a SoftmaxRegression pointer.
// [[Rcpp::export]]
SEXP DeserializeSoftmaxRegressionPtr(Rcpp::RawVector str)
{
  SoftmaxRegression* ptr = new SoftmaxRegression();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    boost::archive::binary_iarchive ia(iss);
    ia >> boost::serialization::make_nvp("SoftmaxRegression", *ptr);
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<SoftmaxRegression>)ptr);
}


