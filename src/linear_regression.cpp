/**
 * @file src/linear_regression.cpp
 *
 * This is an autogenerated file containing implementations of C++ functions to
 * be called by the R linear_regression binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/linear_regression/linear_regression_main.cpp>

// [[Rcpp::export]]
void linear_regression_mlpackMain()
{
  mlpackMain();
}

// Any implementations of methods for dealing with model pointers will be put
// below this comment, if needed.

// Get the pointer to a LinearRegression parameter.
// [[Rcpp::export]]
SEXP IO_GetParamLinearRegressionPtr(const std::string& paramName)
{
  return std::move((Rcpp::XPtr<LinearRegression>) IO::GetParam<LinearRegression*>(paramName));
}

// Set the pointer to a LinearRegression parameter.
// [[Rcpp::export]]
void IO_SetParamLinearRegressionPtr(const std::string& paramName, SEXP ptr)
{
  IO::GetParam<LinearRegression*>(paramName) =  Rcpp::as<Rcpp::XPtr<LinearRegression>>(ptr);
  IO::SetPassed(paramName);
}

// Serialize a LinearRegression pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeLinearRegressionPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::binary_oarchive oa(oss);
    oa << boost::serialization::make_nvp("LinearRegression",
          *Rcpp::as<Rcpp::XPtr<LinearRegression>>(ptr));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "LinearRegression";
  return raw_vec;
}

// Deserialize a LinearRegression pointer.
// [[Rcpp::export]]
SEXP DeserializeLinearRegressionPtr(Rcpp::RawVector str)
{
  LinearRegression* ptr = new LinearRegression();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    boost::archive::binary_iarchive ia(iss);
    ia >> boost::serialization::make_nvp("LinearRegression", *ptr);
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<LinearRegression>)ptr);
}


