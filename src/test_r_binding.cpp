#include <RcppMLPACK.h>
#include <mlpack/bindings/R/test_r_binding_main.cpp>

using namespace mlpack;
using namespace mlpack::util;
using namespace std;
using namespace Rcpp;

typedef Rcpp::XPtr<GaussianKernel> XPtrGaussianKernel;

// [[Rcpp::export]]
SEXP test_r_binding_mlpackMain()
{
  mlpackMain();
}

// [[Rcpp::export]]
SEXP CLI_GetParamGaussianKernelPtr(SEXP paramName)
{
  return Rcpp::wrap((XPtrGaussianKernel) (CLI::GetParam<GaussianKernel*>(Rcpp::as<std::string>(paramName))));
}

// [[Rcpp::export]]
SEXP CLI_SetParamGaussianKernelPtr(SEXP paramName, SEXP ptr)
{
  CLI::GetParam<GaussianKernel*>(Rcpp::as<std::string>(paramName)) = Rcpp::as<XPtrGaussianKernel>(ptr);
  CLI::SetPassed(Rcpp::as<std::string>(paramName));
}
