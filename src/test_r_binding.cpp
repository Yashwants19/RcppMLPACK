#include <RcppMLPACK.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/bindings/R/test_r_binding_main.cpp>

using namespace mlpack;
using namespace mlpack::util;
using namespace std;
using namespace Rcpp;

typedef Rcpp::XPtr<GaussianKernel> XPtrGaussianKernel;

// [[Rcpp::export]]
void test_r_binding_mlpackMain()
{
  mlpackMain();
}

// [[Rcpp::export]]
SEXP CLI_GetParamGaussianKernelPtr(SEXP paramName)
{
  return Rcpp::wrap((XPtrGaussianKernel) (CLI::GetParam<GaussianKernel*>(Rcpp::as<std::string>(paramName))));
}

// [[Rcpp::export]]
void CLI_SetParamGaussianKernelPtr(SEXP paramName, SEXP ptr)
{
  CLI::GetParam<GaussianKernel*>(Rcpp::as<std::string>(paramName)) = Rcpp::as<XPtrGaussianKernel>(ptr);
  CLI::SetPassed(Rcpp::as<std::string>(paramName));
}

// [[Rcpp::export]]
SEXP SerializeTestRBindingToXML(SEXP paramName, SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::xml_oarchive oa(oss);
    oa << boost::serialization::make_nvp(Rcpp::as<std::string>(paramName).c_str(), *(Rcpp::as<XPtrGaussianKernel>(ptr)));
  }
  return Rcpp::wrap(oss.str());
}
