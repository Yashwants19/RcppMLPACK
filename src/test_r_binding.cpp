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
  return std::move((XPtrGaussianKernel) (CLI::GetParam<GaussianKernel*>
            (Rcpp::as<std::string>(paramName))));
}

// [[Rcpp::export]]
void CLI_SetParamGaussianKernelPtr(SEXP paramName, SEXP ptr)
{
  CLI::GetParam<GaussianKernel*>(Rcpp::as<std::string>(paramName)) = 
      Rcpp::as<XPtrGaussianKernel>(ptr);
  CLI::SetPassed(Rcpp::as<std::string>(paramName));
}

// [[Rcpp::export]]
SEXP SerializeGaussianKernelToXML(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::xml_oarchive oa(oss);
    oa << boost::serialization::make_nvp("GaussianKernel",
          *Rcpp::as<XPtrGaussianKernel>(ptr));
  }
  return Rcpp::wrap(oss.str());
}

// [[Rcpp::export]]
RawVector SerializeGaussianKernelPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::binary_oarchive oa(oss);
    oa << boost::serialization::make_nvp("GaussianKernel",
          *Rcpp::as<XPtrGaussianKernel>(ptr));
  }
  Rcpp::RawVector raw_vec(oss.str().size());
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "GaussianKernel";
  return raw_vec;
}

// [[Rcpp::export]]
SEXP UnserializeGaussianKernelPtr(Rcpp::RawVector str)
{
  GaussianKernel* ptr= new GaussianKernel();
  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    boost::archive::binary_iarchive ia(iss);
    ia >> boost::serialization::make_nvp("GaussianKernel", *ptr);
  }
  return std::move((XPtrGaussianKernel)ptr);
}
