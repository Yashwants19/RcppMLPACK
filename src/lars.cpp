#include <RcppMLPACK.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/lars/lars_main.cpp>

using namespace mlpack;
using namespace mlpack::util;
using namespace std;
using namespace Rcpp;

typedef Rcpp::XPtr<LARS> XPtrLARS;

// [[Rcpp::export]]
void lars_mlpackMain()
{
  mlpackMain();
}

// [[Rcpp::export]]
SEXP CLI_GetParamLARSPtr(SEXP paramName)
{
  return Rcpp::wrap((XPtrLARS) (CLI::GetParam<LARS*>
           (Rcpp::as<std::string>(paramName))));
}

// [[Rcpp::export]]
void CLI_SetParamLARSPtr(SEXP paramName, SEXP ptr)
{
  CLI::GetParam<LARS*>(Rcpp::as<std::string>(paramName)) = 
      Rcpp::as<XPtrLARS>(ptr);
  CLI::SetPassed(Rcpp::as<std::string>(paramName));
}

// [[Rcpp::export]]
SEXP SerializeLARSToXML(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::xml_oarchive oa(oss);
    oa << boost::serialization::make_nvp("LARS", *Rcpp::as<XPtrLARS>(ptr));
  }
  return Rcpp::wrap(oss.str());
}

// [[Rcpp::export]]
SEXP SerializeLARSPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::binary_oarchive oa(oss);
    oa << boost::serialization::make_nvp("LARS", *Rcpp::as<XPtrLARS>(ptr));
  }
  Rcpp::RawVector raw_vec(oss.str().size());
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  return raw_vec;
}

// [[Rcpp::export]]
SEXP UnserializeLARSPtr(Rcpp::RawVector str)
{
  LARS* ptr= new LARS();
  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    boost::archive::binary_iarchive ia(iss);
    ia >> boost::serialization::make_nvp("LARS", *ptr);
  }
  return Rcpp::wrap((XPtrLARS) ptr);
}
