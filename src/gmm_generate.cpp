/**
 * @file src/gmm_generate.cpp
 *
 * This is an autogenerated file containing implementations of C functions to be
 * called by the R gmm_generate binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/gmm/gmm_generate_main.cpp>

// [[Rcpp::export]]
void gmm_generate_mlpackMain()
{
  mlpackMain();
}

// Any implementations of methods for dealing with model pointers will be put
// below this comment, if needed.

// Get the pointer to a GMM parameter.
// [[Rcpp::export]]
SEXP IO_GetParamGMMPtr(const std::string& paramName)
{
  return std::move((Rcpp::XPtr<GMM>) IO::GetParam<GMM*>(paramName));
}

// Set the pointer to a GMM parameter.
// [[Rcpp::export]]
void IO_SetParamGMMPtr(const std::string& paramName, SEXP ptr)
{
  IO::GetParam<GMM*>(paramName) =  Rcpp::as<Rcpp::XPtr<GMM>>(ptr);
  IO::SetPassed(paramName);
}

// Serialize a GMM pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeGMMPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::binary_oarchive oa(oss);
    oa << boost::serialization::make_nvp("GMM",
          *Rcpp::as<Rcpp::XPtr<GMM>>(ptr));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "GMM";
  return raw_vec;
}

// Deserialize a GMM pointer.
// [[Rcpp::export]]
SEXP DeserializeGMMPtr(Rcpp::RawVector str)
{
  GMM* ptr = new GMM();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    boost::archive::binary_iarchive ia(iss);
    ia >> boost::serialization::make_nvp("GMM", *ptr);
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<GMM>)ptr);
}


