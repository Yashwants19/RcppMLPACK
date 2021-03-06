/**
 * @file src/sparse_coding.cpp
 *
 * This is an autogenerated file containing implementations of C functions to be
 * called by the R sparse_coding binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/sparse_coding/sparse_coding_main.cpp>

// [[Rcpp::export]]
void sparse_coding_mlpackMain()
{
  mlpackMain();
}

// Any implementations of methods for dealing with model pointers will be put
// below this comment, if needed.

// Get the pointer to a SparseCoding parameter.
// [[Rcpp::export]]
SEXP IO_GetParamSparseCodingPtr(const std::string& paramName)
{
  return std::move((Rcpp::XPtr<SparseCoding>) IO::GetParam<SparseCoding*>(paramName));
}

// Set the pointer to a SparseCoding parameter.
// [[Rcpp::export]]
void IO_SetParamSparseCodingPtr(const std::string& paramName, SEXP ptr)
{
  IO::GetParam<SparseCoding*>(paramName) =  Rcpp::as<Rcpp::XPtr<SparseCoding>>(ptr);
  IO::SetPassed(paramName);
}

// Serialize a SparseCoding pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeSparseCodingPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    boost::archive::binary_oarchive oa(oss);
    oa << boost::serialization::make_nvp("SparseCoding",
          *Rcpp::as<Rcpp::XPtr<SparseCoding>>(ptr));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "SparseCoding";
  return raw_vec;
}

// Deserialize a SparseCoding pointer.
// [[Rcpp::export]]
SEXP DeserializeSparseCodingPtr(Rcpp::RawVector str)
{
  SparseCoding* ptr = new SparseCoding();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    boost::archive::binary_iarchive ia(iss);
    ia >> boost::serialization::make_nvp("SparseCoding", *ptr);
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<SparseCoding>)ptr);
}


