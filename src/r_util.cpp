#include <RcppMLPACK.h>
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/cli.hpp>

using namespace mlpack;
using namespace Rcpp;

 // [[Rcpp::export]]
 void CLI_RestoreSettings(SEXP programName)
 {
   CLI::RestoreSettings(Rcpp::as<std::string>(programName));
 }

  // [[Rcpp::export]]
  void CLI_SetParamInt(SEXP paramName, SEXP paramValue)
  {
    CLI::GetParam<int>(Rcpp::as<std::string>(paramName)) =
      Rcpp::as<int>(paramValue);
    CLI::SetPassed(Rcpp::as<std::string>(paramName));
  }

  // [[Rcpp::export]]
  void CLI_SetParamDouble(SEXP paramName, SEXP paramValue)
  {
    CLI::GetParam<double>(Rcpp::as<std::string>(paramName)) =
      Rcpp::as<double>(paramValue);
    CLI::SetPassed(Rcpp::as<std::string>(paramName));
  }

  // [[Rcpp::export]]
  void CLI_SetParamString(SEXP paramName, SEXP paramValue)
  {
    CLI::GetParam<std::string>(Rcpp::as<std::string>(paramName)) =
      Rcpp::as<std::string>(paramValue);
    CLI::SetPassed(Rcpp::as<std::string>(paramName));
  }

  // [[Rcpp::export]]
  void CLI_SetParamBool(SEXP paramName, SEXP paramValue)
  {
    CLI::GetParam<bool>(Rcpp::as<std::string>(paramName)) =
      Rcpp::as<bool>(paramValue);
    CLI::SetPassed(Rcpp::as<std::string>(paramName));
  }

  // [[Rcpp::export]]
  void CLI_SetParamMat(SEXP paramName, SEXP paramValue)
  {
    Rcpp::NumericMatrix mat(Rcpp::clone(paramValue));
    arma::mat m(mat.begin(), mat.nrow(), mat.ncol(), false);
    CLI::GetParam<arma::mat>(Rcpp::as<std::string>(paramName)) = std::move(m);
    CLI::SetPassed(Rcpp::as<std::string>(paramName));
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamInt(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<int>(Rcpp::as<std::string>(paramName)));
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamDouble(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<double>(Rcpp::as<std::string>(paramName)));
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamString(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<std::string>(Rcpp::as<std::string>(paramName)).c_str());
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamBool(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<bool>(Rcpp::as<std::string>(paramName)));
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamMatRows(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<arma::mat>(Rcpp::as<std::string>(paramName)).n_rows);
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamMatCols(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<arma::mat>(Rcpp::as<std::string>(paramName)).n_cols);
  }

  // [[Rcpp::export]]
  SEXP CLI_GetParamMat(SEXP paramName)
  {
    return Rcpp::wrap(CLI::GetParam<arma::mat>(Rcpp::as<std::string>(paramName)));
  }

  // [[Rcpp::export()]]
  void CLI_EnableVerbose()
  {
    Log::Info.ignoreInput = false;
  }

  // [[Rcpp::export]]
  void CLI_DisableVerbose()
  {
    Log::Info.ignoreInput = true;
  }

  // [[Rcpp::export]]
  void CLI_ResetTimers()
  {
    CLI::GetSingleton().timer.Reset();
  }

  // [[Rcpp::export]]
  void CLI_SetPassed(SEXP paramName)
  {
    CLI::SetPassed(Rcpp::as<std::string>(paramName));
  }

  // [[Rcpp::export()]]
  void CLI_ClearSettings()
  {
    CLI::ClearSettings();
  }
