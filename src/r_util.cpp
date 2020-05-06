#include <RcppMLPACK.h>
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/cli.hpp>

using namespace mlpack;
using namespace Rcpp;

// [[Rcpp::export]]
void CLI_RestoreSettings(const std::string& programName)
{
 CLI::RestoreSettings(programName);
}

// [[Rcpp::export]]
void CLI_SetParamInt(const std::string& paramName, int paramValue)
{
  CLI::GetParam<int>(paramName) = paramValue;
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamDouble(const std::string& paramName, double paramValue)
{
  CLI::GetParam<double>(paramName) = paramValue;
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamString(const std::string& paramName, std::string& paramValue)
{
  CLI::GetParam<std::string>(paramName) = paramValue;
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamBool(const std::string& paramName, bool paramValue)
{
  CLI::GetParam<bool>(paramName) = paramValue;
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamMat(const std::string& paramName,
                     arma::mat& paramValue,
                     const bool copyAllInputs)
{
  if (copyAllInputs)
  {
    arma::mat m(paramValue.memptr(), paramValue.n_rows, paramValue.n_cols, true);
    CLI::GetParam<arma::mat>(paramName) = std::move(m);
  }
  else
  {
    CLI::GetParam<arma::mat>(paramName) = std::move(paramValue);
  }
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
int CLI_GetParamInt(const std::string& paramName)
{
  return CLI::GetParam<int>(paramName);
}

// [[Rcpp::export]]
double CLI_GetParamDouble(const std::string& paramName)
{
  return CLI::GetParam<double>(paramName);
}

// [[Rcpp::export]]
std::string& CLI_GetParamString(const std::string& paramName)
{
  return CLI::GetParam<std::string>(paramName);
}

// [[Rcpp::export]]
bool CLI_GetParamBool(const std::string& paramName)
{
  return CLI::GetParam<bool>(paramName);
}

// [[Rcpp::export]]
arma::mat& CLI_GetParamMat(const std::string& paramName)
{
  return CLI::GetParam<arma::mat>(paramName);
}

// [[Rcpp::export]]
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
void CLI_SetPassed(const std::string& paramName)
{
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_ClearSettings()
{
  CLI::ClearSettings();
}