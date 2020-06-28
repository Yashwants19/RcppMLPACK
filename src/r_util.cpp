/**
 * @file src/r_util.cpp
 * @author Yashwant Singh Parihar
 *
 * Utility functions for R-bindings.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <rcpp_mlpack.h>
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/cli.hpp>

using namespace mlpack;
using namespace Rcpp;

template<typename eT>
bool inline inplace_transpose(arma::Mat<eT>& X)
{
  try
  {
    X = arma::trans(X);
    return false;
  }
  catch (std::bad_alloc&)
  {
    return true;
  }
}

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
void CLI_SetParamVecString(const std::string& paramName,
                           const std::vector<std::string>& str)
{
  CLI::GetParam<std::vector<std::string>>(paramName) = std::move(str);
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamVecInt(const std::string& paramName,
                        const std::vector<int>& ints)
{
  CLI::GetParam<std::vector<int>>(paramName) = std::move(ints);
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamMat(const std::string& paramName,
                     const arma::mat& paramValue)
{
  CLI::GetParam<arma::mat>(paramName) = std::move(paramValue.t());
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamUMat(const std::string& paramName,
                      const arma::Mat<size_t>& paramValue)
{
  CLI::GetParam<arma::Mat<size_t>>(paramName) = std::move(paramValue.t());
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamRow(const std::string& paramName,
                     const arma::rowvec& paramValue)
{
  CLI::GetParam<arma::rowvec>(paramName) = std::move(paramValue);
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamURow(const std::string& paramName,
                      const arma::Row<size_t>& paramValue)
{
  CLI::GetParam<arma::Row<size_t>>(paramName) = std::move(paramValue);
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamCol(const std::string& paramName,
                     const arma::vec& paramValue)
{
  CLI::GetParam<arma::vec>(paramName) = std::move(paramValue);
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamUCol(const std::string& paramName,
                      const arma::Col<size_t>& paramValue)
{
  CLI::GetParam<arma::Col<size_t>>(paramName) = std::move(paramValue);
  CLI::SetPassed(paramName);
}

// [[Rcpp::export]]
void CLI_SetParamMatWithInfo(const std::string& paramName,
                             const LogicalVector& dimensions,
                             const arma::mat& paramValue)
{
  data::DatasetInfo d(paramValue.n_cols);
  for (size_t i = 0; i < d.Dimensionality(); ++i)
  {
    d.Type(i) = (dimensions[i]) ? data::Datatype::categorical :
        data::Datatype::numeric;
  }
  std::get<0>(CLI::GetParam<std::tuple<data::DatasetInfo, arma::mat>>(
      paramName)) = std::move(d);
  std::get<1>(CLI::GetParam<std::tuple<data::DatasetInfo, arma::mat>>(
      paramName)) = std::move(paramValue.t());
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
const std::vector<std::string>& CLI_GetParamVecString(const
                                    std::string& paramName)
{
  return std::move(CLI::GetParam<std::vector<std::string>>(paramName));
}

// [[Rcpp::export]]
const std::vector<int>& CLI_GetParamVecInt(const std::string& paramName)
{
  return std::move(CLI::GetParam<std::vector<int>>(paramName));
}

// [[Rcpp::export]]
const arma::mat& CLI_GetParamMat(const std::string& paramName)
{
  inplace_transpose(CLI::GetParam<arma::mat>(paramName));
  return std::move(CLI::GetParam<arma::mat>(paramName));
}

// [[Rcpp::export]]
const arma::Mat<size_t>& CLI_GetParamUMat(const std::string& paramName)
{
  inplace_transpose(CLI::GetParam<arma::Mat<size_t>>(paramName));
  return std::move(CLI::GetParam<arma::Mat<size_t>>(paramName));
}

// [[Rcpp::export]]
const arma::vec CLI_GetParamRow(const std::string& paramName)
{
  return std::move(CLI::GetParam<arma::rowvec>(paramName).t());
}

// [[Rcpp::export]]
const arma::Col<size_t> CLI_GetParamURow(const std::string& paramName)
{
  return std::move(CLI::GetParam<arma::Row<size_t>>(paramName).t());
}

// [[Rcpp::export]]
const arma::rowvec CLI_GetParamCol(const std::string& paramName)
{
  return std::move(CLI::GetParam<arma::vec>(paramName).t());
}

// [[Rcpp::export]]
const arma::Row<size_t> CLI_GetParamUCol(const std::string& paramName)
{
  return std::move(CLI::GetParam<arma::Col<size_t>>(paramName).t());
}

// [[Rcpp::export]]
List CLI_GetParamMatWithInfo(const std::string& paramName)
{
  const data::DatasetInfo& d = std::get<0>(
      CLI::GetParam<std::tuple<data::DatasetInfo, arma::mat>>(paramName));
  const arma::mat& m = std::get<1>(
      CLI::GetParam<std::tuple<data::DatasetInfo, arma::mat>>(paramName)).t();

  LogicalVector dims(d.Dimensionality());
  for (size_t i = 0; i < d.Dimensionality(); ++i)
    dims[i] = (d.Type(i) == data::Datatype::numeric) ? false : true;

  return List::create (Rcpp::Named("Info") = std::move(dims),
                       Rcpp::Named("Data") = std::move(m));
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
