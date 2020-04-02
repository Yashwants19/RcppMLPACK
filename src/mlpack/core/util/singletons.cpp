/**
 * @file singletons.cpp
 * @author Ryan Curtin
 *
 * Declaration of singletons in libmlpack.so.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include "cli.hpp"
#include "log.hpp"
#include <iostream>
#include <Rcpp.h>

using namespace mlpack;
using namespace mlpack::util;

// Color code escape sequences -- but not on Windows.
#ifndef _WIN32
  #define BASH_RED "\033[0;31m"
  #define BASH_GREEN "\033[0;32m"
  #define BASH_YELLOW "\033[0;33m"
  #define BASH_CYAN "\033[0;36m"
  #define BASH_CLEAR "\033[0m"
#else
  #define BASH_RED ""
  #define BASH_GREEN ""
  #define BASH_YELLOW ""
  #define BASH_CYAN ""
  #define BASH_CLEAR ""
#endif

#ifdef DEBUG
PrefixedOutStream Log::Debug = PrefixedOutStream(Rcpp::Rcout,
    BASH_CYAN "[DEBUG] " BASH_CLEAR);
#else
NullOutStream Log::Debug = NullOutStream();
#endif

PrefixedOutStream Log::Info = PrefixedOutStream(Rcpp::Rcout,
    BASH_GREEN "[INFO ] " BASH_CLEAR, true /* unless --verbose */, false);
PrefixedOutStream Log::Warn = PrefixedOutStream(Rcpp::Rcout,
    BASH_YELLOW "[WARN ] " BASH_CLEAR, false, false);
PrefixedOutStream Log::Fatal = PrefixedOutStream(Rcpp::Rcerr,
    BASH_RED "[FATAL] " BASH_CLEAR, false, true /* fatal */);
