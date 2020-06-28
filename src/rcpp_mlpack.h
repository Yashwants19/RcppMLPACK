/**
 * @file src/RcppMLPACK.h
 * @author Dirk Eddelbuettel
 * @author Yashwant Singh Parihar
 *
 * Include all of the base components required to work mlpack bindings.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_R_RCPP_MLPACK_H
#define MLPACK_BINDINGS_R_RCPP_MLPACK_H

#include <Rcpp.h>

#define BOOST_DISABLE_ASSERTS

// More recently this changes from ARMA_DEFAULT_OSTREAM to ARMA_COUT_STREAM
// and ARMA_CERR_STREAM
#if !defined(ARMA_COUT_STREAM)
  #define ARMA_COUT_STREAM Rcpp::Rcout
#endif
#if !defined(ARMA_CERR_STREAM)
  #define ARMA_CERR_STREAM Rcpp::Rcerr
#endif
#if !defined(ARMA_RNG_ALT)
  #define ARMA_RNG_ALT         RcppArmadillo/Alt_R_RNG.h
#endif

#if !defined(MLPACK_COUT_STREAM)
  #define MLPACK_COUT_STREAM Rcpp::Rcout
#endif
#if !defined(MLPACK_CERR_STREAM)
  #define MLPACK_CERR_STREAM Rcpp::Rcerr
#endif

#endif
