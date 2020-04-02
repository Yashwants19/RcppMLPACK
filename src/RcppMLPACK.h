#ifndef RcppMLPACK__RcppMLPACK__h
#define RcppMLPACK__RcppMLPACK__h

#if _WIN64
#ifndef ARMA_64BIT_WORD
#define ARMA_64BIT_WORD
#endif
#endif

#if defined(__MINGW32__)
#define ARMA_DONT_USE_CXX11
#endif

// Rcpp has its own stream object which cooperates more nicely with R's i/o
// And as of Armadillo 2.4.3, we can use this stream object as well
//
// More recently this changes from ARMA_DEFAULT_OSTREAM to ARMA_COUT_STREAM
// and ARMA_CERR_STREAM
#if !defined(ARMA_COUT_STREAM)
  #define ARMA_COUT_STREAM Rcpp::Rcout
#endif
#if !defined(ARMA_CERR_STREAM)
  #define ARMA_CERR_STREAM Rcpp::Rcerr
#endif



#include <Rcpp.h>
#include <mlpack/core/util/cli.cpp>
#include <mlpack/core/util/backtrace.cpp>
#include <mlpack/core/util/log.cpp>
#include <mlpack/core/util/prefixedoutstream.cpp>
#include <mlpack/core/util/program_doc.cpp>
#include <mlpack/core/util/singletons.cpp>
#include <mlpack/core/util/timers.cpp>
#include <mlpack/core/util/version.cpp>


#include <mlpack/core.hpp>
#undef ARMA_EXTRA_MAT_PROTO
#undef ARMA_EXTRA_MAT_MEAT

// instead of including RcppArmadillo.h -- which re-includes parts
// of Armadillo already brought in by mlpack, we just include pieces
// needed for sugar wrapping etc

#include <RcppArmadilloConfig.h>
#include <RcppArmadilloWrap.h>
#include <RcppArmadilloAs.h>
#include <RcppArmadilloSugar.h>

// prevent inclusion of Rcpp.h and RcppArmadillo.h via the
// autogenerated RcppExports.cpp
#define Rcpp_hpp
#define RcppArmadillo__RcppArmadillo__h

#endif
