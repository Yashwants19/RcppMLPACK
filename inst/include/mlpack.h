/**
 * @file inst/include/mlpack.h
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

// #pragma GCC diagnostic ignored "-Wdeprecated-declarations"

#include <Rcpp.h>

// To suppress Found ‘__assert_fail’, possibly from ‘assert’ (C).
#define BOOST_DISABLE_ASSERTS

// Rcpp has its own stream object which cooperates more nicely with R's i/o
// And as of armadillo and mlpack, we can use this stream object as well.
#if !defined(ARMA_COUT_STREAM)
  #define ARMA_COUT_STREAM Rcpp::Rcout
#endif
#if !defined(ARMA_CERR_STREAM)
  #define ARMA_CERR_STREAM Rcpp::Rcerr
#endif
#if !defined(MLPACK_COUT_STREAM)
  #define MLPACK_COUT_STREAM Rcpp::Rcout
#endif
#if !defined(MLPACK_CERR_STREAM)
  #define MLPACK_CERR_STREAM Rcpp::Rcerr
#endif

// This define makes the R RNG have precedent over the C++11-based
// RNG provided by Armadillo.
#if !defined(ARMA_RNG_ALT)
  #define ARMA_RNG_ALT         RcppArmadillo/Alt_R_RNG.h
#endif

// To suppress warnings related to core/util/arma_util.hpp.
#define MLPACK_CORE_UTIL_ARMA_CONFIG_HPP

#include <mlpack/core/data/load.cpp>
#include <mlpack/core/data/load_csv.cpp>
#include <mlpack/core/data/load_image.cpp>
#include <mlpack/core/data/save_image.cpp>
#include <mlpack/core/dists/diagonal_gaussian_distribution.cpp>
#include <mlpack/core/dists/discrete_distribution.cpp>
#include <mlpack/core/dists/gamma_distribution.cpp>
#include <mlpack/core/dists/gaussian_distribution.cpp>
#include <mlpack/core/dists/laplace_distribution.cpp>
#include <mlpack/core/dists/regression_distribution.cpp>
#include <mlpack/core/kernels/epanechnikov_kernel.cpp>
#include <mlpack/core/kernels/pspectrum_string_kernel.cpp>
#include <mlpack/core/math/columns_to_blocks.cpp>
#include <mlpack/core/math/lin_alg.cpp>
#include <mlpack/core/math/random.cpp>
#include <mlpack/core/math/random_basis.cpp>
#include <mlpack/core/tree/cosine_tree/cosine_tree.cpp>
#include <mlpack/core/util/backtrace.cpp>
#include <mlpack/core/util/io.cpp>
#include <mlpack/core/util/log.cpp>
#include <mlpack/core/util/prefixedoutstream.cpp>
#include <mlpack/core/util/program_doc.cpp>
#include <mlpack/core/util/singletons.cpp>
#include <mlpack/core/util/timers.cpp>
#include <mlpack/core/util/version.cpp>
#include <mlpack/methods/adaboost/adaboost_model.cpp>
#include <mlpack/methods/block_krylov_svd/randomized_block_krylov_svd.cpp>
#include <mlpack/methods/fastmks/fastmks_model.cpp>
#include <mlpack/methods/gmm/diagonal_gmm.cpp>
#include <mlpack/methods/gmm/gmm.cpp>
#include <mlpack/methods/hoeffding_trees/hoeffding_tree_model.cpp>
#include <mlpack/methods/lars/lars.cpp>
#include <mlpack/methods/linear_regression/linear_regression.cpp>
#include <mlpack/methods/local_coordinate_coding/lcc.cpp>
#include <mlpack/methods/matrix_completion/matrix_completion.cpp>
#include <mlpack/methods/neighbor_search/unmap.cpp>
#include <mlpack/methods/quic_svd/quic_svd.cpp>
#include <mlpack/methods/radical/radical.cpp>
#include <mlpack/methods/randomized_svd/randomized_svd.cpp>
#include <mlpack/methods/rann/ra_util.cpp>
#include <mlpack/methods/softmax_regression/softmax_regression.cpp>
#include <mlpack/methods/softmax_regression/softmax_regression_function.cpp>
#include <mlpack/methods/sparse_autoencoder/maximal_inputs.cpp>
#include <mlpack/methods/sparse_autoencoder/sparse_autoencoder.cpp>
#include <mlpack/methods/sparse_autoencoder/sparse_autoencoder_function.cpp>
#include <mlpack/methods/sparse_coding/sparse_coding.cpp>


#include <mlpack/core.hpp>

// These are all the boost files, we need to compile R bindings for mlpack that
// are not a part of mlpack itself.
#include <boost/serialization/archive_exception.cpp>
#include <boost/serialization/basic_archive.cpp>
#include <boost/serialization/basic_oarchive.cpp>
#include <boost/serialization/basic_oserializer.cpp>
#include <boost/serialization/basic_iarchive.cpp>
#include <boost/serialization/basic_iserializer.cpp>
#include <boost/serialization/basic_pointer_iserializer.cpp>
#include <boost/serialization/basic_pointer_oserializer.cpp>
#include <boost/serialization/basic_text_oprimitive.cpp>
#include <boost/serialization/binary_iarchive.cpp>
#include <boost/serialization/binary_oarchive.cpp>
#include <boost/serialization/extended_type_info_typeid.cpp>
#include <boost/serialization/extended_type_info.cpp>
#include <boost/serialization/basic_serializer_map.cpp>
#include <boost/serialization/void_cast.cpp>
#include <boost/serialization/utf8_codecvt_facet.cpp>

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
