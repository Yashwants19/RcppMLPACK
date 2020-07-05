/**
 * @file methods/local_coordinate_coding/lcc_impl.hpp
 * @author Nishant Mehta
 *
 * Implementation of Local Coordinate Coding
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_METHODS_LOCAL_COORDINATE_CODING_LCC_IMPL_HPP
#define MLPACK_METHODS_LOCAL_COORDINATE_CODING_LCC_IMPL_HPP

// In case it hasn't been included yet.
#include "lcc.hpp"

namespace mlpack {
namespace lcc {

template<typename DictionaryInitializer>
LocalCoordinateCoding::LocalCoordinateCoding(
    const arma::mat& data,
    const size_t atoms,
    const double lambda,
    const size_t maxIterations,
    const double tolerance,
    const DictionaryInitializer& initializer) :
    atoms(atoms),
    lambda(lambda),
    maxIterations(maxIterations),
    tolerance(tolerance)
{
  // Train the model.
  Train(data, initializer);
}

template<typename DictionaryInitializer>
double LocalCoordinateCoding::Train(
    const arma::mat& data,
    const DictionaryInitializer& initializer)
{
  Timer::Start("local_coordinate_coding");

  // Initialize the dictionary.
  initializer.Initialize(data, atoms, dictionary);

  double lastObjVal = DBL_MAX;

  // Take the initial coding step, which has to happen before entering the main
  // loop.
  Log::Info << "Initial Coding Step." << std::endl;

  arma::mat codes;
  Encode(data, codes);
  arma::uvec adjacencies = find(codes);

  Log::Info << "  Sparsity level: " << 100.0 * ((double)(adjacencies.n_elem)) /
      ((double)(atoms * data.n_cols)) << "%.\n";
  Log::Info << "  Objective value: " << Objective(data, codes, adjacencies)
      << "." << std::endl;

  for (size_t t = 1; t != maxIterations; t++)
  {
    Log::Info << "Iteration " << t << " of " << maxIterations << "."
        << std::endl;

    // First step: optimize the dictionary.
    Log::Info << "Performing dictionary step..." << std::endl;
    OptimizeDictionary(data, codes, adjacencies);
    double dsObjVal = Objective(data, codes, adjacencies);
    Log::Info << "  Objective value: " << dsObjVal << "." << std::endl;

    // Second step: perform the coding.
    Log::Info << "Performing coding step..." << std::endl;
    Encode(data, codes);
    adjacencies = find(codes);
    Log::Info << "  Sparsity level: " << 100.0 * ((double) (adjacencies.n_elem))
        / ((double)(atoms * data.n_cols)) << "%.\n";

    // Terminate if the objective increased in the coding step.
    double curObjVal = Objective(data, codes, adjacencies);
    if (curObjVal > dsObjVal)
    {
      Log::Warn << "Objective increased in coding step!  Terminating."
          << std::endl;
      break;
    }

    // Find the new objective value and improvement so we can check for
    // convergence.
    double improvement = lastObjVal - curObjVal;
    Log::Info << "Objective value: " << curObjVal << " (improvement "
        << std::scientific << improvement << ")." << std::endl;

    if (improvement < tolerance)
    {
      Log::Info << "Converged within tolerance " << tolerance << ".\n";
      break;
    }

    lastObjVal = curObjVal;
  }

  Timer::Stop("local_coordinate_coding");
  return lastObjVal;
}

template<typename Archive>
void LocalCoordinateCoding::serialize(Archive& ar,
                                      const unsigned int /* version */)
{
  ar & BOOST_SERIALIZATION_NVP(atoms);
  ar & BOOST_SERIALIZATION_NVP(dictionary);
  ar & BOOST_SERIALIZATION_NVP(lambda);
  ar & BOOST_SERIALIZATION_NVP(maxIterations);
  ar & BOOST_SERIALIZATION_NVP(tolerance);
}

} // namespace lcc
} // namespace mlpack

#endif
