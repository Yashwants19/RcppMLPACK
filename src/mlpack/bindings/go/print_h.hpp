/**
 * @file print_h.hpp
 * @author Yasmine Dumouchel
 *
 * Given a list of ParamData structures, emit a .h file defining the
 * Go bindings.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_GO_PRINT_H_HPP
#define MLPACK_BINDINGS_GO_PRINT_H_HPP

#include <mlpack/core.hpp>

namespace mlpack {
namespace bindings {
namespace go {

/**
 * Given a list of parameter definition and program documentation, print a
 * generated .h file to stdout.
 *
 * @param parameters List of parameters the program will use (from CLI).
 * @param programInfo Documentation for the program.
 * @param functionName Name of the function (i.e. "pca").
 */
void PrintH(const util::ProgramDoc& programInfo,
            const std::string& functionName);


} // namespace go
} // namespace bindings
} // namespace mlpack

#endif
