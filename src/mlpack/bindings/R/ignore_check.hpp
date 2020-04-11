/**
 * @file ignore_check.hpp
 * @author Yashwant Singh
 *
 * Implementation of IgnoreCheck() for Python bindings.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_R_IGNORE_CHECK_HPP
#define MLPACK_BINDINGS_R_IGNORE_CHECK_HPP

namespace mlpack {
namespace bindings {
namespace r {

/**
 * Return whether or not a parameter check should be ignored.  For test
 * bindings, we do not ignore any checks, so this always returns false.
 */
template<typename T>
inline bool IgnoreCheck(const T& /* t */) { return false; }

} // namespace r
} // namespace bindings
} // namespace mlpack

#endif
