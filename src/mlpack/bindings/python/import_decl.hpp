/**
 * @file import_decl.hpp
 * @author Ryan Curtin
 *
 * For a serializable model, print the class import.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_PYTHON_IMPORT_DECL_HPP
#define MLPACK_BINDINGS_PYTHON_IMPORT_DECL_HPP

#include <mlpack/prereqs.hpp>
#include "strip_type.hpp"

namespace mlpack {
namespace bindings {
namespace python {

/**
 * For a serializable type, print a cppclass definition.
 */
template<typename T>
void ImportDecl(
    const util::ParamData& d,
    const size_t indent,
    const typename boost::disable_if<arma::is_arma_type<T>>::type* = 0,
    const typename boost::enable_if<data::HasSerialize<T>>::type* = 0)
{
  // First, we have to parse the type.  If we have something like, e.g.,
  // 'LogisticRegression<>', we must convert this to 'LogisticRegression[T=*].'
  std::string strippedType, printedType, defaultsType;
  StripType(d.cppType, strippedType, printedType, defaultsType);

  /**
   * This will give output of the form:
   *
   * cdef cppclass Type:
   *   Type() nogil
   */
  const std::string prefix = std::string(indent, ' ');
  std::cout << prefix << "cdef cppclass " << defaultsType << ":" << std::endl;
  std::cout << prefix << "  " << strippedType << "() nogil" << std::endl;
  std::cout << prefix << std::endl;
}

/**
 * For a non-serializable type, print nothing.
 */
template<typename T>
void ImportDecl(
    const util::ParamData& /* d */,
    const size_t /* indent */,
    const typename boost::disable_if<arma::is_arma_type<T>>::type* = 0,
    const typename boost::disable_if<data::HasSerialize<T>>::type* = 0)
{
  // Print nothing.
}

/**
 * For a matrix type, print nothing.
 */
template<typename T>
void ImportDecl(
    const util::ParamData& /* d */,
    const size_t /* indent */,
    const typename boost::enable_if<arma::is_arma_type<T>>::type* = 0)
{
  // Print nothing.
}

/**
 * Print the cppclass definition for a serializable model; print nothing for a
 * non-serializable type.
 *
 * @param d Parameter info struct.
 * @param input Pointer to size_t indicating indent.
 * @param output Unused parameter.
 */
template<typename T>
void ImportDecl(const util::ParamData& d,
                const void* indent,
                void* /* output */)
{
  ImportDecl<typename std::remove_pointer<T>::type>(d, *((size_t*) indent));
}

} // namespace python
} // namespace bindings
} // namespace mlpack

#endif
