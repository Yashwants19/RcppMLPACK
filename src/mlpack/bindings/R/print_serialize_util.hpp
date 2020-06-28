/**
 * @file bindings/R/print_serialize_util.hpp
 * @author Yashwant Singh Parihar
 *
 * Print the serialize utility in a R binding .R file for a given
 * parameter.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_R_PRINT_SERIALIZE_UTIL_HPP
#define MLPACK_BINDINGS_R_PRINT_SERIALIZE_UTIL_HPP

#include <mlpack/prereqs.hpp>
#include "get_type.hpp"
#include "strip_type.hpp"

namespace mlpack {
namespace bindings {
namespace r {

/**
 * Print serialize utility for a regular parameter type.
 */
template<typename T>
void PrintSerializeUtil(
    const util::ParamData& /* d */,
    const typename boost::disable_if<arma::is_arma_type<T>>::type* = 0,
    const typename boost::disable_if<data::HasSerialize<T>>::type* = 0,
    const typename boost::disable_if<std::is_same<T,
        std::tuple<data::DatasetInfo, arma::mat>>>::type* = 0)
{
  // Do Nothing.
}

/**
 * Print serialize utility for a matrix type.
 */
template<typename T>
void PrintSerializeUtil(
    const util::ParamData& /* d */,
    const typename boost::enable_if<arma::is_arma_type<T>>::type* = 0,
    const typename std::enable_if<!std::is_same<T,
        std::tuple<data::DatasetInfo, arma::mat>>::value>::type* = 0)
{
  // Do Nothing.
}

/**
 * Print serialize utility for a matrix with info type.
 */
template<typename T>
void PrintSerializeUtil(
    const util::ParamData& /* d */,
    const typename boost::enable_if<std::is_same<T,
        std::tuple<data::DatasetInfo, arma::mat>>>::type* = 0)
{
  // Do Nothing.
}

/**
 * Print serialize utility for a serializable model.
 */
template<typename T>
void PrintSerializeUtil(
    const util::ParamData& d,
    const typename boost::disable_if<arma::is_arma_type<T>>::type* = 0,
    const typename boost::enable_if<data::HasSerialize<T>>::type* = 0)
{
  /**
   * This gives us code like:
   *
   *  \<paramName\> <- CLI_GetParam\<ModelType\>Ptr("\<paramName\>")
   *  attr(\<paramName\>, "type") <- "\<ModelType\>"
   *
   */
  MLPACK_COUT_STREAM << std::endl;
  MLPACK_COUT_STREAM << "  " << d.name << " <- CLI_GetParam"
      << StripType(d.cppType) << "Ptr(\"" << d.name << "\")";
  MLPACK_COUT_STREAM << std::endl;
  MLPACK_COUT_STREAM << "  attr(" << d.name << ", \"type\") <- \"" 
      << StripType(d.cppType) << "\"";
  MLPACK_COUT_STREAM << std::endl;
}

/**
 * @param d Parameter data struct.
 * @param * (input) Unused parameter.
 * @param * (output) Unused parameter.
 */
template<typename T>
void PrintSerializeUtil(const util::ParamData& d,
                        const void* /*input*/,
                        void* /* output */)
{
  PrintSerializeUtil<typename std::remove_pointer<T>::type>(d);
}

} // namespace r
} // namespace bindings
} // namespace mlpack

#endif
