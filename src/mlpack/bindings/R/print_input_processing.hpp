/**
 * @file bindings/R/print_input_processing_impl.hpp
 * @author Yashwant Singh Parihar
 *
 * Print R code to handle input arguments.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_R_PRINT_INPUT_PROCESSING_IMPL_HPP
#define MLPACK_BINDINGS_R_PRINT_INPUT_PROCESSING_IMPL_HPP

#include "strip_type.hpp"
#include "get_type.hpp"
#include <mlpack/prereqs.hpp>

namespace mlpack {
namespace bindings {
namespace r {

/**
 * Print input processing for a standard option type.
 */
template<typename T>
void PrintInputProcessing(
    const util::ParamData& d,
    const typename boost::disable_if<arma::is_arma_type<T>>::type* = 0,
    const typename boost::disable_if<data::HasSerialize<T>>::type* = 0,
    const typename boost::disable_if<std::is_same<T,
        std::tuple<data::DatasetInfo, arma::mat>>>::type* = 0)
{
  if (!d.required)
  {
    /**
     * This gives us code like:
     *
     *  if (!identical(\<param_name\>, NA)) {
     *     CLI_SetParam<type>("\<param_name\>", \<param_name\>)
     *  }
     */
    MLPACK_COUT_STREAM << "  if (!identical(" << d.name ;
    if (d.cppType == "bool")
    {
      MLPACK_COUT_STREAM << ", FALSE)) {" << std::endl;
    }
    else
    {
      MLPACK_COUT_STREAM << ", NA)) {" << std::endl;
    }
    MLPACK_COUT_STREAM << "    CLI_SetParam" << GetType<T>(d) << "(\""
        << d.name << "\", " << d.name << ")" << std::endl;
    MLPACK_COUT_STREAM << "  }" << std::endl; // Closing brace.
  }
  else
  {
    /**
     * This gives us code like:
     *
     *     CLI_SetParam<type>("\<param_name\>", \<param_name\>)
     */
    MLPACK_COUT_STREAM << "  CLI_SetParam" << GetType<T>(d) << "(\""
              << d.name << "\", " << d.name << ")" << std::endl;
  }
  MLPACK_COUT_STREAM << std::endl; // Extra line is to clear up the code a bit.
}

/**
 * Print input processing for a matrix type.
 */
template<typename T>
void PrintInputProcessing(
    const util::ParamData& d,
    const typename boost::enable_if<arma::is_arma_type<T>>::type* = 0)
{
  if (!d.required)
  {
    /**
     * This gives us code like:
     *
     *  if (!identical(\<param_name\>, NA)) {
     *     CLI_SetParam<type>("\<param_name\>", to_matrix(\<param_name\>))
     *  }
     */
    MLPACK_COUT_STREAM << "  if (!identical(" << d.name << ", NA)) {"
        << std::endl;
    MLPACK_COUT_STREAM << "    CLI_SetParam" << GetType<T>(d) << "(\""
        << d.name << "\", to_matrix(" << d.name << "))" << std::endl;
    MLPACK_COUT_STREAM << "  }" << std::endl; // Closing brace.
  }
  else
  {    /**
     * This gives us code like:
     *
     *     CLI_SetParam<type>("\<param_name\>", to_matrix(\<param_name\>))
     */
    MLPACK_COUT_STREAM << "  CLI_SetParam" << GetType<T>(d) << "(\""
              << d.name << "\", to_matrix(" << d.name << "))" << std::endl;
  }
  MLPACK_COUT_STREAM << std::endl; // Extra line is to clear up the code a bit.
}

/**
 * Print input processing for a matrix with info type.
 */
template<typename T>
void PrintInputProcessing(
    const util::ParamData& d,
    const typename boost::enable_if<std::is_same<T,
        std::tuple<data::DatasetInfo, arma::mat>>>::type* = 0)
{
  if (!d.required)
  {
    /**
     * This gives us code like:
     *
     *  if (!identical(\<param_name\>, NA)) {
     *     \<param_name\> = to_matrix_with_info(\<param_name\>)
     *     CLI_SetParam\<type\>("\<param_name\>", \<param_name\>$info,
     *                         \<param_name\>$data)
     *  }
     */
    MLPACK_COUT_STREAM << "  if (!identical(" << d.name << ", NA)) {"
        << std::endl;
    MLPACK_COUT_STREAM << "    " << d.name << " <- to_matrix_with_info("
        << d.name << ")" << std::endl;
    MLPACK_COUT_STREAM << "    CLI_SetParam" << GetType<T>(d) << "(\""
        << d.name << "\", " << d.name << "$info, " << d.name
        << "$data)" << std::endl;
    MLPACK_COUT_STREAM << "  }" << std::endl; // Closing brace.
  }
  else
  {    /**
     * This gives us code like:
     *
     *     \<param_name\> = to_matrix_with_info(\<param_name\>)
     *     CLI_SetParam\<type\>("\<param_name\>", \<param_name\>$info,
     *                         \<param_name\>$data)
     */
    MLPACK_COUT_STREAM << "  " << d.name << " <- to_matrix_with_info("
        << d.name << ")" << std::endl;
    MLPACK_COUT_STREAM << "  CLI_SetParam" << GetType<T>(d) << "(\""
        << d.name << "\", " << d.name << "$info, " << d.name
        << "$data)" << std::endl;
  }
  MLPACK_COUT_STREAM << std::endl; // Extra line is to clear up the code a bit.
}

/**
 * Print input processing for a serializable type.
 */
template<typename T>
void PrintInputProcessing(
    const util::ParamData& d,
    const typename boost::disable_if<arma::is_arma_type<T>>::type* = 0,
    const typename boost::enable_if<data::HasSerialize<T>>::type* = 0)
{
  if (!d.required)
  {
    /**
     * This gives us code like:
     *
     *  if (!identical(\<param_name\>, NA)) {
     *     CLI_SetParam\<ModelType\>Ptr("\<param_name\>", \<param_name\>)
     *  }
     */
    MLPACK_COUT_STREAM << "  if (!identical(" << d.name << ", NA)) {"
        << std::endl;
    MLPACK_COUT_STREAM << "    CLI_SetParam" << StripType(d.cppType) << "Ptr(\""
        << d.name << "\", " << d.name << ")" << std::endl;
    MLPACK_COUT_STREAM << "  }" << std::endl; // Closing brace.
  }
  else
  {    /**
     * This gives us code like:
     *
     *     CLI_SetParam/<ModelType/>Ptr("\<param_name\>", \<param_name\>)
     */
    MLPACK_COUT_STREAM << "  CLI_SetParam" << StripType(d.cppType) << "Ptr(\""
              << d.name << "\", " << d.name << ")" << std::endl;
  }
  MLPACK_COUT_STREAM << std::endl; // Extra line is to clear up the code a bit.
}

/**
 * Given parameter information and the current number of spaces for indentation,
 * print the code to process the input to cout.  This code assumes that
 * data.input is true, and should not be called when data.input is false.
 *
 * The number of spaces to indent should be passed through the input pointer.
 *
 * @param d Parameter data struct.
 * @param input Pointer to size_t holding the indentation.
 * @param * (output) Unused parameter.
 */
template<typename T>
void PrintInputProcessing(const util::ParamData& d,
                          const void* /* output */,
                          void* /* output */)
{
  PrintInputProcessing<typename std::remove_pointer<T>::type>(d);
}

} // namespace r
} // namespace bindings
} // namespace mlpack

#endif
