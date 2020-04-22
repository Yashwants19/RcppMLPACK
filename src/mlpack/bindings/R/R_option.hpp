/**
 * @file R_option.hpp
 * @author Yashwant Singh
 *
 * The R option type.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#ifndef MLPACK_BINDINGS_R_R_OPTION_HPP
#define MLPACK_BINDINGS_R_R_OPTION_HPP
#include <mlpack/core/util/param_data.hpp>

namespace mlpack {
namespace bindings {
namespace r {

/**
 * The R option class.
 */
template<typename T>
class ROption
{
 public:
  /**
   * Construct a ROption object.  When constructed, it will register itself
   * with CLI. The testName parameter is not used and added for compatibility
   * reasons.
   */
  ROption(const T defaultValue,
          const std::string& identifier,
          const std::string& description,
          const std::string& alias,
          const std::string& cppName,
          const bool required = false,
          const bool input = true,
          const bool noTranspose = false,
          const std::string& /* testName */ = "")
  {
    // Create the ParamData object to give to CLI.
    util::ParamData data;
    data.desc = description;
    data.name = identifier;
    data.tname = TYPENAME(T);
    data.alias = alias[0];
    data.wasPassed = false;
    data.noTranspose = noTranspose;
    data.required = required;
    data.input = input;
    data.loaded = false;

    // Only "verbose" will be persistent.
    if (identifier == "verbose")
      data.persistent = true;
    else
      data.persistent = false;
    data.cppType = cppName;

    // Every parameter we'll get from R will have the correct type.
    data.value = boost::any(defaultValue);

    // Restore the parameters for this program.
    if (identifier != "verbose")
      CLI::RestoreSettings(CLI::ProgramName(), false);

    // Add the ParamData object, then store.  This is necessary because we may
    // import more than one .so or .o that uses CLI, so we have to keep the options
    // separate.  programName is a global variable from mlpack_main.hpp.
    CLI::Add(std::move(data));
    if (identifier != "verbose")
      CLI::StoreSettings(CLI::ProgramName());
    CLI::ClearSettings();
  }
};

} // namespace r
} // namespace bindings
} // namespace mlpack

#endif