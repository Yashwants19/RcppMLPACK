/**
 * @file julia_binding_test.cpp
 * @author Ryan Curtin
 *
 * A binding test for Julia.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/cli.hpp>
#include <mlpack/core/util/mlpack_main.hpp>
#include <mlpack/core/kernels/gaussian_kernel.hpp>

using namespace std;
using namespace mlpack;
using namespace mlpack::kernel;

PROGRAM_INFO("Julia binding test",
    "A simple program to test Julia binding functionality.",
    "A simple program to test Julia binding functionality.  You can build "
    "mlpack with the BUILD_TESTS option set to off, and this binding will "
    "no longer be built.");

PARAM_STRING_IN_REQ("string_in", "Input string, must be 'hello'.", "s");
PARAM_INT_IN_REQ("int_in", "Input int, must be 12.", "i");
PARAM_DOUBLE_IN_REQ("double_in", "Input double, must be 4.0.", "d");
PARAM_FLAG("flag1", "Input flag, must be specified.", "f");
PARAM_FLAG("flag2", "Input flag, must not be specified.", "F");
PARAM_MATRIX_IN("matrix_in", "Input matrix.", "m");
PARAM_UMATRIX_IN("umatrix_in", "Input unsigned matrix.", "u");
PARAM_COL_IN("col_in", "Input column.", "c");
PARAM_UCOL_IN("ucol_in", "Input unsigned column.", "");
PARAM_ROW_IN("row_in", "Input row.", "");
PARAM_UROW_IN("urow_in", "Input unsigned row.", "");
PARAM_MATRIX_AND_INFO_IN("matrix_and_info_in", "Input matrix and info.", "");
PARAM_VECTOR_IN(int, "vector_in", "Input vector of numbers.", "");
PARAM_VECTOR_IN(string, "str_vector_in", "Input vector of strings.", "");
PARAM_MODEL_IN(GaussianKernel, "model_in", "Input model.", "");
PARAM_FLAG("build_model", "If true, a model will be returned.", "");

PARAM_STRING_OUT("string_out", "Output string, will be 'hello2'.", "S");
PARAM_INT_OUT("int_out", "Output int, will be 13.");
PARAM_DOUBLE_OUT("double_out", "Output double, will be 5.0.");
PARAM_MATRIX_OUT("matrix_out", "Output matrix.", "M");
PARAM_UMATRIX_OUT("umatrix_out", "Output unsigned matrix.", "U");
PARAM_COL_OUT("col_out", "Output column. 2x input column", "");
PARAM_UCOL_OUT("ucol_out", "Output unsigned column. 2x input column.", "");
PARAM_ROW_OUT("row_out", "Output row.  2x input row.", "");
PARAM_UROW_OUT("urow_out", "Output unsigned row.  2x input row.", "");
PARAM_MATRIX_OUT("matrix_and_info_out", "Output matrix and info; all numeric "
    "elements multiplied by 3.", "");
PARAM_VECTOR_OUT(int, "vector_out", "Output vector.", "");
PARAM_VECTOR_OUT(string, "str_vector_out", "Output string vector.", "");
PARAM_MODEL_OUT(GaussianKernel, "model_out", "Output model, with twice the "
    "bandwidth.", "");
PARAM_DOUBLE_OUT("model_bw_out", "The bandwidth of the model.");

static void mlpackMain()
{
  const string s = CLI::GetParam<string>("string_in");
  const int i = CLI::GetParam<int>("int_in");
  const double d = CLI::GetParam<double>("double_in");

  CLI::GetParam<string>("string_out") = "wrong";
  CLI::GetParam<int>("int_out") = 11;
  CLI::GetParam<double>("double_out") = 3.0;

  // Check that everything is right on the input, and then set output
  // accordingly.
  if (!CLI::HasParam("flag2") && CLI::HasParam("flag1"))
  {
    if (s == "hello")
      CLI::GetParam<string>("string_out") = "hello2";

    if (i == 12)
      CLI::GetParam<int>("int_out") = 13;

    if (d == 4.0)
      CLI::GetParam<double>("double_out") = 5.0;
  }

  // Input matrices should be at least 5 rows; the 5th row will be dropped and
  // the 3rd row will be multiplied by two.
  if (CLI::HasParam("matrix_in"))
  {
    arma::mat out = move(CLI::GetParam<arma::mat>("matrix_in"));
    out.shed_row(4);
    out.row(2) *= 2.0;

    CLI::GetParam<arma::mat>("matrix_out") = move(out);
  }

  // Input matrices should be at least 5 rows; the 5th row will be dropped and
  // the 3rd row will be multiplied by two.
  if (CLI::HasParam("umatrix_in"))
  {
    arma::Mat<size_t> out =
        move(CLI::GetParam<arma::Mat<size_t>>("umatrix_in"));
    out.shed_row(4);
    out.row(2) *= 2;

    CLI::GetParam<arma::Mat<size_t>>("umatrix_out") = move(out);
  }

  // An input column or row should have all elements multiplied by two.
  if (CLI::HasParam("col_in"))
  {
    arma::vec out = move(CLI::GetParam<arma::vec>("col_in"));
    out *= 2.0;

    CLI::GetParam<arma::vec>("col_out") = move(out);
  }

  if (CLI::HasParam("ucol_in"))
  {
    arma::Col<size_t> out =
        move(CLI::GetParam<arma::Col<size_t>>("ucol_in"));
    out *= 2;

    CLI::GetParam<arma::Col<size_t>>("ucol_out") = move(out);
  }

  if (CLI::HasParam("row_in"))
  {
    arma::rowvec out = move(CLI::GetParam<arma::rowvec>("row_in"));
    out *= 2.0;

    CLI::GetParam<arma::rowvec>("row_out") = move(out);
  }

  if (CLI::HasParam("urow_in"))
  {
    arma::Row<size_t> out =
        move(CLI::GetParam<arma::Row<size_t>>("urow_in"));
    out *= 2;

    CLI::GetParam<arma::Row<size_t>>("urow_out") = move(out);
  }

  // Vector arguments should have the last element removed.
  if (CLI::HasParam("vector_in"))
  {
    vector<int> out = move(CLI::GetParam<vector<int>>("vector_in"));
    out.pop_back();

    CLI::GetParam<vector<int>>("vector_out") = move(out);
  }

  if (CLI::HasParam("str_vector_in"))
  {
    vector<string> out = move(CLI::GetParam<vector<string>>("str_vector_in"));
    out.pop_back();

    CLI::GetParam<vector<string>>("str_vector_out") = move(out);
  }

  // All numeric elements should be multiplied by 3.
  if (CLI::HasParam("matrix_and_info_in"))
  {
    typedef tuple<data::DatasetInfo, arma::mat> TupleType;
    TupleType tuple = move(CLI::GetParam<TupleType>("matrix_and_info_in"));

    const data::DatasetInfo& di = std::get<0>(tuple);
    arma::mat& m = std::get<1>(tuple);

    for (size_t i = 0; i < m.n_rows; ++i)
    {
      if (di.Type(i) == data::Datatype::numeric)
        m.row(i) *= 2.0;
    }

    CLI::GetParam<arma::mat>("matrix_and_info_out") = move(m);
  }

  // If we got a request to build a model, then build it.
  if (CLI::HasParam("build_model"))
  {
    CLI::GetParam<GaussianKernel*>("model_out") = new GaussianKernel(10.0);
  }

  // If we got an input model, double the bandwidth and output that.
  if (CLI::HasParam("model_in"))
  {
    CLI::GetParam<double>("model_bw_out") =
        CLI::GetParam<GaussianKernel*>("model_in")->Bandwidth() * 2.0;
  }
}
