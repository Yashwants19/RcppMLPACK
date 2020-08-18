/**
 * @file methods/dbscan/dbscan_main.cpp
 * @author Ryan Curtin
 *
 * Implementation of program to run DBSCAN.
 *
 * mlpack is free software; you may redistribute it and/or modify it under the
 * terms of the 3-clause BSD license.  You should have received a copy of the
 * 3-clause BSD license along with mlpack.  If not, see
 * http://www.opensource.org/licenses/BSD-3-Clause for more information.
 */
#include <mlpack/prereqs.hpp>
#include <mlpack/core/util/io.hpp>
#include <mlpack/core/util/mlpack_main.hpp>
#include <mlpack/core/tree/binary_space_tree.hpp>
#include <mlpack/core/tree/rectangle_tree.hpp>
#include <mlpack/core/tree/cover_tree.hpp>
#include <mlpack/methods/dbscan/random_point_selection.hpp>
#include <mlpack/methods/dbscan/ordered_point_selection.hpp>
#include "dbscan.hpp"

using namespace mlpack;
using namespace mlpack::range;
using namespace mlpack::dbscan;
using namespace mlpack::metric;
using namespace mlpack::tree;
using namespace mlpack::util;
using namespace std;

PROGRAM_INFO("DBSCAN clustering",
    // Short description.
    "An implementation of DBSCAN clustering.  Given a dataset, this can "
    "compute and return a clustering of that dataset.",
    // Long description.
    "This program implements the DBSCAN algorithm for clustering using "
    "accelerated tree-based range search.  The type of tree that is used "
    "may be parameterized, or brute-force range search may also be used."
    "\n\n"
    "The input dataset to be clustered may be specified with the " +
    PRINT_PARAM_STRING("input") + " parameter; the radius of each range "
    "search may be specified with the " + PRINT_PARAM_STRING("epsilon") +
    " parameters, and the minimum number of points in a cluster may be "
    "specified with the " + PRINT_PARAM_STRING("min_size") + " parameter."
    "\n\n"
    "The " + PRINT_PARAM_STRING("assignments") + " and " +
    PRINT_PARAM_STRING("centroids") + " output parameters may be "
    "used to save the output of the clustering. " +
    PRINT_PARAM_STRING("assignments") + " contains the cluster assignments of "
    "each point, and " + PRINT_PARAM_STRING("centroids") + " contains the "
    "centroids of each cluster."
    "\n\n"
    "The range search may be controlled with the " +
    PRINT_PARAM_STRING("tree_type") + ", " +
    PRINT_PARAM_STRING("single_mode") + ", and " +
    PRINT_PARAM_STRING("naive") + " parameters.  " +
    PRINT_PARAM_STRING("tree_type") + " can control the type of tree used for "
    "range search; this can take a variety of values: 'kd', 'r', 'r-star', 'x',"
    " 'hilbert-r', 'r-plus', 'r-plus-plus', 'cover', 'ball'. The " +
    PRINT_PARAM_STRING("single_mode") + " parameter will force single-tree "
    "search (as opposed to the default dual-tree search), and '" +
    PRINT_PARAM_STRING("naive") + " will force brute-force range search."
    "\n\n"
    "An example usage to run DBSCAN on the dataset in " +
    PRINT_DATASET("input") + " with a radius of 0.5 and a minimum cluster size"
    " of 5 is given below:"
    "\n\n" +
    PRINT_CALL("dbscan", "input", "input", "epsilon", 0.5, "min_size", 5),
    SEE_ALSO("DBSCAN on Wikipedia", "https://en.wikipedia.org/wiki/DBSCAN"),
    SEE_ALSO("A density-based algorithm for discovering clusters in large "
        "spatial databases with noise (pdf)",
        "http://www.aaai.org/Papers/KDD/1996/KDD96-037.pdf"),
    SEE_ALSO("mlpack::dbscan::DBSCAN class documentation",
        "@doxygen/classmlpack_1_1dbscan_1_1DBSCAN.html"));

PARAM_MATRIX_IN_REQ("input", "Input dataset to cluster.", "i");
PARAM_UROW_OUT("assignments", "Output matrix for assignments of each "
    "point.", "a");
PARAM_MATRIX_OUT("centroids", "Matrix to save output centroids to.", "C");

PARAM_DOUBLE_IN("epsilon", "Radius of each range search.", "e", 1.0);
PARAM_INT_IN("min_size", "Minimum number of points for a cluster.", "m", 5);

PARAM_STRING_IN("tree_type", "If using single-tree or dual-tree search, the "
    "type of tree to use ('kd', 'r', 'r-star', 'x', 'hilbert-r', 'r-plus', "
    "'r-plus-plus', 'cover', 'ball').", "t", "kd");
PARAM_STRING_IN("selection_type", "If using point selection policy, the "
    "type of selection to use ('ordered', 'random').", "s", "ordered");
PARAM_FLAG("single_mode", "If set, single-tree range search (not dual-tree) "
    "will be used.", "S");
PARAM_FLAG("naive", "If set, brute-force range search (not tree-based) "
    "will be used.", "N");

// Actually run the clustering, and process the output.
template<typename RangeSearchType, typename PointSelectionPolicy>
void RunDBSCAN(RangeSearchType rs,
               PointSelectionPolicy pointSelector = PointSelectionPolicy())
{
  if (IO::HasParam("single_mode"))
    rs.SingleMode() = true;

  // Load dataset.
  arma::mat dataset = std::move(IO::GetParam<arma::mat>("input"));
  const double epsilon = IO::GetParam<double>("epsilon");
  const size_t minSize = (size_t) IO::GetParam<int>("min_size");
  arma::Row<size_t> assignments;

  DBSCAN<RangeSearchType, PointSelectionPolicy> d(epsilon, minSize,
      !IO::HasParam("single_mode"), rs, pointSelector);

  // If possible, avoid the overhead of calculating centroids.
  if (IO::HasParam("centroids"))
  {
    arma::mat centroids;

    d.Cluster(dataset, assignments, centroids);

    IO::GetParam<arma::mat>("centroids") = std::move(centroids);
  }
  else
  {
    d.Cluster(dataset, assignments);
  }

  if (IO::HasParam("assignments"))
    IO::GetParam<arma::Row<size_t>>("assignments") = std::move(assignments);
}

// Choose the point selection policy.
template<typename RangeSearchType>
void ChoosePointSelectionPolicy(RangeSearchType rs = RangeSearchType())
{
  const string selectionType = IO::GetParam<string>("selection_type");

  if (selectionType == "ordered")
    RunDBSCAN<RangeSearchType, OrderedPointSelection>(rs);
  else if (selectionType == "random")
    RunDBSCAN<RangeSearchType, RandomPointSelection>(rs);
}

static void mlpackMain()
{
  RequireAtLeastOnePassed({ "assignments", "centroids" }, false,
      "no output will be saved");

  ReportIgnoredParam({{ "naive", true }}, "single_mode");

  RequireParamInSet<string>("tree_type", { "kd", "cover", "r", "r-star", "x",
      "hilbert-r", "r-plus", "r-plus-plus", "ball" }, true,
      "unknown tree type");

  // Value of epsilon should be positive.
  RequireParamValue<double>("epsilon", [](double x) { return x > 0; },
      true, "invalid value of epsilon specified");

  // Value of min_size should be positive.
  RequireParamValue<int>("min_size", [](int y) { return y > 0; },
      true, "invalid value of min_size specified");

  // Fire off naive search if needed.
  if (IO::HasParam("naive"))
  {
    RangeSearch<> rs(true);
    ChoosePointSelectionPolicy(rs);
  }
  else
  {
    const string treeType = IO::GetParam<string>("tree_type");
    if (treeType == "kd")
    {
      ChoosePointSelectionPolicy<RangeSearch<>>();
    }
    else if (treeType == "cover")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          StandardCoverTree>>();
    }
    else if (treeType == "r")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          RTree>>();
    }
    else if (treeType == "r-star")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          RStarTree>>();
    }
    else if (treeType == "x")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          XTree>>();
    }
    else if (treeType == "hilbert-r")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          HilbertRTree>>();
    }
    else if (treeType == "r-plus")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          RPlusTree>>();
    }
    else if (treeType == "r-plus-plus")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          RPlusPlusTree>>();
    }
    else if (treeType == "ball")
    {
      ChoosePointSelectionPolicy<RangeSearch<EuclideanDistance, arma::mat,
          BallTree>>();
    }
  }
}
