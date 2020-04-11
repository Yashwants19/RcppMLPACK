#include <RcppMLPACK.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/pca/pca_main.cpp>

using namespace mlpack;
using namespace mlpack::util;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
void pca_mlpackMain()
{
  mlpackMain();
}

