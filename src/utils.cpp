#include "RcppArmadillo.h"
#include "../inst/include/Countr_types.h"

using namespace std;
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

// Frank Bivariate Copula
double BivariateFrankCopula(double u, double v, double theta) {
  if (theta == 0)
    return (u * v);
  else
    return(log( 1 + ((exp(theta * u) - 1) * (exp(theta * v) - 1) ) /
		(exp(theta) - 1) ) / theta);
}

arma::Col <unsigned> seq0(unsigned to, unsigned by) {
  // unsigned from = 0;
  unsigned length = to + 1;

  arma::Col <unsigned> res (length, arma::fill::zeros);
  for (unsigned i = 0; i < length ; i ++)
    res(i) =  i*by;

  return(res);
}
