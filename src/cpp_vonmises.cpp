// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/special_functions/bessel.hpp>
#include <cmath>

//' @name cpp_vonmises
//' @title cpp_vonmises
//' @description A C++ conversion of the *rvm* function provided by the [CircStats package](https://cran.r-project.org/web/packages/CircStats/index.html).
//' @param N of values to draw.
//' @param MU The mean of the distribution (\eqn{\mu}).
//' @param KAPPA The concentration of the distribution (\eqn{\kappa}).
//' @return A vector drawn from a von Mises distribution equal in length to N.
//'
//' @references
//' S-plus original by Ulric Lund and R port by Claudio Agostinelli (2018). CircStats: Circular Statistics, from Topics in Circular Statistics (2001). R package version 0.2-6. https://CRAN.R-project.org/package=CircStats
//'
// [[Rcpp::export]]
std::vector<double> cpp_vonmises(int N, double MU, double KAPPA) {

  int n = N;
  double mu = MU;
  double k = KAPPA;

  // Rcpp::NumericVector OUTPUT(n);
  std::vector<double> OUTPUT(n);

  // variables directly lifted from CircStats::rvm
  double a, b, r, U1, z, f, c, U2, U3;

  a = 1.0 + std::pow( (1.0 + 4.0 * std::pow(k,2.0)), 0.5);
  b = (a - std::pow((2.0 * a), 0.5))/(2.0 * k);
  r = (1.0 + std::pow(b,2) )/(2.0 * b);

  // added in varaibles to dodge sign() and attempt to make ambiguity in fmod()
  double sign, val;

  for(int obs = 0; obs < n; obs++){
    U1 = Rcpp::runif(1, 0, 1)[0];
    z = std::cos(M_PI * U1);
    f = (1.0 + r * z)/(r + z);
    c = k * (r - f);
    U2 = Rcpp::runif(1, 0, 1)[0];
    if (c * (2 - c) - U2 > 0) {

      U3 = Rcpp::runif(1, 0, 1)[0];

      // sign function wasn't working, alt long form fix
      if ((U3 - 0.5) > 0) {
        sign = 1.0;
      } else if ((U3 - 0.5) == 0) {
        sign = 0.0;
      } else {
        sign = -1.0;
      }

      val = sign * std::acos(f) + mu;
      OUTPUT[obs] = std::fmod(val, (2 * M_PI));

    } else if ( std::log(c/U2) + 1 - c >= 0) {

      U3 = Rcpp::runif(1, 0, 1)[0];

      if ((U3 - 0.5) > 0) {
        sign = 1;
      } else if ((U3 - 0.5) == 0) {
        sign = 0;
      } else {
        sign = -1;
      }

      val = sign * std::acos(f) + mu;
      OUTPUT[obs] = std::fmod(val, (2 * M_PI));
    }

  }

  return(OUTPUT);
}
