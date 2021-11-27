// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/special_functions/bessel.hpp>
#include <cmath>

//' vonmises
//' Based on function fvm from teh CricStats package.
//' @name vonmises
//' @param N number to draw
//' @param KAPPA
//' @param MU
//' @return Vector pulled from vonmises

// [[Rcpp::export]]
Rcpp::NumericVector vonmises(int N, double MU, double KAPPA) {

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

  for(int obs = 0; obs <= n; obs++){
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
  // wrap required to convert Cpp vector to Rcpp NumericVector
  return(Rcpp::wrap(OUTPUT));
}
