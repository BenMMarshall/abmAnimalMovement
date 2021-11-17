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

  Rcpp::NumericVector OUTPUT(n);

  // vm <- c(1:n);
  double a;
  double b;
  double r;

  a = 1.0 + pow( (1.0 + 4.0 * pow(k,2.0)), 0.5);
  b = (a - pow((2.0 * a),0.5))/(2.0 * k);
  r = (1.0 + pow(b,2) )/(2.0 * b);

  double U1, z, f, c, U2, U3;
  double sign, val;

  for(int obs = 0; obs <= n; obs++){
    U1 = Rcpp::runif(1, 0, 1)[0];
    z = cos(M_PI * U1);
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

      val = sign * acos(f) + mu;
      OUTPUT[obs] = fmod(val, (2 * M_PI));

    } else if ( log(c/U2) + 1 - c >= 0) {
      U3 = Rcpp::runif(1, 0, 1)[0];

      if ((U3 - 0.5) > 0) {
        sign = 1;
      } else if ((U3 - 0.5) == 0) {
        sign = 0;
      } else {
        sign = -1;
      }

      val = sign * acos(f) + mu;
      OUTPUT[obs] = fmod(val, (2 * M_PI));
    }

  }

  return(OUTPUT);
}
