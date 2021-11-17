// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/special_functions/bessel.hpp>
#include <cmath>

//' vonmises
//' https://statisticaloddsandends.wordpress.com/2020/02/25/what-is-the-von-mises-distribution/
//' https://gist.github.com/SiyuanQi/10ed29aa850e26e2e1251af04f5427ec
//' http://dirk.eddelbuettel.com/code/bh.html
//' https://garylarson.weebly.com/blog/category/rcpp
//' @name vonmises
//' @param N number to draw
//' @param KAPPA
//' @param MU
//' @return Vector pulled from vonmises
// [[Rcpp::export]]
Rcpp::NumericVector vonmises(int N, double MU, double KAPPA) {
  // Rcpp::NumericVector vonmises(int N, double k, double mu) {
  // // double kappa = 78.8162358257077, mu = -0.00203973270978115;
  // double x = 0;
  // double bessel;
  // bessel = boost::math::cyl_bessel_i(0, kappa);
  // int n = N;
  // Rcpp::NumericVector OUTPUT(n);
  //
  // for(int i = 0; i < n; i++){
  //   // M_PI is a macro for pi
  //   OUTPUT[i] = exp(kappa*cos(i-mu))/(2*M_PI*bessel);
  //   // OUTPUT[i] = exp(kappa*cos(x-mu))/(2*M_PI*bessel);
  // }

  int n = N;
  double mu = MU;
  double k = KAPPA;

  Rcpp::NumericVector OUTPUT(n);

  // vm <- c(1:n);
  double a;
  double b;
  double r;
  int obs(n);

  a = 1 + pow( (1 + 4 * pow(k,2)), 0.5);
  b = (a - pow((2 * a),0.5))/(2 * k);
  r = (1 + pow(b,2) )/(2 * b);

  double U1, z, f, c, U2, U3;
  double sign, val;

  for(int obs = 0; obs <= n; obs++){
    U1 = Rcpp::runif(1, 0, 1)[0];
    z = cos(M_PI * U1);
    f = (1 + r * z)/(r + z);
    c = k * (r - f);
    U2 = Rcpp::runif(1, 0, 1)[0];
    if (c * (2 - c) - U2 > 0) {
      U3 = Rcpp::runif(1, 0, 1)[0];

      // sign function wasn't working, alt long form fix
      if ((U3 - 0.5) > 0) {
        sign = 1;
      } else if ((U3 - 0.5) == 0) {
        sign = 0;
      } else {
        sign = -1;
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

  // while (obs <= n) {
  //   U1 <- runif(1, 0, 1)
  //   z <- cos(pi * U1)
  //   f <- (1 + r * z)/(r + z)
  //   c <- k * (r - f)
  //   U2 <- runif(1, 0, 1)
  //   if (c * (2 - c) - U2 > 0) {
  //     U3 <- runif(1, 0, 1)
  //     vm[obs] <- sign(U3 - 0.5) * acos(f) + mu
  //     vm[obs] <- vm[obs]%%(2 * pi)
  //     obs <- obs + 1
  //   }
  //   else {
  //     if (log(c/U2) + 1 - c >= 0) {
  //       U3 <- runif(1, 0, 1)
  //       vm[obs] <- sign(U3 - 0.5) * acos(f) + mu
  //       vm[obs] <- vm[obs]%%(2 * pi)
  //       obs <- obs + 1
  //     }
  //   }
  // }

  return(OUTPUT);
}
