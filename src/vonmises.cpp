// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <boost/math/special_functions/bessel.hpp>
//' vonmises
//' https://statisticaloddsandends.wordpress.com/2020/02/25/what-is-the-von-mises-distribution/
//' https://gist.github.com/SiyuanQi/10ed29aa850e26e2e1251af04f5427ec
//' http://dirk.eddelbuettel.com/code/bh.html
//' https://garylarson.weebly.com/blog/category/rcpp
//' @name vonmises
//' @param N number to draw
//' @param kappa
//' @param mu
//' @return Vector pulled from vonmises
// [[Rcpp::export]]
Rcpp::NumericVector vonmises(int N, double kappa, double mu) {
  // double kappa = 78.8162358257077, mu = -0.00203973270978115;
  double x = 0;
  double bessel;
  bessel = boost::math::cyl_bessel_i(0, kappa);
  int n = N;
  Rcpp::NumericVector OUTPUT(n);

  for(int i = 0; i < n; i++){
    // M_PI is a macro for pi
    // OUTPUT[i] = exp(kappa*cos(i-mu))/(2*M_PI*bessel);
    OUTPUT[i] = exp(kappa*cos(x-mu))/(2*M_PI*bessel);
  }
  return(OUTPUT);
}
