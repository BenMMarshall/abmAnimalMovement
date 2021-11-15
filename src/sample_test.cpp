#include <Rcpp.h>

//' sample_test
//' @name sample_test
//' @param vect vector length
//' @param probs probs
//' @return Matrix of locations chosen

// [[Rcpp::export]]
int sample_test(Rcpp::NumericVector vect, Rcpp::NumericVector probs){
  int chosen;
  chosen = Rcpp::as<int>(Rcpp::sample(vect, 1, false, probs));
  return chosen;
}
