#include <Rcpp.h>

//' sample_test
//' @name sample_test
//' @param vect vector length
//' @return Matrix of locations chosen

// [[Rcpp::export]]
int sample_test(Rcpp::NumericVector vect){
  int chosen;
  chosen = Rcpp::as<int>(Rcpp::sample(vect, 1, false));
  return chosen;
}
