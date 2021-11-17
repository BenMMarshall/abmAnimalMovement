#include <Rcpp.h>

//' sample_test
//' @name sample_test
//' @param weights probs
//' @return Matrix of locations chosen

// [[Rcpp::export]]
int sample_test(Rcpp::NumericVector weights){

  int N = weights.size();
  // std::vector<double> w(N);

  std::vector<int> num_choices(N) ; // vector with N ints.
  std::iota(std::begin(num_choices), std::end(num_choices), 0); // Fill with 0, 1, ..., 99.

  int chosen;
  int size = 1;
  bool replace = false;

  // converting the vector to a numericVector
  Rcpp::NumericVector w(N);
  for(int i = 0; i < N; i++){
    w[i] = weights[i];
  }

  // ok we need to make sure whatever weighting input is a NumericVector
  chosen = Rcpp::as<int>(Rcpp::sample(N, size, replace, w));

  return chosen;
}
