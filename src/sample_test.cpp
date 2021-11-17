#include <Rcpp.h>

//' sample_test
//' @name sample_test
//' @param weights probs
//' @return Matrix of locations chosen

// [[Rcpp::export]]
int sample_test(Rcpp::NumericVector weights){

  int N = weights.size();
  // redefine the inut numericvector as a c++ vector for testing purposes
  std::vector<double> w(N);
  for(int i = 0; i < N; i++){
    w[i] = weights[i];
  }

  std::vector<int> num_choices(N) ; // vector with N ints.
  std::iota(std::begin(num_choices), std::end(num_choices), 0); // Fill with 0, 1, ..., 99.

  int chosen;
  // int size = 1;
  // bool replace = false;

  // ok we need to make sure whatever weighting input is a NumericVector
  // we can use wrap to convert the vector to a numericVector
  chosen = Rcpp::as<int>(Rcpp::sample(N, 1, false, Rcpp::wrap(w)));

  return chosen;
}
