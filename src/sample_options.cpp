#include <Rcpp.h>

//' sample_options
//' possible alternatives
//' https://stackoverflow.com/questions/57599509/c-random-non-repeated-integers-with-weights
//' https://stackoverflow.com/questions/57599509/c-random-non-repeated-integers-with-weights
//' @name sample_options
//' @param weights probs
//' @return Matrix of locations chosen

// [[Rcpp::export]]
int sample_options(Rcpp::NumericVector weights){

  int N = weights.size();
  // redefine the inut numericvector as a c++ vector for testing purposes
  // std::vector<double> w(N);
  // for(int i = 0; i <= N; i++){
  //   w[i] = weights[i];
  // }

  std::vector<int> num_choices(N) ; // vector with N integers.
  std::iota(std::begin(num_choices), std::end(num_choices), 0); // Fill with 0, 1, ..., 99.

  int chosen;
  // int size = 1;
  // bool replace = false;

  // ok we need to make sure whatever weighting input is a NumericVector
  // we can use wrap to convert the vector to a numericVector
  chosen = Rcpp::as<int>(Rcpp::sample(N, 1, false, weights));

  return chosen;
}
