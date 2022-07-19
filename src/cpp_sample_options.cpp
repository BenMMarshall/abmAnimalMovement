
#include <Rcpp.h>
#include <random>
#include "cpp_maxmin.h"
#define pow2(n) ( 1 << (n) )

//' @title cpp_sample_options
//' @name cpp_sample_options
//' @description The C++ sample function that takes a vector of weights and
//'   randomly returns an integer of the choice using the Rcpp version of
//'   sample().
//'
//' @param W A vector of weights.
//' @details Very simple wrapper to help handle std::vectors, and pre-normalise
//'   the values provided to avoid issues with negative numbers.
//' @return An integer corresponding to the chosen weight in the initially
//'   provided vector. __NOTE: indexing begins at 0 matching C++ convention__.

// [[Rcpp::export]]
int cpp_sample_options(std::vector<double> W){

  int n = W.size();

  double W_min = W[0];
  W_min = cpp_min(W);
  // find MAX
  double W_max = W[0];
  W_max = cpp_max(W);

  if(W_min == W_max){ // if there is no variation in the weights set them all to one
    for(int j = 0; j < n; j++){
      W[j] = 1;
    }
  } else {
    for(int i = 0; i < n; i++){
      W[i] = ((W[i] - W_min) /
        (W_max - W_min));
      // Rcpp::Rcout << "W " << W[i] << " ";
    }
    // Rcpp::Rcout << "\n";
  }

  Rcpp::NumericVector weights = Rcpp::wrap(W);
  int out;
  out = Rcpp::sample(n,
                     1,
                     FALSE,
                     weights)[0];
  return out-1;

}
