#include <Rcpp.h>

//' Get maximum or minimum value
//' @name cpp_max/cpp_min
//' @param x A vector of doubles to the max/min found.
//' @return The maximum or minimum value in the vector.

// [[Rcpp::export]]
double cpp_max(std::vector<double> x) {
  int n = x.size();
  double c_max = x[0];

  for(int l = 0; l < n; l++){
    if(x[l] > c_max){
      c_max = x[l];
    }
  }
  return(c_max);
}

// [[Rcpp::export]]
double cpp_min(std::vector<double> x) {
  int n = x.size();
  double c_min = x[0];

  for(int l = 0; l < n; l++){
    if(x[l] < c_min){
      c_min = x[l];
    }
  }
  return(c_min);
}
