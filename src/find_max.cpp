#include <Rcpp.h>

//' Find max
//' @name find_max
//' @param vect a
//' @return index of max

// [[Rcpp::export]]
int find_max(Rcpp::NumericVector vect) {
  int n = vect.size(), chosen;
  double curr, min = 0;

  // finding the highest value and getting the index
  for(int l = 0; l < n; l++){
    curr = vect[l];
    if(curr > min){
      min = curr;
      chosen = l;
    }
  }
  return(chosen);
}
