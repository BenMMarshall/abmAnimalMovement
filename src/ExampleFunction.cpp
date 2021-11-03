#include <Rcpp.h>
using namespace Rcpp;

//' Multiplies two doubles
//'
//' @param v1 First value
//' @param v2 Second value
//' @return Product of v1 and v2
// [[Rcpp::export]]
double mult( double v1, double v2 ) {return v1*v2;}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
