#include <Rcpp.h>

//' cpp_cycle_draw
//' @name cpp_cycle_draw
//' @param TIME
//' @param A
//' @param M
//' @param THETA
//' @param TAU
//' @return Vector pulled from
//' @details theta must remain proportional to TAU if the cycle is to remained
//'   sync with period. If not proportional the cycle with drift.

// [[Rcpp::export]]
double cpp_cycle_draw(double TIME, double A, double M, double THETA, double TAU) {

  double OUTPUT;
  double y, x, z, B;

  B = A * std::cos(THETA);
  y = -A * std::sin(THETA);
  x = std::cos(2 * M_PI * TIME / TAU);
  z = std::sin(2 * M_PI * TIME / TAU);
  OUTPUT = M + B*x + y*z;

  return(OUTPUT);
}
