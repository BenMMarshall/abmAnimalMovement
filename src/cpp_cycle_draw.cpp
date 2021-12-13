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
std::vector<double> cpp_cycle_draw(std::vector<double> TIME, double A, double M, double THETA, double TAU) {

  int N = TIME.size();
  std::vector<double> OUTPUT(N);
  double y, x, z, B;

  for(int draw = 0; draw < N; draw++){
    B = A * std::cos(THETA);
    y = -A * std::sin(THETA);
    x = std::cos(2 * M_PI * TIME[draw] / TAU);
    z = std::sin(2 * M_PI * TIME[draw] / TAU);
    OUTPUT[draw] = M + B*x + y*z;
  }

  return(OUTPUT);
}
