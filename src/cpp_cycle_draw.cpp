#include <Rcpp.h>

//' cpp_cycle_draw
//' @name cpp_cycle_draw
//' @param TIME A point in time (defined as a double) during the cycle to draw a value from.
//' @param A A double defining the amplitude of the cycle, calculated as the
//'   difference from M (i.e., half of the overall variation in the cycle from
//'   top to bottom).
//' @param M A double defining the cycle offset from 0 (Midline Statistic Of
//'   Rhythm, a rhythm-adjusted mean).
//' @param PHI A double defining the offset of the cycle (\eqn{\phi}; i.e.,
//'   acrophase). Must be kept proportional to \eqn{\tau} to avoid cycle drift.
//' @param TAU A double defining the cycle frequency (\eqn{\tau}; i.e., period),
//'   where \eqn{\tau} is the difference between peaks.
//' @return A double that is between M+A to M-A, draw from the defined cycle at TIME.
//' @details \eqn{\phi} must remain proportional to \eqn{\tau} if the cycle is to remained
//'   sync with period. If not proportional the cycle with drift.
//' @references Cornelissen, G. (2014). Cosinor-based rhythmometry. Theoretical
//'    Biology and Medical Modelling. 11(16)
//'    https://doi.org/10.1186/1742-4682-11-16

// [[Rcpp::export]]
double cpp_cycle_draw(double TIME, double A, double M, double PHI, double TAU) {

  double OUTPUT;
  double y, x, z, B;

  B = A * std::cos(PHI);
  y = -A * std::sin(PHI);
  x = std::cos(2 * M_PI * TIME / TAU);
  z = std::sin(2 * M_PI * TIME / TAU);
  OUTPUT = M + B*x + y*z;

  return(OUTPUT);
}
