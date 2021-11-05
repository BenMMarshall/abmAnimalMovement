// #include <R.h>
// #include <Rdefines.h>

#include <Rcpp.h>
using namespace Rcpp;

//' Basic random walk
//' @name walk_options_xy
//' @param startx The x coord start location
//' @param starty The y coord start location
//' @param steps The number of steps to be simulated
//' @param options The number of options to be considered at each step
//' @param normmean Parameter describing step length
//' @param normsd Parameter describing step angle
//' @param meanang Parameter describing angle
//' @param sdang Parameter describing angle variation
//' @return Matrix of locations chosen

// [[Rcpp::export]]
NumericMatrix walk_options_xy(
    double startx,
    double starty,
    int steps,
    int options,
    double normmean,
    double normsd,
    double meanang,
    double sdang
){

  int n = steps;
  int nopt = options;
  double angle;
  double step;
  int chosen;

  NumericMatrix optionsMatrix(nopt, 3);
  NumericMatrix locMatrix(steps, 3);

  locMatrix(0,0) = startx;
  locMatrix(0,1) = starty;
  for(int i = 1; i < n; ++i){

    optionsMatrix(0,0) = locMatrix(i-1,0);
    optionsMatrix(0,1) = locMatrix(i-1,1);
    for(int j = 1; j < nopt; ++j){
      angle = Rcpp::rnorm(1, meanang, sdang)[0] * PI / 180.0;
      step = Rcpp::rgamma(1, normmean, normsd)[0];
      optionsMatrix(j,0) = optionsMatrix(0,0) + cos(angle) * step;
      optionsMatrix(j,1) = optionsMatrix(0,1) + sin(angle) * step;
    }
    chosen = round(Rcpp::runif(1, 0, nopt-1)[0]);
    optionsMatrix(chosen,2) = 1;

    locMatrix(i,0) = optionsMatrix(chosen,0);
    locMatrix(i,1) = optionsMatrix(chosen,1);

  }
  return locMatrix;

}
