
#include <Rcpp.h>
#include <cmath>

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
//' @param envMat1 Environmental matrix 1
//' @return Matrix of locations chosen

// [[Rcpp::export]]
Rcpp::List walk_options_xy(
    double startx,
    double starty,
    int steps,
    int options,
    double normmean,
    double normsd,
    double meanang,
    double sdang,
    Rcpp::NumericMatrix envMat1
){

  int n = steps;
  int nopt = options;
  double angle;
  double step;
  int chosen;
  double xChosen;
  double yChosen;
  // NumericVector envVal1;

  Rcpp::NumericVector choicesVec(nopt);
  // std::vector<int> choicesVec(nopt);
  // std::iota (std::begin(choicesVec), std::end(choicesVec), 0); // Fill with 0, 1, ..., end.

  // int xOpt;
  // int yOpt;
  // int xOptIndex;
  // int yOptIndex;

  Rcpp::NumericMatrix optionsMatrix(nopt, 4);
  Rcpp::NumericMatrix locMatrix(steps, 2);

  /* initial location is set using the start locations */
  locMatrix(0,0) = startx;
  locMatrix(0,1) = starty;
  for(int i = 1; i < n; i++){

    /* for each step set the location as the previously chosen location */
    optionsMatrix(0,0) = locMatrix(i-1,0);
    optionsMatrix(0,1) = locMatrix(i-1,1);
    optionsMatrix(0,3) = i;
    for(int j = 0; j < nopt; j++){
      angle = Rcpp::rnorm(1, meanang, sdang)[0] * PI / 180.0;
      step = Rcpp::rgamma(1, normmean, normsd)[0];
      optionsMatrix(j,0) = optionsMatrix(0,0) + cos(angle) * step;
      optionsMatrix(j,1) = optionsMatrix(0,1) + sin(angle) * step;
      // add in which step the options are for
      optionsMatrix(j,3) = i+1;

      choicesVec[j] = j;
    }
    /* for each of the options, check values in environment and use equation to pick next move */
    // for(int k = 0; k < nopt; k++){
    //   choicesVec[k] = k;
    //   xOpt = optionsMatrix(k,0);
    //   yOpt = optionsMatrix(k,1);
    //   xOptIndex = floor(xOpt);
    //   yOptIndex = floor(yOpt);
    //   envVal1[k] = envMat1(xOptIndex, yOptIndex);
    // }

    /* Choices to sample from ample data, there is a Rcpp sugar function sample that could help
     Rcpp::sample(choicesVec, 1, false, envVal1) */
    // for now we will just use the maximum value is chosen for testing purposes
    // chosen = Rcpp::which_max(envVal1);
    // optionsMatrix(chosen,2) = 1;

    // old uniform choice doesn't use any environmental input and can pick multiple new locations
    // chosen = round(Rcpp::runif(1, 0, nopt-1)[0]);
    // optionsMatrix(chosen,2) = 1;

    chosen = Rcpp::as<int>(Rcpp::sample(choicesVec, 1, false, R_NilValue));
    optionsMatrix(0,2) = chosen;

    xChosen = optionsMatrix(chosen,0);
    yChosen = optionsMatrix(chosen,1);
    locMatrix(i,0) = xChosen;
    locMatrix(i,1) = yChosen;

  }
  Rcpp::List OUTPUT = Rcpp::List::create(Rcpp::Named("Locations") = locMatrix,
                                         Rcpp::Named("Options") = optionsMatrix,
                                         Rcpp::Named("ChoiceVec") = choicesVec
                                         );
  return OUTPUT;

}
