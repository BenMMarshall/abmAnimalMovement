
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
  Rcpp::NumericVector envVal1(nopt);

  Rcpp::NumericVector choicesVec(nopt);
  // std::vector<int> choicesVec(nopt);
  // std::iota (std::begin(choicesVec), std::end(choicesVec), 0); // Fill with 0, 1, ..., end.

  int xOpt;
  int yOpt;
  int xOptIndex;
  int yOptIndex;

  Rcpp::NumericMatrix optionsMatrix(nopt, 5);
  Rcpp::NumericMatrix optionsMatrixALL(nopt*steps +1, 3); // one added to include the start loc
  Rcpp::NumericMatrix locMatrix(steps, 2);

  /* initial location is set using the start locations */
  locMatrix(0,0) = startx;
  locMatrix(0,1) = starty;
  optionsMatrixALL(0,0) = startx;
  optionsMatrixALL(0,1) = starty;
  optionsMatrixALL(0,2) = 0;
  for(int i = 1, a = 1; i <= n; i++){

    /* for each step set the location as the previously chosen location */
    optionsMatrix(0,0) = locMatrix(i-1,0);
    optionsMatrix(0,1) = locMatrix(i-1,1);
    optionsMatrix(0,3) = i;
    for(int j = 0; j < nopt; j++, a++){
      angle = Rcpp::rnorm(1, meanang, sdang)[0] * PI / 180.0;
      step = Rcpp::rgamma(1, normmean, normsd)[0];
      optionsMatrix(j,0) = optionsMatrix(0,0) + cos(angle) * step;
      optionsMatrix(j,1) = optionsMatrix(0,1) + sin(angle) * step;
      // add in which step the options are for
      optionsMatrix(j,3) = i+1;

      optionsMatrixALL(a,0) = optionsMatrix(j,0);
      optionsMatrixALL(a,1) = optionsMatrix(j,1);
      optionsMatrixALL(a,2) = i;

      choicesVec[j] = j;
    }
    /* for each of the options, check values in environment and use equation to pick next move */
    for(int k = 0; k < nopt; k++){
      xOpt = optionsMatrix(k,0);
      yOpt = optionsMatrix(k,1);
      xOptIndex = floor(xOpt);
      yOptIndex = floor(yOpt);
      envVal1[k] = envMat1(xOptIndex, yOptIndex);
      // store it with the options also
      optionsMatrix(k,4) = envVal1[k];
    }

    /* Choices to sample from ample data, there is a Rcpp sugar function sample that could help
     Rcpp::sample(choicesVec, 1, false, envVal1) */
    // for now we will just use the maximum value is chosen for testing purposes
    // chosen = Rcpp::which_max(envVal1);
    // optionsMatrix(chosen,2) = 1;

    // old uniform choice doesn't use any environmental input and can pick multiple new locations
    // chosen = round(Rcpp::runif(1, 0, nopt-1)[0]);
    // optionsMatrix(chosen,2) = 1;

    chosen = Rcpp::as<int>(Rcpp::sample(choicesVec, 1, false, envVal1));
    /* this is cjust recording the choice in the first possible location of the optionsMatrix,
    not great would rather store 0 and 1 with the location */
    optionsMatrix(0,2) = chosen;
    /* this doesn't seem to work, instead filling an apparent random number of rows with 1 */
    /* maybe we need to reset the 3 column each time in the loop */
    // optionsMatrix(chosen,2) = 1;

    xChosen = optionsMatrix(chosen,0);
    yChosen = optionsMatrix(chosen,1);
    locMatrix(i,0) = xChosen;
    locMatrix(i,1) = yChosen;

  }
  Rcpp::List OUTPUT = Rcpp::List::create(Rcpp::Named("Locations") = locMatrix,
                                         Rcpp::Named("OptionsLast") = optionsMatrix,
                                         Rcpp::Named("OptionsAll") = optionsMatrixALL,
                                         Rcpp::Named("ChoiceVec") = choicesVec, // included to check is choice vector is the source of issues
                                         Rcpp::Named("envValues") = envVal1 // included to check probs used
                                         );
  return OUTPUT;

}
