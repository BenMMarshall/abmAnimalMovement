
#include <Rcpp.h>
#include <cmath>
#include "vonmises.h"

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
    double mu_angle,
    double k_angle,
    Rcpp::NumericMatrix envMat1
){

  int n = steps;
  int nopt = options;
  double angle;
  double step;
  int chosen;

  int mcols = envMat1.ncol();
  int mrows = envMat1.nrow();

  // Rcpp::NumericVector envVal1(nopt);
  std::vector<double> envVal1(nopt);

  // Rcpp::NumericVector choicesVec(nopt);
  std::vector<int> choicesVec(nopt);
  // std::iota (std::begin(choicesVec), std::end(choicesVec), 0); // Fill with 0, 1, ..., end.

  int xOpt;
  int yOpt;
  int xOptIndex;
  int yOptIndex;

  // the options stores for the loop
  // Rcpp::NumericMatrix optionsMatrix(nopt, 5);
  std::vector<double> x_Options(nopt);
  std::vector<double> y_Options(nopt);
  std::vector<int> step_Options(nopt);
  std::vector<double> enVal1_Options(nopt);

  // store the chose at each step
  std::vector<int> chosen_Options(steps);

  // the options stores for the output including all options
  // Rcpp::NumericMatrix optionsMatrixALL(nopt*steps +1, 3); // one added to include the start loc
  std::vector<double> x_OptionsAll(nopt*steps +1);
  std::vector<double> y_OptionsAll(nopt*steps +1);
  std::vector<int> step_OptionsAll(nopt*steps +1);

  //
  // Rcpp::NumericMatrix locMatrix(steps, 2);
  std::vector<double> x_Locations(steps);
  std::vector<double> y_Locations(steps);
  std::vector<int> step_Locations(steps);

  /* initial location is set using the start locations */
  x_Locations[0] = startx;
  y_Locations[0] = starty;
  x_OptionsAll[0] = startx;
  y_OptionsAll[0] = starty;
  step_OptionsAll[0] = 0;
  for(int i = 1, a = 1; i <= n; i++){
    Rcpp::Rcout << "Step : " << i << "\n";

    /* for each step set the location as the previously chosen location */
    x_Options[0] = x_Locations[i-1];
    y_Options[0] = y_Locations[i-1];
    step_Options[0] = i;
    for(int j = 0; j < nopt; j++, a++){

      // angle = Rcpp::rnorm(1, meanang, sdang)[0] * PI / 180.0;
      angle = vonmises(1, mu_angle, k_angle)[0] * 180/M_PI;
      step = Rcpp::rgamma(1, normmean, normsd)[0];

      x_Options[j] = x_Options[0] + cos(angle) * step;
      y_Options[j] = y_Options[0] + sin(angle) * step;

      // add in which step the options are for
      step_Options[j] = i+1;

      // a is keeping tracking of the position in a ong vector steps*nopts
      x_OptionsAll[a] = x_Options[j];
      y_OptionsAll[a] = y_Options[j];
      step_OptionsAll[a] = i;

      // choice vector is needed for the sample function later on
      choicesVec[j] = j;
    }
    /* for each of the options, check values in environment and use equation to pick next move */
    for(int k = 0; k < nopt; k++){

      xOpt = x_Options[k];
      yOpt = y_Options[k];
      // rounding the locations to correspond to matrix location
      xOptIndex = floor(xOpt);
      yOptIndex = floor(yOpt);

      // end function if animal leaves environmental data area
      if( (xOptIndex > mcols) | (yOptIndex > mrows) ){
        Rcpp::Rcerr << "Exceeding background environmental limits or NA in enviornmental data\n";
      }

      // still using the numericMatrix Rcpp form here
      envVal1[k] = envMat1(xOptIndex, yOptIndex);

      if(Rcpp::NumericVector::is_na(envVal1[k])){
        // printing error message
        Rcpp::Rcerr << "NA in enviornmental data\n";
      }

      // store it with the options also
      enVal1_Options[k] = envVal1[k];
    }

    /* Choices to sample from ample data, there is a Rcpp sugar function sample that could help
     Rcpp::sample(choicesVec, 1, false, envVal1) */
    // for now we will just use the maximum value is chosen for testing purposes
    // chosen = Rcpp::which_max(envVal1);
    // optionsMatrix(chosen,2) = 1;

    // old uniform choice doesn't use any environmental input and can pick multiple new locations
    // chosen = round(Rcpp::runif(1, 0, nopt-1)[0]);
    // optionsMatrix(chosen,2) = 1;

    // non Rcpp attempt to randomly sample, there is no weighting of choice however
    std::srand(std::time(0)); // use current time as seed for random generator
    int random_pos = std::rand() % choicesVec.size();
    chosen = choicesVec[random_pos];

    // chosen = Rcpp::sample(choicesVec, 1, false, envVal1);
    // add choice to vector of choices, each location == step
    chosen_Options[i-1] = chosen;

    x_Locations[i] = x_Options[chosen];
    y_Locations[i] = y_Options[chosen];
    step_Locations[i] = i;

  }
  Rcpp::List OUTPUT = Rcpp::List::create(
    // output the location data
    Rcpp::Named("loc_x") = x_Locations,
    Rcpp::Named("loc_y") = y_Locations,
    Rcpp::Named("loc_step") = step_Locations,
    // output for all the optionsALL
    Rcpp::Named("oall_x") = x_OptionsAll,
    Rcpp::Named("oall_y") = y_OptionsAll,
    Rcpp::Named("oall_step") = step_OptionsAll,
    // output for the chosen options at each step
    Rcpp::Named("chosen") = chosen_Options,
    // output for the last options just to check
    Rcpp::Named("ol_x") = x_Options,
    Rcpp::Named("ol_y") = y_Options,
    Rcpp::Named("ol_step") = step_Options, // included to check is choice vector is the source of issues
    Rcpp::Named("ol_enVal1") = enVal1_Options // included to check probs used
  );
  return OUTPUT;

}
