
#include <Rcpp.h>
#include <cmath>
#include "cpp_vonmises.h"
#include "cpp_sample_options.h"
#include "cpp_cycle_draw.h"

//' @name cpp_abm_simulate
//' @title cpp_abm_simulate
//' @description The C++ function that runs the agent based animal movement model.
//' @param startx The x coord start location
//' @param starty The y coord start location
//' @param steps The number of steps to be simulated
//' @param options The number of options to be considered at each step
//' @param k_step Parameter describing step length
//' @param s_step Parameter describing step
//' @param mu_angle Parameter describing angle
//' @param k_angle Parameter describing angle variation
//' @param b0_Options Behave transitional probs for behave 0
//' @param b1_Options Behave transitional probs for behave 1
//' @param b2_Options Behave transitional probs for behave 2
//' @param rest_Cycle_A
//' @param rest_Cycle_M
//' @param rest_Cycle_PHI
//' @param rest_Cycle_TAU
//' @param envMat1 Environmental matrix 1
//' @param seeds

//' @return Matrix of locations chosen

//' @details

// [[Rcpp::export]]
Rcpp::List cpp_abm_simulate(
    double startx,
    double starty,
    int steps,
    int options,
    std::vector<double> k_step,
    std::vector<double> s_step,
    std::vector<double> mu_angle,
    std::vector<double> k_angle,
    std::vector<double> b0_Options,
    std::vector<double> b1_Options,
    std::vector<double> b2_Options,
    double rest_Cycle_A,
    double rest_Cycle_M,
    double rest_Cycle_PHI,
    double rest_Cycle_TAU,
    Rcpp::NumericMatrix envMat1,
    std::vector<int> seeds
){

  int n = steps;
  int nopt = options;
  double angle;
  double step;

  // a means of converting NumVec to std::vec
  double vmdraw;

  // Rcpp::NumericVector envVal1(nopt);
  // std::vector<double> envVal1(nopt);

  // Rcpp::NumericVector choicesVec(nopt);
  // std::vector<int> choicesVec(nopt);
  // std::iota (std::begin(choicesVec), std::end(choicesVec), 0); // Fill with 0, 1, ..., end.

  //// ENVIRONMENTAL OBJECTS //////
  int mcols = envMat1.ncol();
  int mrows = envMat1.nrow();
  //////////
  int xOpt;
  int yOpt;
  int yOptIndex;
  int xOptIndex;
  // the options stores for the loop
  // Rcpp::NumericMatrix optionsMatrix(nopt, 5);
  std::vector<double> x_Options(nopt);
  std::vector<double> y_Options(nopt);
  std::vector<int> step_Options(nopt);
  std::vector<double> enVal1_Options(nopt);
  // needed for the chosing of option
  // double min, curr;

  int chosen;

  // store the chose at each step
  std::vector<int> chosen_Options(steps);

  // the options stores for the output including all options
  // Rcpp::NumericMatrix optionsMatrixALL(nopt*steps +1, 3);
  std::vector<double> x_OptionsAll(nopt*steps);
  std::vector<double> y_OptionsAll(nopt*steps);
  std::vector<int> step_OptionsAll(nopt*steps);

  //
  // Rcpp::NumericMatrix locMatrix(steps, 2);
  std::vector<double> x_Locations(steps);
  std::vector<double> y_Locations(steps);
  std::vector<int> step_Locations(steps);
  // somewhere to store the behaviours at each step
  std::vector<int> behave_Locations(steps);
  double behave_k_step;
  double behave_s_step;
  double behave_mu_angle;
  double behave_k_angle;
  // something that store the time adjusted behavioural shifts
  // and initialise them with the provided base values
  std::vector<double> b0_Options_Current = b0_Options;
  std::vector<double> b1_Options_Current = b1_Options;
  std::vector<double> b2_Options_Current = b2_Options;

  /* initial behaviour set to 0 */
  behave_Locations[0] = 1;
  // and set a basic behave switch balance for testing
  // std::vector<double> behave_Options = {0.2, 0.2, 0.1};

  // CYCLE MODIFIERS
  double b0_dailyMod;

  /* initial location is set using the start locations */
  x_Locations[0] = startx;
  y_Locations[0] = starty;
  x_OptionsAll[0] = startx;
  y_OptionsAll[0] = starty;
  step_OptionsAll[0] = 0;
  for(int i = 1, a = 1; i < n; i++){
    Rcpp::Rcout << "---- Step: " << i << " ----\n";

    Rcpp::Rcout << "--- Step start set" << " ---\n";

    /* working under the assumption that i == minute, but the cycle is defined in
     hours AKA 12 hour cycle offset to be crepusclar, we need to convert i AKA minute to hours */
    b0_dailyMod = cpp_cycle_draw(
      i*1.0 / 60, // make i a double and convert it to hours
      rest_Cycle_A,
      rest_Cycle_M,
      rest_Cycle_PHI / rest_Cycle_TAU, // make sure PHI is kept ~ to TAU so no drift
      rest_Cycle_TAU);

    /* switch to use a given set of transition probabilities that change
     depending on the previous behavioural state*/
    switch(behave_Locations[i-1]){
      case 0:
        // this will update the behaviour shift prob depending on the time of day
        b0_Options_Current[0] = b0_Options[0] + b0_dailyMod;
        // draw from the updated behaviour probs to get the next behavioural state
        behave_Locations[i] = cpp_sample_options(b0_Options_Current, seeds[i-1]);
        break;
      case 1:
        b1_Options_Current[0] = b1_Options[0] + b0_dailyMod;
        behave_Locations[i] = cpp_sample_options(b1_Options_Current, seeds[i-1]);
        break;
      case 2:
        b2_Options_Current[0] = b2_Options[0] + b0_dailyMod;
        behave_Locations[i] = cpp_sample_options(b2_Options_Current, seeds[i-1]);
        break;
        // default:
        //   behave_Locations[i] = sample_options(b0_Options, seeds[i-1]);
        //   break;
    }

    /* assigning the step and angle parameters
     depending on the behaviour */
    switch(behave_Locations[i]){
    case 0:
      behave_k_step = k_step[0];
      behave_s_step = s_step[0];
      behave_mu_angle = mu_angle[0];
      behave_k_angle = k_angle[0];
      break;
      case 1:
        behave_k_step = k_step[1];
        behave_s_step = s_step[1];
        behave_mu_angle = mu_angle[1];
        behave_k_angle = k_angle[1];
        break;
        case 2:
          behave_k_step = k_step[2];
          behave_s_step = s_step[2];
          behave_mu_angle = mu_angle[2];
          behave_k_angle = k_angle[2];
          break;
          // default:
          //   behave_k_step = k_step[0];
          //   behave_s_step = s_step[0];
          //   behave_mu_angle = mu_angle[0];
          //   behave_k_angle = k_angle[0];
          //   break;
    }
    Rcpp::Rcout << "-- Behaviour mode: " << behave_Locations[i] << " ---\n";

    for(int j = 0; j < nopt; j++, a++){

      if(j == 0){
        /* for each step set the location as the previously chosen location */
        x_Options[0] = x_Locations[i-1];
        y_Options[0] = y_Locations[i-1];
        step_Options[0] = i;
        step_OptionsAll[a] = i; // this one needs assignment regardless
        continue;
      }

      step = Rcpp::rgamma(1, behave_k_step, behave_s_step)[0];
      Rcpp::Rcout << "StepLength: " << step << "; ";

      vmdraw = cpp_vonmises(1, behave_mu_angle, behave_k_angle)[0];
      Rcpp::Rcout << "VM ";
      angle = vmdraw * 180/M_PI;
      Rcpp::Rcout << "Angle: " << angle << "\n";

      x_Options[j] = x_Options[0] + cos(angle) * step;
      y_Options[j] = y_Options[0] + sin(angle) * step;

      // add in which step the options are for
      step_Options[j] = i;

      // a is keeping tracking of the position in a ong vector steps*nopts
      x_OptionsAll[a] = x_Options[j];
      y_OptionsAll[a] = y_Options[j];
      step_OptionsAll[a] = i;

      // choice vector is needed for the sample function later on
      // choicesVec[j] = j;

    }

    ////// ENVIRONMENTAL CHECK LOOP //////
    /* for each of the options, check values in environment and use equation to pick next move */
    for(int k = 0; k < nopt; k++){

      xOpt = x_Options[k];
      yOpt = y_Options[k];
      // rounding the locations to correspond to matrix location
      xOptIndex = std::floor(xOpt);
      yOptIndex = std::floor(yOpt);

      Rcpp::Rcout << "Option: " << k << "; " << "Cells: " << xOptIndex << ":" << yOptIndex << "\n";

      // end function if animal leaves environmental data area
      if( (xOptIndex > mcols) | (yOptIndex > mrows) ){
        Rcpp::Rcerr << "Exceeding background environmental limits or NA in enviornmental data\n";
      }

      // still using the numericMatrix Rcpp form here
      enVal1_Options[k] = envMat1(xOptIndex, yOptIndex);

      Rcpp::Rcout << "EnvVal: " << enVal1_Options[k] << "\n";

      if(std::isnan(enVal1_Options[k])){
        // printing error message
        Rcpp::Rcerr << "NA in enviornmental data\n";
      }

    }
    //////
    // min = 0;
    // finding the highest value and getting the index
    // does mean that the animal will locate best spot and remain there
    // for(int l = 0; l < nopt; l++){
    //   curr = enVal1_Options[l];
    //   if(curr > min){
    //     min = curr;
    //     chosen = l;
    //   }
    // }

    /* using the custom sample_options, we need to feed it a different seed each time,
     but overall those seeds are derived from the set.seed() in R prior to running
     (see the R companion/set-up function .Call) */
    chosen = cpp_sample_options(enVal1_Options, seeds[i-1]);

    /* Choices to sample from ample data, there is a Rcpp sugar function sample that could help
     Rcpp::sample(choicesVec, 1, false, enVal1_Options) */

    // old uniform choice doesn't use any environmental input and can pick multiple new locations
    // chosen = round(Rcpp::runif(1, 0, nopt-1)[0]);

    // for testing, pick the first options always, should mean the animal never moves
    // chosen = 0;
    // moves every time
    // chosen = 1;
    // chosen = 2;

    // non Rcpp attempt to randomly sample, there is no weighting of choice however
    // std::srand(std::time(0)); // use current time as seed for random generator
    // int random_pos = std::rand() % choicesVec.size();
    // chosen = choicesVec[random_pos];



    // chosen = Rcpp::sample(choicesVec, 1, false, enVal1_Options);
    // add choice to vector of choices, each location == step
    // -1 is there because initial cycle starts at 1
    chosen_Options[i] = chosen;

    x_Locations[i] = x_Options[chosen];
    y_Locations[i] = y_Options[chosen];
    step_Locations[i] = i;

  }
  Rcpp::List OUTPUT = Rcpp::List::create(
    // output the location data
    Rcpp::Named("loc_x") = x_Locations,
    Rcpp::Named("loc_y") = y_Locations,
    Rcpp::Named("loc_step") = step_Locations,
    Rcpp::Named("loc_behave") = behave_Locations,
    // output for all the optionsALL
    Rcpp::Named("oall_x") = x_OptionsAll,
    Rcpp::Named("oall_y") = y_OptionsAll,
    Rcpp::Named("oall_step") = step_OptionsAll,
    // output for the chosen options at each step
    Rcpp::Named("chosen") = chosen_Options,
    // output for the last options just to check
    Rcpp::Named("ol_x") = x_Options,
    Rcpp::Named("ol_y") = y_Options,
    Rcpp::Named("ol_enVal1") = enVal1_Options, // included to check probs used
    Rcpp::Named("ol_step") = step_Options // included to check is choice vector is the source of issues
  );
  return OUTPUT;

}