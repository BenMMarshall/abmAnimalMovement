#include <Rcpp.h>

//' cpp_get_values
//' @name cpp_get_values
//' @param MATRIX
//' @param XLOCS
//' @param YLOCS
//' @return A vector of values
//' @details \eqn{\phi} must remain proportional to \eqn{\tau} if the cycle is to remained
//'   sync with period. If not proportional the cycle with drift.
//' @references ...

// [[Rcpp::export]]
std::vector<double> cpp_get_values(Rcpp::NumericMatrix MATRIX,
                                   std::vector<double> XLOCS,
                                   std::vector<double> YLOCS) {

  int nLocs = XLOCS.size();
  //// ENVIRONMENTAL OBJECTS //////
  int mcols = MATRIX.ncol();
  int mrows = MATRIX.nrow();
  //////////
  int yOptIndex;
  int xOptIndex;

  //
  std::vector<double> OUTPUT_VALUES(nLocs);

  for(int loc = 0; loc < nLocs; loc++){

    // rounding the locations to correspond to matrix location
    xOptIndex = std::floor(XLOCS[loc]);
    yOptIndex = std::floor(YLOCS[loc]);

    Rcpp::Rcout << "Option: " << loc << "; " << "Cells: " << xOptIndex << ":" << yOptIndex << "\n";

    // end function if animal leaves environmental data area
    if( (xOptIndex > mcols) | (yOptIndex > mrows) ){
      Rcpp::Rcerr << "Exceeding background environmental limits or NA in enviornmental data\n";
    }

    // still using the numericMatrix Rcpp form here
    OUTPUT_VALUES[loc] = MATRIX(xOptIndex, yOptIndex);

    Rcpp::Rcout << "EnvVal: " << OUTPUT_VALUES[loc] << "\n";

    // if(std::isnan(enVal1_Options[loc])){
    //   // printing error message
    //   Rcpp::Rcerr << "NA in enviornmental data\n";
    // }

  }
  return(OUTPUT_VALUES);
}
