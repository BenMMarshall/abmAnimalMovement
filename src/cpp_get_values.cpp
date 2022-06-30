#include <Rcpp.h>

//' cpp_get_values
//' @name cpp_get_values
//' @param MATRIX A numeric matrix that the value will be extracted from.
//' @param XLOCS A vector of x locations.
//' @param YLOCS A vector of y locations.
//' @return A vector of values equal to the length of XLOCS extracted from
//'   MATRIX.
//' @details A simple C++ function to extract a value from a Rcpp::NumericMatrix
//'   using two vectors describing location. Note that the counting begins for x
//'   and y begins at zero.

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

    // Rcpp::Rcout << "Option: " << loc << "; " << "Cells: " << xOptIndex << ":" << yOptIndex << "\n";

    // if animal leaves environmental data area
    if( (xOptIndex > mcols) |
        (yOptIndex > mrows) |
        (xOptIndex < 0) |
        (yOptIndex < 0) ){
      // give a value of zero for exceeding environment so the chances of moving
      // are minimised
      OUTPUT_VALUES[loc] = 0;
      Rcpp::Rcerr << "Animal considering exceeding environmental limits, 0 value returned instead\n";
    }

    // still using the numericMatrix Rcpp form here
    OUTPUT_VALUES[loc] = MATRIX(xOptIndex, yOptIndex);

    // Rcpp::Rcout << "EnvVal: " << OUTPUT_VALUES[loc] << "\n";

    // if(std::isnan(enVal1_Options[loc])){
    //   // printing error message
    //   Rcpp::Rcerr << "NA in environmental data\n";
    // }

  }
  return(OUTPUT_VALUES);
}
