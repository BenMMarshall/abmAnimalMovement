// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_abm_simulate
Rcpp::List cpp_abm_simulate(double startx, double starty, int steps, int des_options, int options, std::vector<double> k_step, std::vector<double> s_step, std::vector<double> mu_angle, std::vector<double> k_angle, std::vector<double> b0_Options, std::vector<double> b1_Options, std::vector<double> b2_Options, double rest_Cycle_A, double rest_Cycle_M, double rest_Cycle_PHI, double rest_Cycle_TAU, Rcpp::NumericMatrix memShelterMatrix, Rcpp::NumericMatrix forageMatrix, Rcpp::NumericMatrix moveMatrix, std::vector<int> seeds);
RcppExport SEXP _abmAnimalMovement_cpp_abm_simulate(SEXP startxSEXP, SEXP startySEXP, SEXP stepsSEXP, SEXP des_optionsSEXP, SEXP optionsSEXP, SEXP k_stepSEXP, SEXP s_stepSEXP, SEXP mu_angleSEXP, SEXP k_angleSEXP, SEXP b0_OptionsSEXP, SEXP b1_OptionsSEXP, SEXP b2_OptionsSEXP, SEXP rest_Cycle_ASEXP, SEXP rest_Cycle_MSEXP, SEXP rest_Cycle_PHISEXP, SEXP rest_Cycle_TAUSEXP, SEXP memShelterMatrixSEXP, SEXP forageMatrixSEXP, SEXP moveMatrixSEXP, SEXP seedsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type startx(startxSEXP);
    Rcpp::traits::input_parameter< double >::type starty(startySEXP);
    Rcpp::traits::input_parameter< int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< int >::type des_options(des_optionsSEXP);
    Rcpp::traits::input_parameter< int >::type options(optionsSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type k_step(k_stepSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type s_step(s_stepSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type mu_angle(mu_angleSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type k_angle(k_angleSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type b0_Options(b0_OptionsSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type b1_Options(b1_OptionsSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type b2_Options(b2_OptionsSEXP);
    Rcpp::traits::input_parameter< double >::type rest_Cycle_A(rest_Cycle_ASEXP);
    Rcpp::traits::input_parameter< double >::type rest_Cycle_M(rest_Cycle_MSEXP);
    Rcpp::traits::input_parameter< double >::type rest_Cycle_PHI(rest_Cycle_PHISEXP);
    Rcpp::traits::input_parameter< double >::type rest_Cycle_TAU(rest_Cycle_TAUSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type memShelterMatrix(memShelterMatrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type forageMatrix(forageMatrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type moveMatrix(moveMatrixSEXP);
    Rcpp::traits::input_parameter< std::vector<int> >::type seeds(seedsSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_abm_simulate(startx, starty, steps, des_options, options, k_step, s_step, mu_angle, k_angle, b0_Options, b1_Options, b2_Options, rest_Cycle_A, rest_Cycle_M, rest_Cycle_PHI, rest_Cycle_TAU, memShelterMatrix, forageMatrix, moveMatrix, seeds));
    return rcpp_result_gen;
END_RCPP
}
// cpp_cycle_draw
double cpp_cycle_draw(double TIME, double A, double M, double PHI, double TAU);
RcppExport SEXP _abmAnimalMovement_cpp_cycle_draw(SEXP TIMESEXP, SEXP ASEXP, SEXP MSEXP, SEXP PHISEXP, SEXP TAUSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type TIME(TIMESEXP);
    Rcpp::traits::input_parameter< double >::type A(ASEXP);
    Rcpp::traits::input_parameter< double >::type M(MSEXP);
    Rcpp::traits::input_parameter< double >::type PHI(PHISEXP);
    Rcpp::traits::input_parameter< double >::type TAU(TAUSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_cycle_draw(TIME, A, M, PHI, TAU));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_values
std::vector<double> cpp_get_values(Rcpp::NumericMatrix MATRIX, std::vector<double> XLOCS, std::vector<double> YLOCS);
RcppExport SEXP _abmAnimalMovement_cpp_get_values(SEXP MATRIXSEXP, SEXP XLOCSSEXP, SEXP YLOCSSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type MATRIX(MATRIXSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type XLOCS(XLOCSSEXP);
    Rcpp::traits::input_parameter< std::vector<double> >::type YLOCS(YLOCSSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_get_values(MATRIX, XLOCS, YLOCS));
    return rcpp_result_gen;
END_RCPP
}
// cpp_sample_options
int cpp_sample_options(std::vector<double> W, int SEED);
RcppExport SEXP _abmAnimalMovement_cpp_sample_options(SEXP WSEXP, SEXP SEEDSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::vector<double> >::type W(WSEXP);
    Rcpp::traits::input_parameter< int >::type SEED(SEEDSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_sample_options(W, SEED));
    return rcpp_result_gen;
END_RCPP
}
// cpp_vonmises
std::vector<double> cpp_vonmises(int N, double MU, double KAPPA);
RcppExport SEXP _abmAnimalMovement_cpp_vonmises(SEXP NSEXP, SEXP MUSEXP, SEXP KAPPASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type MU(MUSEXP);
    Rcpp::traits::input_parameter< double >::type KAPPA(KAPPASEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_vonmises(N, MU, KAPPA));
    return rcpp_result_gen;
END_RCPP
}
// find_max
int find_max(Rcpp::NumericVector vect);
RcppExport SEXP _abmAnimalMovement_find_max(SEXP vectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type vect(vectSEXP);
    rcpp_result_gen = Rcpp::wrap(find_max(vect));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_abmAnimalMovement_cpp_abm_simulate", (DL_FUNC) &_abmAnimalMovement_cpp_abm_simulate, 20},
    {"_abmAnimalMovement_cpp_cycle_draw", (DL_FUNC) &_abmAnimalMovement_cpp_cycle_draw, 5},
    {"_abmAnimalMovement_cpp_get_values", (DL_FUNC) &_abmAnimalMovement_cpp_get_values, 3},
    {"_abmAnimalMovement_cpp_sample_options", (DL_FUNC) &_abmAnimalMovement_cpp_sample_options, 2},
    {"_abmAnimalMovement_cpp_vonmises", (DL_FUNC) &_abmAnimalMovement_cpp_vonmises, 3},
    {"_abmAnimalMovement_find_max", (DL_FUNC) &_abmAnimalMovement_find_max, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_abmAnimalMovement(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
