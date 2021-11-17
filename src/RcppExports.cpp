// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// sample_test
int sample_test(Rcpp::NumericVector weights);
RcppExport SEXP _abmAnimalMovement_sample_test(SEXP weightsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type weights(weightsSEXP);
    rcpp_result_gen = Rcpp::wrap(sample_test(weights));
    return rcpp_result_gen;
END_RCPP
}
// vonmises
Rcpp::NumericVector vonmises(int N, double MU, double KAPPA);
RcppExport SEXP _abmAnimalMovement_vonmises(SEXP NSEXP, SEXP MUSEXP, SEXP KAPPASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    Rcpp::traits::input_parameter< double >::type MU(MUSEXP);
    Rcpp::traits::input_parameter< double >::type KAPPA(KAPPASEXP);
    rcpp_result_gen = Rcpp::wrap(vonmises(N, MU, KAPPA));
    return rcpp_result_gen;
END_RCPP
}
// walk_options_xy
Rcpp::List walk_options_xy(double startx, double starty, int steps, int options, double normmean, double normsd, double mu_angle, double k_angle, Rcpp::NumericMatrix envMat1);
RcppExport SEXP _abmAnimalMovement_walk_options_xy(SEXP startxSEXP, SEXP startySEXP, SEXP stepsSEXP, SEXP optionsSEXP, SEXP normmeanSEXP, SEXP normsdSEXP, SEXP mu_angleSEXP, SEXP k_angleSEXP, SEXP envMat1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type startx(startxSEXP);
    Rcpp::traits::input_parameter< double >::type starty(startySEXP);
    Rcpp::traits::input_parameter< int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< int >::type options(optionsSEXP);
    Rcpp::traits::input_parameter< double >::type normmean(normmeanSEXP);
    Rcpp::traits::input_parameter< double >::type normsd(normsdSEXP);
    Rcpp::traits::input_parameter< double >::type mu_angle(mu_angleSEXP);
    Rcpp::traits::input_parameter< double >::type k_angle(k_angleSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type envMat1(envMat1SEXP);
    rcpp_result_gen = Rcpp::wrap(walk_options_xy(startx, starty, steps, options, normmean, normsd, mu_angle, k_angle, envMat1));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_abmAnimalMovement_sample_test", (DL_FUNC) &_abmAnimalMovement_sample_test, 1},
    {"_abmAnimalMovement_vonmises", (DL_FUNC) &_abmAnimalMovement_vonmises, 3},
    {"_abmAnimalMovement_walk_options_xy", (DL_FUNC) &_abmAnimalMovement_walk_options_xy, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_abmAnimalMovement(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
