// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// gnps_cpp
double gnps_cpp(NumericMatrix x, NumericMatrix y);
RcppExport SEXP _tima_gnps_cpp(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(gnps_cpp(x, y));
    return rcpp_result_gen;
END_RCPP
}
// join_gnps_cpp
List join_gnps_cpp(NumericVector x, NumericVector y, double xPrecursorMz, double yPrecursorMz, double tolerance, double ppm);
RcppExport SEXP _tima_join_gnps_cpp(SEXP xSEXP, SEXP ySEXP, SEXP xPrecursorMzSEXP, SEXP yPrecursorMzSEXP, SEXP toleranceSEXP, SEXP ppmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type xPrecursorMz(xPrecursorMzSEXP);
    Rcpp::traits::input_parameter< double >::type yPrecursorMz(yPrecursorMzSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    Rcpp::traits::input_parameter< double >::type ppm(ppmSEXP);
    rcpp_result_gen = Rcpp::wrap(join_gnps_cpp(x, y, xPrecursorMz, yPrecursorMz, tolerance, ppm));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_tima_gnps_cpp", (DL_FUNC) &_tima_gnps_cpp, 2},
    {"_tima_join_gnps_cpp", (DL_FUNC) &_tima_join_gnps_cpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_tima(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
