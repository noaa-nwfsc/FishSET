// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// shift_sort_xcpp
NumericMatrix shift_sort_xcpp(NumericMatrix x, NumericMatrix ch, NumericVector y, NumericMatrix distance, int alts, int ab);
RcppExport SEXP _FishSET_shift_sort_xcpp(SEXP xSEXP, SEXP chSEXP, SEXP ySEXP, SEXP distanceSEXP, SEXP altsSEXP, SEXP abSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type ch(chSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type distance(distanceSEXP);
    Rcpp::traits::input_parameter< int >::type alts(altsSEXP);
    Rcpp::traits::input_parameter< int >::type ab(abSEXP);
    rcpp_result_gen = Rcpp::wrap(shift_sort_xcpp(x, ch, y, distance, alts, ab));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FishSET_shift_sort_xcpp", (DL_FUNC) &_FishSET_shift_sort_xcpp, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_FishSET(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
