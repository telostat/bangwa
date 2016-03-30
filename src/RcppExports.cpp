// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// mvReturns
Rcpp::NumericMatrix mvReturns(Rcpp::NumericMatrix levels);
RcppExport SEXP bangwa_mvReturns(SEXP levelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type levels(levelsSEXP);
    __result = Rcpp::wrap(mvReturns(levels));
    return __result;
END_RCPP
}
// uvReturns
Rcpp::NumericVector uvReturns(Rcpp::NumericVector levels);
RcppExport SEXP bangwa_uvReturns(SEXP levelsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type levels(levelsSEXP);
    __result = Rcpp::wrap(uvReturns(levels));
    return __result;
END_RCPP
}