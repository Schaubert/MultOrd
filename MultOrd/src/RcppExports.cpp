// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// scoreMO
arma::vec scoreMO(arma::vec alpha, arma::vec Y, arma::mat X, int Q, int q, int n, int I, int pall, int pX, int pXRS, int pthresh, int pshift, int prnd, arma::mat GHweights, arma::vec GHnodes, int scaled, arma::mat dthresh, double cores, double lambda);
RcppExport SEXP _MultOrd_scoreMO(SEXP alphaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP QSEXP, SEXP qSEXP, SEXP nSEXP, SEXP ISEXP, SEXP pallSEXP, SEXP pXSEXP, SEXP pXRSSEXP, SEXP pthreshSEXP, SEXP pshiftSEXP, SEXP prndSEXP, SEXP GHweightsSEXP, SEXP GHnodesSEXP, SEXP scaledSEXP, SEXP dthreshSEXP, SEXP coresSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type pall(pallSEXP);
    Rcpp::traits::input_parameter< int >::type pX(pXSEXP);
    Rcpp::traits::input_parameter< int >::type pXRS(pXRSSEXP);
    Rcpp::traits::input_parameter< int >::type pthresh(pthreshSEXP);
    Rcpp::traits::input_parameter< int >::type pshift(pshiftSEXP);
    Rcpp::traits::input_parameter< int >::type prnd(prndSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type GHweights(GHweightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHnodes(GHnodesSEXP);
    Rcpp::traits::input_parameter< int >::type scaled(scaledSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dthresh(dthreshSEXP);
    Rcpp::traits::input_parameter< double >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(scoreMO(alpha, Y, X, Q, q, n, I, pall, pX, pXRS, pthresh, pshift, prnd, GHweights, GHnodes, scaled, dthresh, cores, lambda));
    return rcpp_result_gen;
END_RCPP
}
// loglikMO
double loglikMO(arma::vec alpha, arma::vec Y, arma::mat X, int Q, int q, int n, int I, int pall, int pX, int pXRS, int pthresh, int pshift, int prnd, arma::mat GHweights, arma::vec GHnodes, int scaled, arma::mat dthresh, int cores, double lambda);
RcppExport SEXP _MultOrd_loglikMO(SEXP alphaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP QSEXP, SEXP qSEXP, SEXP nSEXP, SEXP ISEXP, SEXP pallSEXP, SEXP pXSEXP, SEXP pXRSSEXP, SEXP pthreshSEXP, SEXP pshiftSEXP, SEXP prndSEXP, SEXP GHweightsSEXP, SEXP GHnodesSEXP, SEXP scaledSEXP, SEXP dthreshSEXP, SEXP coresSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type pall(pallSEXP);
    Rcpp::traits::input_parameter< int >::type pX(pXSEXP);
    Rcpp::traits::input_parameter< int >::type pXRS(pXRSSEXP);
    Rcpp::traits::input_parameter< int >::type pthresh(pthreshSEXP);
    Rcpp::traits::input_parameter< int >::type pshift(pshiftSEXP);
    Rcpp::traits::input_parameter< int >::type prnd(prndSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type GHweights(GHweightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHnodes(GHnodesSEXP);
    Rcpp::traits::input_parameter< int >::type scaled(scaledSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dthresh(dthreshSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(loglikMO(alpha, Y, X, Q, q, n, I, pall, pX, pXRS, pthresh, pshift, prnd, GHweights, GHnodes, scaled, dthresh, cores, lambda));
    return rcpp_result_gen;
END_RCPP
}
// loglikMO_noRS
double loglikMO_noRS(arma::vec alpha, arma::vec Y, arma::mat X, int Q, int q, int n, int I, int pall, int pX, int pthresh, int pshift, arma::vec GHweights, arma::vec GHnodes, arma::mat dthresh, int cores, double lambda);
RcppExport SEXP _MultOrd_loglikMO_noRS(SEXP alphaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP QSEXP, SEXP qSEXP, SEXP nSEXP, SEXP ISEXP, SEXP pallSEXP, SEXP pXSEXP, SEXP pthreshSEXP, SEXP pshiftSEXP, SEXP GHweightsSEXP, SEXP GHnodesSEXP, SEXP dthreshSEXP, SEXP coresSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type pall(pallSEXP);
    Rcpp::traits::input_parameter< int >::type pX(pXSEXP);
    Rcpp::traits::input_parameter< int >::type pthresh(pthreshSEXP);
    Rcpp::traits::input_parameter< int >::type pshift(pshiftSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHweights(GHweightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHnodes(GHnodesSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dthresh(dthreshSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(loglikMO_noRS(alpha, Y, X, Q, q, n, I, pall, pX, pthresh, pshift, GHweights, GHnodes, dthresh, cores, lambda));
    return rcpp_result_gen;
END_RCPP
}
// scoreMO_noRS
arma::vec scoreMO_noRS(arma::vec alpha, arma::vec Y, arma::mat X, int Q, int q, int n, int I, int pall, int pX, int pthresh, int pshift, arma::vec GHweights, arma::vec GHnodes, arma::mat dthresh, double cores, double lambda);
RcppExport SEXP _MultOrd_scoreMO_noRS(SEXP alphaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP QSEXP, SEXP qSEXP, SEXP nSEXP, SEXP ISEXP, SEXP pallSEXP, SEXP pXSEXP, SEXP pthreshSEXP, SEXP pshiftSEXP, SEXP GHweightsSEXP, SEXP GHnodesSEXP, SEXP dthreshSEXP, SEXP coresSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type pall(pallSEXP);
    Rcpp::traits::input_parameter< int >::type pX(pXSEXP);
    Rcpp::traits::input_parameter< int >::type pthresh(pthreshSEXP);
    Rcpp::traits::input_parameter< int >::type pshift(pshiftSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHweights(GHweightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHnodes(GHnodesSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type dthresh(dthreshSEXP);
    Rcpp::traits::input_parameter< double >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(scoreMO_noRS(alpha, Y, X, Q, q, n, I, pall, pX, pthresh, pshift, GHweights, GHnodes, dthresh, cores, lambda));
    return rcpp_result_gen;
END_RCPP
}
// loglikMO_cumul
double loglikMO_cumul(arma::vec alpha, arma::vec Y, arma::mat X, int Q, int q, int n, int I, int pall, int pX, int pXRS, int pthresh, int pshift, int prnd, arma::mat GHweights, arma::vec GHnodes, int scaled, int cores, double lambda);
RcppExport SEXP _MultOrd_loglikMO_cumul(SEXP alphaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP QSEXP, SEXP qSEXP, SEXP nSEXP, SEXP ISEXP, SEXP pallSEXP, SEXP pXSEXP, SEXP pXRSSEXP, SEXP pthreshSEXP, SEXP pshiftSEXP, SEXP prndSEXP, SEXP GHweightsSEXP, SEXP GHnodesSEXP, SEXP scaledSEXP, SEXP coresSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type pall(pallSEXP);
    Rcpp::traits::input_parameter< int >::type pX(pXSEXP);
    Rcpp::traits::input_parameter< int >::type pXRS(pXRSSEXP);
    Rcpp::traits::input_parameter< int >::type pthresh(pthreshSEXP);
    Rcpp::traits::input_parameter< int >::type pshift(pshiftSEXP);
    Rcpp::traits::input_parameter< int >::type prnd(prndSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type GHweights(GHweightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHnodes(GHnodesSEXP);
    Rcpp::traits::input_parameter< int >::type scaled(scaledSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(loglikMO_cumul(alpha, Y, X, Q, q, n, I, pall, pX, pXRS, pthresh, pshift, prnd, GHweights, GHnodes, scaled, cores, lambda));
    return rcpp_result_gen;
END_RCPP
}
// loglikMO_cumul_noRS
double loglikMO_cumul_noRS(arma::vec alpha, arma::vec Y, arma::mat X, int Q, int q, int n, int I, int pall, int pX, int pthresh, int pshift, int prnd, arma::vec GHweights, arma::vec GHnodes, int scaled, int cores, double lambda);
RcppExport SEXP _MultOrd_loglikMO_cumul_noRS(SEXP alphaSEXP, SEXP YSEXP, SEXP XSEXP, SEXP QSEXP, SEXP qSEXP, SEXP nSEXP, SEXP ISEXP, SEXP pallSEXP, SEXP pXSEXP, SEXP pthreshSEXP, SEXP pshiftSEXP, SEXP prndSEXP, SEXP GHweightsSEXP, SEXP GHnodesSEXP, SEXP scaledSEXP, SEXP coresSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::vec >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< int >::type Q(QSEXP);
    Rcpp::traits::input_parameter< int >::type q(qSEXP);
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    Rcpp::traits::input_parameter< int >::type I(ISEXP);
    Rcpp::traits::input_parameter< int >::type pall(pallSEXP);
    Rcpp::traits::input_parameter< int >::type pX(pXSEXP);
    Rcpp::traits::input_parameter< int >::type pthresh(pthreshSEXP);
    Rcpp::traits::input_parameter< int >::type pshift(pshiftSEXP);
    Rcpp::traits::input_parameter< int >::type prnd(prndSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHweights(GHweightsSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type GHnodes(GHnodesSEXP);
    Rcpp::traits::input_parameter< int >::type scaled(scaledSEXP);
    Rcpp::traits::input_parameter< int >::type cores(coresSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    rcpp_result_gen = Rcpp::wrap(loglikMO_cumul_noRS(alpha, Y, X, Q, q, n, I, pall, pX, pthresh, pshift, prnd, GHweights, GHnodes, scaled, cores, lambda));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_MultOrd_scoreMO", (DL_FUNC) &_MultOrd_scoreMO, 19},
    {"_MultOrd_loglikMO", (DL_FUNC) &_MultOrd_loglikMO, 19},
    {"_MultOrd_loglikMO_noRS", (DL_FUNC) &_MultOrd_loglikMO_noRS, 16},
    {"_MultOrd_scoreMO_noRS", (DL_FUNC) &_MultOrd_scoreMO_noRS, 16},
    {"_MultOrd_loglikMO_cumul", (DL_FUNC) &_MultOrd_loglikMO_cumul, 18},
    {"_MultOrd_loglikMO_cumul_noRS", (DL_FUNC) &_MultOrd_loglikMO_cumul_noRS, 17},
    {NULL, NULL, 0}
};

RcppExport void R_init_MultOrd(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
