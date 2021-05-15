// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// anova_bibd
List anova_bibd(DataFrame dados, std::string x, std::string y, std::string bloco);
RcppExport SEXP _planejamento_anova_bibd(SEXP dadosSEXP, SEXP xSEXP, SEXP ySEXP, SEXP blocoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dados(dadosSEXP);
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type bloco(blocoSEXP);
    rcpp_result_gen = Rcpp::wrap(anova_bibd(dados, x, y, bloco));
    return rcpp_result_gen;
END_RCPP
}
// anova_glsqd
List anova_glsqd(DataFrame dados, std::string x, std::string chi, std::string y, std::string linha, std::string coluna);
RcppExport SEXP _planejamento_anova_glsqd(SEXP dadosSEXP, SEXP xSEXP, SEXP chiSEXP, SEXP ySEXP, SEXP linhaSEXP, SEXP colunaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dados(dadosSEXP);
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type chi(chiSEXP);
    Rcpp::traits::input_parameter< std::string >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type linha(linhaSEXP);
    Rcpp::traits::input_parameter< std::string >::type coluna(colunaSEXP);
    rcpp_result_gen = Rcpp::wrap(anova_glsqd(dados, x, chi, y, linha, coluna));
    return rcpp_result_gen;
END_RCPP
}
// anova_lsqd
List anova_lsqd(DataFrame dados, std::string x, std::string y, std::string linha, std::string coluna, std::string replica);
RcppExport SEXP _planejamento_anova_lsqd(SEXP dadosSEXP, SEXP xSEXP, SEXP ySEXP, SEXP linhaSEXP, SEXP colunaSEXP, SEXP replicaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dados(dadosSEXP);
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type linha(linhaSEXP);
    Rcpp::traits::input_parameter< std::string >::type coluna(colunaSEXP);
    Rcpp::traits::input_parameter< std::string >::type replica(replicaSEXP);
    rcpp_result_gen = Rcpp::wrap(anova_lsqd(dados, x, y, linha, coluna, replica));
    return rcpp_result_gen;
END_RCPP
}
// anova_rbcd
List anova_rbcd(DataFrame dados, std::string x, std::string y, std::string bloco);
RcppExport SEXP _planejamento_anova_rbcd(SEXP dadosSEXP, SEXP xSEXP, SEXP ySEXP, SEXP blocoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type dados(dadosSEXP);
    Rcpp::traits::input_parameter< std::string >::type x(xSEXP);
    Rcpp::traits::input_parameter< std::string >::type y(ySEXP);
    Rcpp::traits::input_parameter< std::string >::type bloco(blocoSEXP);
    rcpp_result_gen = Rcpp::wrap(anova_rbcd(dados, x, y, bloco));
    return rcpp_result_gen;
END_RCPP
}
// soma_grupo
NumericVector soma_grupo(NumericVector x, CharacterVector grupo);
RcppExport SEXP _planejamento_soma_grupo(SEXP xSEXP, SEXP grupoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type grupo(grupoSEXP);
    rcpp_result_gen = Rcpp::wrap(soma_grupo(x, grupo));
    return rcpp_result_gen;
END_RCPP
}
// mmult
NumericMatrix mmult(NumericMatrix m, NumericMatrix v);
RcppExport SEXP _planejamento_mmult(SEXP mSEXP, SEXP vSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type m(mSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type v(vSEXP);
    rcpp_result_gen = Rcpp::wrap(mmult(m, v));
    return rcpp_result_gen;
END_RCPP
}
// Rcpp_sort
NumericVector Rcpp_sort(CharacterVector x);
RcppExport SEXP _planejamento_Rcpp_sort(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(Rcpp_sort(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_planejamento_anova_bibd", (DL_FUNC) &_planejamento_anova_bibd, 4},
    {"_planejamento_anova_glsqd", (DL_FUNC) &_planejamento_anova_glsqd, 6},
    {"_planejamento_anova_lsqd", (DL_FUNC) &_planejamento_anova_lsqd, 6},
    {"_planejamento_anova_rbcd", (DL_FUNC) &_planejamento_anova_rbcd, 4},
    {"_planejamento_soma_grupo", (DL_FUNC) &_planejamento_soma_grupo, 2},
    {"_planejamento_mmult", (DL_FUNC) &_planejamento_mmult, 2},
    {"_planejamento_Rcpp_sort", (DL_FUNC) &_planejamento_Rcpp_sort, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_planejamento(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
