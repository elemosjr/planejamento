#ifndef UTILIDADE
#define UTILIDADE

#include <Rcpp.h>
using namespace Rcpp;

NumericVector soma_grupo(NumericVector x, CharacterVector grupo);

NumericMatrix mmult(NumericMatrix m, NumericMatrix v);

NumericVector Rcpp_sort(CharacterVector x);

#endif
