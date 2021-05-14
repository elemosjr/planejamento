#include <Rcpp.h>
#include "utilidades.h"
using namespace Rcpp;

//' Soma por grupos
//'
//' @description Faz a soma de um vetor por grupos, semelhante a `tapply(y, x, sum)`.
//'
//' @param x Vetor numérico dos valores que serão somados
//' @param grupo Vetor com fatores que agruparão os dados
//'
//' @return Vetor com as somas resumidas de cada grupo.
//'
// [[Rcpp::export]]

NumericVector soma_grupo(NumericVector x, CharacterVector grupo)
{
  NumericVector idx = Rcpp_sort(grupo);
  
  grupo = grupo[idx];
  x = x[idx];
  
  CharacterVector nomes = unique(grupo);
  nomes = nomes[Rcpp_sort(nomes)];
  
  NumericVector somas(nomes.size());

  for(int i = 0; i < nomes.size(); i++)
  {
    for(int j = 0; j < x.size(); j++)
    {
      if(nomes[i] == grupo[j]) somas[i] += x[j];
    }
  }

  somas.attr("names") = nomes;

  return somas;
}

//' Multiplicação de matrizes
//'
//' @description Multplica duas matrizes.
//'
//' @param m Primeira matriz
//' @param v Segunda matriz
//'
//' @return Matriz.
//'
// [[Rcpp::export]]

NumericMatrix mmult(NumericMatrix m, NumericMatrix v)
{
  Environment base("package:base");
  Function mat_Mult = base["%*%"];
  return mat_Mult(m, v);
}

//' Ordenar vetor
//'
//' @description Gera um vetor com os indices para ordenação.
//'
//' @param x Vetor
//'
//' @return Vetor numerico com indices ordenados de forma que os dados fiquem ordenados.
//'
// [[Rcpp::export]]

NumericVector Rcpp_sort(CharacterVector x)
{
  IntegerVector idx = seq_along(x) - 1;
  std::sort(idx.begin(), idx.end(), [&](int i, int j){return x[i] < x[j];});
  return as<NumericVector>(idx);
}