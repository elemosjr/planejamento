#include <Rcpp.h>
#include "utilidades.h"
using namespace Rcpp;

//' Gera valores de anova para RBCD teste
//'
//' @description Calcula valores utilizados para uma tabela de ANOVA de um delineamento em blocos completamente casualizados
//'
//' @param dados Data frame com colunas separadas para os tratamentos, os blocos e os resultados a serem analisados
//' @param x String com o nome da coluna dos tratamentos
//' @param y String com o nome da coluna dos resultados
//' @param bloco String com o nome da coluna dos blocos
//'
//' @return Objeto de tipo lista contendo todos os valores utilizados para o calculo da tabela da ANOVA, os valores da tabela da ANOVA e os parâmetros estimados.
//'
//' @examples
//'
//' anova_rbcd(mtcars, "gear", "hp", "carb")
//'
//' @export
//'
// [[Rcpp::export]]

List anova_rbcd(DataFrame dados, std::string x, std::string y, std::string bloco)
{
  Environment tidyr = Environment::namespace_env("tidyr");
  Function drop_na = tidyr["drop_na"];

  dados = drop_na(dados);
  
  CharacterVector X = dados[x];
  NumericVector Y = dados[y];
  CharacterVector Bloco = dados[bloco];
  X = na_omit(X); Y = na_omit(Y); Bloco = na_omit(Bloco);
  
  if(X.size() != Y.size() | X.size() != Bloco.size()) stop("Comprimento dos vetores difere!");
  
  double a = unique(X).size();
  double b = unique(Bloco).size();
  double N = dados.nrow();
  double y__ = sum(Y);
  
  NumericVector yi_ = soma_grupo(Y, X);
  NumericVector y_j = soma_grupo(Y, Bloco);
  
  double mu = mean(noNA(Y));
  NumericVector tau = yi_ - mu;
  NumericVector beta = y_j - mu;
  
  double sqt = sum(pow(Y, 2)) - pow(y__, 2) / N;
  double sqtrat = sum(pow(yi_, 2)) / b - pow(y__, 2) / N;
  double sqbloco = sum(pow(y_j, 2)) / a - pow(y__, 2) / N;
  double sqe = sqt- sqtrat - sqbloco;
  
  double qmtrat = sqtrat / (a-1);
  double qmbloco = sqbloco / (b-1);
  double qme = sqe / ((a-1) * (b-1));
  
  double f0 = qmtrat / qme;
  double pvalor = R::pf(f0, a-1, (a-1) * (b-1), false, false);
  
  double tau_ = mean(tau);
  double beta_ = mean(beta);
  double tau_i = mean(yi_);
  double beta_i = mean(y_j);
  
  DataFrame estimados = DataFrame::create(Named("$\\hat{\\mu}$") = mu,
                                          Named("$\\hat{\\tau}$") = tau_,
                                          Named("$\\hat{\\beta}$") = beta_,
                                          Named("$\\hat{\\tau}_i$") = tau_i,
                                          Named("$\\hat{\\beta}_i$") = beta_i);
  
  List L = List::create(Named("a") = a,
                        Named("b") = b,
                        Named("yi.") = yi_,
                        Named("y.j") = y_j,
                        Named("mu") = mu,
                        Named("tau") = tau,
                        Named("beta") = beta,
                        Named("sqt") = sqt,
                        Named("sqtrat") = sqtrat,
                        Named("sqbloco") = sqbloco,
                        Named("sqe") = sqe,
                        Named("qmtrat") = qmtrat,
                        Named("qmbloco") = qmbloco,
                        Named("qme") = qme,
                        Named("f0") = f0,
                        Named("pvalor") = pvalor,
                        Named("estimados") = estimados);
  return L;
}