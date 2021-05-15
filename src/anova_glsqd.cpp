#include <Rcpp.h>
#include "utilidades.h"
using namespace Rcpp;

//' Gera valores de anova para Quadrados Latinos
//'
//' @description Calcula valores utilizados para uma tabela de ANOVA de um delineamento em quadrados latinos
//'
//' @param dados Data frame com colunas separadas para os tratamentos, os blocos e os resultados a serem analisados
//' @param x String com o nome da coluna dos tratamentos
//' @param chi String com o nome da coluna dos outros tratamentos
//' @param y String com o nome da coluna dos resultados
//' @param linha String com o nome da coluna do data frame que representa a linha do quadrado latino
//' @param coluna String com o nome da coluna do data frame que representa a coluna do quadrado latino
//'
//' @import glue
//' @import tidyr
//'
//' @return Objeto de tipo lista contendo todos os valores utilizados para o calculo da tabela da ANOVA, os valores da tabela da ANOVA e os parâmetros estimados.
//' 
//' @examples
//'
//' quadrados_latinos(4)$dados %>%
//'  anova_lsqd("tratamento", "resultado", "linha", "coluna")
//'
//' @export
//'
// [[Rcpp::export]]

List anova_glsqd(DataFrame dados, std::string x, std::string chi, std::string y, std::string linha, std::string coluna)
{
  Environment tidyr = Environment::namespace_env("tidyr");
  Function drop_na = tidyr["drop_na"];

  dados = drop_na(dados);
  
  CharacterVector X = dados[x];
  NumericVector Y = dados[y];
  CharacterVector Linha = dados[linha];
  CharacterVector Coluna = dados[coluna];
  CharacterVector Chi = dados[chi];

  double N = dados.nrow();
  double p = sqrt(N);
  double y____ = sum(Y);
  
  NumericVector yi___ = soma_grupo(Y, Linha);
  NumericVector y_j__ = soma_grupo(Y, X);
  NumericVector y__k_ = soma_grupo(Y, Chi);
  NumericVector y___l = soma_grupo(Y, Coluna);
  
  double mu = mean(Y);
  NumericVector theta = yi___ - mu;
  NumericVector tau = y_j__ - mu;
  NumericVector omega = y__k_ - mu;
  NumericVector psi = y___l - mu;
  
  double sqt = sum(pow(Y, 2)) - pow(y____, 2) / N;
  double sqtrat = sum(pow(y_j__, 2)) / p - pow(y____, 2) / N;
  double sqtrat_g = sum(pow(y__k_, 2)) / p - pow(y____, 2) / N;
  double sqlinha = sum(pow(yi___, 2)) / p - pow(y____, 2) / N;
  double sqcoluna = sum(pow(y___l, 2)) / p - pow(y____, 2) / N;
  double sqe = sqt- sqtrat - sqtrat_g - sqlinha - sqcoluna;
  
  double qmtrat = sqtrat / (p-1);
  double qmtrat_g = sqtrat_g / (p-1);
  double qmlinha = sqlinha / (p-1);
  double qmcoluna = sqcoluna / (p-1);
  double qme = sqe / ((p-3) * (p-1));
  
  double gltrat = p-1;
  double gltrat_g = p-1;
  double gllinha = p-1;
  double glcoluna = p-1;
  double gle = (p-3)*(p-1);
  
  double f0 = qmtrat / qme;
  double pvalor = R::pf(f0, gltrat, gle, false, false);
  
  // valores estimados
  double theta_ = mean(theta);
  double tau_ = mean(tau);
  double omega_ = mean(omega);
  double psi_ = mean(psi);
  double theta_i = mean(yi___);
  double tau_i = mean(y_j__);
  double omega_i = mean(y__k_);
  double psi_i = mean(y___l);

  List stat = List::create(
    Named("N") = N, Named("p") = p, Named("y....") = y____,
    Named("yi...") = yi___, Named("y.j..") = y_j__,
    Named("y..k_") = y__k_, Named("y...l") = y___l,
    Named("mu") = mu, Named("theta") = theta,
    Named("tau") = tau, Named("omega") = omega,
    Named("psi") = psi, Named("theta_") = tau_,
    Named("tau_") = tau_, Named("omega_") = omega_,
    Named("psi_") = psi_, Named("theta_i") = theta_i,
    Named("tau_i") = tau_i, Named("omega_i") = omega_i,
    Named("psi_i") = psi_i);
  
  // p valores
  double ptrat_g = R::pf(qmtrat_g/qme, gltrat_g, gle, false, false);
  double plinha = R::pf(qmlinha/qme, gllinha, gle, false, false);
  double pcoluna = R::pf(qmcoluna/qme, glcoluna, gle, false, false);
  
  DataFrame anova_df = DataFrame::create(
    Named("Fonte de variação") = CharacterVector::create(
      x, chi, linha, coluna, "Erros", "Total"
    ),
    Named("Graus de liberdade") = NumericVector::create(
      gltrat, gltrat_g, gllinha, glcoluna, gle, pow(p, 2)-1
    ),
    Named("Quadrado Médio") = NumericVector::create(
      qmtrat, qmtrat_g, qmlinha, qmcoluna, qme, R_NaN
    ),
    Named("F0") = NumericVector::create(
      f0, qmtrat_g/qme, qmlinha/qme, qmcoluna/qme, qme, R_NaN
    ),
    Named("p-valor") = NumericVector::create(
      pvalor, ptrat_g, plinha, pcoluna,  R_NaN, R_NaN
    )
  );
  
  List anova_list = List::create(
    Named("sqt") = sqt,
    Named("sqtrat") = sqtrat,
    Named("sqtrat_g") = sqtrat_g,
    Named("sqlinha") = sqlinha,
    Named("sqcoluna") = sqcoluna,
    Named("sqe") = sqe,
    Named("qmtrat") = qmtrat,
    Named("qmtrat_g") = qmtrat_g,
    Named("qmlinha") = qmlinha,
    Named("qmcoluna") = qmcoluna,
    Named("f0") = f0,
    Named("pvalor") = pvalor
  );
  
  DataFrame estimados = DataFrame::create(
    Named("$\\hat{\\mu}$") = mu,
    Named("$\\hat{\\theta}$") = theta_,
    Named("$\\hat{\\tau}$") = tau_,
    Named("$\\hat{\\omega}$") = omega_,
    Named("$\\hat{\\psi}$") = psi_,
    Named("$\\hat{\\theta}_i$") = theta_i,
    Named("$\\hat{\\tau}_i$") = tau_i,
    Named("$\\hat{\\omega}_i$") = omega_i,
    Named("$\\hat{\\psi}_i$") = psi_i
  );
  
  List L = List::create(Named("stat") = stat,
                        Named("anova_df") = anova_df,
                        Named("anova_list") = anova_list,
                        Named("estimados") = estimados,
                        Named("pvalor") = pvalor);

  
  return L;
}