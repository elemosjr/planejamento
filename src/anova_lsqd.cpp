#include <Rcpp.h>
#include "utilidades.h"
using namespace Rcpp;

//' Gera valores de anova para Quadrados Latinos
//'
//' @description Calcula valores utilizados para uma tabela de ANOVA de um delineamento em quadrados latinos
//'
//' @param dados Data frame com colunas separadas para os tratamentos, os blocos e os resultados a serem analisados
//' @param x String com o nome da coluna dos tratamentos
//' @param y String com o nome da coluna dos resultados
//' @param linha String com o nome da coluna do data frame que representa a linha do quadrado latino
//' @param coluna String com o nome da coluna do data frame que representa a coluna do quadrado latino
//' @param replica String com o nome da coluna do data frame que representa as replicas
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

List anova_lsqd(DataFrame dados, std::string x, std::string y, std::string linha, std::string coluna, std::string replica = "")
{
  Environment tidyr = Environment::namespace_env("tidyr");
  Function drop_na = tidyr["drop_na"];

  dados = drop_na(dados);
  
  CharacterVector X = dados[x];
  NumericVector Y = dados[y];
  CharacterVector Linha = dados[linha];
  CharacterVector Coluna = dados[coluna];
  
  CharacterVector Replica;
  double n;
  
  if(replica != "")
  {
    Replica = dados[replica];
    n = unique(Replica).size();
  } else
  {
    n = 1;
  }

  double N = dados.nrow();
  double p = sqrt(N/n);
  double y___ = sum(Y);
  
  NumericVector yi__ = soma_grupo(Y, Linha);
  NumericVector y_j_ = soma_grupo(Y, X);
  NumericVector y__k = soma_grupo(Y, Coluna);
  
  double mu = mean(Y);
  NumericVector tau = y_j_ - mu;
  NumericVector alpha = yi__ - mu;
  NumericVector beta = y__k - mu;
  
  double sqt = sum(pow(Y, 2)) - pow(y___, 2) / N;
  double sqtrat = sum(pow(y_j_, 2)) / (p*n) - pow(y___, 2) / N;
  double sqlinha = sum(pow(yi__, 2)) / (p*n) - pow(y___, 2) / N;
  double sqcoluna = sum(pow(y__k, 2)) / (p*n) - pow(y___, 2) / N;
  double sqe = sqt- sqtrat - sqlinha - sqcoluna;
  
  double qmtrat = sqtrat / (p-1);
  double qmlinha = sqlinha / (p-1);
  double qmcoluna = sqcoluna / (p-1);
  double qme = sqe / ((p-2) * (p-1));
  
  double f0 = qmtrat / qme;
  double pvalor = R::pf(f0, p-1, (p-2) * (p-1), false, false);
  
  // valores estimados
  double tau_ = mean(tau);
  double alpha_ = mean(alpha);
  double beta_ = mean(beta);
  double tau_i = mean(y_j_);
  double alpha_i = mean(yi__);
  double beta_i = mean(y__k);
  
  List L;
  
  if(replica == "")
  {
    // Bloco para quando NÃO há replicas
    List stat = List::create(
      Named("N") = N, Named("p") = p, Named("n") = n,
      Named("y...") = y___, Named("yi..") = yi__,
      Named("y.j.") = y_j_, Named("y..k") = y__k,
      Named("mu") = mu, Named("tau") = tau,
      Named("alpha") = alpha, Named("beta") = beta,
      Named("tau_") = tau_, Named("alpha_") = alpha_,
      Named("beta_") = beta_, Named("tau_i") = tau_i,
      Named("alpha_i") = alpha_i, Named("beta_i") = beta_i);
    
    
    // p valores
    double plinha = R::pf(qmlinha/qme, p-1, (p-2) * (p-1), false, false);
    double pcoluna = R::pf(qmcoluna/qme, p-1, (p-2) * (p-1), false, false);
    
    DataFrame anova_df = DataFrame::create(
      Named("Fonte de variação") = CharacterVector::create(
        x, linha, coluna, "Erros", "Total"
      ),
      Named("Graus de liberdade") = NumericVector::create(
        p-1, p-1, p-1, (p-1)*(p-1), pow(p, 2)-1
      ),
      Named("Quadrado Médio") = NumericVector::create(
        qmtrat, qmlinha, qmcoluna, qme, R_NaN
      ),
      Named("F0") = NumericVector::create(
        f0, qmlinha/qme, qmcoluna/qme, qme, R_NaN
      ),
      Named("p-valor") = NumericVector::create(
        pvalor, plinha, pcoluna, R_NaN, R_NaN
      )
    );
    
    List anova_list = List::create(
      Named("sqt") = sqt,
      Named("sqtrat") = sqtrat,
      Named("sqlinha") = sqlinha,
      Named("sqcoluna") = sqcoluna,
      Named("sqe") = sqe,
      Named("qmtrat") = qmtrat,
      Named("qmlinha") = qmlinha,
      Named("qmcoluna") = qmcoluna,
      Named("f0") = f0,
      Named("pvalor") = pvalor
    );
    
    DataFrame estimados = DataFrame::create(
      Named("$\\hat{\\mu}$") = mu,
      Named("$\\hat{\\tau}$") = tau_,
      Named("$\\hat{\\alpha}$") = alpha_,
      Named("$\\hat{\\beta}$") = beta_,
      Named("$\\hat{\\tau}_i$") = tau_i,
      Named("$\\hat{\\alpha}_i$") = alpha_i,
      Named("$\\hat{\\beta}_i$") = beta_i
    );
    
    L = List::create(Named("stat") = stat,
                     Named("anova_df") = anova_df,
                     Named("anova_list") = anova_list,
                     Named("estimados") = estimados,
                     Named("pvalor") = pvalor);
  } else
  {
    // Bloco para quando há replicas
    NumericVector y___l = soma_grupo(Y, Replica);
    NumericVector lambda = y___l - mu;
    double sqrep = sum(pow(y___l, 2)) / pow(p, 2) - pow(y___, 2) / N;
    double qmrep = sqrep / (n-1);
    sqe -= sqrep;
    qme = sqe / ((p-1) * (n * (p+1) - 3));
    f0 = qmtrat / qme;
    pvalor = R::pf(f0, p-1, ((p-1) * (n * (p+1) - 3)), false, false);
    
    // valores estimados
    double lambda_ = mean(lambda);
    double lambda_i = mean(y___l);
    
    List stat = List::create(
      Named("N") = N, Named("p") = p, Named("n") = n,
      Named("y...") = y___, Named("yi..") = yi__,
      Named("y.j.") = y_j_, Named("y..k") = y__k,
      Named("y...l") = y___l,
      Named("mu") = mu, Named("tau") = tau,
      Named("lambda") = lambda,
      Named("alpha") = alpha, Named("beta") = beta,
      Named("tau_") = tau_, Named("alpha_") = alpha_,
      Named("beta_") = beta_, Named("tau_i") = tau_i,
      Named("alpha_i") = alpha_i, Named("beta_i") = beta_i);
    
    
    // p valores
    double plinha = R::pf(qmlinha/qme, p-1, ((p-1) * (n * (p+1) - 3)), false, false);
    double pcoluna = R::pf(qmcoluna/qme, p-1, ((p-1) * (n * (p+1) - 3)), false, false);
    double prep = R::pf(qmrep/qme, n-1, ((p-1) * (n * (p+1) - 3)), false, false);
    
    DataFrame anova_df = DataFrame::create(
      Named("Fonte de variação") = CharacterVector::create(
        x, linha, coluna, replica, "Erros", "Total"
      ),
      Named("Graus de liberdade") = NumericVector::create(
        p-1, p-1, p-1, n-1, ((p-1) * (n * (p+1) - 3)), pow(p, 2)-1
      ),
      Named("Quadrado Médio") = NumericVector::create(
        qmtrat, qmlinha, qmcoluna, qmrep, qme, R_NaN
      ),
      Named("F0") = NumericVector::create(
        f0, qmlinha/qme, qmcoluna/qme, qmrep/qme, qme, R_NaN
      ),
      Named("p-valor") = NumericVector::create(
        pvalor, plinha, pcoluna, prep, R_NaN, R_NaN
      )
    );
    
    List anova_list = List::create(
      Named("sqt") = sqt,
      Named("sqtrat") = sqtrat,
      Named("sqlinha") = sqlinha,
      Named("sqcoluna") = sqcoluna,
      Named("sqrep") = sqrep,
      Named("sqe") = sqe,
      Named("qmtrat") = qmtrat,
      Named("qmlinha") = qmlinha,
      Named("qmcoluna") = qmcoluna,
      Named("qmrep") = qmrep,
      Named("f0") = f0,
      Named("pvalor") = pvalor
    );
    
    DataFrame estimados = DataFrame::create(
      Named("$\\hat{\\mu}$") = mu,
      Named("$\\hat{\\tau}$") = tau_,
      Named("$\\hat{\\alpha}$") = alpha_,
      Named("$\\hat{\\beta}$") = beta_,
      Named("$\\hat{\\lambda}$") = lambda_,
      Named("$\\hat{\\tau}_i$") = tau_i,
      Named("$\\hat{\\alpha}_i$") = alpha_i,
      Named("$\\hat{\\beta}_i$") = beta_i,
      Named("$\\hat{\\lambda}_i$") = lambda_i
    );
    
    L = List::create(Named("stat") = stat,
                     Named("anova_df") = anova_df,
                     Named("anova_list") = anova_list,
                     Named("estimados") = estimados,
                     Named("pvalor") = pvalor);
  }
  
  return L;
}