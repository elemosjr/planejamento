#include <Rcpp.h>
#include "utilidades.h"
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

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
//' bloco_incompleto(4, 6, 2)$dados %>%
//'   anova_bibd("Trat", "resultado", "bloco")
//'
//' @export
//'
// [[Rcpp::export]]

List anova_bibd(DataFrame dados, std::string x, std::string y, std::string bloco)
{
  Environment tidyr = Environment::namespace_env("tidyr");
  Function drop_na = tidyr["drop_na"];
  
  NumericVector Y = dados[y];

  LogicalVector bool_ = is_na(Y);
  
  dados = drop_na(dados);
  
  CharacterVector X = dados[x];
  Y = dados[y];
  CharacterVector Bloco = dados[bloco];
  
//  X = X[Rcpp_sort(X)];
//  Y = Y[Rcpp_sort(X)];
//  Bloco = Bloco[Rcpp_sort(X)];
  
  double a = unique(X).size();
  double b = unique(Bloco).size();
  double N = dados.nrows();
  double r = N/a;
  double k = N/b;
  double lambda = (r * (k - 1)) / (a-1);
  double y__ = sum(Y);
  
  NumericVector yi_ = soma_grupo(Y, X);
  NumericVector y_j = soma_grupo(Y, Bloco);
  
  double mu = mean(Y);
  NumericVector tau = yi_ - mu;
  NumericVector beta = y_j - mu;
  
  double sqt = sum(pow(Y, 2)) - pow(y__, 2) / N;
  double sqtrat = sum(pow(yi_, 2)) / r - pow(y__, 2) / N;
  double sqbloco = sum(pow(y_j, 2)) / k - pow(y__, 2) / N;

  NumericVector nijvec(a*b);
  for(int i = 0; i < a*b; i++)
  {
    if(bool_[i])
    {
      nijvec[i] = 0;
    } else
    {
      nijvec[i] = 1;
    }
  }
  nijvec.attr("dim") = Dimension(a, b);
  NumericMatrix nij = transpose(as<NumericMatrix>(nijvec));
  
  NumericMatrix yi_matrix(a, 1, yi_.begin());
  NumericMatrix y_jmatrix(b, 1, y_j.begin());
  
  NumericMatrix Qitmp = mmult(nij, y_jmatrix) / k;
  NumericMatrix Qi(a, 1); // = yi_matrix - Qitmp;
  for(int i = 0; i < a; i++)
  {
    Qi[i] = yi_matrix[i] - Qitmp[i];
  }

  NumericVector Qi2 = pow(Qi, 2);
  
  double sqtrat_aj = k * sum(Qi2) / (lambda * a);
  double sqe = sqt- sqtrat_aj - sqbloco;
  double qmtrat_aj = sqtrat_aj / (a-1);
  double qmbloco = sqbloco / (b-1);
  double qme = sqe / (N - a - b + 1);
  
  double f0 = qmtrat_aj / qme;
  double pvalor = R::pf(f0, a - 1, N - a - b + 1, false, false);
  double pbloco = R::pf(qmbloco/qme, b - 1, N - a - b + 1, false, false);
  
  double tau_ = mean(tau);
  double beta_ = mean(beta);
  double tau_i = mean(yi_);
  double beta_i = mean(y_j);
  
  List stat = List::create(
    Named("a") = a,
    Named("b") = b,
    Named("N") = N,
    Named("r") = r,
    Named("k") = k,
    Named("lambda") = lambda,
    Named("y..") = y__,
    Named("yi.") = yi_,
    Named("y.j") = y_j,
    Named("nij") = nij,
    Named("Qi") = Qi,
    Named("Qi2") = Qi2,
    Named("mu") = mu,
    Named("tau") = tau,
    Named("beta") = beta,
    Named("tau_") = tau_,
    Named("beta_") = beta_,
    Named("tau_i") = tau_i,
    Named("beta_i") = beta_i
  );
  
  DataFrame anova_df = DataFrame::create(
    Named("Fonte de variação") = CharacterVector::create(
      x, bloco, "Erros", "Total"
    ),
    Named("Graus de liberdade") = NumericVector::create(
      a-1, b-1, N - a - b + 1, N-1
    ),
    Named("Quadrado Médio") = NumericVector::create(
      qmtrat_aj, qmbloco, qme, R_NaN
    ),
    Named("F0") = NumericVector::create(
      f0, qmbloco/qme, R_NaN, R_NaN
    ),
    Named("p-valor") = NumericVector::create(
      pvalor, pbloco, R_NaN, R_NaN
    )
  );
  
  List anova_list = List::create(
    Named("sqt") = sqt,
    Named("sqtrat_aj") = sqtrat_aj,
    Named("sqbloco") = sqbloco,
    Named("sqe") = sqe,
    Named("qmtrat_aj") = qmtrat_aj,
    Named("qmbloco") = qmbloco,
    Named("qme") = qme,
    Named("f0") = f0,
    Named("pvalor") = pvalor
  );
  
  DataFrame estimados = DataFrame::create(
    Named("$\\hat{\\mu}$") = mu,
    Named("$\\hat{\\tau}$") = tau_,
    Named("$\\hat{\\beta}$") = beta_,
    Named("$\\hat{\\tau}_i$") = tau_i,
    Named("$\\hat{\\beta}_i$") = beta_i
  );
  
  List L = List::create(
    Named("stat") = stat,
    Named("anova_df") = anova_df,
    Named("anova_list") = anova_list,
    Named("estimados") = estimados,
    Named("pvalor") = pvalor
  );
  return L;
}