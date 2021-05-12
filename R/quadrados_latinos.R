#' Simula delineamento de quadrados latinos
#'
#' @description Simula um delineamento em quadrados latinos a partir de um banco de dados
#'
#' @param dados Data frame com os resultados
#' @param tratamentos Vetor com fatores dos tratamentos
#'
#' @import dplyr
#'
#' @return Banco de dados com colunas dos tratamentos, linhas e colunas
#'
#' @examples 
#' 
#' quadrados_latinos(tibble(y = rnorm(16)), 1:4)
#'
#' @export

quadrados_latinos <- function(dados, tratamentos)
{
  n <- length(tratamentos)
  ordem <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      k <- i + j - 1
      if (k > n) k <- i + j - n - 1
      ordem[i, j] <- k
    }
  }
  
  dados %>%
    bind_cols(tibble(tratamento = tratamentos[ordem[sample(1:n, n),]])) %>%
    mutate(coluna = sort(rep(1:n, n)),
           linha = rep(1:n, n))
}