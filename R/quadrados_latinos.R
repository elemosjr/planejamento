#' Simula delineamento de quadrados latinos
#'
#' @description Simula um delineamento em quadrados latinos a partir de um banco de dados
#'
#' @param n Número de linhas (ou colunas)
#' @param fun Função que gere valores aleatórios cujo primeiro argumento é a quantidade de valores a serem gerados, Padrão = rnorm()
#'
#' @import dplyr
#'
#' @return Banco de dados com colunas dos tratamentos, linhas e colunas
#'
#' @examples 
#' 
#' quadrados_latinos(4)
#'
#' @export

quadrados_latinos <- function(n, fun = rnorm)
{
  ordem <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      k <- i + j - 1
      if (k > n) k <- i + j - n - 1
      ordem[i, j] <- k
    }
  }
  
  ret_ <- list()
  
  ret_$dados <- tibble(resultado = fun(n^2)) %>%
    bind_cols(tibble(tratamento = factor(1:n)[ordem[sample(1:n, n),]])) %>%
    mutate(coluna = sort(rep(1:n, n)),
           linha = rep(1:n, n))
  
  ret_$dados_matriz <- ret_$dados %>%
    unite("x", tratamento:resultado, sep = " = ") %>%
    spread(coluna, x)
  
  matriz <- ret_$dados %>%
    unite("x", tratamento:resultado, sep = " = ") %>%
    .$x %>% matrix(n, n)
  
  rownames(matriz) <- 1:n
  colnames(matriz) <- 1:n
  
  ret_$matriz <- matriz
  
  ret_
}

#dados %>% mutate(x = lapply(1:nrow(dados),
#                            function(x)
#                            {
#                              with(dados,
#                                   list(tratamento = tratamento[x],
#                                        resultado = resultado[x]))
#                            }))

#quadrados_latinos <- function(dados, tratamentos)
#{
#  n <- length(tratamentos)
#  ordem <- matrix(NA, n, n)
#  
#  for (i in 1:n) {
#    for (j in 1:n) {
#      k <- i + j - 1
#      if (k > n) k <- i + j - n - 1
#      ordem[i, j] <- k
#    }
#  }
#  
#  dados %>%
#    bind_cols(tibble(tratamento = tratamentos[ordem[sample(1:n, n),]])) %>%
#    mutate(coluna = sort(rep(1:n, n)),
#           linha = rep(1:n, n))
#}