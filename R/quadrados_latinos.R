#' Simula delineamento de quadrados latinos
#'
#' @description Simula um delineamento em quadrados latinos a partir de um banco de dados
#'
#' @param n Número de linhas (ou colunas)
#' @param fun Função que gere valores aleatórios cujo primeiro argumento é a quantidade de valores a serem gerados, Padrão = rnorm()
#' @param nrep Número de réplicas a serem feitas, Padrão = 1
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

quadrados_latinos <- function(n, nrep = 1, fun = rnorm)
{
  if(nrep < 1) stop("O número de repetições deve ser um número positivo!")
  
  ordem <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      k <- i + j - 1
      if (k > n) k <- i + j - n - 1
      ordem[i, j] <- k
    }
  }
  
  tratamento <- replicate(nrep, factor(1:n)[ordem[sample(1:n, n),]])
  
  tratamentos <- paste0("[",
                        apply(tratamento, 1,
                              function(x) paste(x, collapse = " ")),
                        "]")
  
  resultado <- replicate(nrep, fun(n^2))
  
  resultados <- apply(resultado, 1, sum)

  coluna <- sort(rep(1:n, n))
  linha <- rep(1:n, n)
  replica <- sort(rep(1:nrep, n^2))
  
  ret_ <- list()
  
  ret_$dados_resumidos <- tibble(resultado = resultados,
                                 tratamento = tratamentos,
                                 coluna = coluna,
                                 linha = linha)
  
  ret_$dados <- tibble(resultado = c(resultado),
                       tratamento = c(tratamento),
                       coluna = c(replicate(nrep, coluna)),
                       linha = c(replicate(nrep, linha)),
                       replica = replica)
  
  if(nrep > 1)
  {
    ret_$dados_individuais <- lapply(
      1:nrep,
      function(x)
      {
        tibble(resultado = resultado[,x],
               tratamento = tratamento[,x],
               coluna = coluna,
               linha = linha,
               replica = x)
      }
    )
  }
  
  ret_$dados_matriz <- ret_$dados_resumidos %>%
    select(-resultado) %>%
    spread(coluna, tratamento)
  
  matriz <- ret_$dados_resumidos %>%
    .$tratamento %>% matrix(n, n)
  
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
