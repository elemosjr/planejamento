#' Simula delineamento de quadrados greco-latinos
#'
#' @description Simula um delineamento em quadrados greco-latinos
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
#' quadrados_glatinos(4)
#'
#' @export

quadrados_glatinos <- function(n, fun = rnorm)
{
  if((n < 1) | (n == 6)) stop("n deve ser maior ou igual a 3 e diferente de 6!")
  
  ordem <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      k <- i + j - 1
      if (k > n) k <- i + j - n - 1
      ordem[i, j] <- k
    }
  }
  
  tratamento <- factor(n)
  trat_g <- factor(n)
  
  while(all(tratamento == trat_g))
  {
    tratamento <- factor(1:n)[ordem[sample(1:n, n),]]
    
    trat_g <- factor(1:n)[ordem[sample(1:n, n),]]
  }

  resultado <- fun(n^2)
  
  coluna <- sort(rep(1:n, n))
  linha <- rep(1:n, n)
  
  ret_ <- list()
  
  ret_$dados <- tibble(resultado = resultado,
                       tratamento = tratamento,
                       trat_g = trat_g,
                       coluna = coluna,
                       linha = linha)

  dados_tratamentos <- ret_$dados %>%
    mutate(tratamento = glue("[{tratamento}, {trat_g}]"))
  
  ret_$dados_matriz <- dados_tratamentos %>%
    select(-c(resultado, trat_g)) %>%
    spread(coluna, tratamento)
  
  matriz <- dados_tratamentos %>%
    .$tratamento %>% matrix(n, n)
  
  rownames(matriz) <- 1:n
  colnames(matriz) <- 1:n
  
  ret_$matriz <- matriz
  
  ret_
}

# greek <- c("\\alpha", "\\beta", "\\gamma", "\\delta", "\\epsilon",
#            "\\zeta", "\\eta", "\\theta", "\\iota", "\\kappa",
#            "\\lambda", "\\mu", "\\nu", "\\xi", "\\omicron",
#            "\\pi", "\\rho", "\\sigma", "\\tau", "\\upsilon",
#            "\\phi", "\\chi", "\\psi", "\\omega")
 