#' Simula delineamento de blocos incompletos
#'
#' @description Simula um delineamento em blocos incompletos a partir de numeros de tratamentos, blocos e repetições.
#'
#' @param ntrat Numero de tratamentos a serem simulados
#' @param nbloco Numero de blocos a serem simulados
#' @param k Numero de tratamentos por bloco
#' @param fun Função que gere valores aleatórios cujo primeiro argumento é a quantidade de valores a serem gerados, Padrão = rnorm()
#' 
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#' @return Objeto de tipo lista com os elementos:
#'  - **dados** Dados simulados em formato longo
#'  - **dados_matriz** Dados simulados em formato largo, como matiz
#'  - **kable** Texto em LaTeX da tabela pré-formatada
#'  - **matriz** Matriz dos dados simulados
#'
#' @examples 
#' 
#' bloco_incompleto(8, 14, 3)$dados_matriz %>% kable_()
#' 
#' bloco_incompleto(4, 6, 2)
#'
#' @export

bloco_incompleto <- function(ntrat, nbloco, k = 0, fun = rnorm)
{
  resultado <- fun(ntrat*nbloco)
  
  if(k > ntrat) stop("k deve ser igual ou menor que o número de tratamentos!")
  if(k < 0) stop("k deve ser um número positivo!")
  
  if(k != ntrat)
  {
    n_na <- ntrat - k
    
    combinacoes <- combn(1:ntrat, n_na)
    
    if(ncol(combinacoes) < nbloco)
    {
      combinacoes <- rep(combinacoes, ceiling(n_na*nbloco/length(combinacoes)))[1:(nbloco*n_na)]
      combinacoes <- matrix(combinacoes, n_na)
    }
    
    rand <- matrix(combinacoes[,sample(1:ncol(combinacoes), nbloco)], n_na)
  
    for(i in 1:nbloco) rand[,i] <- rand[,i] + ntrat * (i - 1)
  
    resultado[rand] <- NA
  }
  
  ret_ <- list()
  
  bloco <- sort(rep(1:nbloco, ntrat))
  sapply(sort(rep(1:nbloco, ntrat)),
         function(x) nchar(x) != nchar(max(bloco))) ->.;
  
  bloco[.] <- bloco[.] %>%
    sapply(function(x) paste0(paste0(rep("0", nchar(max(bloco)) - 1),
                                     collapse = ""), x))
  
  ret_$dados <- tibble(Trat = rep(1:ntrat, nbloco),
                       resultado = as.numeric(resultado),
                       bloco = paste0("B", bloco))
  
  ret_$dados_matriz <- ret_$dados %>%
    mutate(resultado = ifelse(is.na(resultado), "-", format(resultado, digits = 5))) %>%
    spread(bloco, resultado) 
  
  matriz <- ret_$dados %>%
    .$resultado %>% matrix(ntrat, nbloco)
  
  rownames(matriz) <- 1:ntrat
  colnames(matriz) <- 1:nbloco
  
  ret_$matriz <- matriz
  
  ret_
}
