#' Simula delineamento de blocos incompletos
#'
#' @description Simula um delineamento em blocos incompletos a partir de numeros de tratamentos, blocos e repetições.
#'
#' @param ntrat Numero de tratamentos a serem simulados
#' @param nbloco Numero de blocos a serem simulados
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
#' bloco_casualizado(4, 2)
#'
#' @export

bloco_casualizado <- function(ntrat, nbloco, nrep = 1, fun = rnorm)
{
  resultado <- fun(ntrat*nbloco*nrep)
  
  ret_ <- list()
  
  bloco <- sort(rep(1:nbloco, ntrat*nrep))
  . <- sapply(bloco, function(x) nchar(x) != nchar(max(bloco)))
  
  bloco[.] <- bloco[.] %>%
    sapply(function(x) paste0(paste0(rep("0", nchar(max(bloco)) - 1),
                                     collapse = ""), x))
  
  ret_$dados <- tibble(Trat = as.factor(rep(1:ntrat, nbloco*nrep)),
                       resultado = as.numeric(resultado),
                       rep = c(replicate(nbloco, sort(rep(1:nrep, ntrat)))),
                       bloco = as.factor(bloco))
  
  ret_$dados_matriz <- ret_$dados %>%
    mutate(resultado = ifelse(is.na(resultado), "-", format(resultado, digits = 5)),
           bloco = paste0("B", bloco, "-", rep)) %>%
    select(-rep) %>%
    spread(bloco, resultado) 
  
  matriz <- ret_$dados %>%
    .$resultado %>% matrix(ntrat, nbloco*nrep)

  ret_$matriz <- matriz
  
  ret_
}

#bloco_casualizado <- function(ntrat, nbloco, fun)
#{
#  tibble(tratamento = sort(rep(1:ntrat, 4)))
#  
#  bind_cols(dados,
#            tibble(classe = c(replicate(length(blocos),
#                                        sample(classes, length(classes))))))
#}
