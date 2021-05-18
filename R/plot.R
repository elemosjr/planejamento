#' Grafico de comparacoes (DIC, RBCD)
#'
#' @import ggplot2
#' @import patchwork
#' @import glue
#'
#' @description Graficos de comparacoes multiplas de delineamentos inteiramente casualizados e delineamentos em blocos complettamente casualizados
#'
#' @param comparacao Objeto de comparacao resultado da funcao `comparacao`
#' @param trat Variavel logica, caso verdadeira retorna somente o grafico de comparacoes dos tratamentos
#'
#' @return Ggplot com comparações
#' 
#' @examples
#' 
#' plot(comparacoes("gear", "hp", "carb", mtcars))
#' 
#' @export

plot.comparacao <- function(comparacao, trat = TRUE, ...)
{
  v <- comparacao$variaveis
  
  ptrat <- grafico(comparacao$dados, v$tratamento, v$resultado, v$bloco, "top") +
    labs(x = glue("{v$tratamento} (Tratamentos)"))
  
  if(trat) return(ptrat)
  
  pbloco <- grafico(comparacao$dados,  v$bloco, v$resultado, v$tratamento, "bottom") +
    labs(x = glue("{v$bloco} (Blocos)"))
  
  ptrat + pbloco + plot_layout(1)
}


#' Grafico de comparacoes (BIBD)
#'
#' @import ggplot2
#' @import patchwork
#' @import glue
#'
#' @description Graficos de comparacoes multiplas de delineamentos em blocos incompletos balanceados
#'
#' @param comparacao Objeto de comparacao_bib resultado da funcao `comparacao_bib`
#' @param trat Variavel logica, caso verdadeira retorna somente o grafico de comparacoes dos tratamentos
#'
#' @return Ggplot com comparações
#'
#' @examples
#' 
#' bloco_incompleto(4, 6, 2)$dados ->.;
#' comparacao <- comparacoes_bib("Trat", "resultado", "bloco", .)
#'
#' plot(comparacao)  
#'  
#' @export

plot.comparacao_bib <- function(comparacao, trat = TRUE, ...)
{
  v <- comparacao$variaveis
  
  ptrat <- grafico(comparacao$dados, v$tratamento, v$resultado, v$bloco, "top") +
    labs(x = glue("{v$tratamento} (Tratamentos)"))
  
  if(trat) return(ptrat)
  
  pbloco <- grafico(comparacao$dados,  v$bloco, v$resultado, v$tratamento, "bottom") +
    labs(x = glue("{v$bloco} (Blocos)"))
  
  ptrat + pbloco + plot_layout(1)
}


#' Grafico de comparacoes (Quadrados Latinos)
#'
#' @import ggplot2
#' @import glue
#'
#' @description Graficos de comparacoes multiplas de delineamentos em quadrados latinos
#'
#' @param comparacao Objeto de comparacao_lsqd resultado da funcao `comparacao_lsqd`
#'
#' @return Ggplot com comparações
#' 
#' @examples
#'
#' comparacao <- quadrados_latinos(4)$dados %>%
#'   comparacoes_lsqd("tratamento", "resultado", "linha", "coluna")
#'   
#' plot(comparacao)
#' 
#' @export

plot.comparacao_lsqd <- function(comparacao, ...)
{
  v <- comparacao$variaveis
  
  grafico(comparacao$dados, v$tratamento, v$resultado, NULL, "top") +
    labs(x = glue("{v$tratamento} (Tratamentos)"))
}
