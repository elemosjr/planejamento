#'
#' Funcoes de graficos
#'

#' @title Grafico
#'
#' @import ggplot2
#'
#' @description Funcao base para graficos de comparacoes multiplas
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, e caso haja, blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos, manter NULL caso não hajam blocos
#' @param legenda String com a posição da legenda ("top", "bottom", "left", "right"), padrao = NULL
#'
#' @return ggplot
#' @export

grafico <- function(dados, x, y, bloco = NULL, legenda = NULL)
{
  if(!is.null(bloco))
  {
    col_ <- bloco
    if(is.null(legenda))
    {
      legenda <- "top"
    }
  } else
  {
    col_ <- x
    if(is.null(legenda))
    {
      legenda <- "none"
    }
  }
  
  dados %>%
    ggplot(aes(x = "", y = .data[[y]], col = .data[[col_]])) +
    geom_jitter() + facet_grid(. ~ .data[[x]]) + labs(x = x) +
    theme(legend.position = legenda,
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "white"),
          plot.margin = margin(.5, 1, .5, 1, "cm"),
          plot.background = element_rect(colour = "black"))
}


#' @title Grafico de comparacoes (DIC, RBCD)
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
#' @return ggplot
#' @export

plot.comparacao <- function(comparacao, trat = FALSE, ...)
{
  v <- comparacao$variaveis
  
  ptrat <- grafico(dados, v$tratamento, v$resultado, v$bloco, "top") +
    labs(x = glue("{v$tratamento} (Tratamentos)"))
  
  if(trat) return(ptrat)
  
  pbloco <- grafico(dados,  v$bloco, v$resultado, v$tratamento, "bottom") +
    labs(x = glue("{v$bloco} (Blocos)"))
  
  ptrat + pbloco + plot_layout(1)
}


#' @title Grafico de comparacoes (BIBD)
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
#' @return ggplot
#' @export

plot.comparacao_bib <- function(comparacao, trat = FALSE, ...)
{
  v <- comparacao$variaveis
  
  ptrat <- grafico(comparacao$dados, v$tratamento, v$resultado, v$bloco, "top") +
    labs(x = glue("{v$tratamento} (Tratamentos)"))
  
  if(trat) return(ptrat)
  
  pbloco <- grafico(comparacao$dados,  v$bloco, v$resultado, v$tratamento, "bottom") +
    labs(x = glue("{v$bloco} (Blocos)"))
  
  ptrat + pbloco + plot_layout(1)
}


#' @title Grafico de comparacoes (Quadrados Latinos)
#'
#' @import ggplot2
#' @import glue
#'
#' @description Graficos de comparacoes multiplas de delineamentos em quadrados latinos
#'
#' @param comparacao Objeto de comparacao_lsqd resultado da funcao `comparacao_lsqd`
#'
#' @return ggplot
#' @export

plot.comparacao_lsqd <- function(comparacao, ...)
{
  v <- comparacao$variaveis
  
  grafico(comparacao$dados, v$tratamento, v$resultado, NULL, "top") +
    labs(x = glue("{v$tratamento} (Tratamentos)"))
}
