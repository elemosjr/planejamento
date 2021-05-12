#' Base para gerar graficos de comparação
#'
#' @import ggplot2
#'
#' @description Funcao base para graficos de comparacoes multiplas.
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, e caso haja, blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos, manter NULL caso não hajam blocos
#' @param legenda String com a posição da legenda ("top", "bottom", "left", "right"), padrao = NULL
#'
#' @return Ggplot com geometria de pontos e facetes da variavel x
#' 
#' @examples 
#' 
#' grafico(mtcars, "cyl", "hp", "gear")
#' 
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
  
  if(!is.factor(dados[[col_]])) dados[[col_]] <- as.factor(dados[[col_]])
  
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