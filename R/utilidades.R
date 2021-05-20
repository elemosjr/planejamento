pkgcall <- function(pkg)
{
  if(!do.call("require", list(pkg)))
  {
    install.packages(pkg)
    do.call("library", list(pkg))
  }
}

.onAttach <- function(...) mapply(pkgcall, c("tidyverse", "broom", "patchwork", "agricolae",
                                             "knitr", "glue", "outliers", "Deriv", "emmeans"))


#' @useDynLib planejamento, .registration=TRUE
#' @importFrom Rcpp sourceCpp evalCpp
#' @import tidyverse
#' @import knitr
#' @md
{}

#' Kable pre configurada
#'
#' @import knitr
#'
#' @description Kable modificada para centralizar todas as colunas
#'
#' @param x Data frame a ser printado
#'
#' @return Kable com colunas centralizadas
#' 
#' @examples 
#' 
#' kable_(mtcars)
#' 
#' @export

kable_ <- function(x) kable(x, align = rep("c", ncol(x)))

#' Print Anova
#'
#' @import broom
#' @import dplyr
#'
#' @description Organiza e printa uma tabela de anova em formato de LaTeX
#'
#' @param model Modelo de anova
#'
#' @return Kable
#' 
#' @examples 
#' 
#' aov(hp ~ cyl, data = mtcars) %>% print_anova()
#' 
#' @export

print_anova <- function(model)
{
  tidy(model) %>%
    mutate(term = str_replace_all(term, "`", "")) %>%
    select(`\\textbf{Fonte de Variação}` = term,
           `\\textbf{Graus de Liberdade}` = df,
           `\\textbf{Soma de Quadrados}` = sumsq,
           `\\textbf{Quadrado Médio}` = meansq,
           `\\textbf{$F_0$}` = statistic,
           `\\textbf{P-valor}` = p.value) %>%
    kable_()
}


#' Estimando valores ausentes de BIBD
#'
#' @description Estima os valores ausentes de um delineamento incompleto balanceado
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, com valores faltantes, e blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos
#'
#' @return Data frame com os valores estimados
#' 
#' @examples 
#' 
#' bloco_incompleto(4, 6, 2)$dados %>%
#'   estimando_ausentes("Trat", "resultado", "bloco")
#' 
#' @export

estimando_ausentes <- function(dados, x, y, bloco)
{
  faltantes <- which(is.na(dados[[y]]))
  faltantes_x <- dados[[x]][faltantes]
  faltantes_bloco <- dados[[bloco]][faltantes]
  
  res <- numeric(length(faltantes))
  
  a <- length(unique(dados[[x]]))
  b <- length(unique(dados[[bloco]]))
  y.. <- sum(dados[[y]], na.rm = TRUE)
  
  for(i in 1:length(faltantes))
  {
    yi. <- sum(dados[[y]][dados[[x]] == faltantes_x[i]], na.rm = TRUE)
    y.j <- sum(dados[[y]][dados[[bloco]] == faltantes_bloco[i]], na.rm = TRUE)
    res[i] <- (a * yi. + b * y.j - y..) / ((a - 1) * (b - 1))
  }
  
  dados[[y]][faltantes] <- res
  
  dados[[bloco]] <- paste(bloco, dados[[bloco]])
  
  dados
}

#' Print pressupostos
#'
#' @description Printa objeto de pressupostos de anova
#'
#' @param p Objeto de pressupostos
#' 
#' @export
print.pressupostos_anova <- function(p)
{
  print(rbind(sapply(p$testes, function(x) x$p.value)))
  print(p$plot)
}
