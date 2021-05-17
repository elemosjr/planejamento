#' @title Análise de pressupostos (ANOVA)
#'
#' @import ggplot2
#' @import outliers
#'
#' @description Análises de pressupostos para o uso da ANOVA
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, linhas, colunas e, caso haja, blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#'
#' @return Lista que contém:
#' * `dados` Dados alimentados a função.
#' * `var`Nomes das variáveis utilizadas.
#' * `formula` String da formula utilizada nos modelos.
#' * `modelo` Modelo ajustado.
#' * `num` Lista com valores utilizados para os calculos.
#' * `teste` Lista com os diferentes testes utilizados.
#' * `plots` Lista com os gráficos separados.
#' * `plot` Ggplot com todos os gráficos unidos.
#'  
#' @examples 
#' 
#' bloco_casualizado(4, 4, 5)$dados %>%
#'   pressupostos_anova("Trat", "resultado")
#'  
#' @export

pressupostos_anova <- function(dados, x, y)
{
  if(!is.factor(dados[[x]])) dados[[x]] <- as.factor(dados[[x]])
  
  formula_ <- glue("`{y}` ~ `{x}`")
  
  modelo <- aov(as.formula(formula_), data = dados)
  
  N <- nrow(dados)
  
  yi. <- tapply(dados[[y]], dados[[x]], sum)
  
  qme <- last( a[[1]]$`Mean Sq` )
  
  ajuste <- modelo$fitted.values
  eij <- modelo$residuals
  dij <- eij / sqrt(qme)
  Qi <- qnorm((1:N - 0.5)/N)
  
  num_ <- list(N, qme, ajuste, eij, dij, Qi)
  
  testes <- list()
  
  testes$kstest <- ks.test(dij, "pnorm")
  
  testes$shapiro <- shapiro.test(dij)
  
  testes$cochran <- cochran.test(as.formula(formula_), dados)
  
  testes$barlett <- bartlett.test(as.formula(formula_), data = dados)
  
  tmp <- tapply(dados[[y]], dados[[x]], median)
  
  dij_levene <- c()
  
  for(i in seq_along(tmp))
  {
    dij_levene <- c(dij_levene, abs(dados[[y]][dados[[x]] == names(tmp)[i]] - tmp[i]))
  }
  
  testes$levene <- summary(aov(dij_levene ~ dados[[x]]))
  
  # Graficos
  plots <- list()
  
  plots$hist <- tibble(eij) %>% ggplot(aes(eij)) +
    geom_histogram(bins = 7, col = 1) +
    labs(x = "Resíduos", y = "Frequência", title = "Histograma dos resíduos") +
    theme_classic()
  
  plots$qqnorm <- tibble(Qi, dij) %>% ggplot(aes(x = Qi, y = sort(dij))) +
    geom_point() + labs(x = "Quantis Teóricos", y = "Resíduos ordenados",
                        title = "QQ Norm") +
    theme_classic()
  
  plots$resid_ajust <- tibble(ajuste, eij) %>% ggplot(aes(x = ajuste, y = eij)) +
    geom_point(size = 2) + labs(x = "Valores ajustados", y = "Resíduos") +
    theme_classic()
  
  plots$ordem <- tibble(x = 1:N, eij) %>% ggplot(aes(x, eij)) +
    geom_point() + labs(x = "Ordem de coleta", y = "Resíduos") +
    theme_classic()
  
  
  ret_ <- list()
  
  ret_$dados <- dados
  
  ret_$var <- list(x = x, y = y)
  
  ret_$formula <- formula_
  
  ret_$modelo <- modelo
  
  ret_$num <- num_
  
  ret_$testes <- testes
  
  ret_$plots <- plots
  
  ret_$plot <- plots$hist + plots$qqnorm + plots$resid_ajust + plots$ordem +
    plot_layout(2, 2)
  
  ret_
}
