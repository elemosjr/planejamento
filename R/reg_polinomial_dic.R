#' @title Regressão polinomial (DIC)
#'
#' @import ggplot2
#'
#' @description Regressões polinomiais de delineamentos inteiramente casualizados
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, linhas, colunas e, caso haja, blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos, caso haja 
#' @param grau grau maximo a ser considerado no modelo
#' @param alpha Nivel de significancia a ser utilizado, padrao 0.05
#'
#' @return Lista que contém:
#' * `formulas` Lista com as formulas de todos os possíveis modelos de regressão até o `grau` definido.
#' * `formula_str`Lista com as strings das formulas de todos os modelos de ANOVA de todos os possíveis modelos de regressão até o `grau` definido.
#' * `modelos` Lista com  os modelos de ANOVA de todos os possíveis modelos de regressão até o `grau` definido.
#' * `falta_ajuste` Lista com  as faltas de ajuste de todos os possíveis modelos de regressão até o `grau` definido.
#' * `modelo` Modelo com o melhor ajuste.
#' * `grau` Grau do modelo com melhor ajuste.
#' * `plot` Gráfico dos dados com o modelo sobreposto.
#' * `coeficiente` Coeficientes do modelo.
#' * `fun` Função do modelo.
#' * `derivada` Derivada da função do modelo.
#' * `raiz` Valor da raiz da função do modelo.
#' * `pmet` Ponto de máxima eficiência técnica.
#'  
#' @examples 
#' 
#' bloco_casualizado(4, 1, 5)$dados %>%
#'   reg_polinom("Trat", "resultado")
#'  
#' @export

reg_polinomial_dic <- function(dados, x, y, bloco = NULL, grau = NULL, alpha = 0.05)
{
  if(!is.numeric(dados[[x]])) dados[[x]] <- as.numeric(dados[[x]])
  if(!is.numeric(dados[[y]])) dados[[y]] <- as.numeric(dados[[y]])
  if(!is.null(bloco) & !is.factor(dados[[bloco]])) dados[[bloco]] <- as.factor(dados[[bloco]])
  
  gltrat <- length(unique(dados[[x]]))-1
  
  if(is.null(grau)) grau <- gltrat
  
  formula_str <- glue("`{y}` ~")
  function_str <- glue("function(x, coef) coef[1]")
  
  formulas_str <- list()
  functions <- list()
  functions_str <- list()
  coeficientes <- list()
  modelos <- list()
  
  for(i in 1:grau)
  {
    formula_str <- glue("{formula_str} + I(`{x}`^{i})")
    function_str <- glue("{function_str} + (x^{i} * coef[{i+1}])")
    functions_str[[i]] <- function_str
    formulas_str[[i]] <- formula_str
    functions[[i]] <- eval(parse(text = function_str))
    coeficientes[[i]] <- summary(lm(as.formula(formulas_str[[i]]), data = dados))$coef[1:(i+1)]
    formula_aov <- ifelse(is.null(bloco), formula_str, glue("{formula_str} + {bloco}"))
    modelos[[i]] <- aov(as.formula(formula_aov), data = dados)
  }
  
  formulas_fator <- lapply(formulas_str,
                           function(x) str_replace_all(x, "I\\(",
                                                       "as.factor\\("))
  
  falta_ajuste <- list()
  for(i in 1:gltrat)
  {
    falta_ajuste[[i]] <- anova(lm(as.formula(formulas_str[[i]]), data = dados),
                               lm(as.formula(formulas_fator[[i]]), data = dados))
  }
  
  pvalores <- sapply(1:length(modelos),
                     function(i) summary(modelos[[i]])[[1]]$`Pr(>F)`[i])
  
  psign <- pvalores < alpha
  
  idmodelo <- ifelse(any(psign), max(which(psign)), NA)
  
#  fun <- function(x) functions[[idmodelo]](x, coeficientes[[idmodelo]])
  
  if(is.na(idmodelo))
  {  
    warning(glue("Não foi encontrado nenhum modelo significativo com alpha = {alpha}"))
    fun <- NULL
  } else
  {
    fun_str <- str_replace(functions_str[[idmodelo]], "x, coef\\)", "x\\)")
    coef <- as.character(coeficientes[[idmodelo]])
    for(i in seq_along(coef)) fun_str <- str_replace(fun_str, glue("coef\\[{i}\\]"), coef[i])
    fun <- eval(parse(text = fun_str))
    derivada <- Deriv::Deriv(fun)
    raiz <- ifelse(idmodelo > 1, uniroot(derivada, c(-(2^1000), 2^1000))$root, NA)
  }
  
  plot <- dados %>% ggplot(aes(x = .data[[x]], y = .data[[y]],
                               col = factor(.data[[x]]))) +
    geom_point() + stat_function(fun = fun, col = 1) +
    labs(col = x) + theme(legend.position = "top")
  
  if(idmodelo > 1)
  {
    plot <- plot + 
      geom_hline(yintercept = fun(raiz), linetype = 3) +
      geom_vline(xintercept = raiz, linetype = 3)
  }
  
  list(
    formula_str = formulas_str[[idmodelo]],
    modelos = modelos,
    falta_ajuste = falta_ajuste,
    modelo = modelos[[idmodelo]],
    grau = idmodelo,
    plot = plot,
    coeficientes = coeficientes[[idmodelo]],
    fun = fun,
    derivada = derivada,
    raiz = raiz,
    pmet = fun(raiz)
  )
}
