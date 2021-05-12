#' Comparacoes multiplas para DIC e RBCD
#'
#' @import agricolae
#' @import dplyr
#' @import tibble
#'
#' @description Análises múltiplas para ANOVA de delineamentos inteiramente casualizados (DIC) e para delineamentos em blocos completos casualizados
#'
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos, manter NULL caso não hajam blocos
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, e caso haja, blocos
#' @param alpha Nivel de significancia a ser utilizado, padrao 0.05
#' @param group Valor lógico, onde quando verdadeiro, fará a análise de comparacoes multiplas por grupos
#'
#' @return Objeto do tipo `comparacao`, que contém:
#' * `dados` Dados alimentados a função
#' * `variaveis` Lista com os nomes das variaveis
#' * `formula` Objeto `formula` usado para criar o modelo
#' * `modelos` Lista com todos os modelos criados
#' * `fisher` Dados das comparações de fisher
#' * `bonferroni` Dados das comparações de bonferroni
#' * `tukey` Dados das comparações de tukey
#' * `duncan` Dados das comparações de duncan
#'  
#' @examples
#' 
#' comparacoes("gear", "hp", "carb", mtcars)
#' 
#' @export

comparacoes <- function(x, y, bloco = NULL, dados,
                        alpha = 0.05, group = FALSE)
{
  if(!is.factor(dados[[x]])) dados[[x]] <- as.factor(dados[[x]])
  
  formula_ <- paste(paste0("`", y, "`"), "~", paste0("`", x, "`"))
  
  if(!is.null(bloco))
  {
    if(!is.factor(dados[[bloco]])) dados[[bloco]] <- as.factor(dados[[bloco]])
    formula_ <- paste(as.character(formula_), "+", paste(paste0("`", bloco, "`"), collapse = " + "))
  }
  
  formula_ <- as.formula(formula_)
  
  modelo_anova <- aov(formula_, data = dados)
  
  ret_ <- list()
  
  ret_$dados <- dados
  
  ret_$variaveis <- list(tratamento = x,
                         bloco = bloco,
                         resultado = y)
  
  ret_$formula <- formula_
  
  ret_$modelos <- list(Modelo = modelo_anova)
  
  if(group)
  {
    ret_$modelos$fisher <- modelo_anova %>%
      LSD.test(x, group = group, alpha = alpha)
    
    ret_$fisher <- ret_$modelos$fisher %>%
      .$groups %>%
      as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$bonferroni <- modelo_anova %>%
      LSD.test(x, group = group, alpha = alpha, p.adj = "bonferroni")
    
    ret_$bonferroni <- ret_$modelos$bonferroni %>%
      .$groups %>% as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$tukey <- modelo_anova %>%
      HSD.test(x, group = group, alpha = alpha)
    
    ret_$tukey <- ret_$modelos$tukey %>%
      .$groups %>% as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$duncan <- modelo_anova %>%
      duncan.test(x, group = group, alpha = alpha)
    
    ret_$duncan <- ret_$modelos$duncan %>%
      .$groups %>% as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
  } else
  {
    
    ret_$modelos$fisher <- modelo_anova %>%
      LSD.test(x, group = group, alpha = alpha) 
    
    ret_$fisher <- ret_$modelos$fisher %>%
      .$comparison %>%
      as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = difference,
             `Limite Inferior` = LCL, `Limite Superior` = UCL,
             `P-valor` = pvalue)
    
    ret_$modelos$bonferroni <- modelo_anova %>%
      LSD.test(x, group = group, alpha = alpha, p.adj = "bonferroni")
    
    ret_$bonferroni <- ret_$modelos$bonferroni %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = difference,
             `Limite Inferior` = LCL, `Limite Superior` = UCL,
             `P-valor` = pvalue)
    
    ret_$modelos$tukey <- modelo_anova %>%
      HSD.test(x, group = group, alpha = alpha)
    
    ret_$tukey <- ret_$modelos$tukey %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = difference,
             `Limite Inferior` = LCL, `Limite Superior` = UCL,
             `P-valor` = pvalue)
    
    ret_$modelos$duncan <- modelo_anova %>%
      duncan.test(x, group = group, alpha = alpha)
    
    ret_$duncan <- ret_$modelos$duncan %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = difference,
             `Limite Inferior` = LCL, `Limite Superior` = UCL,
             `P-valor` = pvalue)
  }
  
  class(ret_) <- "comparacao"
  
  ret_
}
