#' @title Comparacoes (Quadrados Latinos)
#'
#' @import agricolae
#' @import dplyr
#' @import tibble
#'
#' @description Análises múltiplas para ANOVA de delineamentos em quadrados latinos
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, linhas, colunas e, caso haja, blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param linha String com o nome da coluna dos linha
#' @param coluna String com o nome da coluna dos coluna
#' @param bloco String com o nome da coluna dos blocos, podendo ser as replicas, ou outro tratamento no caso de quadrados greco-latinos
#' @param alpha Nivel de significancia a ser utilizado, padrao 0.05
#' @param group Valor lógico, onde quando verdadeiro, fará a análise de comparacoes multiplas por grupos
#'
#' @return Objeto do tipo `comparacao_lsqd`, que contém:
#' * `variaveis` Lista com os nomes das variaveis
#' * `formula` Objeto `formula` usado para criar o modelo
#' * `modelos` Lista com todos os modelos criados
#' * `dados` Dados alimentados a função
#' * `fisher` Dados das comparações de fisher
#' * `bonferroni` Dados das comparações de bonferroni
#' * `tukey` Dados das comparações de tukey
#' * `duncan` Dados das comparações de duncan
#'  
#' @examples 
#' 
#' quadrados_latinos(4)$dados %>%
#'   comparacoes_lsqd("tratamento", "resultado", "linha", "coluna")
#'  
#' @export

comparacoes_lsqd <- function(dados, x, y, linha, coluna, bloco = NULL,
                             alpha = 0.05, group = FALSE)
{
  if(!is.factor(dados[[x]])) dados[[x]] <- as.factor(dados[[x]])
  if(!is.factor(dados[[linha]])) dados[[linha]] <- as.factor(dados[[linha]])
  if(!is.factor(dados[[coluna]])) dados[[coluna]] <- as.factor(dados[[coluna]])

  ret_ <- list()
  
  if(!is.null(bloco))
  {
    if(!is.factor(dados[[bloco]])) dados[[bloco]] <- as.factor(dados[[bloco]])
    
    formula_ <- as.formula(glue("`{y}` ~ `{x}` + `{linha}` + `{coluna}` + `{bloco}`"))
    
    ret_$variaveis <- list(tratamento = x,
                           linha = linha,
                           coluna = coluna,
                           resultado = y,
                           bloco = bloco)
  } else
  {
    formula_ <- as.formula(glue("`{y}` ~ `{x}` + `{linha}` + `{coluna}`"))
    
    ret_$variaveis <- list(tratamento = x,
                           linha = linha,
                           coluna = coluna,
                           resultado = y)
  }
  
  modelo_anova <- aov(formula_, data = dados)

  ret_$formula <- formula_
  
  ret_$modelos$Modelo <- modelo_anova
  
  ret_$dados <- dados
  
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
  
  class(ret_) <- "comparacao_lsqd"
  
  ret_
}
