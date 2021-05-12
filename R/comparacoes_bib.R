#' Comparacoes multiplas para BIBD
#'
#' @import agricolae
#' @import dplyr
#' @import tibble
#'
#' @description Análises múltiplas para ANOVA de delineamentos em blocos incompletos balanceados
#'
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos, manter NULL caso não hajam blocos
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, e caso haja, blocos
#' @param alpha Nivel de significancia a ser utilizado, padrao 0.05
#' @param group Valor lógico, onde quando verdadeiro, fará a análise de comparacoes multiplas por grupos
#'
#' @return Objeto do tipo `comparacao_bib`, que contém:
#' * `dados` Dados alimentados a função
#' * `variaveis` Lista com os nomes das variaveis
#' * `formula` Objeto `formula` usado para criar o modelo
#' * `modelos` Lista com todos os modelos criados
#' * `fisher` Dados das comparações de fisher
#' * `bonferroni` Dados das comparações de bonferroni
#' * `tukey` Dados das comparações de tukey
#' * `duncan` Dados das comparações de duncan
#' * `waller` Dados das comparações de waller
#' * `snk` Dados das comparações de snk
#'  
#' @examples 
#' 
#' bloco_incompleto(4, 6, 2)$dados ->.;
#'  comparacoes_bib("Trat", "resultado", "bloco", .)
#'  
#' @export

comparacoes_bib <- function(x, y, bloco = NULL, dados, alpha = 0.05, group = FALSE)
{
  dados <- na.omit(dados)
  
  if(!is.factor(dados[[x]])) dados[[x]] <- as.factor(dados[[x]])
  if(!is.factor(dados[[bloco]])) dados[[bloco]] <- as.factor(dados[[bloco]])
  
  formula_ <- paste(paste0("`", y, "`"), "~", paste0("`", x, "`"))
  
  if(!is.null(bloco))
  {
    formula_ <- paste(as.character(formula_), "+", paste(paste0("`", bloco, "`"), collapse = " + "))
  }
  
  formula_ <- as.formula(formula_)
  
  modelo_anova <- aov(formula_, data = dados)
  
  ret_ <- list()
  
  ret_$variaveis <- list(tratamento = x,
                         bloco = bloco,
                         resultado = y)
  
  x <- dados[[x]]
  y <- dados[[y]]
  bloco <- dados[[bloco]]
  
  ret_$formula <- formula_
  
  ret_$modelos$Modelo <- modelo_anova
  
  ret_$modelos$Modelo_aj <- drop1(modelo_anova, test = "F")
  
  ret_$dados <- dados

  
  if(group)
  {
    ret_$modelos$lsd <- BIB.test(bloco, x, y, "lsd",alpha = alpha, group = group)
    
    ret_$lsd <- ret_$modelos$lsd %>%
      .$groups %>%
      as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$tukey <- BIB.test(bloco, x, y, "tukey",alpha = alpha, group = group)
    
    ret_$tukey <- ret_$modelos$tukey %>%
      .$groups %>%
      as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$duncan <- BIB.test(bloco, x, y, "duncan",alpha = alpha, group = group)
    
    ret_$duncan <- ret_$modelos$duncan %>%
      .$groups %>%
      as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$waller <- BIB.test(bloco, x, y, "waller",alpha = alpha, group = group)
    
    ret_$waller <- ret_$modelos$waller %>%
      .$groups %>%
      as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
    
    ret_$modelos$snk <- BIB.test(bloco, x, y, "snk",alpha = alpha, group = group)
    
    ret_$snk <- ret_$modelos$snk %>%
      .$groups %>%
      as.data.frame() %>%
      rownames_to_column("Valores") %>%
      select(`Valores`, y, Grupos = groups)
  }
  else
  {
    ret_$modelos$lsd <- BIB.test(bloco, x, y, "lsd", alpha = alpha, group = group)
    
    ret_$lsd <- ret_$modelos$lsd %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = Difference, `P-valor` = pvalue)
    
    ret_$modelos$tukey <- BIB.test(bloco, x, y, "tukey",alpha = alpha, group = group)
    
    ret_$tukey <- ret_$modelos$tukey %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = Difference, `P-valor` = pvalue)
    
    ret_$modelos$duncan <- BIB.test(bloco, x, y, "duncan",alpha = alpha, group = group)
    
    ret_$duncan <- ret_$modelos$duncan %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = Difference, `P-valor` = pvalue)
    
    ret_$modelos$waller <- BIB.test(bloco, x, y, "waller",alpha = alpha, group = group)
    
    ret_$waller <- ret_$modelos$waller %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = Difference, `Significante` = significant)
    
    ret_$modelos$snk <- BIB.test(bloco, x, y, "snk",alpha = alpha, group = group)
    
    ret_$snk <- ret_$modelos$snk %>%
      .$comparison %>% as.data.frame() %>%
      rownames_to_column("Comparação") %>%
      select(`Comparação`, `Diferença de Médias` = Difference, `P-valor` = pvalue)
  }
  
  class(ret_) <- "comparacao_bib"
  
  ret_
}
