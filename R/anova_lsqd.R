#' Gera valores de anova para Quadrados Latinos
#'
#' @description Calcula valores utilizados para uma tabela de ANOVA de um delineamento em quadrados latinos
#'
#' @param dados Data frame com colunas separadas para os tratamentos, os blocos e os resultados a serem analisados
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param linha String com o nome da coluna do data frame que representa a linha do quadrado latino
#' @param coluna String com o nome da coluna do data frame que representa a coluna do quadrado latino
#'
#' @import glue
#' @import tidyr
#'
#' @return Objeto de tipo lista contendo todos os valores utilizados para o calculo da tabela da ANOVA, os valores da tabela da ANOVA e os parâmetros estimados.
#' 
#' @examples
#'
#' quadrados_latinos(4)$dados %>%
#'  anova_lsqd("tratamento", "resultado", "linha", "coluna")
#'
#' @export

anova_lsqd <- function(dados, x, y, linha, coluna, replica = NULL) # dados tem que ter NA
{
  ret_ <- list()

  ret_$N <- nrow(dados)
  ret_$n <- ifelse(is.null(replica), 1, length(unique(dados[[replica]])))
  ret_$p <- sqrt(ret_$N/ret_$n)
  ret_$y... <- sum(dados[[y]], na.rm = TRUE)
  ret_$y.j. <- tapply(dados[[y]], dados[[x]], sum, na.rm = TRUE)
  ret_$yi.. <- tapply(dados[[y]], dados[[linha]], sum, na.rm = TRUE)
  ret_$y..k <- tapply(dados[[y]], dados[[coluna]], sum, na.rm = TRUE)
  ret_$mu <- mean(dados[[y]], na.rm = TRUE)
  ret_$tau <- ret_$y.j. - ret_$mu  #
  ret_$alpha <- ret_$yi.. - ret_$mu #
  ret_$beta <- ret_$y..k - ret_$mu #
  ret_$sqt <- sum(dados[[y]]^2, na.rm = TRUE) - ret_$y...^2/ret_$N
  ret_$sqtrat <- sum(ret_$y.j.^2, na.rm = TRUE) / (ret_$p*ret_$n) - ret_$y...^2/ret_$N
  ret_$sqlinha <- sum(ret_$yi..^2, na.rm = TRUE) / (ret_$p*ret_$n) - ret_$y...^2/ret_$N
  ret_$sqcol <- sum(ret_$y..k^2, na.rm = TRUE) / (ret_$p*ret_$n) - ret_$y...^2/ret_$N
  ret_$sqe <- ret_$sqt - ret_$sqtrat - ret_$sqlinha - ret_$sqcol
  ret_$qmtrat <- ret_$sqtrat / (ret_$p - 1)
  ret_$qmlinha <- ret_$sqlinh / (ret_$p - 1)
  ret_$qmcol <- ret_$sqcol / (ret_$p - 1)
  ret_$qme <- ret_$sqe / ((ret_$p - 2) * (ret_$p - 1))
  ret_$f0 <- ret_$qmtrat / ret_$qme
  ret_$pvalor <- pf(ret_$f0, ret_$p - 1, (ret_$p - 2) * (ret_$p - 1), lower.tail = FALSE)

  ret_$estimados <- tibble(X1 = c("$\\hat{\\mu}$", "$\\hat{\\alpha}$",
                                  "$\\hat{\\tau}$", "$\\hat{\\beta}$",
                                  "$\\hat{\\alpha}_i$", "$\\hat{\\tau}_i$",
                                  "$\\hat{\\beta}_i$"),
                           X2 = with(ret_, c(mu, mean(alpha), mean(tau),
                                             mean(beta), mean(yi..),
                                             mean(y.j.), mean(y..k)))) %>%
    spread(X1, X2)
  
  ret_$modelo <- with(ret_,
                      glue("$$\\color{red}{y_{ij} = .{mu}. +
                           \\alpha_i +\\tau_k + \\beta_k + e_{ij}}$$",
                           .open = ".{", .close = "}."))
  
  if(!is.null(replica))
  {
    ret_$n <- length(unique(dados[[replica]]))
    ret_$y...l <- tapply(dados[[y]], dados[[replica]], sum, na.rm = TRUE)
    ret_$lambda <- ret_$y...l - ret_$mu
    ret_$sqrep <- (1/ret_$p^2) * sum(ret_$y...l^2, na.rm = TRUE) - ret_$y...^2/ret_$N
    ret_$qmrep <- ret_$sqrep / (ret_$n - 1)
    ret_$sqe <- ret_$sqe - ret_$sqrep
    ret_$qme <- ret_$sqe / ((ret_$p - 1) * (ret_$n * (ret_$p + 1) - 3))
    ret_$f0 <- ret_$qmtrat / ret_$qme
    ret_$pvalor <- pf(ret_$f0, ret_$p - 1,
                      (ret_$p-1)*(ret_$n * (ret_$p + 1) - 3),
                      lower.tail = FALSE)
    
    ret_$estimados <- ret_$estimados %>%
      mutate(`$\\hat{\\lambda}$` = mean(ret_$lambda),
             `$\\hat{\\lambda}_i$` = mean(ret_$y...l))
  }
  
  ret_
}