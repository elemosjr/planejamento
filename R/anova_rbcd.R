#' @title anova_rbcd
#'
#' @description Calcula valores utilizados para uma tabela de ANOVA de um delineamento em blocos completamente casualizados
#'
#' @param dados Data frame com colunas separadas para os tratamentos, os blocos e os resultados a serem analisados
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos
#'
#' @import glue
#' @import tidyr
#'
#' @return list
#' @export

anova_rbcd <- function(dados, x, y, bloco)
{
  ret_ <- list()
  
  ret_$a <- length(unique(dados[[x]]))
  ret_$b <- length(unique(dados[[bloco]]))
  ret_$N <- nrow(dados)
  ret_$y.. <- sum(dados[[y]], na.rm = TRUE)
  ret_$yi. <- tapply(dados[[y]], dados[[x]], sum, na.rm = TRUE)
  ret_$y.j <- tapply(dados[[y]], dados[[bloco]], sum, na.rm = TRUE)
  ret_$mu <- mean(dados[[y]], na.rm = TRUE)
  ret_$tau <- ret_$yi. - ret_$mu
  ret_$beta <- ret_$y.j - ret_$mu
  ret_$sqt <- sum(dados[[y]]^2, na.rm = TRUE) - ret_$y..^2/ret_$N
  ret_$sqtrat <- (1/ret_$b) * sum(ret_$yi.^2) - ret_$y..^2/ret_$N
  ret_$sqbloco <- (1/ret_$a) * sum(ret_$y.j^2) - ret_$y..^2/ret_$N
  ret_$sqe <- ret_$sqt - ret_$sqtrat - ret_$sqbloco
  ret_$qmtrat <- ret_$sqtrat / (ret_$a - 1)
  ret_$qmbloco <- ret_$sqbloco / (ret_$b - 1)
  ret_$qme <- ret_$sqe / ((ret_$a - 1) * (ret_$b - 1))
  ret_$f0 <- ret_$qmtrat / ret_$qme
  ret_$pvalor <- pf(ret_$f0, ret_$a - 1, (ret_$a - 1) * (ret_$b - 1), lower.tail = FALSE)

  ret_$estimados <- tibble(X1 = c("$\\hat{\\mu}$", "$\\hat{\\tau}$",
                                  "$\\hat{\\beta}$", "$\\hat{\\mu_i}$",
                                  "$\\hat{\\beta_i}$"),
                           X2 = with(ret_, c(mu, mean(tau), mean(beta),
                                             mean(yi.), mean(y.j)))) %>%
    spread(X1, X2)
  
  ret_$modelo <- with(ret_,
                      glue("$$\\color{red}{y_{ij} = .{mu}. +
                           \\tau_i + \\beta_j + e_{ij}}$$",
                           .open = ".{", .close = "}."))
  
  ret_
}
