#' @title anova_lsqd
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
#' @return list
#' @export

anova_lsqd <- function(dados, x, y, linha, coluna) # dados tem que ter NA
{
  ret_ <- list()

  ret_$N <- nrow(dados)
  ret_$p <- sqrt(ret_$N)
  ret_$y... <- sum(dados[[y]], na.rm = TRUE)
  ret_$y.j. <- tapply(dados[[y]], dados[[x]], sum, na.rm = TRUE)
  ret_$yi.. <- tapply(dados[[y]], dados[[linha]], sum, na.rm = TRUE)
  ret_$y..k <- tapply(dados[[y]], dados[[coluna]], sum, na.rm = TRUE)
  ret_$mu <- mean(dados[[y]], na.rm = TRUE)
  ret_$tau <- ret_$y.j. - ret_$mu  #
  ret_$alpha <- ret_$yi.. - ret_$mu #
  ret_$beta <- ret_$y..k - ret_$mu #
  ret_$sqt <- sum(dados[[y]]^2, na.rm = TRUE) - ret_$y...^2/ret_$N
  ret_$sqtrat <- (1/ret_$p) * sum(ret_$y.j.^2, na.rm = TRUE) - ret_$y...^2/ret_$N
  ret_$sqlinha <- (1/ret_$p) * sum(ret_$yi..^2, na.rm = TRUE) - ret_$y...^2/ret_$N
  ret_$sqcol <- (1/ret_$p) * sum(ret_$y..k^2) - ret_$y...^2/ret_$N
  ret_$sqe <- ret_$sqt - ret_$sqtrat - ret_$sqlinha - ret_$sqcol
  ret_$qmtrat <- ret_$sqtrat / (ret_$p - 1)
  ret_$qmlinha <- ret_$sqlinh / (ret_$p - 1)
  ret_$qmcol <- ret_$sqcol / (ret_$p - 1)
  ret_$qme <- ret_$sqe / ((ret_$p - 2) * (ret_$p - 1))
  ret_$f0 <- ret_$qmtrat / ret_$qme
  ret_$pvalor <- pf(ret_$f0, ret_$p - 1, (ret_$p - 2) * (ret_$p - 1), lower.tail = FALSE)
  
  ret_$estimados <- list()
  
  ret_$estimados[[1]] <- tibble(X1 = c("$\\hat{\\mu}$", "$\\hat{\\mu}_{i}$",
                                       paste0("$\\tau_{", 1:length(ret_$tau),
                                              "}$")),
                                X2 = c(ret_$mu, sum(ret_$y.j.)/ret_$p, ret_$tau)) %>%
    spread(X1, X2)
  ret_$estimados[[2]] <- tibble(X1 = c(paste0("$\\beta_{",
                                              1:length(ret_$alpha), "}$"),
                                       "$\\hat{\\alpha}_{j}$"),
         X2 = c(ret_$alpha, sum(ret_$yi..)/ret_$p)) %>%
    spread(X1, X2)
  ret_$estimados[[3]] <- tibble(X1 = c(paste0("$\\beta_{",
                                              1:length(ret_$beta), "}$"),
                                       "$\\hat{\\beta}_{k}$"),
         X2 = c(ret_$beta, sum(ret_$y..j)/ret_$p)) %>%
    spread(X1, X2)
  
  ret_$modelo <- with(ret_,
                      glue("$$\\color{red}{y_{ij} = .{mu}. +
                           \\alpha_i +\\tau_k + \\beta_k + e_{ij}}$$",
                           .open = ".{", .close = "}."))
  
  ret_
}
