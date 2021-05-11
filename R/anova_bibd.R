#' @title anova_bibd
#'
#' @description Calcula valores utilizados para uma tabela de ANOVA de um delineamento em blocos incompletos balanceados
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

anova_bibd <- function(dados, x, y, bloco) # dados tem que ter NA
{
  ret_ <- list()
  
  for(i in seq_along(unique(dados[[x]])))
  {
    blocos <- levels(dados[[bloco]])[!levels(dados[[bloco]]) %in% dados[[bloco]][dados[x] == i]]
    bind_ <- tibble(NA, as.factor(i), as.factor(blocos))
    names(bind_) <- c(y, x, bloco)
    dados <- bind_rows(dados, bind_)
  }
  
  dados <- dados %>% arrange(dados[[x]], dados[[bloco]])
  coord_na <- as.numeric(!is.na(dados[[y]]))
  
  dados <- na.omit(dados)
  
  ret_$a <- length(unique(dados[[x]]))
  ret_$b <- length(unique(dados[[bloco]]))
  ret_$N <- nrow(dados)
  ret_$r <- ret_$N/ret_$a
  ret_$k <- ret_$N/ret_$b
  ret_$lambda <- (ret_$r * (ret_$k - 1)) / (ret_$a - 1)
  ret_$y.. <- sum(dados[[y]], na.rm = TRUE)
  ret_$yi. <- tapply(dados[[y]], dados[[x]], sum, na.rm = TRUE)[1:ret_$a]
  ret_$y.j <- tapply(dados[[y]], dados[[bloco]], sum, na.rm = TRUE)[1:ret_$b]
  ret_$mu <- mean(dados[[y]], na.rm = TRUE)
  ret_$tau <- ret_$yi. - ret_$mu  #
  ret_$beta <- ret_$y.j - ret_$mu #
  ret_$sqt <- sum(dados[[y]]^2, na.rm = TRUE) - ret_$y..^2/ret_$N
  ret_$sqtrat <- (1/ret_$r) * sum(ret_$yi.^2, na.rm = TRUE) - ret_$y..^2/ret_$N
  ret_$sqbloco <- (1/ret_$k) * sum(ret_$y.j^2, na.rm = TRUE) - ret_$y..^2/ret_$N
  ret_$sqe <- ret_$sqt - ret_$sqtrat - ret_$sqbloco
  ret_$nij <- matrix(coord_na, ret_$a, ret_$b)
  
  ret_$Qi <- as.matrix(ret_$yi.) - (1 / ret_$k) * ret_$nij %*% as.matrix(ret_$y.j)
  
  ret_$sqtrat_aj <- (ret_$k * sum(ret_$Qi^2) / ret_$lambda * ret_$a)
  ret_$qmtrat_aj <- ret_$sqtrat_aj / (ret_$a - 1)
  ret_$qmbloco <- ret_$sqbloco / (ret_$b - 1)
  ret_$qme <- ret_$sqe / (ret_$N - ret_$a - ret_$b + 1)
  ret_$f0 <- ret_$qmtrat_aj / ret_$qme
  ret_$pvalor <- pf(ret_$f0, ret_$a - 1, (ret_$N - ret_$a - ret_$b + 1), lower.tail = FALSE)
  
  ret_$estimados <- list()
  
  ret_$estimados[[1]] <- tibble(X1 = c("$\\hat{\\mu}$", "$\\hat{\\mu}_{i.}$",
                                       paste0("$\\tau_{", 1:length(ret_$tau),
                                              "j}$")),
                                X2 = c(ret_$mu, sum(ret_$yi.)/ret_$b, ret_$tau)) %>%
    spread(X1, X2)
  ret_$estimados[[2]] <- tibble(X1 = c(paste0("$\\beta_{",
                                              1:length(ret_$beta), "j}$"),
                                       "$\\hat{\\beta}_{.j}$"),
         X2 = c(ret_$beta, sum(ret_$y.j)/ret_$a)) %>%
    spread(X1, X2)
  
  ret_$modelo <- with(ret_,
                      glue("$$\\color{red}{y_{ij} = .{mu}. +
                           \\tau_i + \\beta_j + e_{ij}}$$",
                           .open = ".{", .close = "}."))
  
  ret_
}
