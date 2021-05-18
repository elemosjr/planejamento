suppressWarnings(suppressMessages(library(planejamento)))
suppressWarnings(suppressMessages(library(microbenchmark)))

anova_rbcd_R <- function(dados, x, y, bloco)
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
                                  "$\\hat{\\beta}$", "$\\hat{\\mu}_i$",
                                  "$\\hat{\\beta}_i$"),
                           X2 = with(ret_, c(mu, mean(tau), mean(beta),
                                             mean(yi.), mean(y.j)))) %>%
    spread(X1, X2)
  
  ret_
}

anova_bibd_R <- function(dados, x, y, bloco) # dados tem que ter NA
{
  ret_ <- list()
  
  dados <- as_tibble(dados)
  
  if(!is.factor(dados[[x]])) dados[[x]] <- as.factor(dados[[x]])
  if(!is.factor(dados[[bloco]])) dados[[bloco]] <- as.factor(dados[[bloco]])
  
  for(i in unique(dados[[x]]))
  {
    blocos <- levels(dados[[bloco]])[!levels(dados[[bloco]]) %in% dados[[bloco]][dados[x] == i]]
    bind_ <- tibble(NA, as.factor(i), as.factor(blocos))
    names(bind_) <- c(y, x, bloco)
    dados <- bind_rows(dados, bind_)
  }
  
  dados <- dados %>% arrange(x, bloco)
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
  
  ret_$estimados <- tibble(X1 = c("$\\hat{\\mu}$", "$\\hat{\\tau}$",
                                  "$\\hat{\\beta}$", "$\\hat{\\mu}_i$",
                                  "$\\hat{\\beta}_i$"),
                           X2 = with(ret_, c(mu, mean(tau), mean(beta),
                                             mean(yi.), mean(y.j)))) %>%
    spread(X1, X2)

  ret_
}

anova_lsqd_R <- function(dados, x, y, linha, coluna, replica = NULL) # dados tem que ter NA
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

dados_r <- bloco_casualizado(12, 12)$dados

dados_b <- bloco_incompleto(12, 12, 8)$dados

dados_l <- quadrados_latinos(14)$dados

x <- "Trat"
y <- "resultado"
bloco <- "bloco"
xl <- "tratamento"
linha <- "linha"
coluna <- "coluna"

microbenchmark(anova_rbcd(dados_r, x, y, bloco),
               anova_rbcd_R(dados_r, x, y, bloco),
               anova_bibd(dados_b, x, y, bloco),
               anova_bibd_R(dados_b, x, y, bloco),
               anova_lsqd(dados_l, xl, y, linha, coluna),
               anova_lsqd_R(dados_l, xl, y, linha, coluna),
               times = 1000)
