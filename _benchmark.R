library(planejamento)
library(microbenchmark)

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

dados_r <- bloco_casualizado(12, 18)$dados

dados_b <- bloco_incompleto(12, 18, 8)$dados

x <- "Trat"
y <- "resultado"
bloco <- "bloco"

microbenchmark(anova_rbcd(dados_r, x, y, bloco),
               anova_rbcd_R(dados_r, x, y, bloco),
               anova_bibd(dados_b, x, y, bloco),
               anova_bibd_R(dados_b, x, y, bloco),
               times = 1000)