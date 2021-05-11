#' @title Kable_
#'
#' @import knitr
#'
#' @description Kable modificada para centralizar todas as colunas
#'
#' @param x Data frame a ser printado
#'
#' @return character
#' @export

kable_ <- function(x) kable(x, align = rep("c", ncol(x)))

#' @title Print Anova
#'
#' @import broom
#' @import dplyr
#'
#' @description Organiza e printa uma tabela de anova em formato de LaTeX
#'
#' @param model Modelo de anova
#'
#' @return character
#' @export

print_anova <- function(model)
{
  tidy(model) %>%
    mutate(term = str_replace_all(term, "`", "")) %>%
    select(`\\textbf{Fonte de Variação}` = term,
           `\\textbf{Graus de Liberdade}` = df,
           `\\textbf{Soma de Quadrados}` = sumsq,
           `\\textbf{Quadrado Médio}` = meansq,
           `\\textbf{$F_0$}` = statistic,
           `\\textbf{P-valor}` = p.value) %>%
    kable_()
}

#' @title Quadrados latinos
#'
#' @import dplyr
#'
#' @description Simula um delineamento em quadrados latinos a partir de um banco de dados
#'
#' @param dados Data frame com os resultados
#' @param tratamentos Vetor com fatores dos tratamentos
#'
#' @return data.frame
#' @export

quadrados_latinos <- function(dados, tratamentos)
{
  n <- length(tratamentos)
  ordem <- matrix(NA, n, n)
  
  for (i in 1:n) {
    for (j in 1:n) {
      k <- i + j - 1
      if (k > n) k <- i + j - n - 1
      ordem[i, j] <- k
    }
  }
  
  dados %>%
    bind_cols(tibble(tratamento = tratamentos[ordem[sample(1:n, n),]])) %>%
    mutate(coluna = sort(rep(1:n, n)),
           linha = rep(1:n, n))
}


#' @title Blocos incompletos
#'
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#' @description Simula um delineamento em blocos incompletos
#'
#' @param ntrat Numero de tratamentos a serem simulados
#' @param nbloco Numero de blocos a serem simulados
#' @param nrep Numero de repeticoes a serem simuladas, caso haja, padrao = NULL
#'
#' @return list
#' @export

bloco_incompleto <- function(ntrat, nbloco, nrep = NULL)
{
  if(is.null(nrep)) nrep <- ntrat
  
  n_na <- ntrat - nrep
  
  combinacoes <- combn(1:ntrat, n_na)
  
  rand <- combinacoes[,sample(1:ncol(combinacoes), nbloco)]
  
  trat <- as.factor(rep(1:ntrat, nbloco))
  
  for(i in 1:nbloco) rand[,i] <- rand[,i] + ntrat * (i - 1)
  
  trat[rand] <- NA
  
  ret_ <- list()
  
  bloco <- sort(rep(1:nbloco, ntrat))
  sapply(sort(rep(1:nbloco, ntrat)),
         function(x) nchar(x) != nchar(max(bloco))) ->.;

  bloco[.] <- bloco[.] %>%
    sapply(function(x) paste0(paste0(rep("0", nchar(max(bloco)) - 1),
                                     collapse = ""), x))
  
  ret_$dados_originais <- tibble(Trat = rep(1:ntrat, nbloco),
                       resultado = trat,
                       bloco = paste0("B", bloco))
  
  ret_$dados <- ret_$dados_originais %>%
    mutate(resultado = ifelse(is.na(resultado), "-", as.character(resultado))) %>%
    spread(bloco, resultado) 

  class(ret_$dados) <- c(class(ret_$dados), "bloco_incompleto")
  
  alinhamento <- rep("c", ncol(ret_$dados))
  
  alinhamento_novo <- paste0("{c|", paste0(alinhamento[-1],
                                           collapse = ""), "}\\n\\\\hline")
  
  ret_$kable <- kable(ret_$dados, "latex", align = alinhamento) %>%
    as.character() %>%
    str_replace_all("\\n\\\\hline", "") %>%
    str_replace(paste0("\\{", paste0(alinhamento, collapse = "\\|"), "\\}"),
                paste0("{c|", paste0(alinhamento[-1], collapse = ""), "}\\\n\\\\hline"))
  
  matriz <- ret_$dados_originais %>%
    .$resultado %>% matrix(ntrat, nbloco)
  
  rownames(matriz) <- 1:ntrat
  colnames(matriz) <- 1:nbloco
  
  ret_$matriz <- matriz
  
  ret_
}


#' @title Estimando ausentes bibd
#'
#' @description Estima os valores ausentes de um delineamento incompleto balanceado
#'
#' @param dados Data frame com colunas separadas para os tratamentos, resultados, com valores faltantes, e blocos
#' @param x String com o nome da coluna dos tratamentos
#' @param y String com o nome da coluna dos resultados
#' @param bloco String com o nome da coluna dos blocos
#'
#' @return data.frame
#' @export

estimando_ausentes <- function(dados, x, y, bloco)
{
  faltantes <- which(is.na(dados[[y]]))
  faltantes_x <- dados[[x]][faltantes]
  faltantes_bloco <- dados[[bloco]][faltantes]
  
  res <- numeric(length(faltantes))
  
  a <- length(unique(dados[[x]]))
  b <- length(unique(dados[[bloco]]))
  y.. <- sum(dados[[y]], na.rm = TRUE)
  
  for(i in 1:length(faltantes))
  {
    yi. <- sum(dados[[y]][dados[[x]] == faltantes_x[i]], na.rm = TRUE)
    y.j <- sum(dados[[y]][dados[[bloco]] == faltantes_bloco[i]], na.rm = TRUE)
    res[i] <- (a * yi. + b * y.j - y..) / ((a - 1) * (b - 1))
  }
  
  dados[[y]][faltantes] <- res
  
  dados[[bloco]] <- paste(bloco, dados[[bloco]])
  
  dados
}
