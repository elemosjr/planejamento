#' Simula delineamento de blocos incompletos
#'
#' @description Simula um delineamento em blocos incompletos a partir de numeros de tratamentos, blocos e repetições.
#'
#' @param ntrat Numero de tratamentos a serem simulados
#' @param nbloco Numero de blocos a serem simulados
#' @param nrep Numero de repeticoes a serem simuladas, caso haja, padrao = NULL
#' 
#' @import stringr
#' @import dplyr
#' @import tidyr
#'
#' @return Objeto de tipo lista com os elementos:
#'  - **dados** Dados simulados em formato longo
#'  - **dados_matriz** Dados simulados em formato largo, como matiz
#'  - **kable** Texto em LaTeX da tabela pré-formatada
#'  - **matriz** Matriz dos dados simulados
#'
#' @examples 
#' 
#' bloco_incompleto(8, 14, 3)$dados_matriz %>% kable_()
#' 
#' bloco_incompleto(4, 6, 2)
#'
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
  
  ret_$dados <- tibble(Trat = rep(1:ntrat, nbloco),
                       resultado = as.numeric(trat),
                       bloco = paste0("B", bloco))
  
  ret_$dados_matriz <- ret_$dados %>%
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
  
  matriz <- ret_$dados %>%
    .$resultado %>% matrix(ntrat, nbloco)
  
  rownames(matriz) <- 1:ntrat
  colnames(matriz) <- 1:nbloco
  
  ret_$matriz <- matriz
  
  ret_
}