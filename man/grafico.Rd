% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grafico.R
\name{grafico}
\alias{grafico}
\title{Base para gerar graficos de comparação}
\usage{
grafico(dados, x, y, bloco = NULL, legenda = NULL)
}
\arguments{
\item{dados}{Data frame com colunas separadas para os tratamentos, resultados, e caso haja, blocos}

\item{x}{String com o nome da coluna dos tratamentos}

\item{y}{String com o nome da coluna dos resultados}

\item{bloco}{String com o nome da coluna dos blocos, manter NULL caso não hajam blocos}

\item{legenda}{String com a posição da legenda ("top", "bottom", "left", "right"), padrao = NULL}
}
\value{
Ggplot com geometria de pontos e facetes da variavel x
}
\description{
Funcao base para graficos de comparacoes multiplas.
}
\examples{

grafico(mtcars, "cyl", "hp", "gear")

}
