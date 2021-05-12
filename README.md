
# Planejamento e Análise de Experimentos

Pacote de funções utilizadas na matéria de Planejamento e Análise de
Experimentos I do curso de Estatística da Universidade Estadual de
Maringá.

# Instalação

``` r
install.packages("devtools")
devtools::install_github("elemosjr/planejamento")
```

# Exemplos

``` r
## Simulações de delineamentos
# Delineamento em blocos incompletos
bloco_incompleto(4, 6, 2)$dados %>% kable_()
```

| Trat | B1  | B2  | B3  | B4  | B5  | B6  |
|:----:|:---:|:---:|:---:|:---:|:---:|:---:|
|  1   |  1  | \-  | \-  |  1  |  1  | \-  |
|  2   |  2  |  2  | \-  | \-  | \-  |  2  |
|  3   | \-  |  3  |  3  | \-  |  3  | \-  |
|  4   | \-  | \-  |  4  |  4  | \-  |  4  |

``` r
# Quadrados Latinos
quadrado <- quadrados_latinos(dados = tibble(x = 1:16),
                              tratamentos = letters[1:4])
head(quadrado) %>% kable_()
```

|  x  | tratamento | coluna | linha |
|:---:|:----------:|:------:|:-----:|
|  1  |     d      |   1    |   1   |
|  2  |     c      |   1    |   2   |
|  3  |     b      |   1    |   3   |
|  4  |     a      |   1    |   4   |
|  5  |     a      |   2    |   1   |
|  6  |     d      |   2    |   2   |

``` r
quadrado %>% select(-x) %>%
  spread(coluna, tratamento) %>% kable_()
```

| linha |  1  |  2  |  3  |  4  |
|:-----:|:---:|:---:|:---:|:---:|
|   1   |  d  |  a  |  b  |  c  |
|   2   |  c  |  d  |  a  |  b  |
|   3   |  b  |  c  |  d  |  a  |
|   4   |  a  |  b  |  c  |  d  |

``` r
## ANOVA e comparações múltiplas
# Definindo seed
set.seed(1)
 
# Gerando dados
dados <- tibble(tratamento = as.factor(sort(rep(1:4, 5))),
                bloco = as.factor(rep(1:5, 4)),
                resultado = rnorm(20))

# Gerando valores da anova a partir dos dados
valores <- anova_rbcd(dados, "tratamento", "resultado", "bloco")

# Gerasndo comparações múltiplas 
comparacao <- comparacoes("tratamento", "resultado", "bloco", dados)

# Comparação de tukey
comparacao$tukey %>% kable_()
```

| Comparação | Diferença de Médias | Limite Inferior | Limite Superior | P-valor |
|:----------:|:-------------------:|:---------------:|:---------------:|:-------:|
|   1 - 2    |     -0.0058658      |    -2.095669    |    2.083938     | 1.0000  |
|   1 - 3    |      0.0911469      |    -1.998657    |    2.180950     | 0.9992  |
|   1 - 4    |     -0.3302971      |    -2.420101    |    1.759506     | 0.9644  |
|   2 - 3    |      0.0970127      |    -1.992791    |    2.186816     | 0.9990  |
|   2 - 4    |     -0.3244313      |    -2.414235    |    1.765372     | 0.9662  |
|   3 - 4    |     -0.4214440      |    -2.511248    |    1.668359     | 0.9305  |

``` r
# Gráfico de comparações dos tratamentos
plot(comparacao, trat = TRUE)
```

![](README_files/figure-gfm/exemplo-1.png)<!-- -->

<!--
# To Do

- [x] Pkgdown
- [x] README
- [x] Adicionar exemplos de cada funcao
-->
