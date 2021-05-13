
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
bloco_incompleto(4, 4, 2)$dados_matriz %>% kable_()
```

| Trat |         B1         |        B2         |        B3         |         B4         |
|:----:|:------------------:|:-----------------:|:-----------------:|:------------------:|
|  1   | -1.51369052344602  |        \-         | -1.65104739544377 | 0.519367501487229  |
|  2   |         \-         | -1.19900029596498 | -1.79657624848368 |         \-         |
|  3   | -0.373804600672753 | 0.532115743878701 |        \-         |         \-         |
|  4   |         \-         |        \-         |        \-         | -0.581549407957574 |

``` r
# Quadrados Latinos
quadrado <- quadrados_latinos(4)

head(quadrado$dados) %>% kable_()
```

| resultado  | tratamento | coluna | linha | replica |
|:----------:|:----------:|:------:|:-----:|:-------:|
| -0.8870079 |     3      |   1    |   1   |    1    |
| 2.6521274  |     1      |   1    |   2   |    1    |
| 1.1489758  |     4      |   1    |   3   |    1    |
| -0.1852933 |     2      |   1    |   4   |    1    |
| 0.6944111  |     4      |   2    |   1   |    1    |
| 1.0635626  |     2      |   2    |   2   |    1    |

``` r
quadrado$dados_matriz %>% kable_() 
```

| linha |   1   |   2   |   3   |   4   |
|:-----:|:-----:|:-----:|:-----:|:-----:|
|   1   | \[3\] | \[4\] | \[1\] | \[2\] |
|   2   | \[1\] | \[2\] | \[3\] | \[4\] |
|   3   | \[4\] | \[1\] | \[2\] | \[3\] |
|   4   | \[2\] | \[3\] | \[4\] | \[1\] |

``` r
## ANOVA e comparações múltiplas
# Definindo seed
set.seed(1)
 
# Gerando dados
dados <- bloco_casualizado(4, 5)$dados

# Gerando valores da anova a partir dos dados
valores <- anova_rbcd(dados, "Trat", "resultado", "bloco")

valores$estimados %>% kable_()
```

| *β̂*<sub>*i*</sub> |    *β̂*    | *μ̂*<sub>*i*</sub> |    *μ̂*    |    *τ̂*    |
|:-----------------:|:---------:|:-----------------:|:---------:|:---------:|
|     0.7620955     | 0.5715716 |     0.9526194     | 0.1905239 | 0.7620955 |

``` r
# Gerasndo comparações múltiplas 
comparacao <- comparacoes("Trat", "resultado", "bloco", dados)

# Comparação de tukey
comparacao$tukey %>% kable_()
```

| Comparação | Diferença de Médias | Limite Inferior | Limite Superior | P-valor |
|:----------:|:-------------------:|:---------------:|:---------------:|:-------:|
|   1 - 2    |      0.3708963      |    -1.231121    |    1.9729140    | 0.8999  |
|   1 - 3    |     -0.6936659      |    -2.295684    |    0.9083519    | 0.5885  |
|   1 - 4    |     -0.7262024      |    -2.328220    |    0.8758153    | 0.5536  |
|   2 - 3    |     -1.0645622      |    -2.666580    |    0.5374556    | 0.2509  |
|   2 - 4    |     -1.0970987      |    -2.699116    |    0.5049190    | 0.2297  |
|   3 - 4    |     -0.0325365      |    -1.634554    |    1.5694812    | 0.9999  |

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
