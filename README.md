
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

| Trat |    B1    |    B2     |    B3     |    B4    |
|:----:|:--------:|:---------:|:---------:|:--------:|
|  1   | 1.109136 |    \-     | -2.182401 |    \-    |
|  2   |    \-    |    \-     |    \-     | 0.386244 |
|  3   | 0.503986 | -0.044535 |    \-     | 0.268416 |
|  4   |    \-    | -1.012029 | -0.030695 |    \-    |

``` r
# Quadrados Latinos
quadrado <- quadrados_latinos(4)

head(quadrado$dados) %>% kable_()
```

| resultado  | tratamento | coluna | linha | replica |
|:----------:|:----------:|:------:|:-----:|:-------:|
| 0.0552854  |     1      |   1    |   1   |    1    |
| -0.6430979 |     3      |   1    |   2   |    1    |
| 1.0281741  |     4      |   1    |   3   |    1    |
| 0.4652253  |     2      |   1    |   4   |    1    |
| -0.7614267 |     2      |   2    |   1   |    1    |
| 0.4556577  |     4      |   2    |   2   |    1    |

``` r
quadrado$dados_matriz %>% kable_() 
```

| linha |   1   |   2   |   3   |   4   |
|:-----:|:-----:|:-----:|:-----:|:-----:|
|   1   | \[1\] | \[2\] | \[3\] | \[4\] |
|   2   | \[3\] | \[4\] | \[1\] | \[2\] |
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

| X..hat..mu.. | X..hat..tau.. | X..hat..beta.. | X..hat..tau.\_i. | X..hat..beta.\_i. |
|:------------:|:-------------:|:--------------:|:----------------:|:-----------------:|
|  0.1905239   |   0.7620955   |   0.5715716    |    0.9526194     |     0.7620955     |

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
