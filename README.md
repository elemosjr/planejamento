
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

| Trat |    B1     |    B2     |    B3    |    B4     |
|:----:|:---------:|:---------:|:--------:|:---------:|
|  1   | -0.063781 |    \-     | 0.704961 |    \-     |
|  2   |    \-     | -1.354994 | 0.716293 | -1.937244 |
|  3   | 0.039046  | 0.657968  |    \-    |    \-     |
|  4   |    \-     |    \-     |    \-    | 0.235780  |

``` r
# Quadrados Latinos
quadrado <- quadrados_latinos(4)

head(quadrado$dados) %>% kable_()
```

| resultado  | tratamento | coluna | linha | replica |
|:----------:|:----------:|:------:|:-----:|:-------:|
| 0.1501834  |     3      |   1    |   1   |    1    |
| 0.3669467  |     2      |   1    |   2   |    1    |
| 0.8676153  |     1      |   1    |   3   |    1    |
| -1.6173951 |     4      |   1    |   4   |    1    |
| -0.6653213 |     4      |   2    |   1   |    1    |
| 1.1135816  |     3      |   2    |   2   |    1    |

``` r
quadrado$dados_matriz %>% kable_() 
```

| linha |   1   |   2   |   3   |   4   |
|:-----:|:-----:|:-----:|:-----:|:-----:|
|   1   | \[3\] | \[4\] | \[1\] | \[2\] |
|   2   | \[2\] | \[3\] | \[4\] | \[1\] |
|   3   | \[1\] | \[2\] | \[3\] | \[4\] |
|   4   | \[4\] | \[1\] | \[2\] | \[3\] |

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
