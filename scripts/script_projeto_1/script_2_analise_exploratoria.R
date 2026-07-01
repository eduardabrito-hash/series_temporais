#===========================================================
# PROJETO DE SÉRIES TEMPORAIS
# ANÁLISE EXPLORATÓRIA
#===========================================================

library(readr)
library(dplyr)
library(fpp3)

#===========================================================
# IMPORTAÇÃO DAS BASES
#===========================================================

comercio_limpo <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/comercio_limpo.csv"
)

desemprego <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/desemprego_limpo.csv"
)

#===========================================================
# ANÁLISE EXPLORATÓRIA
# SÉRIE 1 - COMÉRCIO VAREJISTA
#===========================================================

#-----------------------------------------------------------
# 1. ESTRUTURA DA BASE
#-----------------------------------------------------------

str(comercio_limpo)

head(comercio_limpo)

tail(comercio_limpo)

dim(comercio_limpo)

#-----------------------------------------------------------
# 2. VALORES AUSENTES
#-----------------------------------------------------------

sum(is.na(comercio_limpo))

colSums(is.na(comercio_limpo))

#-----------------------------------------------------------
# 3. ESTATÍSTICAS DESCRITIVAS
#-----------------------------------------------------------

summary(comercio_limpo$Indice_Vendas)

mean(comercio_limpo$Indice_Vendas)

median(comercio_limpo$Indice_Vendas)

sd(comercio_limpo$Indice_Vendas)

var(comercio_limpo$Indice_Vendas)

min(comercio_limpo$Indice_Vendas)

max(comercio_limpo$Indice_Vendas)

#-----------------------------------------------------------
# 4. CRIAÇÃO DA SÉRIE TEMPORAL
#-----------------------------------------------------------

comercio_ts <- ts(
  comercio_limpo$Indice_Vendas,
  start = c(2000, 1),
  frequency = 12
)

#-----------------------------------------------------------
# 5. VERIFICAÇÃO DA SÉRIE TEMPORAL
#-----------------------------------------------------------

start(comercio_ts)

end(comercio_ts)

frequency(comercio_ts)

#-----------------------------------------------------------
# 6. GRÁFICO DA SÉRIE
#-----------------------------------------------------------

plot(
  comercio_ts,
  main = "Índice de Volume de Vendas do Comércio Varejista",
  xlab = "Ano",
  ylab = "Número-Índice",
  col = "blue",
  lwd = 2
)

#-----------------------------------------------------------
# 7. TRANSFORMAÇÃO PARA TSIBBLE
#-----------------------------------------------------------

comercio_tbl <- comercio_limpo %>%
  mutate(
    Data = yearmonth(seq(
      from = as.Date("2000-01-01"),
      by = "month",
      length.out = n()
    ))
  ) %>%
  as_tsibble(index = Data)

comercio_tbl

#-----------------------------------------------------------
# 8. GRÁFICO DE SAZONALIDADE
#-----------------------------------------------------------

comercio_tbl %>%
  gg_season(
    Indice_Vendas,
    labels = "both"
  ) +
  labs(
    title = "Sazonalidade - Comércio Varejista",
    x = "Mês",
    y = "Número-Índice"
  )

#-----------------------------------------------------------
# 9. SUBSÉRIES SAZONAIS
#-----------------------------------------------------------

comercio_tbl %>%
  gg_subseries(
    Indice_Vendas
  ) +
  labs(
    title = "Subséries Sazonais - Comércio Varejista",
    x = "Mês",
    y = "Número-Índice"
  )


#-----------------------------------------------------------
# 10. DECOMPOSIÇÃO MULTIPLICATIVA DA SÉRIE
#-----------------------------------------------------------

decomposicao_comercio <- decompose(
  comercio_ts,
  type = "multiplicative"
)

plot(decomposicao_comercio)

#-----------------------------------------------------------
# 11. FUNÇÕES DE AUTOCORRELAÇÃO
#-----------------------------------------------------------

acf(
  comercio_ts,
  main = "ACF - Comércio Varejista"
)

pacf(
  comercio_ts,
  main = "PACF - Comércio Varejista"
)

#-----------------------------------------------------------
# 12. HISTOGRAMA
#-----------------------------------------------------------

hist(
  comercio_limpo$Indice_Vendas,
  main = "Histograma - Comércio Varejista",
  xlab = "Número-Índice",
  col = "deepskyblue",
  border = "navy"
)

#-----------------------------------------------------------
# 13. BOXPLOT
#-----------------------------------------------------------

boxplot(
  comercio_limpo$Indice_Vendas,
  main = "Boxplot - Comércio Varejista",
  ylab = "Número-Índice",
  col = "skyblue"
)

#-----------------------------------------------------------
# 14. IDENTIFICAÇÃO DE OUTLIERS VIA STL - COMÉRCIO
#-----------------------------------------------------------

stl_comercio <- stl(
  comercio_ts,
  s.window = "periodic",
  robust = TRUE
)

plot(stl_comercio)

residuos_comercio <- stl_comercio$time.series[, "remainder"]

outliers_comercio_stl <- residuos_comercio[
  residuos_comercio < quantile(residuos_comercio, 0.25) - 3 * IQR(residuos_comercio) |
    residuos_comercio > quantile(residuos_comercio, 0.75) + 3 * IQR(residuos_comercio)
]

outliers_comercio_stl

#===========================================================
# ANÁLISE EXPLORATÓRIA
# SÉRIE 2 - TAXA DE DESEMPREGO
#===========================================================

#-----------------------------------------------------------
# 1. ESTRUTURA DA BASE
#-----------------------------------------------------------

str(desemprego)

head(desemprego)

tail(desemprego)

dim(desemprego)

#-----------------------------------------------------------
# 2. CONVERSÃO DA DATA
#-----------------------------------------------------------

desemprego$Data <- as.Date(
  desemprego$Data
)

#-----------------------------------------------------------
# 3. VALORES AUSENTES
#-----------------------------------------------------------

sum(is.na(desemprego))

colSums(is.na(desemprego))

#-----------------------------------------------------------
# 4. ESTATÍSTICAS DESCRITIVAS
#-----------------------------------------------------------

summary(desemprego$Taxa_Desemprego)

mean(desemprego$Taxa_Desemprego)

median(desemprego$Taxa_Desemprego)

sd(desemprego$Taxa_Desemprego)

var(desemprego$Taxa_Desemprego)

min(desemprego$Taxa_Desemprego)

max(desemprego$Taxa_Desemprego)

#-----------------------------------------------------------
# 5. CRIAÇÃO DA SÉRIE TEMPORAL
#-----------------------------------------------------------

desemprego_ts <- ts(
  desemprego$Taxa_Desemprego,
  start = c(2012, 3),
  frequency = 12
)

#-----------------------------------------------------------
# 6. VERIFICAÇÃO DA SÉRIE TEMPORAL
#-----------------------------------------------------------

start(desemprego_ts)

end(desemprego_ts)

frequency(desemprego_ts)

#-----------------------------------------------------------
# 7. GRÁFICO DA SÉRIE
#-----------------------------------------------------------

plot(
  desemprego_ts,
  main = "Taxa de Desemprego",
  xlab = "Ano",
  ylab = "Percentual (%)",
  col = "black",
  lwd = 2
)


#-----------------------------------------------------------
# 8. TRANSFORMAÇÃO PARA TSIBBLE
#-----------------------------------------------------------

desemprego_tbl <- desemprego %>%
  mutate(
    Data = yearmonth(Data)
  ) %>%
  as_tsibble(index = Data)

desemprego_tbl

#-----------------------------------------------------------
# 9. GRÁFICO DE SAZONALIDADE
#-----------------------------------------------------------

desemprego_tbl %>%
  gg_season(
    Taxa_Desemprego,
    labels = "both"
  ) +
  labs(
    title = "Sazonalidade - Taxa de Desemprego",
    x = "Mês",
    y = "Percentual (%)"
  )

#-----------------------------------------------------------
# 10. SUBSÉRIES SAZONAIS
#-----------------------------------------------------------

desemprego_tbl %>%
  gg_subseries(
    Taxa_Desemprego
  ) +
  labs(
    title = "Subséries Sazonais - Taxa de Desemprego",
    x = "Mês",
    y = "Percentual (%)"
  )

#-----------------------------------------------------------
# 11. DECOMPOSIÇÃO ADITIVA DA SÉRIE
#-----------------------------------------------------------

decomposicao_desemprego <- decompose(
  desemprego_ts,
  type = "additive"
)

plot(decomposicao_desemprego)

#-----------------------------------------------------------
# 12. FUNÇÕES DE AUTOCORRELAÇÃO
#-----------------------------------------------------------

acf(
  desemprego_ts,
  main = "ACF - Taxa de Desemprego"
)

pacf(
  desemprego_ts,
  main = "PACF - Taxa de Desemprego"
)

#-----------------------------------------------------------
# 13. HISTOGRAMA
#-----------------------------------------------------------

hist(
  desemprego$Taxa_Desemprego,
  main = "Histograma - Taxa de Desemprego",
  xlab = "Percentual (%)",
  col = "mediumseagreen",
  border = "darkgreen"
)

#-----------------------------------------------------------
# 14. BOXPLOT
#-----------------------------------------------------------

boxplot(
  desemprego$Taxa_Desemprego,
  main = "Boxplot - Taxa de Desemprego",
  ylab = "Percentual (%)",
  col = "lightgreen"
)

#-----------------------------------------------------------
# 15. IDENTIFICAÇÃO DE OUTLIERS VIA STL - DESEMPREGO
#-----------------------------------------------------------

stl_desemprego <- stl(
  desemprego_ts,
  s.window = "periodic",
  robust = TRUE
)

plot(stl_desemprego)

residuos_desemprego <- stl_desemprego$time.series[, "remainder"]

outliers_desemprego_stl <- residuos_desemprego[
  residuos_desemprego < quantile(residuos_desemprego, 0.25) - 3 * IQR(residuos_desemprego) |
    residuos_desemprego > quantile(residuos_desemprego, 0.75) + 3 * IQR(residuos_desemprego)
]

outliers_desemprego_stl
#===========================================================
# FIM DA ANÁLISE EXPLORATÓRIA
#===========================================================

