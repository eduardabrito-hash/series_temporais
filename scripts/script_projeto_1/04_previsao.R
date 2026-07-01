#===========================================================
# PROJETO DE SÉRIES TEMPORAIS
# SCRIPT 04 - PREVISÕES E DIAGNÓSTICO DOS RESÍDUOS
#===========================================================

library(readr)
library(forecast)

#-----------------------------------------------------------
# 1. IMPORTAÇÃO DAS BASES TRATADAS
#-----------------------------------------------------------

comercio_limpo <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/comercio_limpo.csv"
)

desemprego <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/desemprego_limpo.csv"
)

#-----------------------------------------------------------
# 2. CRIAÇÃO DAS SÉRIES TEMPORAIS
#-----------------------------------------------------------

comercio_ts <- ts(
  comercio_limpo$Indice_Vendas,
  start = c(2000, 1),
  frequency = 12
)

desemprego_ts <- ts(
  desemprego$Taxa_Desemprego,
  start = c(2012, 3),
  frequency = 12
)

#===========================================================
# 3. PREVISÃO FINAL - COMÉRCIO VAREJISTA
#===========================================================

modelo_ets_comercio <- ets(
  comercio_ts,
  model = "MAM"
)

summary(modelo_ets_comercio)

modelo_ets_comercio$method

accuracy(modelo_ets_comercio)

previsao_comercio <- forecast(
  modelo_ets_comercio,
  h = 12
)

previsao_comercio

plot(
  previsao_comercio,
  main = "Previsão ETS - Comércio Varejista",
  xlab = "Ano",
  ylab = "Número-Índice"
)

checkresiduals(modelo_ets_comercio)

#===========================================================
# 4. PREVISÃO FINAL - TAXA DE DESEMPREGO
#===========================================================

modelo_ets_desemprego <- ets(
  desemprego_ts,
  model = "AAA"
)

summary(modelo_ets_desemprego)

modelo_ets_desemprego$method

accuracy(modelo_ets_desemprego)

previsao_desemprego <- forecast(
  modelo_ets_desemprego,
  h = 12
)

previsao_desemprego

plot(
  previsao_desemprego,
  main = "Previsão ETS - Taxa de Desemprego",
  xlab = "Ano",
  ylab = "Percentual (%)"
)

checkresiduals(modelo_ets_desemprego)

#===========================================================
# FIM DO SCRIPT 04 - PREVISÕES
#===========================================================