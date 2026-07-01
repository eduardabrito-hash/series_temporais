#===========================================================
# PROJETO DE SÉRIES TEMPORAIS
# SCRIPT 03 - MODELAGEM POR SUAVIZAÇÃO EXPONENCIAL (ETS)
#===========================================================

#-----------------------------------------------------------
# 1. CARREGAMENTO DOS PACOTES
#-----------------------------------------------------------

library(readr)
library(forecast)

#-----------------------------------------------------------
# 2. IMPORTAÇÃO DAS BASES TRATADAS
#-----------------------------------------------------------

comercio_limpo <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/comercio_limpo.csv"
)

desemprego <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/desemprego_limpo.csv"
)

#-----------------------------------------------------------
# 3. CRIAÇÃO DAS SÉRIES TEMPORAIS
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

#-----------------------------------------------------------
# 4. VERIFICAÇÃO DAS SÉRIES TEMPORAIS
#-----------------------------------------------------------

start(comercio_ts)
end(comercio_ts)
frequency(comercio_ts)

start(desemprego_ts)
end(desemprego_ts)
frequency(desemprego_ts)

#===========================================================
# 5. VALIDAÇÃO COM TREINO E TESTE
# SÉRIE 1 - COMÉRCIO VAREJISTA
#===========================================================

# Últimos 12 meses reservados para teste:
# treino: jan/2000 a mar/2025
# teste: abr/2025 a mar/2026

treino_comercio <- window(
  comercio_ts,
  end = c(2025, 3)
)

teste_comercio <- window(
  comercio_ts,
  start = c(2025, 4)
)

#-----------------------------------------------------------
# AJUSTE DOS MODELOS ETS - COMÉRCIO
#-----------------------------------------------------------

modelo_ses_comercio <- ets(
  treino_comercio,
  model = "ANN"
)

modelo_holt_comercio <- ets(
  treino_comercio,
  model = "AAN"
)

modelo_holt_amortecido_comercio <- ets(
  treino_comercio,
  model = "AAN",
  damped = TRUE
)

modelo_hw_aditivo_comercio <- ets(
  treino_comercio,
  model = "AAA"
)

modelo_hw_multiplicativo_comercio <- ets(
  treino_comercio,
  model = "MAM"
)

#-----------------------------------------------------------
# PREVISÕES PARA O CONJUNTO DE TESTE - COMÉRCIO
#-----------------------------------------------------------

prev_ses_comercio <- forecast(
  modelo_ses_comercio,
  h = 12
)

prev_holt_comercio <- forecast(
  modelo_holt_comercio,
  h = 12
)

prev_holt_amortecido_comercio <- forecast(
  modelo_holt_amortecido_comercio,
  h = 12
)

prev_hw_aditivo_comercio <- forecast(
  modelo_hw_aditivo_comercio,
  h = 12
)

prev_hw_multiplicativo_comercio <- forecast(
  modelo_hw_multiplicativo_comercio,
  h = 12
)

#-----------------------------------------------------------
# MEDIDAS DE ACURÁCIA - COMÉRCIO
#-----------------------------------------------------------

acuracia_comercio <- rbind(
  SES = accuracy(prev_ses_comercio, teste_comercio)[2, ],
  Holt = accuracy(prev_holt_comercio, teste_comercio)[2, ],
  Holt_Amortecido = accuracy(prev_holt_amortecido_comercio, teste_comercio)[2, ],
  HW_Aditivo = accuracy(prev_hw_aditivo_comercio, teste_comercio)[2, ],
  HW_Multiplicativo = accuracy(prev_hw_multiplicativo_comercio, teste_comercio)[2, ]
)

acuracia_comercio

#-----------------------------------------------------------
# SELEÇÃO DO MODELO COM MENOR MAPE - COMÉRCIO
#-----------------------------------------------------------

melhor_modelo_comercio <- acuracia_comercio[
  which.min(acuracia_comercio[, "MAPE"]),
]

melhor_modelo_comercio

#-----------------------------------------------------------
# GRÁFICO DE VALIDAÇÃO - COMÉRCIO
#-----------------------------------------------------------

plot(
  prev_hw_multiplicativo_comercio,
  main = "Validação ETS - Comércio Varejista",
  xlab = "Ano",
  ylab = "Número-Índice"
)

lines(
  teste_comercio,
  col = "red",
  lwd = 2
)

#===========================================================
# 6. VALIDAÇÃO COM TREINO E TESTE
# SÉRIE 2 - TAXA DE DESEMPREGO
#===========================================================

# Últimos 12 meses reservados para teste:
# treino: mar/2012 a abr/2025
# teste: mai/2025 a abr/2026

treino_desemprego <- window(
  desemprego_ts,
  end = c(2025, 4)
)

teste_desemprego <- window(
  desemprego_ts,
  start = c(2025, 5)
)

#-----------------------------------------------------------
# AJUSTE DOS MODELOS ETS - DESEMPREGO
#-----------------------------------------------------------

modelo_ses_desemprego <- ets(
  treino_desemprego,
  model = "ANN"
)

modelo_holt_desemprego <- ets(
  treino_desemprego,
  model = "AAN"
)

modelo_holt_amortecido_desemprego <- ets(
  treino_desemprego,
  model = "AAN",
  damped = TRUE
)

modelo_hw_aditivo_desemprego <- ets(
  treino_desemprego,
  model = "AAA"
)

modelo_hw_multiplicativo_desemprego <- ets(
  treino_desemprego,
  model = "MAM"
)

#-----------------------------------------------------------
# PREVISÕES PARA O CONJUNTO DE TESTE - DESEMPREGO
#-----------------------------------------------------------

prev_ses_desemprego <- forecast(
  modelo_ses_desemprego,
  h = 12
)

prev_holt_desemprego <- forecast(
  modelo_holt_desemprego,
  h = 12
)

prev_holt_amortecido_desemprego <- forecast(
  modelo_holt_amortecido_desemprego,
  h = 12
)

prev_hw_aditivo_desemprego <- forecast(
  modelo_hw_aditivo_desemprego,
  h = 12
)

prev_hw_multiplicativo_desemprego <- forecast(
  modelo_hw_multiplicativo_desemprego,
  h = 12
)

#-----------------------------------------------------------
# MEDIDAS DE ACURÁCIA - DESEMPREGO
#-----------------------------------------------------------

acuracia_desemprego <- rbind(
  SES = accuracy(prev_ses_desemprego, teste_desemprego)[2, ],
  Holt = accuracy(prev_holt_desemprego, teste_desemprego)[2, ],
  Holt_Amortecido = accuracy(prev_holt_amortecido_desemprego, teste_desemprego)[2, ],
  HW_Aditivo = accuracy(prev_hw_aditivo_desemprego, teste_desemprego)[2, ],
  HW_Multiplicativo = accuracy(prev_hw_multiplicativo_desemprego, teste_desemprego)[2, ]
)

acuracia_desemprego

#-----------------------------------------------------------
# SELEÇÃO DO MODELO COM MENOR MAPE - DESEMPREGO
#-----------------------------------------------------------

melhor_modelo_desemprego <- acuracia_desemprego[
  which.min(acuracia_desemprego[, "MAPE"]),
]

melhor_modelo_desemprego

#-----------------------------------------------------------
# GRÁFICO DE VALIDAÇÃO - DESEMPREGO
#-----------------------------------------------------------

plot(
  prev_hw_aditivo_desemprego,
  main = "Validação ETS - Taxa de Desemprego",
  xlab = "Ano",
  ylab = "Percentual (%)"
)

lines(
  teste_desemprego,
  col = "red",
  lwd = 2
)

#===========================================================
# FIM da - MODELAGEM ETS
#===========================================================

