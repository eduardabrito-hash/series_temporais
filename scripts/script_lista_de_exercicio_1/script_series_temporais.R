#====================================================
# PACOTES
#====================================================

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)


#====================================================
# QUESTÃO 1
# SÉRIES CLIMÁTICAS DE DELHI
#====================================================

#----------------------------------------------------
# IMPORTAÇÃO DOS DADOS
#----------------------------------------------------

dados <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/git_eduarda/Series_Temporais/dados/dados_lista_de_exercicio_1/DailyDelhiClimateTrain.csv"
)

str(dados)

dados$date <- ymd(dados$date)

dados <- dados %>%
  arrange(date)

head(dados)


#----------------------------------------------------
# GRÁFICOS DAS SÉRIES DIÁRIAS
#----------------------------------------------------

# Temperatura

ggplot(dados, aes(x = date, y = meantemp)) +
  geom_line(color = "steelblue") +
  labs(
    title = "Série Temporal - Temperatura Média",
    x = "Tempo",
    y = "Temperatura"
  ) +
  theme_minimal()


# Umidade

ggplot(dados, aes(x = date, y = humidity)) +
  geom_line(color = "darkred") +
  labs(
    title = "Série Temporal - Umidade",
    x = "Tempo",
    y = "Umidade"
  ) +
  theme_minimal()


# Velocidade do vento

ggplot(dados, aes(x = date, y = wind_speed)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Série Temporal - Velocidade do Vento",
    x = "Tempo",
    y = "Velocidade do vento"
  ) +
  theme_minimal()


# Pressão atmosférica

ggplot(dados, aes(x = date, y = meanpressure)) +
  geom_line(color = "purple") +
  labs(
    title = "Série Temporal - Pressão Atmosférica",
    x = "Tempo",
    y = "Pressão"
  ) +
  theme_minimal()


#----------------------------------------------------
# TRANSFORMAÇÃO PARA FREQUÊNCIA MENSAL
#----------------------------------------------------

dados_mensal <- dados %>%
  group_by(mes = floor_date(date, "month")) %>%
  summarise(
    meantemp = mean(meantemp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    meanpressure = mean(meanpressure, na.rm = TRUE),
    .groups = "drop"
  )


#----------------------------------------------------
# GRÁFICOS DAS SÉRIES MENSAIS
#----------------------------------------------------

# Temperatura mensal

ggplot(dados_mensal, aes(x = mes, y = meantemp)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Temperatura Média - Mensal",
    x = "Tempo",
    y = "Temperatura média"
  )


# Umidade mensal

ggplot(dados_mensal, aes(x = mes, y = humidity)) +
  geom_line(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Umidade - Mensal",
    x = "Tempo",
    y = "Umidade média"
  )


# Velocidade do vento mensal

ggplot(dados_mensal, aes(x = mes, y = wind_speed)) +
  geom_line(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Velocidade do Vento - Mensal",
    x = "Tempo",
    y = "Velocidade média do vento"
  )


# Pressão atmosférica mensal

ggplot(dados_mensal, aes(x = mes, y = meanpressure)) +
  geom_line(color = "purple") +
  theme_minimal() +
  labs(
    title = "Pressão Atmosférica - Mensal",
    x = "Tempo",
    y = "Pressão média"
  )


#----------------------------------------------------
# TRANSFORMAÇÃO PARA FREQUÊNCIA TRIMESTRAL
#----------------------------------------------------

dados_trimestral <- dados %>%
  group_by(trimestre = floor_date(date, "quarter")) %>%
  summarise(
    meantemp = mean(meantemp, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE),
    wind_speed = mean(wind_speed, na.rm = TRUE),
    meanpressure = mean(meanpressure, na.rm = TRUE),
    .groups = "drop"
  )


#----------------------------------------------------
# GRÁFICOS DAS SÉRIES TRIMESTRAIS
#----------------------------------------------------

# Temperatura trimestral

ggplot(dados_trimestral, aes(x = trimestre, y = meantemp)) +
  geom_line(color = "steelblue") +
  theme_minimal() +
  labs(
    title = "Temperatura Média - Trimestral",
    x = "Tempo",
    y = "Temperatura média"
  )


# Umidade trimestral

ggplot(dados_trimestral, aes(x = trimestre, y = humidity)) +
  geom_line(color = "darkred") +
  theme_minimal() +
  labs(
    title = "Umidade - Trimestral",
    x = "Tempo",
    y = "Umidade média"
  )


# Velocidade do vento trimestral

ggplot(dados_trimestral, aes(x = trimestre, y = wind_speed)) +
  geom_line(color = "darkgreen") +
  theme_minimal() +
  labs(
    title = "Velocidade do Vento - Trimestral",
    x = "Tempo",
    y = "Velocidade média do vento"
  )


# Pressão atmosférica trimestral

ggplot(dados_trimestral, aes(x = trimestre, y = meanpressure)) +
  geom_line(color = "purple") +
  theme_minimal() +
  labs(
    title = "Pressão Atmosférica - Trimestral",
    x = "Tempo",
    y = "Pressão média"
  )


#====================================================
# QUESTÃO 2
# SÉRIES DE VACINAÇÃO - BRASIL
#====================================================

#----------------------------------------------------
# IMPORTAÇÃO DOS DADOS
#----------------------------------------------------

dados_vacina <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/git_eduarda/Series_Temporais/dados/dados_lista_de_exercicio_1/vaccinations.csv"
)

dados_vacina$date <- as.Date(dados_vacina$date)

brasil <- dados_vacina %>%
  filter(location == "Brazil")


#----------------------------------------------------
# GRÁFICOS DAS SÉRIES DE VACINAÇÃO
#----------------------------------------------------

# Total de vacinações

ggplot(brasil, aes(x = date, y = total_vaccinations)) +
  geom_line(color = "blue") +
  labs(
    title = "Total de vacinações - Brasil",
    x = "Data",
    y = "Total de doses"
  ) +
  theme_minimal()


# Pessoas vacinadas

ggplot(brasil, aes(x = date, y = people_vaccinated)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Pessoas vacinadas - Brasil",
    x = "Data",
    y = "Pessoas vacinadas"
  ) +
  theme_minimal()


# Pessoas totalmente vacinadas

ggplot(brasil, aes(x = date, y = people_fully_vaccinated)) +
  geom_line(color = "purple") +
  labs(
    title = "Pessoas totalmente vacinadas - Brasil",
    x = "Data",
    y = "Pessoas totalmente vacinadas"
  ) +
  theme_minimal()


# Vacinação diária

ggplot(brasil, aes(x = date, y = daily_vaccinations)) +
  geom_line(color = "red") +
  labs(
    title = "Vacinação diária - Brasil",
    x = "Data",
    y = "Doses diárias"
  ) +
  theme_minimal()


#====================================================
# QUESTÃO 3
# SÉRIES DE DESMATAMENTO POR ESTADO
#====================================================

#----------------------------------------------------
# IMPORTAÇÃO DOS DADOS
#----------------------------------------------------

dados_desmatamento <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/git_eduarda/Series_Temporais/dados/dados_lista_de_exercicio_1/desmatamento_prodes.csv"
)

str(dados_desmatamento)


#----------------------------------------------------
# ORGANIZAÇÃO DOS DADOS
#----------------------------------------------------

dados_long <- dados_desmatamento %>%
  pivot_longer(
    cols = acre:tocantins,
    names_to = "estado",
    values_to = "desmatamento"
  )


#----------------------------------------------------
# GRÁFICOS DAS SÉRIES TEMPORAIS POR ESTADO
#----------------------------------------------------

ggplot(dados_long,
       aes(x = referencia,
           y = desmatamento,
           color = estado)) +
  geom_line() +
  facet_wrap(~ estado, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Séries temporais do desmatamento por estado",
    x = "Ano",
    y = "Área desmatada"
  )

