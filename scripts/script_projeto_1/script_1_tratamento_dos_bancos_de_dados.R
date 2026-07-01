#===========================================================
# PROJETO DE SÉRIES TEMPORAIS
# TRATAMENTO DOS DADOS
# SÉRIE 1: ÍNDICE DE VOLUME DE VENDAS DO COMÉRCIO VAREJISTA
# Fonte: SIDRA/IBGE - Tabela 8880
#===========================================================

#-----------------------------------------------------------
# 1. CARREGAMENTO DO PACOTE NECESSÁRIO
#-----------------------------------------------------------

library(readr)

#-----------------------------------------------------------
# 2. IMPORTAÇÃO DA BASE DE DADOS
#-----------------------------------------------------------

comercio <- read_csv2(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/tabela8880 (2).csv"
)

#-----------------------------------------------------------
# 3. INSPEÇÃO INICIAL DA BASE
#-----------------------------------------------------------

str(comercio)

head(comercio)

dim(comercio)

#-----------------------------------------------------------
# 4. VERIFICAÇÃO DE VALORES AUSENTES
#-----------------------------------------------------------

sum(is.na(comercio))

#-----------------------------------------------------------
# 5. LEITURA DO ARQUIVO COMO TEXTO
#-----------------------------------------------------------

linhas <- readLines(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/tabela8880 (2).csv"
)

#-----------------------------------------------------------
# 6. EXTRAÇÃO DAS DATAS
#-----------------------------------------------------------

datas <- strsplit(
  linhas[4],
  ";"
)[[1]]

datas <- datas[
  seq(
    from = 4,
    to = length(datas),
    by = 2
  )
]

datas <- gsub(
  "\"",
  "",
  datas
)

#-----------------------------------------------------------
# 7. EXTRAÇÃO DOS VALORES DA SÉRIE
#-----------------------------------------------------------

dados <- strsplit(
  linhas[6],
  ";"
)[[1]]

serie_comercio <- dados[
  seq(
    from = 4,
    to = length(dados),
    by = 2
  )
]

serie_comercio <- gsub(
  "\"",
  "",
  serie_comercio
)

serie_comercio <- gsub(
  ",",
  ".",
  serie_comercio
)

serie_comercio <- as.numeric(
  serie_comercio
)

#-----------------------------------------------------------
# 8. CONSTRUÇÃO DA BASE TRATADA
#-----------------------------------------------------------

comercio_limpo <- data.frame(
  Data = datas,
  Indice_Vendas = serie_comercio
)

#-----------------------------------------------------------
# 9. AUDITORIA FINAL DA BASE
#-----------------------------------------------------------

str(comercio_limpo)

head(comercio_limpo)

tail(comercio_limpo)

dim(comercio_limpo)

sum(is.na(comercio_limpo))

summary(comercio_limpo)

#-----------------------------------------------------------
# 10. CRIAÇÃO DA SÉRIE TEMPORAL
#-----------------------------------------------------------

comercio_ts <- ts(
  comercio_limpo$Indice_Vendas,
  start = c(2000, 1),
  frequency = 12
)



start(comercio_ts)
end(comercio_ts)
frequency(comercio_ts)


#-----------------------------------------------------------
# 11. SALVAMENTO DA BASE TRATADA
#-----------------------------------------------------------

write.csv(
  comercio_limpo,
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/comercio_limpo.csv",
  row.names = FALSE
)

#===========================================================
# PROJETO DE SÉRIES TEMPORAIS
# TRATAMENTO DOS DADOS
# SÉRIE 2: TAXA DE DESEMPREGO
# Fonte: IPEADATA / IBGE - PNAD Contínua
#===========================================================

#-----------------------------------------------------------
# CARREGAMENTO DO PACOTE
#-----------------------------------------------------------

library(readr)

#-----------------------------------------------------------
# 1. IMPORTAÇÃO DA BASE DE DADOS
#-----------------------------------------------------------

desemprego <- read_csv2(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/ipeadata.csv"
)

#-----------------------------------------------------------
# 2. INSPEÇÃO INICIAL DA BASE
#-----------------------------------------------------------

str(desemprego)

head(desemprego)

tail(desemprego)

dim(desemprego)

#-----------------------------------------------------------
# 3. VERIFICAÇÃO DE VALORES AUSENTES
#-----------------------------------------------------------

sum(is.na(desemprego))

colSums(is.na(desemprego))

#-----------------------------------------------------------
# 4. REMOÇÃO DE COLUNA VAZIA
#-----------------------------------------------------------

desemprego <- desemprego[, 1:2]

#-----------------------------------------------------------
# 5. PADRONIZAÇÃO DOS NOMES DAS VARIÁVEIS
#-----------------------------------------------------------

names(desemprego) <- c(
  "Data",
  "Taxa_Desemprego"
)

#-----------------------------------------------------------
# 6. CONVERSÃO DA DATA
#-----------------------------------------------------------

desemprego$Data <- as.character(
  desemprego$Data
)

desemprego$Data <- as.Date(
  paste0(desemprego$Data, "01"),
  format = "%Y%m%d"
)

#-----------------------------------------------------------
# 7. AUDITORIA FINAL DA BASE
#-----------------------------------------------------------

str(desemprego)

head(desemprego)

tail(desemprego)

dim(desemprego)

summary(desemprego)

sum(is.na(desemprego))

colSums(is.na(desemprego))

#-----------------------------------------------------------
# 8. VERIFICAÇÃO DO PERÍODO DA SÉRIE
#-----------------------------------------------------------

range(desemprego$Data)



#-----------------------------------------------------------
# 9. CRIAÇÃO DA SÉRIE TEMPORAL
#-----------------------------------------------------------

desemprego_ts <- ts(
  desemprego$Taxa_Desemprego,
  start = c(2012, 3),
  frequency = 12
)

start(desemprego_ts)
end(desemprego_ts)
frequency(desemprego_ts)


#-----------------------------------------------------------
# 10. SALVAMENTO DA BASE TRATADA
#-----------------------------------------------------------

write.csv(
  desemprego,
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/projeto_unidade_I_series_temporais/desemprego_limpo.csv",
  row.names = FALSE
)

#-----------------------------------------------------------
# FIM DA ETAPA DE TRATAMENTO DOS DADOS
#-----------------------------------------------------------