# =========================================================
# ATIVIDADE — SÉRIES TEMPORAIS
# SÉRIE COM TENDÊNCIA: SHAMPOO SALES
# =========================================================

# ---------------------------------------------------------
# 1. CARREGAMENTO DO PACOTE
# ---------------------------------------------------------

# O pacote ggplot2 será utilizado para construir os gráficos
# da série original e da série diferenciada.

library(ggplot2)

# ---------------------------------------------------------
# 2. IMPORTAÇÃO DA BASE DE DADOS
# ---------------------------------------------------------

# A função read.csv() importa o arquivo contendo as vendas
# mensais de shampoo.
#
# A base possui duas colunas principais:
# Month: representa o período da observação;
# Sales: representa o valor das vendas de shampoo.

dados_shampoo <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/shampoo_sales.csv"
)

# ---------------------------------------------------------
# 3. INSPEÇÃO INICIAL DA BASE
# ---------------------------------------------------------

# Verifica os nomes das variáveis presentes na base.

names(dados_shampoo)

# Mostra as primeiras linhas do conjunto de dados.

head(dados_shampoo)

# Exibe a estrutura da base, incluindo o tipo das variáveis.

str(dados_shampoo)

# ---------------------------------------------------------
# 4. ORGANIZAÇÃO DA SÉRIE TEMPORAL
# ---------------------------------------------------------

# Nesta etapa, cria-se um novo objeto contendo apenas as
# informações necessárias para a análise da série temporal.
#
# A variável tempo representa os períodos da série.
# A variável valor representa as vendas de shampoo.

shampoo_ts <- data.frame(
  tempo = dados_shampoo$Month,
  valor = dados_shampoo$Sales
)

# Visualiza as primeiras linhas da série organizada.

head(shampoo_ts)

# =========================================================
# 5. GRÁFICO DA SÉRIE ORIGINAL
# =========================================================

# O gráfico da série original permite observar o comportamento
# das vendas de shampoo ao longo do tempo.
#
# Neste caso, o objetivo é verificar visualmente se existe
# tendência de crescimento ou redução na série.

ggplot(
  shampoo_ts,
  aes(
    x = tempo,
    y = valor,
    group = 1
  )
) +
  # geom_line() constrói a linha da série temporal.
  # A cor preta foi utilizada para deixar o gráfico mais limpo.
  geom_line(
    linewidth = 1.1,
    color = "black"
  ) +
  # labs() define título e nomes dos eixos.
  labs(
    title = "Série com Tendência - Shampoo Sales",
    x = "Tempo",
    y = "Vendas"
  ) +
  # theme_minimal() deixa o gráfico com aparência mais limpa.
  theme_minimal() +
  # theme() ajusta elementos visuais do gráfico.
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10)
  )

# =========================================================
# 6. APLICAÇÃO DO OPERADOR DIFERENÇA
# =========================================================

# O operador diferença é utilizado para calcular a variação
# entre observações consecutivas da série.
#
# A fórmula aplicada é:
# diferença = valor atual - valor anterior
#
# Esse procedimento é útil para reduzir a tendência da série,
# tornando o comportamento dos dados mais estável ao longo do tempo.

shampoo_ts$dif <- c(
  NA,
  diff(shampoo_ts$valor)
)

# ---------------------------------------------------------
# REMOVE O VALOR AUSENTE GERADO PELA DIFERENÇA
# ---------------------------------------------------------

# Como a primeira observação não possui valor anterior para
# comparação, a primeira diferença gerada é NA.
# Por isso, utiliza-se na.omit() para remover esse valor ausente.

shampoo_dif <- na.omit(shampoo_ts)

# Visualiza as primeiras linhas da série diferenciada.

head(shampoo_dif)

# =========================================================
# 7. GRÁFICO DA SÉRIE DIFERENCIADA
# =========================================================

# O gráfico da série diferenciada mostra as variações entre
# períodos consecutivos.
#
# Após a diferenciação, espera-se que a tendência presente na
# série original seja reduzida, facilitando a análise do
# comportamento da série.

ggplot(
  shampoo_dif,
  aes(
    x = tempo,
    y = dif,
    group = 1
  )
) +
  # Linha da série diferenciada.
  geom_line(
    linewidth = 1.1,
    color = "black"
  ) +
  # Título e nomes dos eixos.
  labs(
    title = "Série Diferenciada - Shampoo Sales",
    x = "Tempo",
    y = "Diferença"
  ) +
  # Tema visual limpo.
  theme_minimal() +
  # Ajustes visuais do gráfico.
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10)
  )



# =========================================================
# ATIVIDADE — SÉRIES TEMPORAIS
# SÉRIE SAZONAL: CONSUMO MENSAL DE GÁS NATURAL
# =========================================================

# ---------------------------------------------------------
# 1. CARREGAMENTO DOS PACOTES
# ---------------------------------------------------------

# ggplot2: construção de gráficos
# fpp3: ferramentas para séries temporais, incluindo
# tsibble, gg_season, gg_subseries e decomposição
# dplyr: manipulação dos dados

library(ggplot2)
library(fpp3)
library(dplyr)

# ---------------------------------------------------------
# 2. IMPORTAÇÃO DA BASE DE DADOS
# ---------------------------------------------------------

# A base contém o consumo mensal de gás natural,
# medido em bilhões de pés cúbicos.
# A série é mensal e não ajustada sazonalmente.

gas <- read.csv(
  "C:/Users/dudas/OneDrive/Área de Trabalho/disciplinas_do_7_periodo/series_temporais/NATURALGAS.csv"
)

# ---------------------------------------------------------
# 3. INSPEÇÃO INICIAL DA BASE
# ---------------------------------------------------------

names(gas)

head(gas)

str(gas)

# ---------------------------------------------------------
# 4. ORGANIZAÇÃO DA SÉRIE TEMPORAL
# ---------------------------------------------------------

# A variável observation_date representa o tempo.
# A variável NATURALGAS representa o consumo mensal de gás natural.

gas$observation_date <- as.Date(gas$observation_date)

gas_ts <- data.frame(
  tempo = gas$observation_date,
  valor = gas$NATURALGAS
)

head(gas_ts)

# =========================================================
# 5. GRÁFICO DA SÉRIE ORIGINAL
# =========================================================

# Este gráfico permite observar o comportamento geral da série,
# especialmente a presença de oscilações que se repetem ao longo dos anos.

ggplot(
  gas_ts,
  aes(
    x = tempo,
    y = valor
  )
) +
  geom_line(
    linewidth = 1.1,
    color = "black"
  ) +
  labs(
    title = "Série Sazonal - Consumo Mensal de Gás Natural",
    x = "Tempo",
    y = "Consumo de Gás Natural"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# =========================================================
# 6. TRANSFORMAÇÃO PARA O FORMATO TS
# =========================================================

# A função ts() transforma os dados em uma série temporal clássica.
# Como os dados são mensais, utiliza-se frequency = 12.
# A série começa em janeiro de 2000, por isso start = c(2000, 1).

gas_serie_ts <- ts(
  gas_ts$valor,
  start = c(2000, 1),
  frequency = 12
)

gas_serie_ts

# =========================================================
# 7. GRÁFICO DA SÉRIE NO FORMATO TS
# =========================================================

# O formato ts permite representar corretamente a série
# considerando a frequência mensal.

plot.ts(
  gas_serie_ts,
  col = "black",
  lwd = 2,
  main = "Série Temporal - Consumo de Gás Natural",
  xlab = "Tempo",
  ylab = "Consumo de Gás Natural"
)

# =========================================================
# 8. TRANSFORMAÇÃO PARA O FORMATO TSIBBLE
# =========================================================

# O formato tsibble organiza os dados em estrutura temporal
# compatível com as funções do pacote fpp3.

gas_tbl <- gas_ts %>%
  mutate(
    tempo = yearmonth(tempo)
  ) %>%
  as_tsibble(index = tempo)

gas_tbl

# =========================================================
# 9. GRÁFICO DE SAZONALIDADE
# =========================================================

# O gráfico sazonal permite comparar o comportamento dos meses
# ao longo dos diferentes anos.
# Para o consumo de gás natural, espera-se maior consumo
# em determinados períodos do ano, devido às variações climáticas.

gas_tbl %>%
  gg_season(
    valor,
    labels = "both"
  ) +
  labs(
    title = "Sazonalidade do Consumo de Gás Natural",
    x = "Mês",
    y = "Consumo de Gás Natural"
  ) +
  theme_minimal()

# =========================================================
# 10. GRÁFICO DE SUBSÉRIES SAZONAIS
# =========================================================

# O gráfico de subséries permite observar separadamente
# o comportamento de cada mês ao longo dos anos.

gas_tbl %>%
  gg_subseries(
    valor
  ) +
  labs(
    title = "Subséries Sazonais - Consumo de Gás Natural",
    x = "Mês",
    y = "Consumo de Gás Natural"
  ) +
  theme_minimal()

# =========================================================
# 11. DECOMPOSIÇÃO ADITIVA
# =========================================================

# A decomposição aditiva representa a série como:
# valor observado = tendência + sazonalidade + erro.
# Ela é adequada quando a amplitude das oscilações sazonais
# permanece aproximadamente constante ao longo do tempo.

gas_tbl %>%
  model(
    classical_decomposition(
      valor,
      type = "additive"
    )
  ) %>%
  components() %>%
  autoplot() +
  labs(
    title = "Decomposição Aditiva - Consumo de Gás Natural"
  ) +
  theme_minimal()

# =========================================================
# 12. DECOMPOSIÇÃO MULTIPLICATIVA
# =========================================================

# A decomposição multiplicativa representa a série como:
# valor observado = tendência x sazonalidade x erro.
# Ela é adequada quando a amplitude das oscilações sazonais
# aumenta ou diminui conforme o nível da série.

gas_tbl %>%
  model(
    classical_decomposition(
      valor,
      type = "multiplicative"
    )
  ) %>%
  components() %>%
  autoplot() +
  labs(
    title = "Decomposição Multiplicativa - Consumo de Gás Natural"
  ) +
  theme_minimal()
