pacman::p_load(tidyverse, forecast, lubridate, ggplot2)

#################### TBATS dados Diarios ########################

# 1) Importar dados diários de bike sharing
caminho <- "day.csv"

bike <- read.csv(caminho) %>%
  as_tibble() %>%
  mutate(date = ymd(dteday)) %>%        # cria coluna de data em formato Date
  arrange(date) %>%
  # mantém só as colunas que vamos usar
  select(date,
         cnt,                          # contagem diária total
         temp,                         # temperatura normalizada
         hum,                          # umidade normalizada
         wind = windspeed)             # renomeia windspeed para wind

glimpse(bike)

# 2) Gráfico da série diária
ggplot(bike, aes(x = date, y = cnt)) +
  geom_line(alpha = 0.6) +
  labs(title = "Contagem de bicicletas por dia",
       x = "Data",
       y = "Total de bikes (cnt)") +
  theme_minimal()

# 3) Série com múltiplas sazonalidades (semanal, 'mensal', anual)
bike_msts <- msts(
  bike$cnt,
  seasonal.periods = c(7, 365)
)

# 4) Decomposição MSTL
bike_mstl <- mstl(bike_msts, iterate = 2)

plot(
  bike_mstl,
  main = "Decomposição MSTL - Dados Diários do Bike Sharing"
)

# Ajusta TBATS na série com múltiplas sazonalidades
mod_tbats <- tbats(bike_msts, num.cores = NULL, use.parallel = TRUE)

mod_tbats   # para ver os parâmetros do modelo

# Previsão de 30 dias à frente
prev_tbats <- forecast(mod_tbats, h = 30)

autoplot(prev_tbats) +
  labs(title = "Previsão TBATS - Contagem diária de bicicletas",
       x = "Data",
       y = "Total de bikes (cnt)") +
  theme_minimal()

#################### TBATS dados horarios########################

# 1) Importar dados diários de bike sharing
caminho <- "hour.csv"

bike_hour <- read.csv(caminho) %>%
  as_tibble() %>%
  mutate(
    dteday   = ymd(dteday),
    datetime = ymd_h(paste(dteday, hr))
  ) %>%
  arrange(datetime)

glimpse(bike_hour)

# 2) Gráfico da série horaria
ggplot(bike_hour, aes(x = datetime, y = cnt)) +
  geom_line(alpha = 0.6) +
  labs(title = "Contagem de bicicletas por hora",
       x = "Data/Hora",
       y = "Total de bikes (cnt)") +
  theme_minimal()

# 3) Série com múltiplas sazonalidades (semanal, 'mensal', anual)

bike_msts <- msts(
  bike_hour$cnt,
  seasonal.periods = c(24, 24*7, 24*30,24*365)
)


# 4) Decomposição MSTL
bike_mstl <- mstl(bike_msts, iterate = 2)

plot(
  bike_mstl,
  main = "Decomposição MSTL - Dados Diários do Bike Sharing"
)

# Ajusta TBATS na série com múltiplas sazonalidades
mod_tbats <- tbats(bike_msts, num.cores = NULL, use.parallel = TRUE)

mod_tbats   # para ver os parâmetros do modelo

# Previsão de 30 dias à frente
prev_tbats <- forecast(mod_tbats, h = 30)

autoplot(prev_tbats) +
  labs(title = "Previsão TBATS - Contagem diária de bicicletas",
       x = "Data",
       y = "Total de bikes (cnt)") +
  theme_minimal()

#################### regressao ARIMAX ########################
bike_reg <- bike %>%
  mutate(
    lag_temp = lag(temp, 1),
    lag_hum  = lag(hum, 1),
    lag_wind = lag(wind, 1)
  ) %>%
  drop_na()   # remove primeiras linhas que viram NA por causa do lag

cnt_ts <- ts(bike_reg$cnt, frequency = 7)

xreg <- cbind(
  lag_temp = bike_reg$lag_temp,
  lag_hum  = bike_reg$lag_hum,
  lag_wind = bike_reg$lag_wind
)

# auto.arima escolhe a parte ARIMA, você fornece as regressoras
fit_arimax <- auto.arima(
  y    = cnt_ts,
  xreg = xreg
)

summary(fit_arimax)   # coeficientes, significância, ordem ARIMA etc.

checkresiduals(fit_arimax)

# pegar as últimas 30 linhas de covariáveis como "cenário"
future_xreg <- tail(xreg, 30)

prev_dyn <- forecast(
  fit_arimax,
  xreg = future_xreg
)

autoplot(prev_dyn) +
  labs(title = "Previsão ARIMAX - Regressão dinâmica diária",
       x = "Passos à frente",
       y = "Total de bikes (cnt)") +
  theme_minimal()
#################################################################################
# 1) Importação e Tratamento
caminho <- "hour.csv"

bike_hour <- read.csv(caminho) %>%
  as_tibble() %>%
  mutate(
    dteday = ymd(dteday),
    # Cria indice temporal ordenado
    datetime = ymd_h(paste(dteday, hr)) 
  ) %>%
  arrange(datetime)

# 2) Criação do objeto msts (Multi-Seasonal Time Series)
# DICA: Removemos 24*30 e 24*365 para o TBATS não travar seu PC.
# O TBATS lida bem com a mudança lenta da tendência anual sem precisar forçar o ciclo.
bike_msts <- msts(
  bike_hour$cnt,
  seasonal.periods = c(24, 7*24, 30*24),
  start = c(2011, 1) 
)

# Decomposição MSTL
bike_mstl <- mstl(bike_msts, iterate = 4)

plot(
  bike_mstl,
  main = "Decomposição MSTL - Dados Diários do Bike Sharing"
)

# 3) Divisão Treino e Teste (Obrigatório para o relatório) 
# Vamos deixar as últimas 3 semanas (21 dias) para testar o modelo
h_teste <- 21 * 24 
treino  <- subset(bike_msts, end = length(bike_msts) - h_teste)
teste   <- subset(bike_msts, start = length(bike_msts) - h_teste + 1)

# 4) Ajuste TBATS (apenas no Treino)
# use.parallel = TRUE ajuda, mas ainda pode demorar uns minutos
mod_tbats <- tbats(treino, use.parallel = TRUE, use.box.cox = FALSE, num.cores = NULL) 

# 4) Previsão
previsao <- forecast(mod_tbats, h = h_teste) 

print(mod_tbats) # Verifique quais sazonalidades ele escolheu

# 5) Gráfico com Zoom
# Como a série é muito longa (2 anos), vamos plotar apenas o finalzinho (tail)
# para você conseguir ver a previsão comparada com o real.
autoplot(previsao, include = 400) + # include = 500 mostra apenas as últimas 500 obs do histórico
  autolayer(teste, series="Dados Reais", color="red") +
  labs(title = "Previsão TBATS (Estabilizada) - Últimos 21 dias",
       x = "Tempo", 
       y = "Total de Bikes") +
  theme_minimal()


# 5) Previsão e Gráfico

autoplot(previsao) +
  autolayer(teste, series="Dados Reais (Teste)", alpha=0.5) +
  labs(title = "Previsão TBATS vs Real - Bike Sharing",
       x = "Tempo (Horas)", y = "Total de Bikes") +
  theme_minimal()

# 6) Cálculo do Erro (Para a tabela de resultados do relatório)
acuracia <- accuracy(previsao, teste)
print(acuracia)
# Aqui você pega o RMSE e MAE para colocar no seu artigo.

###############################################################################

# 3) Criar os regressores de Fourier (Senos e Cossenos) para o TREINO
# O parâmetro K define a complexidade da onda. 
# K=c(10, 5) significa: 10 pares de ondas para o ciclo diário (24) 
# e 5 pares para o semanal (168).
# Você pode brincar com esses números. Se K for muito alto, overfitting.
K_params <- c(10, 7, 5) 

xreg_fourier_treino <- fourier(treino, K = K_params)

# 4) Ajustar o modelo (ARIMA com regressors externos)
# seasonal = FALSE é CRUCIAL aqui. Dizemos ao ARIMA: "Não procure sazonalidade,
# eu já te dei ela pronta nas colunas xreg (fourier)". Isso torna o ajuste muito mais rápido.
mod_harmonico <- auto.arima(treino, 
                            xreg = xreg_fourier_treino, 
                            seasonal = FALSE, 
                            stepwise = TRUE,    # Deixe TRUE para ser mais rápido
                            approximation = TRUE) # Deixe TRUE para ser mais rápido

print(mod_harmonico)

# 5) Previsão
# Precisamos criar os termos de Fourier FUTUROS para que o modelo possa projetar
xreg_fourier_futuro <- fourier(treino, K = K_params, h = h_teste)

# Faz a previsão passando os novos regressores
prev_harmonico <- forecast(mod_harmonico, xreg = xreg_fourier_futuro)

# 6) Gráfico e Avaliação
autoplot(prev_harmonico, include = 500) + # Zoom nas últimas 500 horas
  autolayer(teste, series="Dados Reais", color = "red", alpha=0.6) +
  labs(title = "Regressão Harmônica Dinâmica vs Real",
       subtitle = paste("K =", paste(K_params, collapse=", ")),
       x = "Tempo", y = "Total Bikes") +
  theme_minimal()

# Cálculo do erro para comparar com o TBATS
print(accuracy(prev_harmonico, teste))

################################################################################
# 1) Importar e Verificar Nomes
bike_hour <- read.csv("hour.csv") %>%
  as_tibble() %>%
  mutate(datetime = ymd_h(paste(dteday, hr))) %>%
  arrange(datetime)

# IMPRIME OS NOMES PARA VOCÊ CONFERIR
print("As colunas do seu dataset são:")
print(colnames(bike_hour)) 

# 2) Preparação (Ajuste aqui se os nomes impressos forem diferentes)
bike_reg <- bike_hour %>%
  mutate(
    # Se sua coluna chamar 'humidity', mude 'hum' para 'humidity' abaixo
    lag_temp = lag(temp, 1),      
    lag_hum  = lag(hum, 1),       
    lag_wind = lag(windspeed, 1)  # CORREÇÃO: geralmente é 'windspeed'
  ) %>%
  drop_na() # Remove a primeira linha que ficou NA pelo lag

# 3) Define Série Temporal (MSTS)
# Importante: start c(2011, 1) assume que o dado começa em 1o Jan 2011
y_msts <- msts(bike_reg$cnt, seasonal.periods = c(24, 168), start = c(2011, 1))

# 4) Matriz de Covariáveis (XREG)
matriz_xreg <- cbind(
  Temp = bike_reg$lag_temp,
  Hum  = bike_reg$lag_hum,
  Wind = bike_reg$lag_wind
)

# 5) Divisão Treino/Teste
h_teste <- 21 * 24 
n_total <- length(y_msts)
n_treino <- n_total - h_teste

y_treino <- subset(y_msts, end = n_treino)
y_teste  <- subset(y_msts, start = n_treino + 1)

xreg_treino <- matriz_xreg[1:n_treino, ]
xreg_teste  <- matriz_xreg[(n_treino + 1):n_total, ]

# 6) Fourier + Arima
K_params <- c(10, 5) # 10 harmônicos para dia, 5 para semana
fourier_treino <- fourier(y_treino, K = K_params)
xreg_completo_treino <- cbind(xreg_treino, fourier_treino)

# Ajuste do Modelo
fit_final <- auto.arima(y_treino, xreg = xreg_completo_treino, seasonal = FALSE)
print(summary(fit_final))

# 7) Previsão
fourier_teste <- fourier(y_treino, K = K_params, h = h_teste)
xreg_completo_teste <- cbind(xreg_teste, fourier_teste)

previsao <- forecast(fit_final, xreg = xreg_completo_teste)

# 8) Gráfico Final
autoplot(previsao, include = 500) +
  autolayer(y_teste, series="Real", color="red", alpha=0.5) +
  labs(title = "Regressão Harmônica com Covariáveis", y = "Bicicletas")

print(accuracy(previsao, y_teste))









