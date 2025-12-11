# ==============================================================================
# PACOTES 
# ==============================================================================
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, forecast, lubridate, ggplot2, gridExtra, tseries)

# ==============================================================================
# TRATAMENTO
# ==============================================================================
caminho <- "hour.csv" 

raw_data <- read.csv(caminho) %>%
  as_tibble() %>%
  mutate(
    dteday = ymd(dteday),
    datetime = ymd_h(paste(dteday, hr))
  ) %>%
  arrange(datetime)

# Lags para Regressão Dinâmica
bike_df <- raw_data %>%
  mutate(
    lag_temp = lag(temp, 1),
    lag_hum  = lag(hum, 1),
    lag_wind = lag(windspeed, 1) 
  ) %>%
  drop_na()

# MSTS 
y_msts <- msts(bike_df$cnt, seasonal.periods = c(24, 7*24), start = c(2011, 1))

# Covariáveis
xreg_clima <- cbind(
  Temp = bike_df$lag_temp,
  Hum  = bike_df$lag_hum,
  Wind = bike_df$lag_wind
)

# ==============================================================================
# 3. Gráfico Exploratorio
# ==============================================================================

# 1. Gráfico da Série Completa (2011-2012)
g_completo <- ggplot(bike_df, aes(x = datetime, y = cnt)) +
  geom_line(color = "#2c3e50", size = 0.3, alpha = 0.7) +
  labs(title = "A. Série Histórica Completa (2011-2012)",
       subtitle = "Visualização da Tendência de Alta e Sazonalidade Anual",
       x = NULL, y = "Aluguéis / Hora") +
  theme_minimal()

# 2. Gráfico Recorte 
zoom_inicio <- ymd_h("2012-05-06 00") 
zoom_fim    <- ymd_h("2012-05-20 23") 

df_zoom <- bike_df %>%
  filter(datetime >= zoom_inicio & datetime <= zoom_fim)

g_zoom <- ggplot(df_zoom, aes(x = datetime, y = cnt)) +
  geom_line(color = "#e67e22", size = 0.8) +
  geom_point(size = 0.9, alpha = 0.4, color = "#d35400") + 
  labs(title = "B. Zoom: Detalhe do Comportamento Horário (2 Semanas)",
       subtitle = "Ciclos Diários (Picos de Trabalho) e Diferença Dia Útil vs Fim de Semana",
       x = "Data", y = "Aluguéis / Hora") +
  scale_x_datetime(date_labels = "%a %d/%m", date_breaks = "1 day") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g_completo, g_zoom, ncol = 1, heights = c(1, 1.2))

# ==============================================================================
# Decomposição
# ==============================================================================

decomp <- mstl(y_msts, iterate = 6, s.window = 5)

plot_decomp <- autoplot(decomp) +
  labs(title = "Decomposição MSTL: Demanda de Bicicletas",
       subtitle = "Componentes: Dados, Tendência, Sazonalidade Diária (24h) e Semanal (168h)",
       x = "Tempo (Anos)",
       y = "Componentes") +
  theme_minimal()

print(plot_decomp)

# ==============================================================================
# TREINO E TESTE
# ==============================================================================

h_teste  <- 21 * 24 
n_total  <- length(y_msts)
n_treino <- n_total - h_teste

y_treino <- subset(y_msts, end = n_treino)
y_teste  <- subset(y_msts, start = n_treino + 1)

xreg_clima_treino <- xreg_clima[1:n_treino, ]
xreg_clima_teste  <- xreg_clima[(n_treino + 1):n_total, ]

# ==============================================================================
# MODELAGEM
# ==============================================================================

# TBATS 
print("Treinando TBATS...")
fit_tbats <- tbats(y_treino, use.box.cox = FALSE, use.parallel = TRUE)
prev_tbats <- forecast(fit_tbats, h = h_teste, level = 95) 

# FOURIER 
K_params <- c(10, 5) 
fourier_treino <- fourier(y_treino, K = K_params)
fourier_teste  <- fourier(y_treino, K = K_params, h = h_teste)

# HARMÔNICA 
print("Treinando Harmônica...")
fit_harm <- auto.arima(y_treino, xreg = fourier_treino, seasonal = FALSE, 
                       stepwise = FALSE, approximation = TRUE)
prev_harm <- forecast(fit_harm, xreg = fourier_teste, level = 95)

# DINÂMICA 
print("Treinando Dinâmica...")
xreg_final_treino <- cbind(xreg_clima_treino, fourier_treino)
xreg_final_teste  <- cbind(xreg_clima_teste,  fourier_teste)

colnames(xreg_final_treino) <- paste0("Reg", 1:ncol(xreg_final_treino))
colnames(xreg_final_teste)  <- paste0("Reg", 1:ncol(xreg_final_teste))

fit_dyn <- auto.arima(y_treino, xreg = xreg_final_treino, seasonal = FALSE,
                      stepwise = FALSE, approximation = TRUE)
prev_dyn <- forecast(fit_dyn, xreg = xreg_final_teste, level = 95)

# ==============================================================================
# 5. ANÁLISE DE RESÍDUOS E TESTES 
# ==============================================================================

diagnostico_residuos <- function(modelo, nome) {
  cat(paste0("\n>>> DIAGNÓSTICO DE RESÍDUOS: ", nome, " <<<\n"))
  
  # Gráficos 
  checkresiduals(modelo) 
  
  # Extrair resíduos
  res <- residuals(modelo)
  
  # jung-Box 
  lb_test <- Box.test(res, type = "Ljung-Box", lag = 24) #ciclo diário
  print(lb_test)
  
  # normalidade 
  jb_test <- jarque.bera.test(res) 
  print(jb_test)
}

diagnostico_residuos(fit_tbats, "1. TBATS")
diagnostico_residuos(fit_harm,  "2. Harmônica")
diagnostico_residuos(fit_dyn,   "3. Dinâmica")

# ==============================================================================
# DESEMPENHO 
# ==============================================================================

# Função para métricas 
get_full_metrics <- function(previsao, real, nome_modelo) {
  
  # Métricas Pontuais
  acc <- accuracy(previsao, real)
  
  # Lower e Upper bounds
  lower <- previsao$lower[, "95%"]
  upper <- previsao$upper[, "95%"]
  
  # Cobertura
  is_covered <- (real >= lower) & (real <= upper)
  coverage_pct <- mean(is_covered) * 100
  
  data.frame(
    Modelo = nome_modelo,
    RMSE   = round(acc[2, "RMSE"], 2),
    MAE    = round(acc[2, "MAE"], 2),
    MAPE   = round(acc[2, "MAPE"], 2),
    Cobertura_95 = round(coverage_pct, 2)
  )
}

resultados <- rbind(
  get_full_metrics(prev_tbats, y_teste, "1. TBATS"),
  get_full_metrics(prev_harm,  y_teste, "2. Harmônica"),
  get_full_metrics(prev_dyn,   y_teste, "3. Dinâmica")
)

print("--- RESULTADOS FINAIS (Pontual e Intervalar) ---")
print(resultados)

# ==============================================================================
# GRÁFICO
# ==============================================================================

# Recuperar datas originais
datas_teste <- tail(bike_df$datetime, h_teste)

# Montar Dataframe 
df_grafico_final <- data.frame(
  Data = datas_teste,
  Real = as.numeric(y_teste),
  TBATS = as.numeric(prev_tbats$mean),
  Harmonica = as.numeric(prev_harm$mean),
  Dinamica = as.numeric(prev_dyn$mean),
  Lower_Dyn = as.numeric(prev_dyn$lower[, "95%"]),
  Upper_Dyn = as.numeric(prev_dyn$upper[, "95%"])
)

# 3. Plotagem
ggplot(df_grafico_final, aes(x = Data)) +
  
  # IC
  geom_ribbon(aes(ymin = Lower_Dyn, ymax = Upper_Dyn, fill = "IC 95% (Dinâmica)"), 
              alpha = 0.15) +
  # Dados Reais (Preto)
  geom_line(aes(y = Real, color = "0. Real"), size = 0.8, alpha = 1) +
  geom_line(aes(y = TBATS, color = "1. TBATS"), size = 0.6, alpha = 0.7) +
  geom_line(aes(y = Harmonica, color = "2. Harmônica"), size = 0.6, alpha = 0.7) +
  geom_line(aes(y = Dinamica, color = "3. Dinâmica"), size = 0.6) +
  scale_x_datetime(
    date_labels = "%d/%m %Hh", 
    date_breaks = "2 days",      # Mostra datas a cada 2 dias
    minor_breaks = "12 hours"    # Marcações menores a cada 12h
  ) +
  scale_color_manual(name = "Séries", values = c(
    "0. Real" = "black", 
    "1. TBATS" = "#E41A1C",       
    "2. Harmônica" = "#377EB8",   
    "3. Dinâmica" = "#4DAF4A"     
  )) +
  
  scale_fill_manual(name = "Incerteza", values = c(
    "IC 95% (Dinâmica)" = "green"
  )) +
  labs(
    title = "Comparação Final: TBATS vs Harmônica vs Dinâmica",
    subtitle = "A faixa verde representa o Intervalo de Confiança (95%) do modelo Dinâmico",
    y = "Demanda de Bicicletas",
    x = "Data e Hora"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ==============================================================================
# 8. GRÁFICOS INDIVIDUAIS 
# ==============================================================================

datas_teste <- tail(bike_df$datetime, h_teste)

# Função auxiliar para criar o gráfico 
plot_modelo_individual <- function(dados_reais, previsao_obj, nome_modelo, cor_linha) {
  
  # Monta dataframe 
  df_temp <- data.frame(
    Data = datas_teste,
    Real = as.numeric(dados_reais),
    Predito = as.numeric(previsao_obj$mean),
    Lower = as.numeric(previsao_obj$lower[, "95%"]),
    Upper = as.numeric(previsao_obj$upper[, "95%"])
  )
  
  # Cria o plot
  ggplot(df_temp, aes(x = Data)) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), fill = cor_linha, alpha = 0.15) +
    # Linha Real
    geom_line(aes(y = Real), color = "black", alpha = 0.6, size = 0.4) +
    # Linha do Modelo
    geom_line(aes(y = Predito), color = cor_linha, size = 0.8) +
    labs(title = paste("Modelo:", nome_modelo),
         subtitle = "Preto: Real | Colorido: Previsão (com IC 95%)",
         y = "Bikes", x = NULL) +
    scale_x_datetime(date_labels = "%d/%m", date_breaks = "3 days") +
    theme_minimal()
}

# 2. Gerar os 3 gráficos
g1 <- plot_modelo_individual(y_teste, prev_tbats, "1. TBATS", "#E41A1C") # Vermelho
g2 <- plot_modelo_individual(y_teste, prev_harm,  "2. Harmônica", "#377EB8") # Azul
g3 <- plot_modelo_individual(y_teste, prev_dyn,   "3. Dinâmica", "#4DAF4A") # Verde

# 3. Exibir juntos (Painel)
grid.arrange(g1, g2, g3, ncol = 1)
