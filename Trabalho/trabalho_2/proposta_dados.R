pacman::p_load(tidyverse, lubridate, rbcb, fpp3, jsonlite, tidyquant, tsibble, purrr, forecast)


# Ticker do USD/BRL no Yahoo: "BRL=X"
dolar <- tq_get("BRL=X",
                from = "2010-01-01",
                to   = Sys.Date()) %>%
  select(data = date, dolar = adjusted) %>%   # pode usar close ou adjusted
  arrange(data)

head(dolar)

# Ticker do Brent: BZ=F
brent <- tq_get("BZ=F",
                from = "2010-01-01",
                to   = Sys.Date()) %>%
  select(data = date, brent = adjusted) %>%
  arrange(data)

head(brent)


# Dados gasolina 2010-2025.1
gasolina_completa <- readRDS("gasolina_completa.rds")

# Gasolina já semanal (ANP)
gasolina_formatada <- gasolina_completa %>%
  rename(data = data_coleta)

# Dólar semanal
dolar_formatada <- dolar %>%
  filter(data <= as.Date("2025-06-30")) 

# Brent semanal
brent_formatada <- brent %>%
  filter(data <= as.Date("2025-06-30")) 

base_final <- gasolina_formatada %>%
  full_join(dolar_formatada, by = "data") %>%
  full_join(brent_formatada, by = "data") %>%
  mutate(
    brent = na.locf(brent, na.rm = FALSE),
    dolar = na.locf(dolar, fromLast = TRUE) # se sobrar NA no começo
  ) %>%
  arrange(data)


base_long <- base_final %>%
  select(data,
         gasolina = preco_medio,
         dolar,
         brent) %>%
  pivot_longer(
    cols      = -data,
    names_to  = "serie",
    values_to = "valor"
  )

ggplot(base_long, aes(x = data, y = valor)) +
  geom_line() +
  facet_wrap(~ serie, scales = "free_y", ncol = 1) +
  labs(x = "Data", y = "Valor", title = "Séries: Gasolina, Dólar e Brent") +
  theme_minimal()



# Gasolina
gas_ts <- msts(
  base_final$preco_medio,
  seasonal.periods = c(7, 30, 365)
)
gas_mstl <- mstl(gas_ts, iterate = 2)
plot(gas_mstl, main = "Decomposição MSTL - Gasolina")

# Dólar
dol_ts <- msts(base_final$dolar, seasonal.periods = c(7, 30, 365))
dol_mstl <- mstl(dol_ts, iterate = 2)
plot(dol_mstl, main = "Decomposição MSTL - Dólar")

# Brent
brent_ts <- msts(base_final$brent, seasonal.periods = c(7, 30, 365))
brent_mstl <- mstl(brent_ts, iterate = 2)
plot(brent_mstl, main = "Decomposição MSTL - Brent")





















