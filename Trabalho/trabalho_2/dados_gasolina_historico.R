pacman::p_load(tidyverse, lubridate, rbcb, fpp3, jsonlite, tidyquant, tsibble, purr, arrow)


# 1) PASTA ONDE ESTÃO OS CSVs DA ANP
#    >>> troque pelo caminho da SUA máquina <<<
pasta <- "historico_gasolina_2010-2025-1"

# 2) Lista todos os arquivos tipo "ca-XXXX-YY.csv"
arquivos <- list.files(
  path       = pasta,
  pattern    = "^ca-.*\\.csv$",  # pega só os "ca-..."
  full.names = TRUE
)

length(arquivos)   # só pra conferir quantos arquivos foram encontrados

# 3) Função que lê e padroniza UM arquivo
ler_ca <- function(arq) {
  read.csv(arq,
           sep = ";",
           dec = ",",
           encoding = "Latin1") |>
    as_tibble() |>
    # alguns arquivos vêm com BOM no nome da primeira coluna,
    # então uso matches() pra encontrar pelo texto
    rename(
      regiao       = matches("Regiao"),
      estado       = matches("Estado"),
      municipio    = matches("Municipio"),
      revenda      = matches("Revenda"),
      cnpj_revenda = matches("CNPJ"),
      produto      = matches("^Produto$"),
      data_coleta  = matches("Data.da.Coleta"),
      valor_venda  = matches("Valor.de.Venda"),
      valor_compra = matches("Valor.de.Compra"),
      unidade      = matches("Unidade.de.Medida"),
      bandeira     = matches("Bandeira")
    ) |>
    mutate(
      data_coleta  = dmy(data_coleta),
      valor_venda  = as.numeric(gsub(",", ".", valor_venda)),
      valor_compra = as.numeric(gsub(",", ".", valor_compra)),
      arquivo      = basename(arq)           # de qual semestre veio
    )
}

# 4) Ler TODOS os arquivos e empilhar
gasolina_br <- map_dfr(arquivos, ler_ca)

# 5) Se quiser, ficar só com gasolina comum
gasolina_comum <- gasolina_br |>
  filter(str_detect(toupper(produto), "GASOLINA")) |>
  arrange(data_coleta)

# Ver resultado
gasolina_br
# ou
gasolina_comum

#Formatando

gasolina_nacional <- gasolina_br %>%
  # garante que a coluna é Date e tira datas NA
  mutate(data_coleta = as.Date(data_coleta)) %>%
  filter(!is.na(data_coleta)) %>%
  filter(str_detect(toupper(produto), "GASOLINA")) %>%
  group_by(data_coleta) %>%
  summarise(
    preco_medio = mean(valor_venda, na.rm = TRUE),
    n           = n(),
    .groups     = "drop"
  ) %>%
  arrange(data_coleta)

# gera sequência diária completa com base no mínimo e máximo da ANP
datas_completas <- tibble(
  data_coleta = seq.Date(
    from = min(gasolina_nacional$data_coleta, na.rm = TRUE),
    to   = max(gasolina_nacional$data_coleta, na.rm = TRUE),
    by   = "day"
  )
)


# juntar para ver onde faltam preços
checagem <- datas_completas %>%
  left_join(gasolina_nacional, by = "data_coleta")

sum(is.na(checagem$preco_medio))



gasolina_completa <- datas_completas %>%
  left_join(gasolina_nacional, by = "data_coleta") %>%
  arrange(data_coleta) %>%
  mutate(
    preco_medio = na.locf(preco_medio, na.rm = FALSE),
    preco_medio = na.locf(preco_medio, fromLast = TRUE) # se sobrar NA no começo
  )


write_parquet(gasolina_completa, "gasolina_completa.parquet")

saveRDS(gasolina_completa, "gasolina_completa.rds")
