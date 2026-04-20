# =============================================================================
# global.R
#
# O que faz:
#   - Carrega pacotes
#   - Lê os dados processados
#   - Define variáveis globais usadas pelo ui.R e server.R
# =============================================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)

# -----------------------------------------------------------------------------
# Lê os dados
# Lê direto do GitHub para o painel estar sempre atualizado
# -----------------------------------------------------------------------------

URL_DADOS <- paste0(
  "https://raw.githubusercontent.com/Brunoavfer/",
  "sihsus-icsap-bh/main/data/processed/icsap_bh_regional.csv"
)

dados <- tryCatch(
  read_csv(URL_DADOS, show_col_types = FALSE),
  error = function(e) {
    message("Erro ao carregar dados online. Tentando local...")
    read_csv("../data/processed/icsap_bh_regional.csv", 
             show_col_types = FALSE)
  }
)

# -----------------------------------------------------------------------------
# Variáveis globais para os filtros do painel
# -----------------------------------------------------------------------------

REGIONAIS <- c(
  "Todas",
  "Barreiro",
  "Centro-Sul",
  "Leste",
  "Nordeste",
  "Noroeste",
  "Norte",
  "Oeste",
  "Pampulha",
  "Venda Nova"
)

ANOS <- dados %>%
  pull(ano_cmpt) %>%
  unique() %>%
  sort()

GRUPOS_ICSAP <- dados %>%
  filter(!is.na(descricao)) %>%
  pull(descricao) %>%
  unique() %>%
  sort()
