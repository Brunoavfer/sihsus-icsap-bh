# =============================================================================
# global.R
#
# O que faz:
#   - Carrega pacotes
#   - Lê os dados processados e os polígonos da PBH
#   - Define variáveis globais usadas pelo ui.R e server.R
# =============================================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(stringr)

# -----------------------------------------------------------------------------
# Lê os dados
# -----------------------------------------------------------------------------

# Tenta carregar dados com regional (após rodar 03_cep_regional.R)
# Se não existir, usa dados sem regional para desenvolvimento
arquivo_dados <- if (file.exists("../data/processed/icsap_bh_regional.csv")) {
  "../data/processed/icsap_bh_regional.csv"
} else {
  "../data/processed/icsap_bh.csv"
}

dados <- tryCatch(
  read_csv(arquivo_dados, show_col_types = FALSE),
  error = function(e) {
    # Fallback: lê direto do GitHub
    read_csv(
      paste0(
        "https://raw.githubusercontent.com/Brunoavfer/",
        "sihsus-icsap-bh/main/data/processed/icsap_bh.csv"
      ),
      show_col_types = FALSE
    )
  }
)

# Garante que colunas regional e nome_cs existam
if (!"regional" %in% names(dados)) dados$regional <- NA_character_
if (!"nome_cs"  %in% names(dados)) dados$nome_cs  <- NA_character_

# -----------------------------------------------------------------------------
# Lê polígonos de área de abrangência dos Centros de Saúde
# -----------------------------------------------------------------------------

arquivo_geo <- if (file.exists("../data/ref/areas_abrangencia_cs.geojson")) {
  "../data/ref/areas_abrangencia_cs.geojson"
} else {
  "data/ref/areas_abrangencia_cs.geojson"
}

poligonos_cs <- tryCatch(
  st_read(arquivo_geo, quiet = TRUE),
  error = function(e) NULL
)

# -----------------------------------------------------------------------------
# Variáveis globais para os filtros
# -----------------------------------------------------------------------------

REGIONAIS <- c("Todas", sort(unique(na.omit(dados$regional))))

ANOS <- dados %>%
  pull(ano_cmpt) %>%
  unique() %>%
  sort()

CONDICOES <- dados %>%
  filter(!is.na(descricao)) %>%
  pull(descricao) %>%
  unique() %>%
  sort()

CENTROS_SAUDE <- c("Todos", sort(unique(na.omit(dados$nome_cs))))
