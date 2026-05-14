# =============================================================================
# global.R
#
# O que faz:
#   - Carrega pacotes
#   - Lê os dados processados e os polígonos da PBH
#   - Define variáveis globais usadas pelo ui.R e server.R
#
# Estratégia de carregamento:
#   Tenta path local (desenvolvimento) → fallback GitHub raw (shinyapps.io)
# =============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(stringr)

GITHUB_RAW <- "https://raw.githubusercontent.com/Brunoavfer/sihsus-icsap-bh/main"

# -----------------------------------------------------------------------------
# Dados ICSAP com regional e CS (produto final do pipeline)
# -----------------------------------------------------------------------------

dados <- tryCatch(
  read_csv("../data/processed/icsap_bh_regional.csv", show_col_types = FALSE),
  error = function(e) tryCatch(
    read_csv(
      paste0(GITHUB_RAW, "/data/processed/icsap_bh_regional.csv"),
      show_col_types = FALSE
    ),
    error = function(e2) read_csv(
      paste0(GITHUB_RAW, "/data/processed/icsap_bh.csv"),
      show_col_types = FALSE
    )
  )
)

# -----------------------------------------------------------------------------
# Total de internações BH — denominador correto da taxa ICSAP
# -----------------------------------------------------------------------------

total_internacoes_ref <- tryCatch(
  read_csv("../data/processed/internacoes_bh.csv", show_col_types = FALSE) %>%
    select(ano_cmpt, mes_cmpt),
  error = function(e) tryCatch(
    read_csv(
      paste0(GITHUB_RAW, "/data/processed/internacoes_bh.csv"),
      show_col_types = FALSE
    ) %>% select(ano_cmpt, mes_cmpt),
    error = function(e2) NULL
  )
)

# -----------------------------------------------------------------------------
# Polígonos de área de abrangência dos Centros de Saúde
# -----------------------------------------------------------------------------

poligonos_cs <- tryCatch(
  st_read("../data/ref/areas_abrangencia_cs.geojson", quiet = TRUE),
  error = function(e) tryCatch(
    st_read(
      paste0(GITHUB_RAW, "/data/ref/areas_abrangencia_cs.geojson"),
      quiet = TRUE
    ),
    error = function(e2) NULL
  )
)

# -----------------------------------------------------------------------------
# Garante colunas regional e nome_cs
# -----------------------------------------------------------------------------

if (!"regional" %in% names(dados)) dados$regional <- NA_character_
if (!"nome_cs"  %in% names(dados)) dados$nome_cs  <- NA_character_

# -----------------------------------------------------------------------------
# Reconstrói data_internacao a partir de ano_cmpt + mes_cmpt
# (make_date() em 02_process.R lê ano_cmpt como fator e retorna o índice
#  do nível em vez do valor numérico, gerando ano 1 d.C.)
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    data_internacao = as.Date(paste(
      as.integer(as.character(ano_cmpt)),
      formatC(as.integer(as.character(mes_cmpt)), width = 2, flag = "0"),
      "01", sep = "-"
    ))
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
