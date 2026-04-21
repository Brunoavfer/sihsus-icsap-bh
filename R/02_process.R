# =============================================================================
# 02_process.R
#
# O que faz:
#   - Lê os arquivos .dbc baixados pelo 01_download.R
#   - Filtra internações de residentes EM BH E internados EM BH
#   - Cruza com a lista ICSAP (data/ref/lista_icsap.csv)
#   - Mantém o total de internações para cálculo da taxa ICSAP (%)
#   - Salva o resultado tratado em data/processed/
#
# Decisão metodológica:
#   São incluídas APENAS internações que satisfazem os dois critérios:
#   1. MUNIC_RES == 310620 → paciente RESIDE em Belo Horizonte
#   2. MUNIC_MOV == 310620 → internação OCORREU em Belo Horizonte
#   Isso garante que estamos avaliando a efetividade da APS de BH
#   para sua própria população, excluindo:
#   - Residentes de BH internados em outros municípios
#   - Residentes de outros municípios internados em BH
# =============================================================================

library(read.dbc)
library(dplyr)
library(stringr)
library(readr)
library(lubridate)

# -----------------------------------------------------------------------------
# Configurações
# -----------------------------------------------------------------------------

COD_BH    <- "310620"   # Código IBGE de Belo Horizonte
DIR_RAW   <- "data/raw"
DIR_PROC  <- "data/processed"
DIR_REF   <- "data/ref"

if (!dir.exists(DIR_PROC)) dir.create(DIR_PROC, recursive = TRUE)

# -----------------------------------------------------------------------------
# Carrega a lista ICSAP
# -----------------------------------------------------------------------------

icsap <- read_csv(
  file.path(DIR_REF, "lista_icsap.csv"),
  show_col_types = FALSE
)

cids_icsap <- icsap$cid

message("Lista ICSAP carregada: ", length(cids_icsap), " CIDs")

# -----------------------------------------------------------------------------
# Função: lê um arquivo .dbc e processa
# -----------------------------------------------------------------------------

processar_arquivo <- function(arquivo) {

  message("Processando: ", basename(arquivo))

  dados <- tryCatch(
    read.dbc(arquivo),
    error = function(e) {
      message("Erro ao ler ", basename(arquivo), ": ", e$message)
      return(NULL)
    }
  )

  if (is.null(dados)) return(NULL)

  # Padroniza nomes das colunas para minúsculo
  names(dados) <- tolower(names(dados))

  # -------------------------------------------------------------------------
  # FILTRO DUPLO — decisão metodológica central do projeto
  # Inclui apenas internações onde o paciente:
  #   1. Reside em BH (munic_res)
  #   2. Foi internado em BH (munic_mov)
  # -------------------------------------------------------------------------
  dados_bh <- dados %>%
    filter(
      munic_res == COD_BH,  # reside em BH
      munic_mov == COD_BH   # internado em BH
    )

  # Registra quantos registros foram filtrados
  message(
    "  Total no arquivo: ", nrow(dados),
    " | Residentes E internados em BH: ", nrow(dados_bh)
  )

  if (nrow(dados_bh) == 0) return(NULL)

  # -------------------------------------------------------------------------
  # Prepara base completa (denominador para cálculo da taxa ICSAP)
  # -------------------------------------------------------------------------
  total_internacoes <- dados_bh %>%
    mutate(
      cid3 = str_sub(diag_princ, 1, 3),
      data_internacao = make_date(
        year  = as.integer(ano_cmpt),
        month = as.integer(mes_cmpt),
        day   = 1L
      ),
      icsap = cid3 %in% cids_icsap  # TRUE se for ICSAP, FALSE se não for
    ) %>%
    select(
      ano_cmpt,
      mes_cmpt,
      data_internacao,
      cep,
      munic_res,
      munic_mov,
      diag_princ,
      cid3,
      icsap,           # flag indicando se é ICSAP ou não
      idade,
      sexo,
      dias_perm,
      val_tot
    ) %>%
    # Junta descrição ICSAP apenas para as internações ICSAP
    left_join(
      icsap %>% select(cid, grupo, subgrupo, descricao),
      by = c("cid3" = "cid")
    )

  return(total_internacoes)
}

# -----------------------------------------------------------------------------
# Processa todos os arquivos .dbc da pasta raw
# -----------------------------------------------------------------------------

arquivos <- list.files(DIR_RAW, pattern = "\\.dbc$", full.names = TRUE)

if (length(arquivos) == 0) {
  stop("Nenhum arquivo .dbc encontrado em ", DIR_RAW,
       ". Rode primeiro o script 01_download.R")
}

message("Arquivos encontrados: ", length(arquivos))

dados_completos <- arquivos %>%
  lapply(processar_arquivo) %>%
  bind_rows()

# -----------------------------------------------------------------------------
# Resumo e cálculo da taxa ICSAP
# -----------------------------------------------------------------------------

resumo <- dados_completos %>%
  summarise(
    total_internacoes = n(),
    total_icsap       = sum(icsap, na.rm = TRUE),
    taxa_icsap        = round(total_icsap / total_internacoes * 100, 1)
  )

message("======================================")
message("Total de internações em BH: ",    resumo$total_internacoes)
message("Total de internações ICSAP: ",    resumo$total_icsap)
message("Taxa ICSAP: ",                    resumo$taxa_icsap, "%")
message("======================================")

# -----------------------------------------------------------------------------
# Salva o resultado
# -----------------------------------------------------------------------------

# Base completa (todas as internações, com flag icsap)
write_csv(
  dados_completos,
  file.path(DIR_PROC, "internacoes_bh.csv")
)

# Base apenas ICSAP (para análises específicas)
write_csv(
  dados_completos %>% filter(icsap == TRUE),
  file.path(DIR_PROC, "icsap_bh.csv")
)

message("Arquivos salvos em: ", DIR_PROC)
message("  - internacoes_bh.csv  (todas as internações — denominador)")
message("  - icsap_bh.csv        (apenas ICSAP — numerador)")
