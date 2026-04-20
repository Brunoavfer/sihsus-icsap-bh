# =============================================================================
# 02_process.R
#
# O que faz:
#   - Lê os arquivos .dbc baixados pelo 01_download.R
#   - Filtra apenas internações de residentes em BH
#   - Cruza com a lista ICSAP (data/ref/lista_icsap.csv)
#   - Salva o resultado tratado em data/processed/
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

cids_icsap <- icsap$cid  # vetor com todos os CIDs ICSAP

message("Lista ICSAP carregada: ", length(cids_icsap), " CIDs")

# -----------------------------------------------------------------------------
# Função: lê um arquivo .dbc e filtra BH + ICSAP
# -----------------------------------------------------------------------------

processar_arquivo <- function(arquivo) {
  
  message("Processando: ", basename(arquivo))
  
  # Lê o arquivo .dbc
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
  
  dados %>%
    # Filtra apenas residentes de BH
    filter(munic_res == COD_BH) %>%
    
    # Extrai os 3 primeiros caracteres do CID (ex: "I10.0" → "I10")
    mutate(cid3 = str_sub(diag_princ, 1, 3)) %>%
    
    # Filtra apenas internações ICSAP
    filter(cid3 %in% cids_icsap) %>%
    
    # Seleciona e renomeia as colunas mais importantes
    select(
      ano_cmpt,          # Ano de competência
      mes_cmpt,          # Mês de competência
      cep,               # CEP do paciente
      munic_res,         # Município de residência
      diag_princ,        # Diagnóstico principal (CID)
      cid3,              # CID com 3 caracteres
      idade,             # Idade do paciente
      sexo,              # Sexo
      dias_perm,         # Dias de permanência
      val_tot            # Valor total da internação
    ) %>%
    
    # Cria coluna de data
    mutate(
      data_internacao = make_date(
        year  = as.integer(ano_cmpt),
        month = as.integer(mes_cmpt),
        day   = 1L
      )
    ) %>%
    
    # Junta com a descrição da lista ICSAP
    left_join(
      icsap %>% select(cid, grupo, subgrupo, descricao),
      by = c("cid3" = "cid")
    )
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

# Processa e empilha todos os arquivos
dados_completos <- arquivos %>%
  lapply(processar_arquivo) %>%
  bind_rows()

message("Total de internações ICSAP em BH: ", nrow(dados_completos))

# -----------------------------------------------------------------------------
# Salva o resultado
# -----------------------------------------------------------------------------

# Salva em CSV para uso no painel Shiny
write_csv(
  dados_completos,
  file.path(DIR_PROC, "icsap_bh.csv")
)

message("Dados salvos em: ", file.path(DIR_PROC, "icsap_bh.csv"))
