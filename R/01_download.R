# =============================================================================
# 01_download.R
# Download dos dados SIHSUS para Belo Horizonte
# Fonte: FTP DATASUS - ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/
# =============================================================================

# Pacotes necessários
# install.packages(c("read.dbc", "dplyr", "stringr"))
library(read.dbc)
library(dplyr)
library(stringr)

# -----------------------------------------------------------------------------
# Configurações
# -----------------------------------------------------------------------------

# Código IBGE de Belo Horizonte
COD_BH <- "310620"

# Período desejado (ano e mês)
ANO_INICIO  <- 2025
ANO_FIM     <- 2025
MESES       <- str_pad(1:12, 2, pad = "0")  # "01" a "12"

# Pasta de destino
DIR_RAW <- "data/raw"
if (!dir.exists(DIR_RAW)) dir.create(DIR_RAW, recursive = TRUE)

# -----------------------------------------------------------------------------
# Função de download
# -----------------------------------------------------------------------------

baixar_sih <- function(ano, mes) {
  
  # Nome do arquivo no FTP
  # Exemplo: RDMG2301.dbc = Reduzida, MG, ano 23, mês 01
  ano_curto <- str_sub(as.character(ano), 3, 4)
  arquivo   <- paste0("RDMG", ano_curto, mes, ".dbc")
  url       <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/", arquivo)
  destino   <- file.path(DIR_RAW, arquivo)
  
  # Pula se já baixado
  if (file.exists(destino)) {
    message("Já existe: ", arquivo)
    return(invisible(NULL))
  }
  
  message("Baixando: ", arquivo)
  tryCatch(
    download.file(url, destino, mode = "wb", quiet = TRUE),
    error = function(e) message("Erro ao baixar ", arquivo, ": ", e$message)
  )
}

# -----------------------------------------------------------------------------
# Loop de download
# -----------------------------------------------------------------------------

for (ano in ANO_INICIO:ANO_FIM) {
  for (mes in MESES) {
    baixar_sih(ano, mes)
  }
}

message("Download concluído! Arquivos em: ", DIR_RAW)
