# =============================================================================
# 03_cep_regional.R
#
# O que faz:
#   - Lê os dados processados (icsap_bh.csv)
#   - Para cada CEP único, consulta a API de CEP do governo
#   - Identifica o bairro e cruza com a tabela bairro → regional de BH
#   - Salva o resultado final com a coluna "regional"
#
# Referência API: https://viacep.com.br (alternativa mais estável)
# Regionais de BH: Barreiro, Centro-Sul, Leste, Nordeste, Noroeste,
#                  Norte, Oeste, Pampulha, Venda Nova
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(httr)
library(jsonlite)

# -----------------------------------------------------------------------------
# Configurações
# -----------------------------------------------------------------------------

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# -----------------------------------------------------------------------------
# Tabela de bairros → regionais de BH
# Fonte: Portal da PBH
# -----------------------------------------------------------------------------

# Carrega a tabela de referência (vamos criar esse arquivo a seguir)
bairro_regional <- read_csv(
  file.path(DIR_REF, "bairros_regional_bh.csv"),
  show_col_types = FALSE
)

# -----------------------------------------------------------------------------
# Função: consulta CEP na API ViaCEP
# -----------------------------------------------------------------------------

consultar_cep <- function(cep) {
  
  # Limpa o CEP (remove traços e espaços)
  cep_limpo <- str_replace_all(cep, "[^0-9]", "")
  
  # CEP deve ter 8 dígitos
  if (nchar(cep_limpo) != 8) return(NA_character_)
  
  url <- paste0("https://viacep.com.br/ws/", cep_limpo, "/json/")
  
  resposta <- tryCatch(
    GET(url, timeout(10)),
    error = function(e) return(NULL)
  )
  
  if (is.null(resposta) || status_code(resposta) != 200) return(NA_character_)
  
  conteudo <- fromJSON(content(resposta, "text", encoding = "UTF-8"))
  
  # Retorna o bairro se existir
  if (!is.null(conteudo$bairro) && conteudo$bairro != "") {
    return(conteudo$bairro)
  }
  
  return(NA_character_)
}

# -----------------------------------------------------------------------------
# Carrega os dados processados
# -----------------------------------------------------------------------------

dados <- read_csv(
  file.path(DIR_PROC, "icsap_bh.csv"),
  show_col_types = FALSE
)

message("Total de registros: ", nrow(dados))

# -----------------------------------------------------------------------------
# Consulta CEPs únicos (evita repetir consultas)
# -----------------------------------------------------------------------------

ceps_unicos <- dados %>%
  filter(!is.na(cep), cep != "") %>%
  distinct(cep) %>%
  pull(cep)

message("CEPs únicos para consultar: ", length(ceps_unicos))

# Consulta cada CEP único com pausa de 0.3s para não sobrecarregar a API
cache_cep <- tibble(
  cep    = ceps_unicos,
  bairro = NA_character_
)

for (i in seq_along(ceps_unicos)) {
  if (i %% 100 == 0) message("Consultando CEP ", i, " de ", length(ceps_unicos))
  cache_cep$bairro[i] <- consultar_cep(ceps_unicos[i])
  Sys.sleep(0.3)  # pausa para respeitar o limite da API
}

# -----------------------------------------------------------------------------
# Cruza bairro com regional
# -----------------------------------------------------------------------------

cache_cep <- cache_cep %>%
  # Padroniza o nome do bairro para minúsculo sem acento
  mutate(
    bairro_norm = bairro %>%
      tolower() %>%
      str_trim() %>%
      iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
  ) %>%
  left_join(
    bairro_regional %>%
      mutate(
        bairro_norm = bairro %>%
          tolower() %>%
          str_trim() %>%
          iconv(from = "UTF-8", to = "ASCII//TRANSLIT")
      ),
    by = "bairro_norm"
  ) %>%
  select(cep, bairro, regional)

# -----------------------------------------------------------------------------
# Junta com os dados principais
# -----------------------------------------------------------------------------

dados_final <- dados %>%
  left_join(cache_cep, by = "cep")

# Resumo de cobertura
cobertura <- dados_final %>%
  summarise(
    total        = n(),
    com_regional = sum(!is.na(regional)),
    pct          = round(com_regional / total * 100, 1)
  )

message("Cobertura de regional: ", cobertura$com_regional, 
        " de ", cobertura$total, 
        " registros (", cobertura$pct, "%)")

# -----------------------------------------------------------------------------
# Salva o resultado final
# -----------------------------------------------------------------------------

write_csv(
  dados_final,
  file.path(DIR_PROC, "icsap_bh_regional.csv")
)

message("Arquivo final salvo: data/processed/icsap_bh_regional.csv")
