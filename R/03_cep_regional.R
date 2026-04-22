# =============================================================================
# 03_cep_regional.R
#
# O que faz:
#   - Busca automaticamente o arquivo mais recente de área de abrangência
#     da PBH via API do Portal de Dados Abertos
#   - Lê os dados processados (icsap_bh.csv)
#   - Para cada CEP único consulta o ViaCEP para obter o endereço
#   - Geocodifica o endereço via Nominatim (OpenStreetMap)
#   - Cruza as coordenadas com os polígonos oficiais da PBH
#   - Identifica o Centro de Saúde e a Regional de cada paciente
#   - Salva cache de CEPs para evitar consultas repetidas
#   - Gera log de CEPs não encontrados para análise de cobertura
#
# Fonte dos polígonos:
#   Portal de Dados Abertos da PBH — Área de Abrangência Saúde (SMSA)
#   https://dados.pbh.gov.br/dataset/area-de-abrangencia-saude
#   Licença: Creative Commons Attribution
#
# Decisão metodológica:
#   A identificação do Centro de Saúde e Regional é feita por
#   geoprocessamento — cruzamento das coordenadas do endereço do
#   paciente com os polígonos oficiais de área de abrangência da SMSA.
#   Isso garante maior precisão do que abordagens baseadas em bairro,
#   especialmente em áreas limítrofes entre regionais.
# =============================================================================

library(dplyr)
library(readr)
library(stringr)
library(httr)
library(jsonlite)
library(sf)

# -----------------------------------------------------------------------------
# Configurações
# -----------------------------------------------------------------------------

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# Arquivo de cache (evita reconsultar CEPs já processados)
CACHE_CEP <- file.path(DIR_REF, "cache_cep.csv")

# Arquivo de log de CEPs não encontrados
LOG_CEP   <- file.path(DIR_PROC, "ceps_nao_encontrados.csv")

# Pausa entre consultas para respeitar limites das APIs (segundos)
PAUSA_VIACEP    <- 0.3
PAUSA_NOMINATIM <- 1.0  # Nominatim exige mínimo 1s entre consultas

# -----------------------------------------------------------------------------
# Função: busca automaticamente o arquivo mais recente da PBH
# -----------------------------------------------------------------------------

buscar_url_abrangencia <- function() {

  message("Buscando arquivo mais recente de área de abrangência da PBH...")

  url_api <- paste0(
    "https://dados.pbh.gov.br/api/3/action/package_show",
    "?id=area-de-abrangencia-saude"
  )

  resp <- tryCatch(
    GET(url_api, timeout(30)),
    error = function(e) NULL
  )

  if (is.null(resp) || status_code(resp) != 200) {
    stop("Não foi possível acessar a API do Portal de Dados Abertos da PBH.")
  }

  conteudo <- fromJSON(content(resp, "text", encoding = "UTF-8"))

  # Extrai lista de recursos CSV
  recursos <- as_tibble(conteudo$result$resources) %>%
    filter(str_detect(tolower(format), "csv")) %>%
    arrange(desc(created))

  if (nrow(recursos) == 0) {
    stop("Nenhum arquivo CSV encontrado no dataset.")
  }

  url_recente  <- recursos$url[1]
  nome_recente <- recursos$name[1]

  message("Arquivo mais recente: ", nome_recente)
  message("URL: ", url_recente)

  return(url_recente)
}

# -----------------------------------------------------------------------------
# Carrega polígonos de área de abrangência da PBH
# -----------------------------------------------------------------------------

URL_ABRANGENCIA <- buscar_url_abrangencia()

abrangencia_sf <- read_csv2(URL_ABRANGENCIA, show_col_types = FALSE) %>%
  filter(!is.na(GEOMETRIA)) %>%
  st_as_sf(wkt = "GEOMETRIA", crs = 31983) %>%
  st_transform(crs = 4326) %>%
  select(
    cod_smsa = COD_SMSA,
    nome_cs  = NOME_CENTRO_SAUDE,
    regional = DISTRITO_SANITARIO
  )

message("Polígonos carregados: ", nrow(abrangencia_sf), " Centros de Saúde")

# -----------------------------------------------------------------------------
# Carrega dados ICSAP processados
# -----------------------------------------------------------------------------

dados <- read_csv(
  file.path(DIR_PROC, "icsap_bh.csv"),
  show_col_types = FALSE
)

message("Registros ICSAP carregados: ", nrow(dados))

# -----------------------------------------------------------------------------
# Carrega cache de CEPs já consultados (se existir)
# -----------------------------------------------------------------------------

if (file.exists(CACHE_CEP)) {
  cache <- read_csv(CACHE_CEP, show_col_types = FALSE)
  message("Cache carregado: ", nrow(cache), " CEPs já processados")
} else {
  cache <- tibble(
    cep    = character(),
    lat    = numeric(),
    lon    = numeric(),
    motivo = character()
  )
  message("Cache vazio — iniciando do zero")
}

# -----------------------------------------------------------------------------
# Identifica CEPs únicos ainda não processados
# -----------------------------------------------------------------------------

ceps_dados <- dados %>%
  filter(!is.na(cep), cep != "",
         str_length(str_remove_all(cep, "\\D")) == 8) %>%
  distinct(cep) %>%
  pull(cep)

ceps_novos <- setdiff(ceps_dados, cache$cep)

message("CEPs únicos nos dados: ",      length(ceps_dados))
message("CEPs já no cache: ",           length(ceps_dados) - length(ceps_novos))
message("CEPs novos para consultar: ",  length(ceps_novos))

# -----------------------------------------------------------------------------
# Funções de consulta
# -----------------------------------------------------------------------------

consultar_viacep <- function(cep) {
  cep_limpo <- str_remove_all(cep, "\\D")
  url  <- paste0("https://viacep.com.br/ws/", cep_limpo, "/json/")
  resp <- tryCatch(GET(url, timeout(10)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  dados <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(dados) || !is.null(dados$erro)) return(NULL)
  return(dados)
}

geocodificar <- function(logradouro, bairro, cidade = "Belo Horizonte") {
  endereco <- paste0(logradouro, ", ", bairro, ", ", cidade, ", MG")
  resp <- tryCatch(
    GET(
      "https://nominatim.openstreetmap.org/search",
      query = list(q = endereco, format = "json", limit = 1),
      add_headers("User-Agent" = "sihsus-icsap-bh/1.0 (github.com/Brunoavfer)")
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  geo <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(geo) || length(geo) == 0) return(NULL)
  return(list(lat = as.numeric(geo$lat[1]), lon = as.numeric(geo$lon[1])))
}

# -----------------------------------------------------------------------------
# Loop principal — consulta CEPs novos
# -----------------------------------------------------------------------------

if (length(ceps_novos) > 0) {

  novos_registros <- vector("list", length(ceps_novos))

  for (i in seq_along(ceps_novos)) {

    cep <- ceps_novos[i]

    if (i %% 50 == 0) {
      message("Processando CEP ", i, " de ", length(ceps_novos),
              " (", round(i / length(ceps_novos) * 100), "%)")
    }

    # Passo 1: ViaCEP
    Sys.sleep(PAUSA_VIACEP)
    dados_cep <- consultar_viacep(cep)

    if (is.null(dados_cep)) {
      novos_registros[[i]] <- tibble(
        cep    = cep, lat = NA_real_, lon = NA_real_,
        motivo = "CEP não encontrado no ViaCEP"
      )
      next
    }

    # Verifica se é BH
    if (!str_detect(tolower(dados_cep$localidade), "belo horizonte")) {
      novos_registros[[i]] <- tibble(
        cep    = cep, lat = NA_real_, lon = NA_real_,
        motivo = paste0("CEP de outro município: ", dados_cep$localidade)
      )
      next
    }

    # Passo 2: Nominatim
    Sys.sleep(PAUSA_NOMINATIM)
    coords <- geocodificar(dados_cep$logradouro, dados_cep$bairro)

    if (is.null(coords)) {
      novos_registros[[i]] <- tibble(
        cep    = cep, lat = NA_real_, lon = NA_real_,
        motivo = "Endereço não geocodificado"
      )
      next
    }

    novos_registros[[i]] <- tibble(
      cep    = cep,
      lat    = coords$lat,
      lon    = coords$lon,
      motivo = NA_character_
    )
  }

  # Atualiza cache
  cache <- bind_rows(cache, bind_rows(novos_registros))
  write_csv(cache, CACHE_CEP)
  message("Cache atualizado: ", nrow(cache), " CEPs no total")
}

# -----------------------------------------------------------------------------
# Cruzamento com polígonos da PBH
# -----------------------------------------------------------------------------

message("Cruzando coordenadas com polígonos da PBH...")

cache_valido <- cache %>% filter(!is.na(lat), !is.na(lon))

pontos_sf <- cache_valido %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cruzamento <- st_join(pontos_sf, abrangencia_sf) %>%
  st_drop_geometry() %>%
  select(cep, cod_smsa, nome_cs, regional)

cache_completo <- cache %>%
  left_join(cruzamento, by = "cep")

# -----------------------------------------------------------------------------
# Junta com os dados ICSAP
# -----------------------------------------------------------------------------

dados_final <- dados %>%
  left_join(
    cache_completo %>% select(cep, nome_cs, regional),
    by = "cep"
  )

# -----------------------------------------------------------------------------
# Relatório de cobertura
# -----------------------------------------------------------------------------

cobertura <- dados_final %>%
  summarise(
    total         = n(),
    com_regional  = sum(!is.na(regional)),
    sem_regional  = sum(is.na(regional)),
    pct_cobertura = round(com_regional / total * 100, 1)
  )

message("==========================================")
message("Total de registros ICSAP: ",    cobertura$total)
message("Com regional identificada: ",   cobertura$com_regional)
message("Sem regional identificada: ",   cobertura$sem_regional)
message("Cobertura: ",                   cobertura$pct_cobertura, "%")
message("==========================================")

# Log de CEPs não encontrados
ceps_nao_encontrados <- cache %>%
  filter(!is.na(motivo)) %>%
  left_join(
    dados %>% count(cep, name = "n_pacientes"),
    by = "cep"
  ) %>%
  arrange(desc(n_pacientes))

write_csv(ceps_nao_encontrados, LOG_CEP)
message("Log salvo em: ", LOG_CEP)

# -----------------------------------------------------------------------------
# Salva resultado final
# -----------------------------------------------------------------------------

write_csv(
  dados_final,
  file.path(DIR_PROC, "icsap_bh_regional.csv")
)

message("Arquivo final salvo: data/processed/icsap_bh_regional.csv")
