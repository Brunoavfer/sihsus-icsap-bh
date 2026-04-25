# =============================================================================
# 04_melhora_cobertura.R
#
# O que faz:
#   - Lê o cache_cep.csv existente
#   - Identifica CEPs que falharam na geocodificação anterior
#   - Tenta novamente usando cascata de APIs open source:
#     1. BrasilAPI (coordenadas diretas)
#     2. OpenCage (até 2.500 req/dia gratuito)
#     3. Photon/Komoot (sem limite)
#   - Atualiza o cache com os novos resultados
#   - Regera o arquivo icsap_bh_regional.csv com cobertura melhorada
#
# Todas as APIs utilizadas são gratuitas e open source.
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

DIR_PROC  <- "data/processed"
DIR_REF   <- "data/ref"
CACHE_CEP <- file.path(DIR_REF, "cache_cep.csv")
LOG_CEP   <- file.path(DIR_PROC, "ceps_nao_encontrados.csv")

PAUSA <- 1.0  # pausa entre requisições (segundos)

# -----------------------------------------------------------------------------
# Carrega cache existente
# -----------------------------------------------------------------------------

cache <- read_csv(
  CACHE_CEP,
  col_types = cols(cep = col_character()),
  show_col_types = FALSE
)

# Identifica apenas os CEPs que falharam
ceps_falhos <- cache %>%
  filter(!is.na(motivo)) %>%
  filter(!str_detect(motivo, "outro município")) %>%  # não tenta fora de BH
  pull(cep)

message("Total no cache: ", nrow(cache))
message("CEPs com falha para retentar: ", length(ceps_falhos))

# -----------------------------------------------------------------------------
# Função 1 — BrasilAPI (retorna coordenadas diretamente)
# -----------------------------------------------------------------------------

geocode_brasilapi <- function(cep) {
  cep_limpo <- str_remove_all(cep, "\\D")
  url  <- paste0("https://brasilapi.com.br/api/cep/v2/", cep_limpo)
  resp <- tryCatch(
    GET(url, timeout(10)),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  dados <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(dados)) return(NULL)

  # BrasilAPI retorna latitude/longitude diretamente
  if (!is.null(dados$location$coordinates$latitude) &&
      !is.null(dados$location$coordinates$longitude)) {
    lat <- as.numeric(dados$location$coordinates$latitude)
    lon <- as.numeric(dados$location$coordinates$longitude)
    if (!is.na(lat) && !is.na(lon) && lat != 0 && lon != 0) {
      return(list(lat = lat, lon = lon, fonte = "BrasilAPI"))
    }
  }

  # Se não tem coordenadas, tenta geocodificar pelo endereço
  if (!is.null(dados$street) && !is.null(dados$neighborhood)) {
    return(list(
      endereco = paste0(dados$street, ", ", dados$neighborhood, 
                        ", Belo Horizonte, MG"),
      fonte    = "BrasilAPI_endereco"
    ))
  }
  return(NULL)
}

# -----------------------------------------------------------------------------
# Função 2 — Photon/Komoot (open source, sem limite)
# -----------------------------------------------------------------------------

geocode_photon <- function(endereco) {
  resp <- tryCatch(
    GET(
      "https://photon.komoot.io/api/",
      query = list(q = endereco, limit = 1, lang = "pt"),
      add_headers("User-Agent" = "sihsus-icsap-bh/1.0"),
      timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  dados <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(dados) || length(dados$features) == 0) return(NULL)

  coords <- dados$features$geometry$coordinates[[1]]
  if (is.null(coords) || length(coords) < 2) return(NULL)

  return(list(lat = coords[2], lon = coords[1], fonte = "Photon"))
}

# -----------------------------------------------------------------------------
# Função 3 — Nominatim (nova tentativa com query diferente)
# -----------------------------------------------------------------------------

geocode_nominatim_v2 <- function(cep) {
  cep_limpo <- str_remove_all(cep, "\\D")
  # Tenta busca direta pelo CEP no Nominatim
  resp <- tryCatch(
    GET(
      "https://nominatim.openstreetmap.org/search",
      query = list(
        postalcode = cep_limpo,
        country    = "Brazil",
        format     = "json",
        limit      = 1
      ),
      add_headers("User-Agent" = "sihsus-icsap-bh/1.0"),
      timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  geo <- tryCatch(
    fromJSON(content(resp, "text", encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(geo) || length(geo) == 0) return(NULL)
  return(list(
    lat   = as.numeric(geo$lat[1]),
    lon   = as.numeric(geo$lon[1]),
    fonte = "Nominatim_CEP"
  ))
}

# -----------------------------------------------------------------------------
# Função principal — cascata de APIs
# -----------------------------------------------------------------------------

tentar_geocodificar <- function(cep) {

  # Tentativa 1: BrasilAPI
  Sys.sleep(PAUSA)
  resultado <- geocode_brasilapi(cep)

  if (!is.null(resultado)) {
    # Se retornou coordenadas diretas
    if (!is.null(resultado$lat)) {
      return(tibble(
        cep    = as.character(cep),
        lat    = resultado$lat,
        lon    = resultado$lon,
        motivo = NA_character_,
        fonte  = resultado$fonte
      ))
    }
    # Se retornou endereço para geocodificar
    if (!is.null(resultado$endereco)) {
      Sys.sleep(PAUSA)
      coords <- geocode_photon(resultado$endereco)
      if (!is.null(coords)) {
        return(tibble(
          cep    = as.character(cep),
          lat    = coords$lat,
          lon    = coords$lon,
          motivo = NA_character_,
          fonte  = "BrasilAPI+Photon"
        ))
      }
    }
  }

  # Tentativa 2: Nominatim direto pelo CEP
  Sys.sleep(PAUSA)
  resultado2 <- geocode_nominatim_v2(cep)
  if (!is.null(resultado2)) {
    return(tibble(
      cep    = as.character(cep),
      lat    = resultado2$lat,
      lon    = resultado2$lon,
      motivo = NA_character_,
      fonte  = resultado2$fonte
    ))
  }

  # Tentativa 3: Photon direto pelo CEP
  Sys.sleep(PAUSA)
  resultado3 <- geocode_photon(paste0(str_remove_all(cep, "\\D"), 
                                      " Belo Horizonte Brasil"))
  if (!is.null(resultado3)) {
    return(tibble(
      cep    = as.character(cep),
      lat    = resultado3$lat,
      lon    = resultado3$lon,
      motivo = NA_character_,
      fonte  = resultado3$fonte
    ))
  }

  # Todas as tentativas falharam
  return(tibble(
    cep    = as.character(cep),
    lat    = NA_real_,
    lon    = NA_real_,
    motivo = "Falhou em todas as APIs (BrasilAPI, Nominatim_CEP, Photon)",
    fonte  = NA_character_
  ))
}

# -----------------------------------------------------------------------------
# Loop principal
# -----------------------------------------------------------------------------

if (length(ceps_falhos) == 0) {
  message("Nenhum CEP para retentar!")
} else {

  message("Iniciando cascata para ", length(ceps_falhos), " CEPs...")
  message("Tempo estimado: ~", round(length(ceps_falhos) * 3 / 60), " minutos")

  novos_resultados <- vector("list", length(ceps_falhos))

  for (i in seq_along(ceps_falhos)) {

    if (i %% 50 == 0) {
      message("Processando ", i, " de ", length(ceps_falhos),
              " (", round(i / length(ceps_falhos) * 100), "%)")

      # Salva progresso a cada 50 CEPs
      cache_atualizado <- cache %>%
        filter(!cep %in% ceps_falhos[1:i]) %>%
        bind_rows(bind_rows(novos_resultados[1:i]))
      write_csv(cache_atualizado, CACHE_CEP)
    }

    novos_resultados[[i]] <- tentar_geocodificar(ceps_falhos[i])
  }

  # Atualiza cache — substitui registros falhos pelos novos
  novos_df <- bind_rows(novos_resultados) %>%
    mutate(cep = as.character(cep))

  cache_final <- cache %>%
    filter(!cep %in% novos_df$cep) %>%
    bind_rows(novos_df %>% select(cep, lat, lon, motivo))

  write_csv(cache_final, CACHE_CEP)

  # Resumo
  resolvidos <- novos_df %>% filter(!is.na(lat)) %>% nrow()
  message("==========================================")
  message("CEPs retentados: ",  length(ceps_falhos))
  message("Resolvidos agora: ", resolvidos)
  message("Ainda sem solução: ", length(ceps_falhos) - resolvidos)
  message("==========================================")
}

# -----------------------------------------------------------------------------
# Regera arquivo final com cobertura melhorada
# -----------------------------------------------------------------------------

message("Regerando icsap_bh_regional.csv com cobertura melhorada...")

# Carrega polígonos
abrangencia_sf <- st_read(
  file.path(DIR_REF, "areas_abrangencia_cs.geojson"),
  quiet = TRUE
)

# Carrega dados ICSAP
dados <- read_csv(
  file.path(DIR_PROC, "icsap_bh.csv"),
  col_types = cols(cep = col_character()),
  show_col_types = FALSE
)

# Cruzamento espacial
cache_valido <- read_csv(
  CACHE_CEP,
  col_types = cols(cep = col_character()),
  show_col_types = FALSE
) %>%
  filter(!is.na(lat), !is.na(lon))

pontos_sf <- cache_valido %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cruzamento <- st_join(pontos_sf, abrangencia_sf) %>%
  st_drop_geometry() %>%
  select(cep, cod_smsa, nome_cs, regional) %>%
  mutate(cep = as.character(cep))

dados_final <- dados %>%
  mutate(cep = as.character(cep)) %>%
  left_join(
    cruzamento %>% select(cep, nome_cs, regional),
    by = "cep"
  )

# Relatório final
cobertura <- dados_final %>%
  summarise(
    total         = n(),
    com_regional  = sum(!is.na(regional)),
    sem_regional  = sum(is.na(regional)),
    pct_cobertura = round(com_regional / total * 100, 1)
  )

message("==========================================")
message("COBERTURA FINAL")
message("Total de registros ICSAP: ",    cobertura$total)
message("Com regional identificada: ",   cobertura$com_regional)
message("Sem regional identificada: ",   cobertura$sem_regional)
message("Cobertura: ",                   cobertura$pct_cobertura, "%")
message("==========================================")

# Salva
write_csv(dados_final, file.path(DIR_PROC, "icsap_bh_regional.csv"))
message("Arquivo final salvo: data/processed/icsap_bh_regional.csv")
