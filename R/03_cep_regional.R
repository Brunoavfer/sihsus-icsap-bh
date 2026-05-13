# =============================================================================
# 03_cep_regional.R
#
# O que faz:
#   - Busca automaticamente o arquivo mais recente de área de abrangência
#     da PBH via web scraping do Portal de Dados Abertos
#   - Se a busca online falhar, usa o arquivo local em data/ref/
#   - Lê os dados processados (icsap_bh.csv)
#   - Para cada CEP único executa a cascata de geocodificação:
#       1. AwesomeAPI  — retorna lat/lon diretamente (pausa 0.2s)
#       2. BrasilAPI   — retorna lat/lon diretamente (pausa 0.3s)
#       3. ViaCEP + Nominatim — fallback textual (pausa 0.3s + 1.0s)
#   - Registra qual API geocodificou cada CEP (coluna "fonte" no cache)
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
library(rvest)

# -----------------------------------------------------------------------------
# Configurações
# -----------------------------------------------------------------------------

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

CACHE_CEP     <- file.path(DIR_REF, "cache_cep.csv")
LOG_CEP       <- file.path(DIR_PROC, "ceps_nao_encontrados.csv")
ARQUIVO_LOCAL <- file.path(DIR_REF, "area_abrangencia_saude.csv")

PAUSA_AWESOME   <- 0.2
PAUSA_BRASIL    <- 0.3
PAUSA_VIACEP    <- 0.3
PAUSA_NOMINATIM <- 1.0

# -----------------------------------------------------------------------------
# Função auxiliar
# -----------------------------------------------------------------------------

como_texto <- function(x) as.character(x)

coord_valida <- function(lat, lon) {
  !is.null(lat) && !is.null(lon) &&
    !is.na(lat)  && !is.na(lon)  &&
    lat != 0     && lon != 0     &&
    is.numeric(lat) && is.numeric(lon)
}

cidade_bh <- function(nome) {
  str_detect(tolower(as.character(nome)), "belo horizonte")
}

# -----------------------------------------------------------------------------
# Cascata de geocodificação
# -----------------------------------------------------------------------------

# Etapa 1: AwesomeAPI — retorna lat/lon diretamente
# Documentação: https://cep.awesomeapi.com.br
geocod_awesome <- function(cep) {
  cep_limpo <- str_remove_all(como_texto(cep), "\\D")
  resp <- tryCatch(
    GET(
      paste0("https://cep.awesomeapi.com.br/json/", cep_limpo),
      add_headers("User-Agent" = "sihsus-icsap-bh/1.0"),
      timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  d <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                error = function(e) NULL)
  if (is.null(d) || is.null(d$lat) || is.null(d$lng)) return(NULL)
  if (!cidade_bh(d$city)) return(NULL)
  lat <- suppressWarnings(as.numeric(d$lat))
  lon <- suppressWarnings(as.numeric(d$lng))
  if (!coord_valida(lat, lon)) return(NULL)
  list(lat = lat, lon = lon, fonte = "awesomeapi")
}

# Etapa 2: BrasilAPI v2 — retorna lat/lon em location.coordinates
# Documentação: https://brasilapi.com.br/docs#tag/CEP-V2
geocod_brasil <- function(cep) {
  cep_limpo <- str_remove_all(como_texto(cep), "\\D")
  resp <- tryCatch(
    GET(
      paste0("https://brasilapi.com.br/api/cep/v2/", cep_limpo),
      add_headers("User-Agent" = "sihsus-icsap-bh/1.0"),
      timeout(10)
    ),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  d <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                error = function(e) NULL)
  if (is.null(d)) return(NULL)
  if (!cidade_bh(d$city)) return(NULL)
  coords <- tryCatch(d$location$coordinates, error = function(e) NULL)
  if (is.null(coords)) return(NULL)
  lat <- suppressWarnings(as.numeric(coords$latitude))
  lon <- suppressWarnings(as.numeric(coords$longitude))
  if (!coord_valida(lat, lon)) return(NULL)
  list(lat = lat, lon = lon, fonte = "brasilapi")
}

# Etapa 3a: ViaCEP — obtém endereço textual para o Nominatim
consultar_viacep <- function(cep) {
  cep_limpo <- str_remove_all(como_texto(cep), "\\D")
  resp <- tryCatch(
    GET(paste0("https://viacep.com.br/ws/", cep_limpo, "/json/"), timeout(10)),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  d <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                error = function(e) NULL)
  if (is.null(d) || !is.null(d$erro)) return(NULL)
  if (!cidade_bh(d$localidade)) return(NULL)
  d
}

# Etapa 3b: Nominatim — geocodifica endereço textual do ViaCEP
geocod_nominatim <- function(logradouro, bairro, cidade = "Belo Horizonte") {
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
  geo <- tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8")),
                  error = function(e) NULL)
  if (is.null(geo) || length(geo) == 0) return(NULL)
  lat <- suppressWarnings(as.numeric(geo$lat[1]))
  lon <- suppressWarnings(as.numeric(geo$lon[1]))
  if (!coord_valida(lat, lon)) return(NULL)
  list(lat = lat, lon = lon, fonte = "nominatim")
}

# Orquestrador da cascata: tenta cada etapa em ordem
geocodificar_cep <- function(cep) {

  # Etapa 1: AwesomeAPI
  Sys.sleep(PAUSA_AWESOME)
  res <- geocod_awesome(cep)
  if (!is.null(res)) return(res)

  # Etapa 2: BrasilAPI
  Sys.sleep(PAUSA_BRASIL)
  res <- geocod_brasil(cep)
  if (!is.null(res)) return(res)

  # Etapa 3: ViaCEP + Nominatim
  Sys.sleep(PAUSA_VIACEP)
  dados_cep <- consultar_viacep(cep)

  if (is.null(dados_cep)) {
    return(list(lat = NA_real_, lon = NA_real_,
                fonte = NA_character_,
                motivo = "CEP nao encontrado em nenhuma API"))
  }

  Sys.sleep(PAUSA_NOMINATIM)
  res <- geocod_nominatim(dados_cep$logradouro, dados_cep$bairro)

  if (!is.null(res)) return(res)

  list(lat    = NA_real_,
       lon    = NA_real_,
       fonte  = NA_character_,
       motivo = "Endereco nao geocodificado pelo Nominatim")
}

# -----------------------------------------------------------------------------
# Carrega polígonos de área de abrangência da PBH
# -----------------------------------------------------------------------------

buscar_url_abrangencia <- function() {

  message("Buscando arquivo mais recente de area de abrangencia da PBH...")

  resp <- tryCatch(
    GET(
      "https://dados.pbh.gov.br/dataset/area-de-abrangencia-saude",
      add_headers(
        "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
        "Accept"     = "text/html"
      ),
      timeout(30)
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || status_code(resp) != 200) {
    stop("Nao foi possivel acessar o Portal de Dados Abertos da PBH.")
  }

  html  <- read_html(content(resp, "text", encoding = "UTF-8"))
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    .[str_detect(., "download.*\\.csv|download.*abrangencia")]

  links_csv <- links[str_detect(links, "\\.csv$")]

  if (length(links_csv) == 0) {
    stop("Nenhum arquivo CSV encontrado na pagina da PBH.")
  }

  url_recente <- tail(links_csv, 1)
  message("Arquivo mais recente: ", basename(url_recente))
  return(url_recente)
}

URL_ABRANGENCIA <- tryCatch(
  buscar_url_abrangencia(),
  error = function(e) {
    message("Busca online falhou — usando arquivo local.")
    if (!file.exists(ARQUIVO_LOCAL)) {
      stop(paste0(
        "Arquivo local nao encontrado: ", ARQUIVO_LOCAL,
        "\nBaixe manualmente em: ",
        "https://dados.pbh.gov.br/dataset/area-de-abrangencia-saude",
        "\ne salve em data/ref/area_abrangencia_saude.csv"
      ))
    }
    message("Arquivo local encontrado: ", ARQUIVO_LOCAL)
    return(ARQUIVO_LOCAL)
  }
)

abrangencia_sf <- read_csv2(URL_ABRANGENCIA, show_col_types = FALSE) %>%
  filter(!is.na(GEOMETRIA)) %>%
  st_as_sf(wkt = "GEOMETRIA", crs = 31983) %>%
  st_transform(crs = 4326) %>%
  select(
    cod_smsa = COD_SMSA,
    nome_cs  = NOME_CENTRO_SAUDE,
    regional = DISTRITO_SANITARIO
  )

message("Poligonos carregados: ", nrow(abrangencia_sf), " Centros de Saude")

# -----------------------------------------------------------------------------
# Carrega dados ICSAP processados
# -----------------------------------------------------------------------------

dados <- read_csv(
  file.path(DIR_PROC, "icsap_bh.csv"),
  col_types = cols(cep = col_character()),
  show_col_types = FALSE
)

message("Registros ICSAP carregados: ", nrow(dados))

# -----------------------------------------------------------------------------
# Carrega cache de CEPs já consultados (se existir)
# -----------------------------------------------------------------------------

if (file.exists(CACHE_CEP)) {
  cache <- read_csv(
    CACHE_CEP,
    col_types = cols(cep = col_character(), fonte = col_character()),
    show_col_types = FALSE
  )
  # Garante coluna "fonte" para caches gerados por versoes anteriores do script
  if (!"fonte" %in% names(cache)) {
    cache <- cache %>% mutate(fonte = NA_character_)
  }
  message("Cache carregado: ", nrow(cache), " CEPs ja processados")
} else {
  cache <- tibble(
    cep    = character(),
    lat    = numeric(),
    lon    = numeric(),
    motivo = character(),
    fonte  = character()
  )
  message("Cache vazio — iniciando do zero")
}

# -----------------------------------------------------------------------------
# Identifica CEPs únicos ainda não processados
# -----------------------------------------------------------------------------

ceps_dados <- dados %>%
  filter(!is.na(cep), cep != "",
         str_length(str_remove_all(cep, "\\D")) == 8) %>%
  mutate(cep = como_texto(cep)) %>%
  distinct(cep) %>%
  pull(cep)

ceps_novos <- setdiff(ceps_dados, cache$cep)

message("CEPs unicos nos dados: ",      length(ceps_dados))
message("CEPs ja no cache: ",           length(ceps_dados) - length(ceps_novos))
message("CEPs novos para consultar: ",  length(ceps_novos))

# -----------------------------------------------------------------------------
# Loop principal — cascata de geocodificação para CEPs novos
# -----------------------------------------------------------------------------

if (length(ceps_novos) > 0) {

  contadores <- list(awesomeapi = 0L, brasilapi = 0L,
                     nominatim  = 0L, falha     = 0L)

  novos_registros <- vector("list", length(ceps_novos))

  for (i in seq_along(ceps_novos)) {

    cep <- como_texto(ceps_novos[i])

    if (i %% 50 == 0 || i == length(ceps_novos)) {
      message(sprintf("CEP %d/%d (%d%%) | awesome=%d brasil=%d nominatim=%d falha=%d",
                      i, length(ceps_novos), round(i / length(ceps_novos) * 100),
                      contadores$awesomeapi, contadores$brasilapi,
                      contadores$nominatim,  contadores$falha))

      # Salva cache parcial a cada 50 CEPs
      registros_ok <- novos_registros[!sapply(novos_registros, is.null)]
      if (length(registros_ok) > 0) {
        cache_parcial <- bind_rows(cache, bind_rows(registros_ok))
        write_csv(cache_parcial, CACHE_CEP)
      }
    }

    res <- geocodificar_cep(cep)

    motivo <- if (!is.null(res$motivo)) res$motivo else NA_character_
    fonte  <- if (!is.null(res$fonte))  res$fonte  else NA_character_

    novos_registros[[i]] <- tibble(
      cep    = cep,
      lat    = res$lat,
      lon    = res$lon,
      motivo = motivo,
      fonte  = fonte
    )

    if (!is.na(fonte)) {
      contadores[[fonte]] <- contadores[[fonte]] + 1L
    } else {
      contadores$falha <- contadores$falha + 1L
    }
  }

  # Salva cache final
  cache <- bind_rows(cache, bind_rows(novos_registros))
  write_csv(cache, CACHE_CEP)

  message("======================================")
  message("Cache atualizado: ", nrow(cache), " CEPs no total")
  message("Fontes dos novos CEPs:")
  message("  AwesomeAPI : ", contadores$awesomeapi)
  message("  BrasilAPI  : ", contadores$brasilapi)
  message("  Nominatim  : ", contadores$nominatim)
  message("  Falha      : ", contadores$falha)
  message("======================================")
}

# -----------------------------------------------------------------------------
# Cruzamento com polígonos da PBH
# -----------------------------------------------------------------------------

message("Cruzando coordenadas com poligonos da PBH...")

cache_valido <- cache %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(cep = como_texto(cep))

pontos_sf <- cache_valido %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

cruzamento <- st_join(pontos_sf, abrangencia_sf) %>%
  st_drop_geometry() %>%
  select(cep, cod_smsa, nome_cs, regional) %>%
  mutate(cep = como_texto(cep))

cache_completo <- cache %>%
  mutate(cep = como_texto(cep)) %>%
  left_join(cruzamento, by = "cep")

# -----------------------------------------------------------------------------
# Junta com os dados ICSAP
# -----------------------------------------------------------------------------

dados_final <- dados %>%
  mutate(cep = como_texto(cep)) %>%
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

# Resumo por fonte no cache completo
if ("fonte" %in% names(cache)) {
  message("Distribuicao do cache por fonte de geocodificacao:")
  cache %>%
    count(fonte, sort = TRUE) %>%
    mutate(pct = round(n / sum(n) * 100, 1)) %>%
    as.data.frame() %>%
    apply(1, function(r) message("  ", r["fonte"], ": ", r["n"], " (", r["pct"], "%)"))
}

# Log de CEPs não encontrados
ceps_nao_encontrados <- cache %>%
  mutate(cep = como_texto(cep)) %>%
  filter(!is.na(motivo)) %>%
  left_join(
    dados %>%
      mutate(cep = como_texto(cep)) %>%
      count(cep, name = "n_pacientes"),
    by = "cep"
  ) %>%
  arrange(desc(n_pacientes))

write_csv(ceps_nao_encontrados, LOG_CEP)
message("Log de nao encontrados salvo em: ", LOG_CEP)

# -----------------------------------------------------------------------------
# Salva resultado final
# -----------------------------------------------------------------------------

write_csv(
  dados_final,
  file.path(DIR_PROC, "icsap_bh_regional.csv")
)

message("Arquivo final salvo: data/processed/icsap_bh_regional.csv")
