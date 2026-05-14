# =============================================================================
# 05_coleta_variaveis.R
#
# Coleta variáveis independentes para o modelo ecológico longitudinal:
#   Taxa ICSAP × Centro de Saúde de BH, jan/2023–dez/2025
#   (153 CS × 36 competências = 5.508 observações potenciais)
#
# Saída: data/ref/variaveis_cs.csv
#   Uma linha por CS por competência, com COD_SMSA e nome_cs como chaves.
#
# Execute na raiz do projeto (sihsus-icsap-bh/), não dentro de R/.
#
# ─────────────────────────────────────────────────────────────────────────────
# LIMITAÇÕES CONHECIDAS
# ─────────────────────────────────────────────────────────────────────────────
#
# 1. e-Gestor AB — dados de cobertura da APS disponíveis apenas por município
#    (não por CS nem por equipe) via portal público. Os valores mensais gerados
#    aqui são de nível municipal e serão atribuídos igualmente a todos os 153 CS
#    como variável de contexto (nível 2). Não há série histórica por CS/equipe.
#
# 2. CNES → nome_cs / COD_SMSA — o CNES identifica estabelecimentos pelo
#    CO_UNIDADE (código nacional). A tabela de-para entre CO_UNIDADE e o
#    COD_SMSA interno da SMSA/PBH não é pública e não está nos dados abertos.
#    O script constrói a de-para por similaridade textual (Jaro-Winkler) entre
#    o nome do estabelecimento no CNES e nome_cs dos polígonos da PBH.
#    Pares com similaridade < 0,70 são exportados para revisão manual em:
#    data/ref/depara_cnes_cs_revisar.csv
#
# 3. Censo 2022 — dados se referem ao Censo realizado em 2022; são aplicados
#    ao período 2023–2025 sem ajuste intercensitário. Variações populacionais
#    intra-período (crescimento, migração) não são capturadas. Limitação
#    reconhecida em estudos ecológicos com denominador censitário estático.
#
# 4. Aglomerados Subnormais — malha de Favelas e Comunidades Urbanas é de 2022.
#    Expansões ou retrações ocorridas em 2023–2025 não são refletidas.
#
# 5. CNES mensal × variação temporal — equipes raramente mudam de competência
#    para competência. Para reduzir o volume de download (~1.5 GB total),
#    edite COMPETENCIAS abaixo para baixar apenas competências anuais (ex.:
#    janeiro de cada ano) e interpolar os demais meses.
# =============================================================================

# --- Pacotes -----------------------------------------------------------------

suppressPackageStartupMessages({
  library(read.dbc)
  library(dplyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(sf)
  library(httr)
  library(jsonlite)
})

# Pacotes opcionais — instala se ausente
pkg_opt <- c("censobr", "geobr", "stringdist", "readxl")
for (pkg in pkg_opt) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Instalando ", pkg, "...")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(censobr)
library(geobr)
library(stringdist)
library(readxl)

# --- Configuração -------------------------------------------------------------

COD_BH   <- "310620"   # IBGE 6 dígitos (sem dígito verificador)
COD_BH_7 <- "3106200"  # IBGE 7 dígitos (com dígito verificador)

DIR_RAW  <- "data/raw/cnes"
DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

for (d in c(DIR_RAW, DIR_PROC, DIR_REF)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

ANO_INICIO <- 2023
ANO_FIM    <- 2025

# Para baixar todas as competências mensais (36 arquivos por tipo):
COMPETENCIAS <- outer(
  str_sub(ANO_INICIO:ANO_FIM, 3, 4),
  str_pad(1:12, 2, pad = "0"),
  paste0
) |> as.vector() |> sort()
# Para download mais rápido (apenas janeiros, ~3 arquivos por tipo),
# comente a linha acima e descomente a seguinte:
# COMPETENCIAS <- paste0(str_sub(ANO_INICIO:ANO_FIM, 3, 4), "01")

FTP_CNES <- "ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados"

# Tipos de equipe CNES (TP_EQUIPE) — verificar em datasus.gov.br/SCTIE/tabela_tp_equipe
TP_ESF    <- c("70", "71")   # ESF completa e ESF Ribeirinha/Fluvial
TP_EMULTI <- c("76", "77")   # eMulti (tipo 76) e eMulti Ampliada (tipo 77)
# Nota: o código "70.36" citado em documentos ministeriais refere-se à
# classificação AMS/CNES de 2023; no campo TP_EQUIPE do banco o valor
# esperado é "76". Confirme após leitura do primeiro arquivo EQ.
TP_ESB    <- c("87", "88")   # Saúde Bucal tipo I e II (quando registradas em EQ)

# CBO de interesse
CBO_MED_REGEX <- "^(2251|2252)"  # todos os médicos (str_detect)
CBO_MFC       <- "225125"        # Médico de Família e Comunidade
CBO_ACS       <- "515110"        # Agente Comunitário de Saúde

# --- Funções auxiliares -------------------------------------------------------

# Download com cache (não re-baixa se arquivo já existir)
baixar_dbc <- function(url, destfile) {
  if (file.exists(destfile)) {
    message("  Já existe: ", basename(destfile))
    return(invisible(destfile))
  }
  message("  Baixando: ", basename(destfile))
  result <- tryCatch(
    download.file(url, destfile, mode = "wb", quiet = TRUE),
    error = function(e) {
      message("  ERRO: ", conditionMessage(e))
      if (file.exists(destfile)) file.remove(destfile)
      return(1L)
    }
  )
  if (!is.null(result) && result != 0) return(NULL)
  invisible(destfile)
}

# Lê .dbc e retorna com colunas em minúsculo
ler_dbc <- function(arquivo) {
  tryCatch({
    df <- read.dbc(arquivo)
    names(df) <- tolower(names(df))
    df
  }, error = function(e) {
    message("  Falha ao ler ", basename(arquivo), ": ", conditionMessage(e))
    NULL
  })
}

# Detecta coluna de município (CNES usa nomes variados conforme versão do layout)
col_mun_cnes <- function(nomes) {
  intersect(c("co_municipio", "cod_munic", "co_municipio_gestor", "municipio"), nomes)[1]
}

# Filtro padrão de município BH (aceita 6 ou 7 dígitos)
filtrar_bh <- function(df) {
  col <- col_mun_cnes(names(df))
  if (is.na(col)) {
    warning("Coluna de município não encontrada. Colunas disponíveis: ",
            paste(names(df), collapse = ", "))
    return(df)
  }
  df %>%
    filter(str_starts(as.character(.data[[col]]), COD_BH))
}

# Normaliza texto para matching (remove acentos, pontuação, caixa)
normalizar_str <- function(x) {
  x |>
    str_to_lower() |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish()
}

# Extrai competência do nome do arquivo CNES (ex.: "EQMG2301.dbc" → "2301")
comp_do_arquivo <- function(arq) {
  str_extract(basename(arq), "(?<=[A-Z]{2}MG)\\d{4}(?=\\.dbc)")
}

# =============================================================================
# SEÇÃO 1 — CNES: Equipes (EQ) e Profissionais (PF)
# =============================================================================

message("\n========================================")
message("SEÇÃO 1: CNES — EQ e PF")
message("========================================")

# ---------------------------------------------------------------------------
# 1a. Download dos arquivos .dbc
# ---------------------------------------------------------------------------

baixar_cnes <- function(tipo, competencias) {
  arquivos_ok <- character()
  for (comp in competencias) {
    fname  <- paste0(tipo, "MG", comp, ".dbc")
    url    <- paste0(FTP_CNES, "/", tipo, "/", fname)
    dest   <- file.path(DIR_RAW, fname)
    res    <- baixar_dbc(url, dest)
    if (!is.null(res) && file.exists(dest)) arquivos_ok <- c(arquivos_ok, dest)
  }
  arquivos_ok
}

message("\nBaixando EQ (Equipes) — MG ", ANO_INICIO, "-", ANO_FIM,
        " (", length(COMPETENCIAS), " competências)...")
arqs_eq <- baixar_cnes("EQ", COMPETENCIAS)

message("\nBaixando PF (Profissionais) — MG ", ANO_INICIO, "-", ANO_FIM, "...")
arqs_pf <- baixar_cnes("PF", COMPETENCIAS)

# ST (Estabelecimentos) — competência de referência para o de-para
message("\nBaixando ST (Estabelecimentos) — jan/2025 para de-para CNES ↔ CS...")
arq_st <- file.path(DIR_RAW, "STMG2501.dbc")
baixar_dbc(paste0(FTP_CNES, "/ST/STMG2501.dbc"), arq_st)

# ---------------------------------------------------------------------------
# 1b. Constrói de-para CNES (CO_UNIDADE) ↔ nome_cs / COD_SMSA
# ---------------------------------------------------------------------------

depara_path <- file.path(DIR_REF, "depara_cnes_cs.csv")

if (!file.exists(depara_path)) {

  message("\nConstruindo de-para CNES ↔ nome_cs por similaridade textual...")

  # Lista de CS de referência (dos polígonos PBH)
  arq_geo <- file.path(DIR_REF, "areas_abrangencia_cs.geojson")
  if (!file.exists(arq_geo)) {
    stop("GeoJSON não encontrado: ", arq_geo,
         "\nExecute o app ao menos uma vez para que o arquivo seja baixado do GitHub.")
  }

  poligonos_ref <- st_read(arq_geo, quiet = TRUE) |>
    st_drop_geometry() |>
    select(nome_cs, any_of(c("regional", "cod_smsa", "co_unidade", "cnes"))) |>
    distinct() |>
    mutate(nome_cs_norm = normalizar_str(nome_cs))

  if (file.exists(arq_st)) {
    st_raw <- ler_dbc(arq_st)

    if (!is.null(st_raw)) {
      # Inspeciona colunas disponíveis
      message("  Colunas no arquivo ST: ", paste(names(st_raw), collapse = ", "))

      col_cnes_st <- intersect(c("co_unidade", "cod_cnes", "cnes", "co_cnes"), names(st_raw))[1]
      col_nome_st <- intersect(
        c("no_razao_social", "no_fantasia", "nm_razao_social", "no_estabelecimento"),
        names(st_raw)
      )[1]

      if (!is.na(col_cnes_st) && !is.na(col_nome_st)) {
        est_bh <- filtrar_bh(st_raw) |>
          mutate(
            co_unidade         = as.character(.data[[col_cnes_st]]),
            no_estabelecimento = as.character(.data[[col_nome_st]]),
            no_norm            = normalizar_str(no_estabelecimento)
          ) |>
          select(co_unidade, no_estabelecimento, no_norm) |>
          distinct()

        message("  Estabelecimentos CNES em BH: ", nrow(est_bh))

        # Jaro-Winkler: 0 = idêntico, 1 = totalmente diferente
        sim_mat <- stringdistmatrix(
          poligonos_ref$nome_cs_norm,
          est_bh$no_norm,
          method = "jw", p = 0.1
        )
        best_idx  <- apply(sim_mat, 1, which.min)
        best_dist <- apply(sim_mat, 1, min)

        depara <- poligonos_ref |>
          mutate(
            co_unidade         = est_bh$co_unidade[best_idx],
            no_estabelecimento = est_bh$no_estabelecimento[best_idx],
            similaridade       = round(1 - best_dist, 3),
            revisar            = similaridade < 0.70
          )

        write_csv(depara, depara_path)

        n_revisar <- sum(depara$revisar, na.rm = TRUE)
        if (n_revisar > 0) {
          rev_path <- file.path(DIR_REF, "depara_cnes_cs_revisar.csv")
          write_csv(filter(depara, revisar), rev_path)
          message("  ATENÇÃO: ", n_revisar, " CS com similaridade < 0,70.")
          message("  Revisar manualmente: ", rev_path)
        }
        message("  De-para salva: ", depara_path)

      } else {
        message("  Colunas CO_UNIDADE ou NO_RAZAO_SOCIAL não encontradas no ST.")
        message("  Colunas disponíveis: ", paste(names(st_raw), collapse = ", "))
      }
    }
  }
} else {
  message("\nDe-para já existe: ", depara_path)
}

depara <- if (file.exists(depara_path)) {
  read_csv(depara_path, show_col_types = FALSE)
} else {
  message("  De-para indisponível — CNES não terá mapeamento para nome_cs.")
  NULL
}

# ---------------------------------------------------------------------------
# 1c. Processa EQ → equipes ESF, eMulti, ESB por CS/competência
# ---------------------------------------------------------------------------

message("\nProcessando arquivos EQ...")

processar_eq <- function(arquivo) {
  comp <- comp_do_arquivo(arquivo)
  df   <- ler_dbc(arquivo)
  if (is.null(df)) return(NULL)

  # Inspeciona layout na primeira competência processada
  if (comp == comp_do_arquivo(arqs_eq[1])) {
    message("  Layout EQ — colunas: ", paste(names(df), collapse = ", "))
    message("  Valores únicos de TP_EQUIPE em BH:")
    tp_bh <- filtrar_bh(df)
    col_tp <- intersect(c("tp_equipe", "tipo_equipe", "tp_equipe_ab"), names(tp_bh))[1]
    if (!is.na(col_tp)) {
      message("    ", paste(sort(unique(tp_bh[[col_tp]])), collapse = ", "))
      message("  Confirme se ESF = {", paste(TP_ESF, collapse=","),
              "}, eMulti = {", paste(TP_EMULTI, collapse=","),
              "}, ESB = {", paste(TP_ESB, collapse=","),
              "} e ajuste as constantes no topo do script se necessário.")
    }
  }

  col_tp    <- intersect(c("tp_equipe", "tipo_equipe", "tp_equipe_ab"), names(df))[1]
  col_cnes  <- intersect(c("co_unidade", "cod_cnes", "cnes"),           names(df))[1]
  col_desatv <- intersect(
    c("dt_desativacao", "dt_desativ", "dt_desativac", "dta_desativacao"),
    names(df)
  )[1]

  if (is.na(col_tp) || is.na(col_cnes)) return(NULL)

  df_bh <- filtrar_bh(df) |>
    mutate(
      co_unidade = as.character(.data[[col_cnes]]),
      tp_equipe  = as.character(.data[[col_tp]])
    )

  # Mantém apenas equipes ativas (sem data de desativação preenchida)
  if (!is.na(col_desatv)) {
    df_bh <- df_bh |>
      filter(is.na(.data[[col_desatv]]) | .data[[col_desatv]] %in% c("", "0", NA))
  }

  df_bh |>
    group_by(co_unidade) |>
    summarise(
      n_esf    = sum(tp_equipe %in% TP_ESF,    na.rm = TRUE),
      n_emulti = sum(tp_equipe %in% TP_EMULTI, na.rm = TRUE),
      n_esb    = sum(tp_equipe %in% TP_ESB,    na.rm = TRUE),
      .groups  = "drop"
    ) |>
    mutate(competencia = comp)
}

eq_bh <- lapply(arqs_eq, processar_eq) |> bind_rows()
message("  EQ processado: ", nrow(eq_bh), " registros (CS × competência)")

# ---------------------------------------------------------------------------
# 1d. Processa PF → médicos, ACS, carga horária por CS/competência
# ---------------------------------------------------------------------------

message("\nProcessando arquivos PF...")

processar_pf <- function(arquivo) {
  comp <- comp_do_arquivo(arquivo)
  df   <- ler_dbc(arquivo)
  if (is.null(df)) return(NULL)

  if (comp == comp_do_arquivo(arqs_pf[1])) {
    message("  Layout PF — colunas: ", paste(names(df), collapse = ", "))
  }

  col_cnes    <- intersect(c("co_unidade", "cod_cnes", "cnes"),           names(df))[1]
  col_cbo     <- intersect(c("cbo", "co_cbo", "tp_cbo", "cbo_ocupacao"),  names(df))[1]
  col_ch_amb  <- intersect(c("ch_amb",    "carga_hora_amb",  "horas_amb"), names(df))[1]
  col_ch_hosp <- intersect(c("ch_hosp",   "carga_hora_hosp", "horas_hosp"),names(df))[1]
  col_ch_out  <- intersect(c("ch_outros", "carga_hora_out",  "horas_out"), names(df))[1]

  if (is.na(col_cnes) || is.na(col_cbo)) return(NULL)

  df_bh <- filtrar_bh(df) |>
    mutate(
      co_unidade = as.character(.data[[col_cnes]]),
      cbo        = as.character(.data[[col_cbo]]),
      ch_amb     = if (!is.na(col_ch_amb))  suppressWarnings(as.numeric(.data[[col_ch_amb]]))  else 0,
      ch_hosp    = if (!is.na(col_ch_hosp)) suppressWarnings(as.numeric(.data[[col_ch_hosp]])) else 0,
      ch_outros  = if (!is.na(col_ch_out))  suppressWarnings(as.numeric(.data[[col_ch_out]]))  else 0,
      ch_total   = replace_na(ch_amb, 0) + replace_na(ch_hosp, 0) + replace_na(ch_outros, 0),
      is_medico  = str_detect(cbo, CBO_MED_REGEX),
      is_mfc     = cbo == CBO_MFC,
      is_acs     = cbo == CBO_ACS
    )

  df_bh |>
    group_by(co_unidade) |>
    summarise(
      n_medicos       = sum(is_medico,             na.rm = TRUE),
      n_mfc           = sum(is_mfc,                na.rm = TRUE),
      n_acs           = sum(is_acs,                na.rm = TRUE),
      ch_medica_total = sum(ch_total * is_medico,  na.rm = TRUE),
      .groups         = "drop"
    ) |>
    mutate(
      pct_mfc     = ifelse(n_medicos > 0, round(n_mfc / n_medicos * 100, 1), NA_real_),
      competencia = comp
    )
}

pf_bh <- lapply(arqs_pf, processar_pf) |> bind_rows()
message("  PF processado: ", nrow(pf_bh), " registros (CS × competência)")

# ---------------------------------------------------------------------------
# 1e. Une EQ + PF e mapeia CO_UNIDADE → nome_cs via de-para
# ---------------------------------------------------------------------------

cnes_bh <- full_join(eq_bh, pf_bh, by = c("co_unidade", "competencia")) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))

if (!is.null(depara) && "co_unidade" %in% names(depara)) {
  cnes_bh <- cnes_bh |>
    left_join(
      depara |> select(nome_cs, co_unidade = co_unidade, any_of("cod_smsa")),
      by = "co_unidade"
    )
} else {
  cnes_bh$nome_cs <- NA_character_
}

n_mapeados <- sum(!is.na(cnes_bh$nome_cs))
message("  CNES combinado: ", nrow(cnes_bh), " registros; ",
        n_mapeados, " (", round(n_mapeados / nrow(cnes_bh) * 100, 1), "%) com nome_cs.")

# =============================================================================
# SEÇÃO 2 — e-Gestor AB (cobertura APS — nível municipal)
# =============================================================================

message("\n========================================")
message("SEÇÃO 2: e-Gestor AB — Cobertura APS")
message("========================================")
message("LIMITAÇÃO: dados disponíveis apenas em nível municipal via portal público.")
message("O script atribui os valores de BH a todos os 153 CS (variável de nível 2).")

egestor_bh <- NULL

cobertura_xlsx <- file.path(DIR_REF, "egestor_cobertura_bh.xlsx")
cobertura_csv  <- file.path(DIR_REF, "egestor_cobertura_bh.csv")

if (file.exists(cobertura_xlsx)) {
  # ── Opção A: planilha baixada manualmente ──────────────────────────────────
  # Baixe em: https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/
  # → Relatório Histórico de Cobertura AB
  # → Município: Belo Horizonte | Competência: 01/2023 a 12/2025
  # → Exportar → Excel → Salvar como data/ref/egestor_cobertura_bh.xlsx
  message("\nCarregando arquivo manual do e-Gestor AB: ", cobertura_xlsx)
  egestor_raw <- tryCatch(
    read_xlsx(cobertura_xlsx, skip = 1),
    error = function(e) { message("  Erro ao ler Excel: ", conditionMessage(e)); NULL }
  )
  if (!is.null(egestor_raw)) {
    names(egestor_raw) <- tolower(str_replace_all(names(egestor_raw), "[^a-z0-9]", "_"))
    # Renomeia colunas pelo padrão mais comum do e-Gestor (ajustar se necessário)
    col_comp <- grep("compet",              names(egestor_raw), value = TRUE)[1]
    col_cob  <- grep("cob|perc|cobert",     names(egestor_raw), value = TRUE)[1]
    col_pop  <- grep("pop.*ref|populac",    names(egestor_raw), value = TRUE)[1]
    col_eq   <- grep("equip.*fin|n.*equip", names(egestor_raw), value = TRUE)[1]

    egestor_bh <- egestor_raw |>
      mutate(
        competencia = str_replace(
          as.character(if (!is.na(col_comp)) .data[[col_comp]] else ""),
          "(\\d{2})/(20)(\\d{2})", "\\3\\1"  # MM/AAAA → AAMM
        ),
        cobertura_aps_pct     = if (!is.na(col_cob)) as.numeric(.data[[col_cob]]) else NA_real_,
        populacao_referencia  = if (!is.na(col_pop)) as.numeric(.data[[col_pop]]) else NA_real_,
        n_equipes_financiadas = if (!is.na(col_eq))  as.numeric(.data[[col_eq]])  else NA_real_,
        fonte_egestor         = "eGestor_manual",
        data_extracao_egestor = format(Sys.Date())
      ) |>
      select(competencia, cobertura_aps_pct, populacao_referencia,
             n_equipes_financiadas, fonte_egestor, data_extracao_egestor) |>
      filter(!is.na(competencia), competencia != "")

    write_csv(egestor_bh, cobertura_csv)
    message("  e-Gestor carregado: ", nrow(egestor_bh), " competências.")
  }

} else if (file.exists(cobertura_csv)) {
  egestor_bh <- read_csv(cobertura_csv, show_col_types = FALSE)
  message("\ne-Gestor AB carregado do CSV em cache: ", nrow(egestor_bh), " registros.")

} else {
  # ── Opção B: placeholder — download manual necessário ─────────────────────
  message("\n  ATENÇÃO: dados do e-Gestor AB não encontrados.")
  message("  Para obtê-los:")
  message("  1. Acesse: https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/")
  message("  2. Clique em 'Relatório Histórico de Cobertura AB'")
  message("  3. Filtros: Belo Horizonte | Jan/2023 → Dez/2025")
  message("  4. Exporte como Excel e salve em: data/ref/egestor_cobertura_bh.xlsx")
  message("  5. Reexecute este script.")
  message("  Variáveis do e-Gestor serão preenchidas com NA por enquanto.")

  egestor_bh <- tibble(
    competencia           = COMPETENCIAS,
    cobertura_aps_pct     = NA_real_,
    populacao_referencia  = NA_real_,
    n_equipes_financiadas = NA_real_,
    fonte_egestor         = "eGestor_pendente",
    data_extracao_egestor = format(Sys.Date())
  )
}

# =============================================================================
# SEÇÃO 3 — IBGE Censo 2022 via censobr + geobr
# =============================================================================

message("\n========================================")
message("SEÇÃO 3: IBGE Censo 2022 — censobr")
message("========================================")

censo_path <- file.path(DIR_REF, "censo2022_cs_bh.csv")

if (file.exists(censo_path)) {
  message("Cache encontrado. Carregando: ", censo_path)
  censo_cs <- read_csv(censo_path, show_col_types = FALSE)

} else {

  message("Baixando geometria dos setores censitários de BH via geobr...")
  message("(Primeira execução demora vários minutos — dados são cacheados.)")

  geo_setores <- tryCatch(
    geobr::read_census_tract(code_muni = 3106200, year = 2022, showProgress = FALSE),
    error = function(e) {
      message("  geobr falhou: ", conditionMessage(e))
      NULL
    }
  )

  message("Baixando dados Básicos dos setores via censobr...")
  dados_basico <- tryCatch(
    censobr::read_tracts(year = 2022, dataset = "Basico", showProgress = FALSE) |>
      filter(str_starts(code_muni, COD_BH_7)),
    error = function(e) {
      message("  censobr::read_tracts('Basico') falhou: ", conditionMessage(e))
      NULL
    }
  )

  message("Baixando dados de Domicílios (saneamento) via censobr...")
  dados_dom <- tryCatch(
    censobr::read_tracts(year = 2022, dataset = "Domicilio01", showProgress = FALSE) |>
      filter(str_starts(code_muni, COD_BH_7)),
    error = function(e) {
      message("  censobr::read_tracts('Domicilio01') falhou: ", conditionMessage(e))
      NULL
    }
  )

  message("Baixando dados de Responsáveis (renda) via censobr...")
  dados_resp <- tryCatch(
    censobr::read_tracts(year = 2022, dataset = "Responsavel", showProgress = FALSE) |>
      filter(str_starts(code_muni, COD_BH_7)),
    error = function(e) {
      message("  censobr::read_tracts('Responsavel') falhou: ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(geo_setores) && !is.null(dados_basico)) {

    message("  Setores BH: geo=", nrow(geo_setores), " | Basico=", nrow(dados_basico))
    message("  Colunas Basico (primeiras 20): ",
            paste(head(names(dados_basico), 20), collapse = ", "))

    # ── Variáveis-chave do Censo 2022 ─────────────────────────────────────
    # Consulte o dicionário completo em:
    # ftp://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Documentacao/
    #
    # Basico (por setor):
    #   V001 = pop. em domicílios particulares permanentes ocupados
    #   V002 = pop. em domicílios particulares improvisados
    #   V003 = pop. em domicílios coletivos
    # Grupos etários estão nos arquivos de Pessoa ou em Basico (V005 a V077+)
    # A estrutura exata depende da versão do censobr — inspecione names(dados_basico)
    #
    # Abaixo, tentamos detectar as colunas automaticamente:

    col_pop   <- intersect(c("V001", "v001", "pop_total"), names(dados_basico))[1]
    # Idosos ≥60: soma de faixas etárias; colunas variam — ajuste após inspeção
    # Exemplo: V049 a V065 (grupos quinquenais de 60+) — verificar dicionário
    cols_idosos   <- grep("^[Vv](04[9]|05[0-9]|06[0-5])$", names(dados_basico), value = TRUE)
    # Crianças <5: V006 e V007 (0-1 e 1-4 anos) — verificar dicionário
    cols_criancas <- grep("^[Vv](006|007)$", names(dados_basico), value = TRUE)

    basico_proc <- dados_basico |>
      as_tibble() |>
      mutate(
        pop_total    = if (!is.na(col_pop)) as.numeric(.data[[col_pop]]) else NA_real_,
        pop_idosos   = if (length(cols_idosos) > 0)
          rowSums(across(all_of(cols_idosos), as.numeric), na.rm = TRUE) else NA_real_,
        pop_criancas = if (length(cols_criancas) > 0)
          rowSums(across(all_of(cols_criancas), as.numeric), na.rm = TRUE) else NA_real_,
        pct_idosos   = ifelse(pop_total > 0, pop_idosos   / pop_total * 100, NA_real_),
        pct_criancas = ifelse(pop_total > 0, pop_criancas / pop_total * 100, NA_real_)
      ) |>
      select(code_tract, pop_total, pct_idosos, pct_criancas)

    # ── Saneamento (Domicilio01) ───────────────────────────────────────────
    # Domicílios sem saneamento adequado: sem água de rede geral ou sem
    # esgoto via rede coletora/fossa séptica.
    # Colunas exatas: inspecione names(dados_dom) e consulte o dicionário.
    if (!is.null(dados_dom)) {
      message("  Colunas Domicilio01 (primeiras 20): ",
              paste(head(names(dados_dom), 20), collapse = ", "))
      col_dom_tot     <- intersect(c("V001","v001","dom_total"), names(dados_dom))[1]
      # Sem abastecimento adequado (ex.: V006 = outras formas de abastecimento)
      col_sem_agua    <- grep("^[Vv]00[5-9]$|^[Vv]0[1-9][0-9]$",
                               names(dados_dom), value = TRUE)[1]
      col_sem_esgoto  <- grep("sem.*esgo|^[Vv]01[5-9]$",
                               names(dados_dom), value = TRUE, ignore.case = TRUE)[1]

      dom_proc <- dados_dom |>
        as_tibble() |>
        mutate(
          dom_total      = if (!is.na(col_dom_tot))    as.numeric(.data[[col_dom_tot]])    else NA_real_,
          dom_sem_agua   = if (!is.na(col_sem_agua))   as.numeric(.data[[col_sem_agua]])   else NA_real_,
          dom_sem_esgoto = if (!is.na(col_sem_esgoto)) as.numeric(.data[[col_sem_esgoto]]) else NA_real_,
          pct_sem_saneam = ifelse(dom_total > 0,
            rowMeans(cbind(
              replace_na(dom_sem_agua,   0) / pmax(dom_total, 1),
              replace_na(dom_sem_esgoto, 0) / pmax(dom_total, 1)
            ), na.rm = TRUE) * 100,
            NA_real_)
        ) |>
        select(code_tract, pct_sem_saneam)

      basico_proc <- left_join(basico_proc, dom_proc, by = "code_tract")
    } else {
      basico_proc$pct_sem_saneam <- NA_real_
    }

    # ── Renda (Responsavel) ───────────────────────────────────────────────
    # V005 = Valor do rendimento nominal médio mensal per capita
    # dos domicílios particulares permanentes ocupados (R$)
    if (!is.null(dados_resp)) {
      message("  Colunas Responsavel (primeiras 20): ",
              paste(head(names(dados_resp), 20), collapse = ", "))
      col_renda <- intersect(c("V005", "v005", "renda_media", "renda_nom_med"), names(dados_resp))[1]

      if (!is.na(col_renda)) {
        renda_proc <- dados_resp |>
          as_tibble() |>
          mutate(renda_media = suppressWarnings(as.numeric(.data[[col_renda]]))) |>
          select(code_tract, renda_media)
        basico_proc <- left_join(basico_proc, renda_proc, by = "code_tract")
      } else {
        basico_proc$renda_media <- NA_real_
      }
    } else {
      basico_proc$renda_media <- NA_real_
    }

    # ── Cruzamento espacial: setor × polígono CS ──────────────────────────
    message("  Cruzando setores censitários com polígonos dos 153 CS...")

    # Garante CRS compatível (WGS84)
    geo_setores_wgs  <- st_transform(geo_setores, 4326)
    poligonos_load   <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)
    poligonos_wgs    <- st_transform(poligonos_load, 4326)

    # Centróides dos setores para join ponto × polígono (mais estável)
    centroides_sf <- geo_setores_wgs |>
      select(code_tract) |>
      st_centroid() |>
      left_join(basico_proc, by = "code_tract")

    join_cs <- st_join(centroides_sf, poligonos_wgs |> select(nome_cs), left = FALSE)

    # Agrega por CS (média ponderada pela população do setor)
    censo_cs <- join_cs |>
      st_drop_geometry() |>
      group_by(nome_cs) |>
      summarise(
        pop_total_censo    = sum(pop_total,      na.rm = TRUE),
        pct_idosos         = weighted.mean(pct_idosos,    pop_total, na.rm = TRUE),
        pct_criancas       = weighted.mean(pct_criancas,  pop_total, na.rm = TRUE),
        pct_sem_saneamento = weighted.mean(pct_sem_saneam,pop_total, na.rm = TRUE),
        renda_media        = weighted.mean(renda_media,   pop_total, na.rm = TRUE),
        n_setores          = n(),
        .groups            = "drop"
      ) |>
      mutate(
        across(starts_with("pct_"), ~round(.x, 2)),
        renda_media       = round(renda_media, 2),
        fonte_censo       = "IBGE_Censo2022",
        data_extracao_censo = format(Sys.Date())
      )

    write_csv(censo_cs, censo_path)
    message("  Censo 2022 processado: ", nrow(censo_cs), " CS com dados censitários.")

  } else {
    message("  Não foi possível processar Censo 2022.")
    message("  Verifique: censobr >= 0.3.0 e geobr >= 1.8.0")
    message("  install.packages(c('censobr','geobr'))")

    poligonos_load <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)
    censo_cs <- tibble(
      nome_cs            = poligonos_load$nome_cs,
      pop_total_censo    = NA_real_,
      pct_idosos         = NA_real_,
      pct_criancas       = NA_real_,
      pct_sem_saneamento = NA_real_,
      renda_media        = NA_real_,
      n_setores          = NA_integer_,
      fonte_censo        = "IBGE_Censo2022_indisponivel",
      data_extracao_censo = format(Sys.Date())
    )
  }
}

# =============================================================================
# SEÇÃO 4 — Aglomerados Subnormais (Favelas) — IBGE 2022
# =============================================================================

message("\n========================================")
message("SEÇÃO 4: Aglomerados Subnormais (IBGE 2022)")
message("========================================")

favelas_path <- file.path(DIR_REF, "favelas_cs_bh.csv")

if (file.exists(favelas_path)) {
  message("Cache encontrado. Carregando: ", favelas_path)
  favelas_cs <- read_csv(favelas_path, show_col_types = FALSE)

} else {

  message("Baixando polígonos de Aglomerados Subnormais via geobr...")

  # geobr::read_urban_area() ou download direto do geoftp.ibge.gov.br
  agsn <- tryCatch({
    # Tenta geobr primeiro (função pode variar conforme versão)
    g <- tryCatch(
      geobr::read_urban_concentrations(year = 2022, showProgress = FALSE),
      error = function(e) NULL
    )
    if (is.null(g)) {
      g <- geobr::read_statistical_grid(code_grid = 3106200, year = 2022, showProgress = FALSE)
    }
    g
  }, error = function(e) NULL)

  if (is.null(agsn)) {
    # Fallback: download direto do FTP IBGE (Aglomerados Subnormais 2022)
    message("  geobr indisponível. Tentando FTP IBGE diretamente...")
    url_agsn <- paste0(
      "https://geoftp.ibge.gov.br/organizacao_do_territorio/",
      "estrutura_territorial/aglomerados_subnormais/",
      "agsn2022/aglomerados_subnormais_poligonos_2022.zip"
    )
    dest_zip <- file.path(DIR_RAW, "agsn2022.zip")
    tryCatch({
      download.file(url_agsn, dest_zip, mode = "wb", quiet = FALSE)
      unzip(dest_zip, exdir = file.path(DIR_RAW, "agsn2022"))
      shp <- list.files(
        file.path(DIR_RAW, "agsn2022"), pattern = "\\.shp$",
        full.names = TRUE, recursive = TRUE
      )[1]
      if (!is.na(shp)) agsn <- st_read(shp, quiet = TRUE)
    }, error = function(e) message("  FTP IBGE falhou: ", conditionMessage(e)))
  }

  poligonos_load <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)

  if (!is.null(agsn) && nrow(agsn) > 0) {
    # Filtra BH
    col_mun_agsn <- grep("muni|ibge|cod.*mun|co_mun", names(agsn),
                          ignore.case = TRUE, value = TRUE)[1]
    agsn_bh <- if (!is.na(col_mun_agsn)) {
      filter(agsn, str_starts(as.character(.data[[col_mun_agsn]]), str_sub(COD_BH_7, 1, 6)))
    } else {
      # Intersecção com bounding box de BH como fallback
      bbox_bh <- st_bbox(st_transform(poligonos_load, st_crs(agsn)))
      st_crop(agsn, bbox_bh)
    }

    message("  Aglomerados subnormais em BH: ", nrow(agsn_bh), " polígonos")

    if (nrow(agsn_bh) > 0) {
      # Projeção métrica para cálculo de área
      agsn_utm      <- st_transform(agsn_bh,      31983)  # SIRGAS 2000 UTM 23S
      poligonos_utm <- st_transform(poligonos_load, 31983)

      area_total_cs <- poligonos_utm |>
        mutate(area_cs_m2 = as.numeric(st_area(geometry))) |>
        st_drop_geometry() |>
        select(nome_cs, area_cs_m2)

      # Interseção: área de favela dentro de cada CS
      # Usa suppressWarnings para mensagens de geometria inválida
      intersec <- suppressWarnings(
        st_intersection(
          poligonos_utm |> select(nome_cs),
          st_union(agsn_utm)
        )
      )

      area_favela_cs <- intersec |>
        mutate(area_favela_m2 = as.numeric(st_area(geometry))) |>
        st_drop_geometry() |>
        group_by(nome_cs) |>
        summarise(area_favela_m2 = sum(area_favela_m2, na.rm = TRUE), .groups = "drop")

      favelas_cs <- area_total_cs |>
        left_join(area_favela_cs, by = "nome_cs") |>
        mutate(
          area_favela_m2  = replace_na(area_favela_m2, 0),
          pct_area_favela = round(area_favela_m2 / area_cs_m2 * 100, 2),
          fonte_favelas   = "IBGE_AGSN2022",
          data_extracao_favelas = format(Sys.Date())
        ) |>
        select(nome_cs, pct_area_favela, area_favela_m2, area_cs_m2,
               fonte_favelas, data_extracao_favelas)

      write_csv(favelas_cs, favelas_path)
      message("  Favelas processadas: ", nrow(favelas_cs), " CS.")

    } else {
      message("  Nenhum aglomerado subnormal encontrado para BH.")
      favelas_cs <- tibble(
        nome_cs = poligonos_load$nome_cs, pct_area_favela = 0,
        fonte_favelas = "IBGE_AGSN2022_zero", data_extracao_favelas = format(Sys.Date())
      )
      write_csv(favelas_cs, favelas_path)
    }

  } else {
    message("  Aglomerados subnormais não disponíveis. pct_area_favela = NA.")
    favelas_cs <- tibble(
      nome_cs = poligonos_load$nome_cs, pct_area_favela = NA_real_,
      fonte_favelas = "IBGE_AGSN2022_indisponivel", data_extracao_favelas = format(Sys.Date())
    )
    write_csv(favelas_cs, favelas_path)
  }
}

# =============================================================================
# SEÇÃO 5 — Combinação final e exportação
# =============================================================================

message("\n========================================")
message("SEÇÃO 5: Combinação e exportação")
message("========================================")

# Grade completa: todos os CS × todas as competências
poligonos_ref2 <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE) |>
  st_drop_geometry() |>
  select(nome_cs, any_of(c("regional", "cod_smsa"))) |>
  distinct()

grade <- expand.grid(
  nome_cs     = unique(na.omit(poligonos_ref2$nome_cs)),
  competencia = COMPETENCIAS,
  stringsAsFactors = FALSE
) |>
  as_tibble() |>
  mutate(
    ano_cmpt = as.integer(paste0("20", str_sub(competencia, 1, 2))),
    mes_cmpt = as.integer(str_sub(competencia, 3, 4))
  ) |>
  left_join(poligonos_ref2, by = "nome_cs")

message("Grade base: ", nrow(grade), " linhas (",
        n_distinct(grade$nome_cs), " CS × ",
        n_distinct(grade$competencia), " competências)")

# ── Junta CNES (mensal) ──────────────────────────────────────────────────────
if ("nome_cs" %in% names(cnes_bh)) {
  cnes_join <- cnes_bh |>
    select(nome_cs, competencia,
           any_of(c("n_esf", "n_emulti", "n_esb", "n_medicos", "n_mfc",
                    "n_acs", "ch_medica_total", "pct_mfc")))
  grade <- left_join(grade, cnes_join, by = c("nome_cs", "competencia"))
}

# ── Junta e-Gestor (mensal, nível municipal) ─────────────────────────────────
if (!is.null(egestor_bh) && "competencia" %in% names(egestor_bh)) {
  grade <- left_join(grade,
    egestor_bh |> select(competencia, cobertura_aps_pct,
                          populacao_referencia, n_equipes_financiadas,
                          any_of(c("fonte_egestor", "data_extracao_egestor"))),
    by = "competencia"
  )
}

# ── Junta Censo 2022 (estático) ──────────────────────────────────────────────
if (exists("censo_cs") && !is.null(censo_cs)) {
  grade <- left_join(grade,
    censo_cs |> select(nome_cs, pop_total_censo, pct_idosos, pct_criancas,
                        pct_sem_saneamento, renda_media, n_setores,
                        any_of(c("fonte_censo", "data_extracao_censo"))),
    by = "nome_cs"
  )
}

# ── Junta Favelas (estático) ─────────────────────────────────────────────────
if (exists("favelas_cs") && !is.null(favelas_cs)) {
  grade <- left_join(grade,
    favelas_cs |> select(nome_cs, pct_area_favela,
                          any_of(c("fonte_favelas", "data_extracao_favelas"))),
    by = "nome_cs"
  )
}

# ── Metadados de extração ────────────────────────────────────────────────────
grade <- grade |>
  mutate(data_extracao_cnes = format(Sys.Date()))

# ── Ordena colunas ───────────────────────────────────────────────────────────
grade <- grade |>
  select(
    # Chaves
    nome_cs, competencia, ano_cmpt, mes_cmpt,
    any_of("cod_smsa"), any_of("regional"),
    # CNES — Equipes
    any_of(c("n_esf", "n_emulti", "n_esb")),
    # CNES — Profissionais
    any_of(c("n_medicos", "n_mfc", "pct_mfc", "n_acs", "ch_medica_total")),
    # e-Gestor AB
    any_of(c("cobertura_aps_pct", "populacao_referencia", "n_equipes_financiadas")),
    # Censo 2022
    any_of(c("pop_total_censo", "pct_idosos", "pct_criancas",
             "pct_sem_saneamento", "renda_media", "n_setores")),
    # Favelas
    any_of("pct_area_favela"),
    # Metadados
    everything()
  )

# ── Exporta ──────────────────────────────────────────────────────────────────
saida_path <- file.path(DIR_REF, "variaveis_cs.csv")
write_csv(grade, saida_path)

message("\n======================================")
message("CONCLUÍDO: ", saida_path)
message("Dimensões: ", nrow(grade), " × ", ncol(grade))
message("")
message("Cobertura por variável:")
vars_analise <- setdiff(
  names(grade),
  c("nome_cs", "competencia", "ano_cmpt", "mes_cmpt", "cod_smsa", "regional",
    grep("fonte|data_extra", names(grade), value = TRUE))
)
for (v in vars_analise) {
  pct <- round(mean(!is.na(grade[[v]])) * 100, 1)
  message("  ", str_pad(v, 22), " ", pct, "%")
}
message("======================================")
