# =============================================================================
# 16_tabela1.R
#
# Tabela 1 — Características dos 153 Centros de Saúde de Belo Horizonte
#
# Inclui:
#   - Distribuição por regional administrativa (N, %)
#   - IVS predominante (N, %)
#   - Variáveis contínuas: % favela, n_esf médio, taxa ICSAP média
#     (mediana, IQR [P25–P75], mínimo, máximo)
#   - Estratificação por IVS (Baixo / Médio / Elevado / Muito Elevado)
#
# Fontes:
#   data/ref/variaveis_cs.csv    — variáveis independentes por CS×mês
#   data/ref/ivs_por_cs.csv      — IVS agregado por CS
#   data/processed/n_icsap_cs_mes_prop.csv  — n_icsap com alocação proporcional
#
# Saídas:
#   docs/tabela1.csv             — tabela em formato tidy
#   docs/tabela1_formatada.html  — tabela HTML formatada (gt)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(stringr)
})

DIR_REF  <- "data/ref"
DIR_PROC <- "data/processed"
DIR_DOCS <- "docs"

# =============================================================================
# 1. Carrega dados
# =============================================================================

variaveis <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                      show_col_types = FALSE)

ivs <- read_csv(file.path(DIR_REF, "ivs_por_cs.csv"),
                show_col_types = FALSE)

# n_icsap com alocação proporcional por CS×mês
n_icsap_prop <- tryCatch(
  read_csv(file.path(DIR_PROC, "n_icsap_cs_mes_prop.csv"),
           show_col_types = FALSE),
  error = function(e) {
    message("n_icsap_cs_mes_prop.csv não encontrado — usando icsap_bh_regional.csv")
    NULL
  }
)

regional_bh <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                        col_types = cols(ano_cmpt = col_integer(),
                                         mes_cmpt = col_integer()),
                        show_col_types = FALSE)

message("variaveis_cs.csv: ", nrow(variaveis), " obs (",
        length(unique(variaveis$nome_cs)), " CS × ", length(unique(variaveis$mes_cmpt)), " meses)")
message("ivs_por_cs.csv: ", nrow(ivs), " CS")

# =============================================================================
# 2. Taxa ICSAP média por CS (jan/2023–dez/2025 = 36 meses)
# =============================================================================

if (!is.null(n_icsap_prop)) {
  # Usa alocação proporcional (script 14)
  taxa_cs <- n_icsap_prop %>%
    filter(ano_cmpt >= 2023, ano_cmpt <= 2025) %>%
    group_by(nome_cs) %>%
    summarise(
      n_icsap_total = sum(n_icsap_prop, na.rm = TRUE),
      n_meses       = n(),
      .groups = "drop"
    )
} else {
  # Fallback: conta direto de icsap_bh_regional
  taxa_cs <- regional_bh %>%
    filter(!is.na(nome_cs), ano_cmpt >= 2023, ano_cmpt <= 2025) %>%
    count(nome_cs, ano_cmpt, mes_cmpt, name = "n_icsap") %>%
    group_by(nome_cs) %>%
    summarise(
      n_icsap_total = sum(n_icsap, na.rm = TRUE),
      n_meses       = n(),
      .groups = "drop"
    )
}

# Variáveis por CS (agrega mês-a-mês com média de colunas fixas no tempo)
variaveis_cs <- variaveis %>%
  filter(ano_cmpt >= 2023, ano_cmpt <= 2025) %>%
  group_by(nome_cs, regional) %>%
  summarise(
    pop_media        = mean(as.numeric(populacao_referencia), na.rm = TRUE),
    n_esf_medio      = mean(as.numeric(n_esf), na.rm = TRUE),
    pct_favela       = mean(as.numeric(pct_area_favela), na.rm = TRUE),
    pct_sem_sanea    = mean(as.numeric(pct_sem_saneamento), na.rm = TRUE),
    renda_media      = mean(as.numeric(renda_media), na.rm = TRUE),
    n_meses_var      = n(),
    .groups = "drop"
  )

# Une taxa ICSAP
cs_completo <- variaveis_cs %>%
  left_join(taxa_cs, by = "nome_cs") %>%
  left_join(ivs %>% select(nome_cs, ivs_score, ivs_predominante,
                            pct_baixo, pct_medio, pct_elevado, pct_muito_elevado),
            by = "nome_cs") %>%
  mutate(
    taxa_icsap_mes = if_else(
      !is.na(n_icsap_total) & !is.na(pop_media) & pop_media > 0,
      n_icsap_total / (pop_media * 3) * 10000,   # 3 anos × pop = 36 meses
      NA_real_
    ),
    ivs_cat = factor(ivs_predominante,
                     levels = c("Baixo", "Médio", "Elevado", "Muito Elevado"))
  )

n_cs_total <- nrow(cs_completo)
message("CS na tabela final: ", n_cs_total)

# =============================================================================
# 3. Funções de estatística descritiva
# =============================================================================

resumo_continua <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(rep(NA_real_, 6))
  # unname() evita que quantile() crie nomes compostos (ex. "p25.25%")
  c(
    media   = round(mean(x), digits),
    mediana = round(median(x), digits),
    p25     = round(unname(quantile(x, 0.25)), digits),
    p75     = round(unname(quantile(x, 0.75)), digits),
    minimo  = round(min(x), digits),
    maximo  = round(max(x), digits)
  )
}

fmt_mediana_iqr <- function(x, digits = 1) {
  r <- resumo_continua(x, digits)
  if (anyNA(r[c("mediana", "p25", "p75")])) return(NA_character_)
  sprintf("%.*f [%.*f–%.*f]", digits, r["mediana"], digits, r["p25"], digits, r["p75"])
}

# =============================================================================
# 4. Monta tabela 1
# =============================================================================

# --- Bloco 1: distribuição por regional ---
tab_regional <- cs_completo %>%
  count(regional, name = "n") %>%
  arrange(regional) %>%
  mutate(
    pct      = round(n / n_cs_total * 100, 1),
    variavel = "Regional",
    categoria = regional,
    n_pct    = sprintf("%d (%.1f%%)", n, pct)
  ) %>%
  select(variavel, categoria, n, pct, n_pct)

# --- Bloco 2: distribuição por IVS predominante ---
tab_ivs_cat <- cs_completo %>%
  count(ivs_cat, name = "n") %>%
  filter(!is.na(ivs_cat)) %>%
  mutate(
    pct      = round(n / n_cs_total * 100, 1),
    variavel = "IVS predominante",
    categoria = as.character(ivs_cat),
    n_pct    = sprintf("%d (%.1f%%)", n, pct)
  ) %>%
  select(variavel, categoria, n, pct, n_pct)

# --- Bloco 3: variáveis contínuas (total e por estrato IVS) ---

variaveis_cont <- list(
  list(col = "n_esf_medio",   label = "Equipes ESF (n médio)",          digits = 1),
  list(col = "pct_favela",    label = "Área de favela (%)",              digits = 1),
  list(col = "pct_sem_sanea", label = "Domicílios sem rede geral (%)",   digits = 1),
  list(col = "renda_media",   label = "Renda per capita (SM)",           digits = 2),
  list(col = "ivs_score",     label = "IVS score (1–4)",                 digits = 2),
  list(col = "taxa_icsap_mes",label = "Taxa ICSAP (por 10.000 hab/mês)", digits = 2)
)

# Total
tab_cont_total <- bind_rows(lapply(variaveis_cont, function(v) {
  x <- cs_completo[[v$col]]
  r <- resumo_continua(x, v$digits)
  tibble(
    variavel  = v$label,
    categoria = "Total",
    n         = sum(!is.na(x)),
    mediana   = r["mediana"],
    p25       = r["p25"],
    p75       = r["p75"],
    minimo    = r["minimo"],
    maximo    = r["maximo"],
    mediana_iqr = fmt_mediana_iqr(x, v$digits)
  )
}))

# Por estrato IVS
niveis_ivs <- c("Baixo", "Médio", "Elevado", "Muito Elevado")
tab_cont_ivs <- bind_rows(lapply(niveis_ivs, function(niv) {
  df_niv <- cs_completo %>% filter(ivs_predominante == niv)
  bind_rows(lapply(variaveis_cont, function(v) {
    x <- df_niv[[v$col]]
    r <- resumo_continua(x, v$digits)
    tibble(
      variavel    = v$label,
      categoria   = niv,
      n           = sum(!is.na(x)),
      mediana     = r["mediana"],
      p25         = r["p25"],
      p75         = r["p75"],
      minimo      = r["minimo"],
      maximo      = r["maximo"],
      mediana_iqr = fmt_mediana_iqr(x, v$digits)
    )
  }))
}))

tab_cont <- bind_rows(tab_cont_total, tab_cont_ivs)

# =============================================================================
# 5. Tabela final em formato amplo (variáveis × colunas)
# =============================================================================

tab_wide <- tab_cont %>%
  select(variavel, categoria, mediana_iqr) %>%
  pivot_wider(names_from = categoria, values_from = mediana_iqr) %>%
  select(variavel, Total, all_of(niveis_ivs))

message("\n=== TABELA 1 — VARIÁVEIS CONTÍNUAS (mediana [IQR]) ===")
print(tab_wide, n = Inf)

message("\n=== DISTRIBUIÇÃO POR REGIONAL ===")
print(tab_regional %>% select(categoria, n_pct), n = Inf)

message("\n=== DISTRIBUIÇÃO POR IVS ===")
print(tab_ivs_cat %>% select(categoria, n_pct), n = Inf)

# =============================================================================
# 6. Salva CSV tidy
# =============================================================================

# CSV tidy com todas as estatísticas
tab_tidy <- bind_rows(
  tab_regional %>%
    mutate(tipo = "categorica") %>%
    select(tipo, variavel, categoria, n, pct, n_pct),
  tab_ivs_cat %>%
    mutate(tipo = "categorica") %>%
    select(tipo, variavel, categoria, n, pct, n_pct),
  tab_cont %>%
    mutate(tipo = "continua", pct = NA_real_, n_pct = mediana_iqr) %>%
    select(tipo, variavel, categoria, n, pct, n_pct, mediana, p25, p75, minimo, maximo)
)

write_csv(tab_tidy, file.path(DIR_DOCS, "tabela1.csv"))
message("\nTabela salva: docs/tabela1.csv")

# =============================================================================
# 7. HTML formatado (gt)
# =============================================================================

# n_niveis calculado aqui para ficar disponível também na seção 8
n_niveis <- sapply(niveis_ivs, function(niv)
  sum(cs_completo$ivs_predominante == niv, na.rm = TRUE))

if (requireNamespace("gt", quietly = TRUE)) {
  library(gt)

  n_total <- n_cs_total
  colnames_gt <- c(
    "Variável",
    sprintf("Total\n(N=%d)", n_total),
    sprintf("Baixo\n(N=%d)", n_niveis["Baixo"]),
    sprintf("Médio\n(N=%d)", n_niveis["Médio"]),
    sprintf("Elevado\n(N=%d)", n_niveis["Elevado"]),
    sprintf("Muito Elevado\n(N=%d)", n_niveis["Muito Elevado"])
  )

  tab_html <- tab_wide %>%
    rename_with(~ colnames_gt) %>%
    gt() %>%
    tab_header(
      title    = md("**Tabela 1.** Características dos 153 Centros de Saúde de Belo Horizonte, estratificadas por Índice de Vulnerabilidade Social (IVS)"),
      subtitle = md("Dados: SIHSUS/DATASUS (jan/2023–dez/2025), Censo IBGE 2022, CNES, IVS-BH (SMSA/PBH)")
    ) %>%
    tab_spanner(
      label   = "IVS Predominante",
      columns = 3:6
    ) %>%
    tab_footnote(
      footnote = "Mediana [Intervalo interquartil P25–P75]. SM = salários mínimos. ICSAP = Internações por Condições Sensíveis à Atenção Primária.",
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_options(
      table.font.size     = px(12),
      heading.align       = "left",
      column_labels.border.top.width    = px(2),
      column_labels.border.bottom.width = px(1),
      table_body.border.bottom.width    = px(2)
    )

  gt::gtsave(tab_html, file.path(DIR_DOCS, "tabela1_formatada.html"))
  message("HTML salvo: docs/tabela1_formatada.html")
} else {
  message("Pacote 'gt' não instalado — tabela HTML não gerada.")
  message("Instale com: install.packages('gt')")
}

# =============================================================================
# 8. Resumo descritivo final
# =============================================================================

message("\n======================================")
message("TABELA 1 CONCLUÍDA")
message(sprintf("  %d Centros de Saúde em %d regionais", n_cs_total,
                length(unique(cs_completo$regional[!is.na(cs_completo$regional)]))))
message("  IVS: Baixo=", n_niveis["Baixo"],
        " | Médio=", n_niveis["Médio"],
        " | Elevado=", n_niveis["Elevado"],
        " | Muito Elevado=", n_niveis["Muito Elevado"])
message(sprintf("  Taxa ICSAP: mediana=%s por 10.000 hab/mês",
                fmt_mediana_iqr(cs_completo$taxa_icsap_mes, 2)))
message("Saídas:")
message("  docs/tabela1.csv")
message("  docs/tabela1_formatada.html")
message("======================================")
