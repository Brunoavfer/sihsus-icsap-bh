# =============================================================================
# 28_tabela_s6_sensibilidade_pico.R
# Tabela S6 — Análise de sensibilidade ITS: exclusão de meses de pico (abr/2024)
# 4 especificações: Completo / Sem abr/2024 / Sem mar-abr/2024 / Sem jan-abr/2024
# Saída: docs/tabela_s6_sensibilidade_pico.html
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(gt)
  library(tibble)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_DOCS <- "docs"

fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "<0,001",
    TRUE       ~ gsub("\\.", ",", sprintf("%.4f", p))
  )
}
fmt_num <- function(x, d=1) gsub("\\.", ",", sprintf(paste0("%+.", d, "f"), x))

raw <- read_csv(file.path(DIR_PROC, "sensibilidade_pico_resultados.csv"),
                show_col_types = FALSE)

tab_s6 <- raw |>
  mutate(
    col_apc_pre = sprintf("%s%% %s",
                          fmt_num(APC_pos + (APC_pos - APC_pos)),  # placeholder filled below
                          ""),
    col_apc_pos = sprintf("%s%% (%s; %s)%%",
                          fmt_num(APC_pos),
                          gsub("[^-+0-9,.]", "",
                               sub(".*\\((.+);.*", "\\1", IC95)),
                          gsub("[^-+0-9,.]", "",
                               sub(".+; (.+)\\)", "\\1", IC95))),
    col_p = fmt_p(p_beta3),
    col_phi = gsub("\\.", ",", sprintf("%.3f", phi_AR1)),
    col_n = as.character(n_meses)
  )

# Recompute cleanly from raw columns
tab_s6_clean <- raw |>
  mutate(
    Especificação = dplyr::case_when(
      Especificação == "Completo"         ~ "Modelo base (n=51 meses)",
      Especificação == "Sem abr/2024"     ~ "Sem abr/2024 (n=50 meses)",
      Especificação == "Sem mar-abr/2024" ~ "Sem mar–abr/2024 (n=49 meses)",
      Especificação == "Sem jan-abr/2024" ~ "Sem jan–abr/2024 (n=47 meses)",
      TRUE ~ Especificação
    ),
    ic_lo  = as.numeric(sub("\\((.+);.*", "\\1", IC95)),
    ic_hi  = as.numeric(sub(".+; (.+)\\)", "\\1", IC95)),
    col_apc_pos = sprintf("%s%% (%s; %s)",
                          gsub("\\.", ",", sprintf("%+.1f", APC_pos)),
                          gsub("\\.", ",", sprintf("%+.1f", ic_lo)),
                          gsub("\\.", ",", sprintf("%+.1f", ic_hi))),
    col_p   = dplyr::case_when(
      p_beta3 < 0.001 ~ "<0,001",
      p_beta3 < 0.01  ~ gsub("\\.", ",", sprintf("%.4f", p_beta3)),
      TRUE            ~ gsub("\\.", ",", sprintf("%.4f", p_beta3))
    ),
    col_phi = gsub("\\.", ",", sprintf("%.3f", phi_AR1)),
    causa = dplyr::case_when(
      Especificação == "Modelo base (n=51 meses)"     ~ "—",
      Especificação == "Sem abr/2024 (n=50 meses)"    ~ "Pico da epidemia de dengue 2024 (abr/2024: n=3.615, +3.570% vs 2023)",
      Especificação == "Sem mar–abr/2024 (n=49 meses)"~ "Início + pico da epidemia de dengue (mar/2024: n=3.128; abr/2024: n=3.615)",
      Especificação == "Sem jan–abr/2024 (n=47 meses)"~ "Período de alta circulação de dengue (jan–abr/2024: acima da média histórica)",
      TRUE ~ "—"
    ),
    sig = p_beta3 < 0.01,
    negativo = APC_pos < 0
  ) |>
  select(
    `Especificação do modelo`               = Especificação,
    `N meses`                               = n_meses,
    `Causa da exclusão`                     = causa,
    `APC pós — absoluta (%/ano, IC 95%)`    = col_apc_pos,
    `p(β₃)`                                 = col_p,
    `φ AR(1)`                               = col_phi,
    sig, negativo
  )

gt_s6 <- tab_s6_clean |>
  select(-sig, -negativo) |>
  gt() |>
  tab_header(
    title    = md("**Tabela S6.** Análise de sensibilidade ITS-GLS AR(1) — exclusão de meses de pico"),
    subtitle = paste0(
      "Intervenção: Portaria GM/MS 3.493/2024 (mai/2024; mes_num=29). ",
      "Série: jan/2022–mar/2026. Modelo: GLS AR(1) com sazonalidade (sin12/cos12). ",
      "IC 95% via método delta [Var(β₁+β₃) = Var(β₁)+Var(β₃)+2·Cov(β₁,β₃)]."
    )
  ) |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#1a5276"),
    locations = cells_body(
      rows    = `Especificação do modelo` == "Modelo base (n=51 meses)"
    )
  ) |>
  tab_style(
    style     = cell_fill(color = "#eafaf1"),
    locations = cells_body(
      rows    = `Especificação do modelo` != "Modelo base (n=51 meses)"
    )
  ) |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#c0392b"),
    locations = cells_body(
      columns = `p(β₃)`
    )
  ) |>
  cols_align(align = "center",
             columns = c(`N meses`, `APC pós — absoluta (%/ano, IC 95%)`, `p(β₃)`, `φ AR(1)`)) |>
  cols_align(align = "left",
             columns = c(`Especificação do modelo`, `Causa da exclusão`)) |>
  cols_width(`Causa da exclusão` ~ px(240)) |>
  tab_source_note(md(
    paste0(
      "**Contexto do pico:** O aumento atípico de internações em março–abril/2024 foi causado pela ",
      "epidemia de dengue de 2024 — a maior da história do Brasil (Grupo G04 da lista ICSAP: ",
      "+3.570% em abril/2024 vs média mensal de 2023). ",
      "Análises complementares (Script 23) excluindo o Grupo G04 de todos os meses produziram ",
      "APC pós = −6,15%/ano (IC 95%: −10,6; −1,4), cujo IC se sobrepõe ao das especificações ",
      "por exclusão de meses (−8,3% a −10,1%/ano), confirmando consistência entre as duas abordagens. ",
      "APC pós = (β₁+β₃)×12 = variação percentual anual líquida pós-intervenção (tendência pré + slope change). ",
      "β₃ = coeficiente de mudança de slope (tempo_pos): mede exclusivamente a diferença de inclinação pós vs. pré. ",
      "**Resultado-chave:** A APC pós permanece negativa e altamente significativa em todas as especificações ",
      "(−8,3% a −10,1%/ano). Valores exatos de p(β₃): modelo completo p=0,0003; modelos com exclusão p<0,0001 ",
      "(precisão numérica esgotada). A exclusão dos meses de pico estreita os ICs e reduz o φ AR(1) de ",
      "0,634 para 0,202, indicando que esses meses são a principal fonte de autocorrelação residual. ",
      "O efeito estimado da Portaria 3.493/2024 é robusto à epidemia de dengue: a dengue atenuou, ",
      "não criou, a queda pós-Portaria."
    )
  )) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) |>
  tab_options(
    table.font.size           = 12,
    data_row.padding          = px(6),
    column_labels.font.weight = "bold",
    source_notes.font.size    = 10,
    heading.subtitle.font.size = 11
  )

html_s6 <- file.path(DIR_DOCS, "tabela_s6_sensibilidade_pico.html")
gtsave(gt_s6, html_s6)
cat(sprintf("Tabela S6 salva: %s\n", html_s6))
