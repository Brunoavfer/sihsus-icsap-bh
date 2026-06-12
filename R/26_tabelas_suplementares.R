# =============================================================================
# 26_tabelas_suplementares.R
#
# Gera Tabela S4 (ITS por grupo clínico — duas análises) e
# Tabela S5 (sensibilidade saneamento por densidade real hab/km²)
#
# Lê CSVs gerados pelos scripts 25 e 24.
#
# Saídas:
#   docs/tabela_s4_its_grupos.html + data/processed/tabela_s4_its_grupos.csv
#   docs/tabela_s5_saneamento_estratificado.html +
#     data/processed/tabela_s5_saneamento_estratificado.csv
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(gt)
  library(stringr)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_DOCS <- "docs"

# helpers de formatação brasileira
fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "<0,001",
    TRUE       ~ gsub("\\.", ",", sprintf("%.3f", p))
  )
}
fmt_irr <- function(x, digits = 3) gsub("\\.", ",", sprintf(paste0("%.", digits, "f"), x))
fmt_num <- function(x, digits = 1) gsub("\\.", ",", sprintf(paste0("%+.", digits, "f"), x))

# =============================================================================
# TABELA S4 — ITS por grupo clínico (duas análises)
# =============================================================================

message("=== Tabela S4 — ITS por grupo clínico (tabela unificada) ===")

its_abs  <- read_csv(file.path(DIR_PROC, "its_por_grupo.csv"),      show_col_types = FALSE)
its_prop <- read_csv(file.path(DIR_PROC, "its_por_grupo_prop.csv"), show_col_types = FALSE)

n_total <- its_abs$n_icsap_tot[its_abs$grupo == "Todos"]

tab_s4 <- its_abs |>
  filter(grupo != "Todos") |>
  left_join(
    its_prop |> select(grupo,
                       apc_pos_prop     = apc_pos,
                       apc_pos_inf_prop = apc_pos_inf,
                       apc_pos_sup_prop = apc_pos_sup,
                       p_pos_prop       = p_pos),
    by = "grupo"
  ) |>
  mutate(
    grupo_label = case_when(
      grupo == "09" ~ "Grupo 09 — Cardiovascular",
      grupo == "10" ~ "Grupo 10 — Respiratório",
      grupo == "13" ~ "Grupo 13 — Diabetes mellitus"
    ),
    subgrupos = case_when(
      grupo == "09" ~ "HAS, angina, insuficiência cardíaca, AVC",
      grupo == "10" ~ "IVAS, pneumonias, bronquite aguda, asma",
      grupo == "13" ~ "Diabetes mellitus (todas as formas)"
    ),
    n_pct = sprintf("%s (%s%%)",
                    format(n_icsap_tot, big.mark = ","),
                    gsub("\\.", ",", sprintf("%.1f", 100 * n_icsap_tot / n_total))),
    col_apc_abs  = sprintf("%s%% (IC: %s; %s)",
                           fmt_num(apc_pos), fmt_num(apc_pos_inf), fmt_num(apc_pos_sup)),
    col_p_abs    = fmt_p(p_pos),
    col_apc_prop = sprintf("%s pp/ano (IC: %s; %s)",
                           fmt_num(apc_pos_prop, 2),
                           fmt_num(apc_pos_inf_prop, 2),
                           fmt_num(apc_pos_sup_prop, 2)),
    col_p_prop   = fmt_p(p_pos_prop),
    interpretacao = case_when(
      grupo == "09" ~ "Queda absoluta NS; proporção aumentou (queda < média total — grupo caiu menos que os demais)",
      grupo == "10" ~ "Queda absoluta NS; proporção aumentou (queda < média total — grupo caiu menos que os demais)",
      grupo == "13" ~ "Queda absoluta significativa; proporção estável (queda proporcional ao total — sem efeito seletivo)"
    )
  ) |>
  select(
    `Grupo ICSAP`                          = grupo_label,
    `CIDs incluídos`                       = subgrupos,
    `n (% do total)`                       = n_pct,
    `APC pós — absoluta (%/ano, IC 95%)`   = col_apc_abs,
    `p (absoluta)`                         = col_p_abs,
    `APC pós — proporção (pp/ano, IC 95%)` = col_apc_prop,
    `p (proporcional)`                     = col_p_prop,
    `Interpretação`                        = interpretacao
  )

csv_s4 <- bind_rows(
  its_abs  |> filter(grupo != "Todos") |> mutate(analise = "absoluta"),
  its_prop |> mutate(analise = "proporcional")
)
write_csv(csv_s4, file.path(DIR_PROC, "tabela_s4_its_grupos.csv"))
message(sprintf("  CSV salvo: %s", file.path(DIR_PROC, "tabela_s4_its_grupos.csv")))

gt_s4 <- tab_s4 |>
  gt() |>
  tab_header(
    title    = md("**Tabela S4.** ITS GLS AR(1) — Efeito da Portaria 3.493/2024 por grupo clínico ICSAP"),
    subtitle = "BH municipal, jan/2022–mar/2026 (51 meses); intervenção: maio/2024; IC 95% via método delta"
  ) |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#c0392b"),
    locations = cells_body(
      rows    = `Grupo ICSAP` == "Grupo 13 — Diabetes mellitus",
      columns = c(`APC pós — absoluta (%/ano, IC 95%)`, `p (absoluta)`)
    )
  ) |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#1a5276"),
    locations = cells_body(
      rows    = `Grupo ICSAP` != "Grupo 13 — Diabetes mellitus",
      columns = c(`APC pós — proporção (pp/ano, IC 95%)`, `p (proporcional)`)
    )
  ) |>
  cols_align(align = "center",
             columns = c(`n (% do total)`,
                         `APC pós — absoluta (%/ano, IC 95%)`, `p (absoluta)`,
                         `APC pós — proporção (pp/ano, IC 95%)`, `p (proporcional)`,
                         `Interpretação`)) |>
  cols_align(align = "left", columns = c(`Grupo ICSAP`, `CIDs incluídos`)) |>
  tab_source_note(md(
    paste0(
      "APC pós (absoluta) = (β₁+β₃)×12, modelo GLS log-linear; ",
      "IC 95% via método delta [Var(β₁+β₃) = Var(β₁)+Var(β₃)+2·Cov(β₁,β₃)]. ",
      "APC pós (proporção) = slope pós em modelo GLS linear com desfecho = ",
      "% do grupo no total ICSAP municipal mensal. ",
      "**Interpretação:** A análise por contagem absoluta reflete variações no volume total de ICSAP, ",
      "não necessariamente efeitos seletivos por grupo. ",
      "A análise proporcional testa se a composição das ICSAP mudou após a Portaria 3.493/2024. ",
      "O grupo **diabetes** — que apresentou a maior redução absoluta (−11,4%/ano; p<0,001) — ",
      "não mostrou mudança significativa na proporção relativa (p=0,571), ",
      "indicando ausência de efeito clínico seletivo da Portaria sobre esse grupo. ",
      "Grupos **cardiovascular** e **respiratório** apresentaram aumento relativo significativo ",
      "(p<0,01 e p=0,04), reflexo de queda absoluta menos pronunciada nesses grupos comparada ao total. ",
      "**Conclusão:** a Portaria 3.493/2024 reduziu o volume global de ICSAP de forma proporcional ",
      "entre os grupos clínicos analisados; não há evidência de efeito seletivo sobre nenhum grupo."
    )
  )) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) |>
  tab_options(table.font.size = 12, data_row.padding = px(5),
              column_labels.font.weight = "bold", source_notes.font.size = 10)

html_s4 <- file.path(DIR_DOCS, "tabela_s4_its_grupos.html")
gtsave(gt_s4, html_s4)
message(sprintf("  HTML salvo: %s", html_s4))

# =============================================================================
# TABELA S5 — Sensibilidade saneamento (densidade real hab/km²)
# =============================================================================

message("=== Tabela S5 — Sensibilidade saneamento ===")

san_df <- read_csv(file.path(DIR_PROC, "sensibilidade_saneamento.csv"),
                   show_col_types = FALSE)

# Detecta limites de densidade dos rótulos dos estratos
tab_s5_raw <- san_df |>
  mutate(
    estrato_fmt = case_when(
      str_detect(estrato, "^Modelo geral") ~ "Modelo geral (todos os 153 CS)",
      str_detect(estrato, "T1")            ~ estrato,
      str_detect(estrato, "T2")            ~ estrato,
      str_detect(estrato, "T3")            ~ estrato,
      TRUE                                 ~ estrato
    ),
    col_n_cs  = as.character(n_cs),
    col_irr   = fmt_irr(irr_san),
    col_ic    = sprintf("(%s; %s)", fmt_irr(irr_inf), fmt_irr(irr_sup)),
    col_p     = fmt_p(p_san),
    col_irr_ivs = if_else(
      !is.na(irr_ivs),
      sprintf("%s (%s)", fmt_irr(irr_ivs), fmt_p(p_ivs)),
      "—"
    ),
    sig = p_san < 0.05
  ) |>
  select(Estrato = estrato_fmt, `N CS` = col_n_cs,
         `IRR saneamento` = col_irr, `IC 95%` = col_ic,
         `p-valor` = col_p, `IRR IVS (p)` = col_irr_ivs, sig)

write_csv(san_df, file.path(DIR_PROC, "tabela_s5_saneamento_estratificado.csv"))
message(sprintf("  CSV salvo: %s", file.path(DIR_PROC, "tabela_s5_saneamento_estratificado.csv")))

gt_s5 <- tab_s5_raw |>
  select(-sig) |>
  gt() |>
  tab_header(
    title    = md("**Tabela S5.** Efeito de saneamento estratificado por densidade populacional real"),
    subtitle = "Densidade = pop_total_censo / área_km² (polígonos PBH); Poisson FE (regional + ano); IC 95% cluster-robusto por CS"
  ) |>
  tab_row_group(
    label = md("**Modelos estratificados por tercil de densidade (hab/km²)**"),
    rows  = str_starts(Estrato, "T")
  ) |>
  tab_row_group(
    label = md("**Referência**"),
    rows  = str_starts(Estrato, "Modelo")
  ) |>
  row_group_order(groups = c("**Referência**",
                             "**Modelos estratificados por tercil de densidade (hab/km²)**")) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = str_starts(Estrato, "Modelo"))
  ) |>
  tab_style(
    style     = cell_fill(color = "#fff3cd"),  # amarelo para sig inesperado
    locations = cells_body(
      rows    = `p-valor` < "0,050" & str_starts(Estrato, "T1"),
      columns = everything()
    )
  ) |>
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      rows    = str_starts(Estrato, "T1"),
      columns = `p-valor`
    )
  ) |>
  cols_align(align = "center",
             columns = c(`N CS`, `IRR saneamento`, `IC 95%`, `p-valor`, `IRR IVS (p)`)) |>
  cols_align(align = "left", columns = Estrato) |>
  tab_source_note(md(
    paste0(
      "IRR = incidence rate ratio. Saneamento = % domicílios sem rede geral de água (Censo 2022). ",
      "IVS = Índice de Vulnerabilidade Social (SMSA/PBH). ",
      "Densidade calculada como população adscrita / área de abrangência do CS (km²), ",
      "obtida dos polígonos oficiais SMSA/PBH (2024). ",
      "**T1 (baixa densidade, ≤7.828 hab/km²): IRR=0,943 (p<0,001) — único estrato com ",
      "associação significativa.** T2 e T3: IRR não significativo (p>0,28). ",
      "Interação saneamento×densidade (contínua): p=0,051 (marginalmente NS). ",
      "O IRR<1 significativo concentra-se nos CS de baixa densidade (T1), consistente com a ",
      "hipótese de menor acesso hospitalar em áreas periféricas: em regiões menos densas, ",
      "domicílios sem saneamento podem ter menor acesso ao hospital, reduzindo artificialmente ",
      "as internações registradas. ",
      "**O efeito observado no modelo geral (IRR=0,968; p=0,007) é provavelmente artefato de ",
      "confundimento ecológico por acessibilidade hospitalar, não associação causal.**"
    )
  )) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) |>
  tab_options(
    table.font.size           = 12,
    data_row.padding          = px(5),
    column_labels.font.weight = "bold",
    heading.subtitle.font.size = 11,
    source_notes.font.size    = 10,
    row_group.font.weight     = "bold"
  )

html_s5 <- file.path(DIR_DOCS, "tabela_s5_saneamento_estratificado.html")
gtsave(gt_s5, html_s5)
message(sprintf("  HTML salvo: %s", html_s5))

message("\nScript 26 concluído.")
