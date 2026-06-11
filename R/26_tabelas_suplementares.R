# =============================================================================
# 26_tabelas_suplementares.R
#
# Gera Tabela S4 (ITS por grupo clínico) e Tabela S5 (sensibilidade
# saneamento estratificado) para submissão ao manuscrito.
#
# Lê CSVs gerados pelos scripts 25 e 24 respectivamente.
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

# helpers de formatação (padrão brasileiro: vírgula decimal)
fmt_pct <- function(x, digits = 1) {
  gsub("\\.", ",", sprintf(paste0("%+.", digits, "f%%/ano"), x))
}
fmt_p <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "<0,001",
    TRUE       ~ gsub("\\.", ",", sprintf("%.3f", p))
  )
}
fmt_irr <- function(x, digits = 3) {
  gsub("\\.", ",", sprintf(paste0("%.", digits, "f"), x))
}

# =============================================================================
# TABELA S4 — ITS por grupo clínico
# =============================================================================

message("=== Tabela S4 — ITS por grupo clínico ===")

its_grupos <- read_csv(file.path(DIR_PROC, "its_por_grupo.csv"),
                       show_col_types = FALSE)

# Total para calcular % (linha "Todos")
n_total <- its_grupos$n_icsap_tot[its_grupos$grupo == "Todos"]

# Seleciona apenas os 3 grupos clínicos (exclui "Todos")
tab_s4_raw <- its_grupos %>%
  filter(grupo != "Todos") %>%
  mutate(
    grupo_label = case_when(
      grupo == "09" ~ "Doenças cardiovasculares",
      grupo == "10" ~ "Doenças respiratórias",
      grupo == "13" ~ "Diabetes mellitus",
      TRUE          ~ grupo_nome
    ),
    subgrupos = case_when(
      grupo == "09" ~ "HAS, angina, insuficiência cardíaca, AVC",
      grupo == "10" ~ "IVAS, pneumonias, bronquite aguda, asma",
      grupo == "13" ~ "Diabetes mellitus (todas as formas clínicas)",
      TRUE          ~ ""
    ),
    n_pct = sprintf("%s (%s%%)",
                    format(n_icsap_tot, big.mark=","),
                    gsub("\\.", ",", sprintf("%.1f", 100 * n_icsap_tot / n_total))),
    col_apc_pre  = sprintf("%s (IC95%%: %s; %s)",
                           fmt_pct(apc_pre),
                           fmt_pct(apc_pre_inf),
                           fmt_pct(apc_pre_sup)),
    col_apc_pos  = fmt_pct(apc_pos),
    col_ic_pos   = sprintf("(%s; %s)",
                           fmt_pct(apc_pos_inf),
                           fmt_pct(apc_pos_sup)),
    col_p        = fmt_p(p_pos)
  ) %>%
  select(Grupo = grupo_label, `CIDs incluídos` = subgrupos,
         `n (% do total)` = n_pct,
         `APC pré-Portaria` = col_apc_pre,
         `APC pós-Portaria` = col_apc_pos,
         `IC 95%` = col_ic_pos,
         `p-valor` = col_p)

# CSV exportável (versão tidy com colunas numéricas)
csv_s4 <- its_grupos %>%
  filter(grupo != "Todos") %>%
  select(grupo, grupo_nome, n_icsap_tot,
         apc_pre, apc_pre_inf, apc_pre_sup,
         apc_pos, apc_pos_inf, apc_pos_sup, p_pos, phi_ar1, modelo)

write_csv(csv_s4, file.path(DIR_PROC, "tabela_s4_its_grupos.csv"))
message(sprintf("  CSV salvo: %s", file.path(DIR_PROC, "tabela_s4_its_grupos.csv")))

# Tabela gt
gt_s4 <- tab_s4_raw %>%
  gt() %>%
  tab_header(
    title    = md("**Tabela S4.** ITS GLS AR(1) por grupo clínico de ICSAP — efeito da Portaria GM/MS 3.493/2024"),
    subtitle = "BH municipal, jan/2022–mar/2026 (51 meses); intervenção: maio/2024 (mes_num = 29)"
  ) %>%
  tab_spanner(
    label   = md("**Mudança de tendência pós-Portaria**"),
    columns = c(`APC pós-Portaria`, `IC 95%`, `p-valor`)
  ) %>%
  tab_source_note(md(
    "APC = Annual Percentage Change (% ao ano). IC 95% via método delta (Var(β₁+β₃) = Var(β₁) + \
Var(β₃) + 2·Cov(β₁,β₃)). Modelo: GLS com estrutura de correlação AR(1) (nlme). \
Grupos clínicos conforme Portaria SAS/MS nº 221/2008. \
O efeito da Portaria 3.493/2024 é heterogêneo entre grupos: Diabetes mellitus apresenta \
a maior redução de tendência (APC pós = −11,4%/ano, p<0,001), consistente com os indicadores \
de qualidade do ISF Previne Brasil voltados ao controle metabólico. Cardiovascular e \
respiratório não atingem significância estatística no slope change (p>0,05)."
  )) %>%
  cols_align(align = "center",
             columns = c(`n (% do total)`, `APC pré-Portaria`,
                         `APC pós-Portaria`, `IC 95%`, `p-valor`)) %>%
  cols_align(align = "left", columns = c(Grupo, `CIDs incluídos`)) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(
      rows   = `p-valor` == "<0,001",
      columns = c(`APC pós-Portaria`, `IC 95%`, `p-valor`)
    )
  ) %>%
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body(
      rows    = `p-valor` == "<0,001",
      columns = `p-valor`
    )
  ) %>%
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) %>%
  tab_options(
    table.font.size      = 12,
    data_row.padding     = px(5),
    column_labels.font.weight = "bold",
    heading.subtitle.font.size = 11,
    source_notes.font.size     = 10
  )

html_s4 <- file.path(DIR_DOCS, "tabela_s4_its_grupos.html")
gtsave(gt_s4, html_s4)
message(sprintf("  HTML salvo: %s", html_s4))

# =============================================================================
# TABELA S5 — Sensibilidade saneamento estratificado por densidade
# =============================================================================

message("=== Tabela S5 — Sensibilidade saneamento por tercil de densidade ===")

san_df <- read_csv(file.path(DIR_PROC, "sensibilidade_saneamento.csv"),
                   show_col_types = FALSE)

tab_s5_raw <- san_df %>%
  mutate(
    estrato_fmt = case_when(
      str_detect(estrato, "Geral")   ~ "Modelo geral (todos os CS)",
      str_detect(estrato, "T1")      ~ "T1 — Menor densidade (≤ p33)",
      str_detect(estrato, "T2")      ~ "T2 — Densidade intermediária (p33–p66)",
      str_detect(estrato, "T3")      ~ "T3 — Maior densidade (≥ p66)",
      TRUE                           ~ estrato
    ),
    col_n_cs  = as.character(n_cs),
    col_n_obs = format(n_obs, big.mark=","),
    col_irr   = fmt_irr(irr_san),
    col_ic    = sprintf("(%s; %s)", fmt_irr(irr_inf), fmt_irr(irr_sup)),
    col_p     = fmt_p(p_san),
    col_irr_ivs = if_else(
      !is.na(irr_ivs),
      sprintf("%s (%s)", fmt_irr(irr_ivs), fmt_p(p_ivs)),
      "—"
    )
  ) %>%
  select(Estrato = estrato_fmt,
         `N CS`  = col_n_cs,
         `N obs` = col_n_obs,
         `IRR saneamento` = col_irr,
         `IC 95%` = col_ic,
         `p-valor` = col_p,
         `IRR IVS (p)` = col_irr_ivs)

# CSV exportável
write_csv(san_df, file.path(DIR_PROC, "tabela_s5_saneamento_estratificado.csv"))
message(sprintf("  CSV salvo: %s", file.path(DIR_PROC, "tabela_s5_saneamento_estratificado.csv")))

# Tabela gt
gt_s5 <- tab_s5_raw %>%
  gt() %>%
  tab_header(
    title    = md("**Tabela S5.** Análise de sensibilidade: efeito de saneamento por tercil de densidade populacional"),
    subtitle = "Poisson FE two-way (regional + ano); IC 95% cluster-robusto por CS; série jan/2022–mar/2026"
  ) %>%
  tab_row_group(
    label = md("**Modelos estratificados**"),
    rows  = str_starts(Estrato, "T")
  ) %>%
  tab_row_group(
    label = md("**Referência**"),
    rows  = str_starts(Estrato, "Modelo")
  ) %>%
  row_group_order(groups = c("**Referência**", "**Modelos estratificados**")) %>%
  tab_source_note(md(
    "IRR = incidence rate ratio. Saneamento = % domicílios sem rede geral de água (Censo 2022). \
IVS = Índice de Vulnerabilidade Social (SMSA/PBH). Tercis calculados sobre \
população total por CS (Censo 2022): T1 ≤ 11.323 hab; T2 = 11.323–15.696 hab; T3 ≥ 15.696 hab. \
O IRR de saneamento torna-se não significativo em todos os estratos de densidade (p = 0,28–0,96), \
contrariamente ao modelo geral (p = 0,007). Este padrão é consistente com confundimento \
ecológico: a associação observada entre pior saneamento e menor ICSAP reflete, provavelmente, \
a correlação entre áreas periféricas (menor densidade, pior saneamento) e menor acesso \
hospitalar — e não um efeito protetor direto do saneamento inadequado."
  )) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = str_starts(Estrato, "Modelo"))
  ) %>%
  tab_style(
    style     = cell_fill(color = "#f8f9fa"),
    locations = cells_body(rows = str_starts(Estrato, "Modelo"))
  ) %>%
  cols_align(align = "center",
             columns = c(`N CS`, `N obs`, `IRR saneamento`, `IC 95%`, `p-valor`, `IRR IVS (p)`)) %>%
  cols_align(align = "left", columns = Estrato) %>%
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) %>%
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
