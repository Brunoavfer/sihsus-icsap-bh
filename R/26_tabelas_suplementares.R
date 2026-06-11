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

message("=== Tabela S4 — ITS por grupo clínico ===")

its_abs  <- read_csv(file.path(DIR_PROC, "its_por_grupo.csv"),      show_col_types = FALSE)
its_prop <- read_csv(file.path(DIR_PROC, "its_por_grupo_prop.csv"), show_col_types = FALSE)

n_total <- its_abs$n_icsap_tot[its_abs$grupo == "Todos"]

# --- Painel A: contagem absoluta ---
painel_a <- its_abs |>
  filter(grupo != "Todos") |>
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
                    format(n_icsap_tot, big.mark=","),
                    gsub("\\.", ",", sprintf("%.1f", 100 * n_icsap_tot / n_total))),
    col_apc_pre = sprintf("%s%% (IC: %s; %s)",
                          fmt_num(apc_pre), fmt_num(apc_pre_inf), fmt_num(apc_pre_sup)),
    col_apc_pos = sprintf("%s%% (IC: %s; %s)",
                          fmt_num(apc_pos), fmt_num(apc_pos_inf), fmt_num(apc_pos_sup)),
    col_p = fmt_p(p_pos)
  ) |>
  select(Grupo = grupo_label, `CIDs incluídos` = subgrupos,
         `n (% do total)` = n_pct,
         `APC pré (%/ano)` = col_apc_pre,
         `APC pós (%/ano, IC 95%)` = col_apc_pos,
         `p-valor` = col_p)

# --- Painel B: proporção (p.p./ano) ---
painel_b <- its_prop |>
  mutate(
    grupo_label = case_when(
      grupo == "09" ~ "Grupo 09 — Cardiovascular",
      grupo == "10" ~ "Grupo 10 — Respiratório",
      grupo == "13" ~ "Grupo 13 — Diabetes mellitus"
    ),
    col_slope_pre = sprintf("%s pp/ano (IC: %s; %s)",
                            fmt_num(apc_pre, 2), fmt_num(apc_pre_inf, 2), fmt_num(apc_pre_sup, 2)),
    col_slope_pos = sprintf("%s pp/ano (IC: %s; %s)",
                            fmt_num(apc_pos, 2), fmt_num(apc_pos_inf, 2), fmt_num(apc_pos_sup, 2)),
    col_p = fmt_p(p_pos),
    interpretacao = dplyr::case_when(
      grupo == "09" ~ "NS vol. abs. / sig. prop.",
      grupo == "10" ~ "NS vol. abs. / sig. prop.",
      grupo == "13" ~ "Sig. vol. abs. / NS prop."
    )
  ) |>
  select(Grupo = grupo_label,
         `Slope pré (pp/ano)` = col_slope_pre,
         `Slope pós (pp/ano, IC 95%)` = col_slope_pos,
         `p-valor` = col_p,
         Interpretação = interpretacao)

# CSV combinado
csv_s4 <- bind_rows(
  its_abs  |> filter(grupo != "Todos") |> mutate(analise = "absoluta"),
  its_prop |> mutate(analise = "proporcional")
)
write_csv(csv_s4, file.path(DIR_PROC, "tabela_s4_its_grupos.csv"))
message(sprintf("  CSV salvo: %s", file.path(DIR_PROC, "tabela_s4_its_grupos.csv")))

# gt — Painel A
gt_a <- painel_a |>
  gt() |>
  tab_header(
    title    = md("**Tabela S4A.** ITS GLS AR(1) — Contagem absoluta por grupo clínico"),
    subtitle = "APC = Annual Percentage Change (%/ano); BH municipal, jan/2022–mar/2026; intervenção: maio/2024"
  ) |>
  tab_style(
    style     = cell_text(weight = "bold", color = "#c0392b"),
    locations = cells_body(rows = `p-valor` == "<0,001",
                           columns = c(`APC pós (%/ano, IC 95%)`, `p-valor`))
  ) |>
  cols_align(align = "center",
             columns = c(`n (% do total)`, `APC pré (%/ano)`, `APC pós (%/ano, IC 95%)`, `p-valor`)) |>
  cols_align(align = "left", columns = c(Grupo, `CIDs incluídos`)) |>
  tab_source_note(md(
    "IC 95% via método delta (Var(β₁+β₃) = Var(β₁)+Var(β₃)+2·Cov(β₁,β₃)). \
APC pós = mudança de tendência no volume absoluto do grupo — **não implica efeito clínico seletivo** \
(ver Tabela S4B para análise proporcional)."
  )) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) |>
  tab_options(table.font.size=12, data_row.padding=px(5),
              column_labels.font.weight="bold", source_notes.font.size=10)

# gt — Painel B
gt_b <- painel_b |>
  gt() |>
  tab_header(
    title    = md("**Tabela S4B.** ITS GLS AR(1) — Composição proporcional (% do total ICSAP)"),
    subtitle = "GLS linear (outcome: % do grupo no total ICSAP municipal por mês); slope em p.p./ano"
  ) |>
  tab_source_note(md(
    "Slope pós = (β₁+β₃)×12, IC 95% via método delta. \
**Interpretação crítica (P17/P21):** Diabetes mellitus apresenta redução significativa no \
volume absoluto (Tabela S4A, p<0,001) mas sem mudança significativa na composição proporcional \
(p=0,571). Isso indica que a redução de diabetes acompanha proporcionalmente a redução geral de ICSAP, \
sem evidência de efeito clínico seletivo da Portaria 3.493/2024 sobre esse grupo. \
Cardiovascular e respiratório sem redução absoluta significativa, mas com aumento de participação \
proporcional (p<0,01 e p=0,035), reflexo do declínio mais acentuado de outros grupos. \
A interpretação causal deve ser moderada: 23 meses de seguimento (mai/2024–mar/2026) \
podem ser insuficientes para atribuir causalidade isolada à Portaria, dada a possível \
influência de fatores concorrentes (melhora no manejo ambulatorial pós-pandemia, \
mudanças na codificação SIHSUS). Ausência de padronização etária é limitação adicional."
  )) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Grupo == "Grupo 13 — Diabetes mellitus",
                           columns = c(`Slope pós (pp/ano, IC 95%)`, `p-valor`, Interpretação))
  ) |>
  cols_align(align = "center",
             columns = c(`Slope pré (pp/ano)`, `Slope pós (pp/ano, IC 95%)`, `p-valor`, Interpretação)) |>
  cols_align(align = "left", columns = Grupo) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) |>
  tab_options(table.font.size=12, data_row.padding=px(5),
              column_labels.font.weight="bold", source_notes.font.size=10)

# Salva ambos no mesmo arquivo HTML concatenando
html_a <- as_raw_html(gt_a)
html_b <- as_raw_html(gt_b)
html_s4 <- file.path(DIR_DOCS, "tabela_s4_its_grupos.html")
writeLines(c(html_a, "<br><br>", html_b), html_s4)
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
    "IRR = incidence rate ratio. Saneamento = % domicílios sem rede geral de água (Censo 2022). \
IVS = Índice de Vulnerabilidade Social (SMSA/PBH). Densidade real (hab/km²) calculada \
a partir dos polígonos oficiais de área de abrangência dos CS (PBH/SMSA). \
**T1 (baixa densidade, ≤ 7.828 hab/km²): IRR = 0,943 (p<0,001) — associação significativa, \
confirmando a hipótese de confundimento ecológico.** Em áreas pouco densas, pior saneamento \
coexiste com menor acesso hospitalar, produzindo menor registro de internações. \
T2 e T3 (densidade intermediária e alta): IRR não significativo (p>0,28). \
Interação saneamento×densidade (contínua): IRR = 1,014; p = 0,051 (marginalmente NS). \
Esses resultados sugerem que a associação observada no modelo geral é mediada pela \
concentração do efeito em áreas de baixa densidade, consistente com viés de acesso diferencial, \
mas análises adicionais com dados de utilização hospitalar são necessárias para confirmar \
o mecanismo."
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
