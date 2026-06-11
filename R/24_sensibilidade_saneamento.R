# =============================================================================
# 24_sensibilidade_saneamento.R
#
# P8 — Análise de sensibilidade: saneamento estratificado por densidade
# populacional (tercis de pop_total_censo)
#
# Objetivo: verificar se o coeficiente contraintuitivo de pct_sem_saneamento
# (IRR < 1, modelo M2 do script 21) reflete confundimento por densidade
# populacional. CS em áreas menos densas tendem a ter pior saneamento E
# menores taxas registradas de internação (menor acesso hospitalar).
#
# Método: Poisson FE two-way (CS + ano), identical ao M2 do script 21,
# re-estimado separadamente em cada tercil de densidade (pop_total_censo).
#
# Saídas:
#   data/processed/sensibilidade_saneamento.csv
#   docs/tabela_s3_sensibilidade_nb.html   (atualiza tabela de sensibilidade)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(fixest)
  library(gt)
  library(stringr)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

# =============================================================================
# 1. Carrega dados (idêntico ao script 21)
# =============================================================================

message("=== 1. Carregando dados ===")

icsap_reg  <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                       show_col_types = FALSE)
variaveis  <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                       show_col_types = FALSE)
pop_faixas <- read_csv(file.path(DIR_REF, "pop_cs_faixas.csv"),
                       show_col_types = FALSE)

# =============================================================================
# 2. Constrói painel CS × mês (idêntico ao script 21)
# =============================================================================

message("=== 2. Construindo painel ===")

static_cs <- variaveis %>%
  group_by(nome_cs) %>%
  slice(1) %>%
  ungroup() %>%
  select(nome_cs, cod_smsa, regional,
         pop_total_censo, pct_sem_saneamento, renda_media, pct_area_favela,
         ivs_score, ivs_predominante)

pop_cs_total <- pop_faixas %>%
  group_by(nome_cs) %>%
  summarise(pop_censo = sum(pop_faixa, na.rm = TRUE), .groups = "drop")

cs_ref <- static_cs %>% select(nome_cs, cod_smsa, regional)

meses_ref <- tibble(
  ano_cmpt = c(rep(2022L, 12), rep(2023L, 12), rep(2024L, 12),
               rep(2025L, 12), rep(2026L,  3)),
  mes_cmpt = c(1:12, 1:12, 1:12, 1:12, 1:3)
) %>%
  mutate(mes_num = row_number())

skeleton <- tidyr::crossing(cs_ref, meses_ref)

n_icsap_cs_mes <- icsap_reg %>%
  filter(!is.na(nome_cs)) %>%
  mutate(mes_cmpt = as.integer(mes_cmpt)) %>%
  group_by(nome_cs, ano_cmpt, mes_cmpt) %>%
  summarise(n_icsap = n(), .groups = "drop")

painel <- skeleton %>%
  left_join(n_icsap_cs_mes, by = c("nome_cs", "ano_cmpt", "mes_cmpt")) %>%
  mutate(n_icsap = replace_na(n_icsap, 0L)) %>%
  left_join(static_cs %>% select(-regional, -cod_smsa), by = "nome_cs") %>%
  left_join(pop_cs_total, by = "nome_cs") %>%
  mutate(
    sin12   = sin(2 * pi * mes_num / 12),
    cos12   = cos(2 * pi * mes_num / 12),
    ano_fac = factor(ano_cmpt),
    pop_cs  = if_else(!is.na(pop_censo) & pop_censo > 0,
                      as.double(pop_censo),
                      median(pop_censo, na.rm = TRUE))
  )

message(sprintf("  Painel: %s obs | %d CS | %d meses",
                format(nrow(painel), big.mark=","),
                n_distinct(painel$nome_cs),
                n_distinct(painel$mes_num)))

# =============================================================================
# 3. Estratificação por tercil de densidade (pop_total_censo)
# =============================================================================

message("=== 3. Criando tercis de densidade ===")

# Densidade: pop por CS — Censo 2022 (tercis definidos 1× por CS, não por obs)
tercis_cs <- static_cs %>%
  select(nome_cs, pop_total_censo) %>%
  mutate(
    tercil_pop = ntile(pop_total_censo, 3),
    tercil_label = case_when(
      tercil_pop == 1 ~ "T1 — Menor densidade (≤ p33)",
      tercil_pop == 2 ~ "T2 — Densidade média (p33–p66)",
      tercil_pop == 3 ~ "T3 — Maior densidade (≥ p66)"
    )
  )

# Limites dos tercis para relatório
breaks_pop <- quantile(static_cs$pop_total_censo, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
message(sprintf("  Limites tercis pop: %.0f | %.0f | %.0f | %.0f",
                breaks_pop[1], breaks_pop[2], breaks_pop[3], breaks_pop[4]))
message(sprintf("  N CS por tercil: %s",
                paste(table(tercis_cs$tercil_pop), collapse = " / ")))

painel <- painel %>%
  left_join(tercis_cs %>% select(nome_cs, tercil_pop, tercil_label), by = "nome_cs")

# =============================================================================
# 4. Modelo de referência — geral (replica M2 do script 21)
# =============================================================================

message("=== 4. Modelo geral (M2 do script 21) ===")

setFixest_notes(FALSE)

m_geral <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 +
    ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
    regional + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel,
  vcov   = ~nome_cs
)
message(sprintf("  Geral: N = %s | coef san = %.4f | se = %.4f | p = %.4f",
                format(nobs(m_geral), big.mark=","),
                coef(m_geral)["pct_sem_saneamento"],
                se(m_geral)["pct_sem_saneamento"],
                pvalue(m_geral)["pct_sem_saneamento"]))

# =============================================================================
# 5. Modelos estratificados por tercil
# =============================================================================

message("=== 5. Modelos estratificados por tercil de densidade ===")

resultados <- list()

# --- Modelo geral (referência) ---
ci_g <- confint(m_geral)
resultados[["Geral"]] <- tibble(
  estrato      = "Geral (todos os CS)",
  n_cs         = n_distinct(painel$nome_cs),
  n_obs        = nobs(m_geral),
  irr_san      = exp(coef(m_geral)["pct_sem_saneamento"]),
  irr_inf      = exp(ci_g["pct_sem_saneamento", 1]),
  irr_sup      = exp(ci_g["pct_sem_saneamento", 2]),
  p_san        = pvalue(m_geral)["pct_sem_saneamento"],
  irr_ivs      = exp(coef(m_geral)["ivs_score"]),
  p_ivs        = pvalue(m_geral)["ivs_score"]
)

# --- Modelo por tercil ---
for (t in 1:3) {
  lbl <- unique(tercis_cs$tercil_label[tercis_cs$tercil_pop == t])
  d_t <- painel %>% filter(tercil_pop == t)

  message(sprintf("  Ajustando tercil %d (%s) — %d CS, %s obs ...",
                  t, lbl, n_distinct(d_t$nome_cs), format(nrow(d_t), big.mark=",")))

  # Com regional FE alguns tercis podem ter poucos CS por regional
  # Tenta primeiro regional FE, depois CS FE se singular
  m_t <- tryCatch(
    feglm(
      n_icsap ~ mes_num + sin12 + cos12 +
        ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
        regional + ano_fac,
      offset = ~log(pop_cs),
      family = "poisson",
      data   = d_t,
      vcov   = ~nome_cs
    ),
    error = function(e) {
      message(sprintf("    regional FE falhou (%s); usando nome_cs FE", conditionMessage(e)))
      feglm(
        n_icsap ~ mes_num + sin12 + cos12 +
          ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
          nome_cs + ano_fac,
        offset = ~log(pop_cs),
        family = "poisson",
        data   = d_t,
        vcov   = ~nome_cs
      )
    }
  )

  ci_t <- tryCatch(confint(m_t), error = function(e) NULL)
  if (is.null(ci_t)) {
    message(sprintf("    AVISO: confint falhou para tercil %d", t))
    irr_inf <- NA_real_; irr_sup <- NA_real_
  } else {
    irr_inf <- exp(ci_t["pct_sem_saneamento", 1])
    irr_sup <- exp(ci_t["pct_sem_saneamento", 2])
  }

  message(sprintf("    IRR san = %.3f (%.3f–%.3f) p = %.4f",
                  exp(coef(m_t)["pct_sem_saneamento"]),
                  irr_inf, irr_sup,
                  pvalue(m_t)["pct_sem_saneamento"]))

  resultados[[lbl]] <- tibble(
    estrato  = lbl,
    n_cs     = n_distinct(d_t$nome_cs),
    n_obs    = nobs(m_t),
    irr_san  = exp(coef(m_t)["pct_sem_saneamento"]),
    irr_inf  = irr_inf,
    irr_sup  = irr_sup,
    p_san    = pvalue(m_t)["pct_sem_saneamento"],
    irr_ivs  = tryCatch(exp(coef(m_t)["ivs_score"]),  error = function(e) NA_real_),
    p_ivs    = tryCatch(pvalue(m_t)["ivs_score"], error = function(e) NA_real_)
  )
}

res_df <- bind_rows(resultados)

# =============================================================================
# 6. Salva CSV
# =============================================================================

write_csv(res_df, file.path(DIR_PROC, "sensibilidade_saneamento.csv"))
message(sprintf("\nSalvo: %s", file.path(DIR_PROC, "sensibilidade_saneamento.csv")))

# =============================================================================
# 7. Tabela HTML (atualiza tabela_s3_sensibilidade_nb.html)
# =============================================================================

message("=== 6. Gerando tabela HTML ===")

fmt_irr <- function(irr, inf, sup, p) {
  p_str <- if (!is.na(p) && p < 0.001) "p<0,001"
            else if (!is.na(p) && p < 0.01) sprintf("p=%s", gsub("\\.", ",", sprintf("%.3f", p)))
            else if (!is.na(p)) sprintf("p=%s", gsub("\\.", ",", sprintf("%.2f",  p)))
            else "—"
  if (is.na(irr)) return("—")
  irr_f <- gsub("\\.", ",", sprintf("%.3f", irr))
  inf_f <- gsub("\\.", ",", sprintf("%.3f", inf))
  sup_f <- gsub("\\.", ",", sprintf("%.3f", sup))
  sprintf("%s (%s–%s); %s", irr_f, inf_f, sup_f, p_str)
}

tab_display <- res_df %>%
  rowwise() %>%
  mutate(
    `IRR saneamento (IC 95%; p)` = fmt_irr(irr_san, irr_inf, irr_sup, p_san),
    `IRR IVS (p)` = if (!is.na(irr_ivs))
                      sprintf("%s (%s)",
                              gsub("\\.", ",", sprintf("%.3f", irr_ivs)),
                              if (!is.na(p_ivs) && p_ivs < 0.001) "p<0,001"
                              else gsub("\\.", ",", sprintf("p=%.3f", p_ivs)))
                    else "—",
    `N CS` = as.character(n_cs),
    `N obs` = format(n_obs, big.mark=",")
  ) %>%
  ungroup() %>%
  select(Estrato = estrato, `N CS`, `N obs`,
         `IRR saneamento (IC 95%; p)`, `IRR IVS (p)`)

gt_tab <- tab_display %>%
  gt() %>%
  tab_header(
    title    = "Tabela S3. Análise de sensibilidade: efeito de saneamento estratificado por densidade populacional",
    subtitle = "Poisson FE two-way (regional + ano); IC 95% cluster-robusto por CS; série jan/2022–mar/2026"
  ) %>%
  tab_source_note(md(
    "IRR = incidence rate ratio. Saneamento = % domicílios sem rede geral de água (Censo 2022). \
IVS = Índice de Vulnerabilidade Social (SMSA/PBH). Tercis calculados sobre pop_total_censo (Censo 2022). \
Hipótese: IRR < 1 nos tercis de maior densidade indica confundimento por acesso diferencial a hospitais."
  )) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 1)
  ) %>%
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) %>%
  tab_options(table.font.size = 12, data_row.padding = px(4))

html_path <- file.path(DIR_DOCS, "tabela_s3_sensibilidade_nb.html")
gtsave(gt_tab, html_path)
message(sprintf("Tabela HTML salva: %s", html_path))

# =============================================================================
# 8. Resumo interpretativo
# =============================================================================

message("\n=== RESUMO — SENSIBILIDADE SANEAMENTO ===")
for (i in seq_len(nrow(res_df))) {
  r <- res_df[i, ]
  message(sprintf("  %-45s IRR=%.3f (%.3f–%.3f) p=%.4f",
                  r$estrato, r$irr_san, r$irr_inf, r$irr_sup, r$p_san))
}

# Interpretação automática
irr_t1 <- res_df$irr_san[res_df$estrato == unique(res_df$estrato)[2]]
irr_t3 <- res_df$irr_san[res_df$estrato == unique(res_df$estrato)[4]]
if (!is.na(irr_t1) && !is.na(irr_t3)) {
  if (irr_t1 < 1 && irr_t3 >= 1) {
    message("\n  INTERPRETAÇÃO: IRR saneamento < 1 apenas no tercil de menor densidade (T1),")
    message("  e >= 1 no tercil de maior densidade (T3). Padrão CONSISTENTE com hipótese de")
    message("  confundimento por acesso: em áreas menos densas com pior saneamento, as")
    message("  internações são subestimadas por menor acesso hospitalar.")
  } else if (irr_t1 < 1 && irr_t3 < 1) {
    message("\n  INTERPRETAÇÃO: IRR < 1 em todos os tercis. Efeito de saneamento persiste")
    message("  independentemente da densidade. Pode refletir viés de acesso uniforme ou")
    message("  outro mecanismo (ex.: correlação com qualidade da APS local).")
  } else {
    message("\n  INTERPRETAÇÃO: padrão complexo — reportar estratificação e discutir limitações.")
  }
}

message("\nScript 24 concluído.")
