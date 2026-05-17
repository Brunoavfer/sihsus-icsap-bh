# =============================================================================
# 11_its.R
#
# Análise de Interrupção de Série Temporal (Interrupted Time Series — ITS)
#
# Hipótese: A Portaria GM/MS nº 3.493, de 10 abr. 2024 — que instituiu novo
#   cofinanciamento federal da APS com aumento do repasse por equipe ESF de
#   R$21mil para R$24-30mil e componente de qualidade — produziu mudança
#   detectável na trajetória das ICSAP em Belo Horizonte.
#
# Evidência motivadora: joinpoint regression (script 10) identificou
#   inflexão em ~abr/2024 em TODAS as 9 regionais simultaneamente —
#   padrão síncrono e de abrangência municipal, consistente com efeito
#   de política nacional.
#
# Níveis de análise:
#   1. BH municipal — série completa (100% dos dados)
#      n_icsap / n_total_internações × 100 (% ICSAP)
#   2. Regional — 9 regionais (86,1% geocodificados)
#      n_icsap / (pop_referência/12) × 10.000 hab.
#
# Modelo ITS (Bernal et al., BMJ 2017):
#   log(Y_t) = β₀ + β₁·T + β₂·X_t + β₃·P_t + sazonalidade + ε_t
#
#   T   = mês sequencial (1=jan/2023 … 39=mar/2026)
#   X_t = 0 antes de mai/2024; 1 a partir de mai/2024 (mudança de nível)
#   P_t = 0 antes de mai/2024; 1,2,3,… após mai/2024 (mudança de tendência)
#   β₁  = tendência pré-intervenção (slope pré)
#   β₂  = mudança imediata no nível em mai/2024
#   β₃  = mudança na inclinação pós-intervenção (slope pós = β₁ + β₃)
#
# Correções:
#   - Sazonalidade: termos de Fourier sin/cos (período = 12 meses)
#   - Autocorrelação residual: GLS com estrutura AR(1) via nlme::gls
#
# Referência:
#   Bernal JL, Cummins S, Gasparrini A. BMJ. 2017;359:j2981.
#   doi:10.1136/bmj.j2981
#
# Saídas:
#   data/processed/its_resultados.csv — coeficientes, IC 95%, p-valor
#   docs/its_bh.png                   — gráfico BH: observado, ajustado, contrafactual
#   docs/its_regional.png             — gráfico regional: facet 3×3
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(nlme)      # gls() com corARMA
})

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

# =============================================================================
# 1. Série municipal (numerador: icsap_bh; denominador: internacoes_bh)
# =============================================================================

icsap      <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                       col_types = cols(ano_cmpt = col_integer(),
                                        mes_cmpt = col_integer()),
                       show_col_types = FALSE)
internacoes <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"),
                        col_types = cols(ano_cmpt = col_integer(),
                                         mes_cmpt = col_integer()),
                        show_col_types = FALSE)

serie_bh <- icsap %>%
  count(ano_cmpt, mes_cmpt, name = "n_icsap") %>%
  left_join(
    internacoes %>% count(ano_cmpt, mes_cmpt, name = "n_total"),
    by = c("ano_cmpt", "mes_cmpt")
  ) %>%
  filter(!is.na(n_total), n_total > 0) %>%
  arrange(ano_cmpt, mes_cmpt) %>%
  mutate(
    data     = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num  = row_number(),            # 1 = jan/2022
    taxa_pct = n_icsap / n_total * 100,
    log_taxa = log(taxa_pct),
    # ITS: portaria vigente a partir de mai/2024 = mes_num 29
    interv    = as.integer(mes_num >= 29L),
    tempo_pos = as.integer(pmax(0L, mes_num - 28L)),
    # Fourier para sazonalidade (período 12 meses)
    sin12 = sin(2 * pi * mes_num / 12),
    cos12 = cos(2 * pi * mes_num / 12)
  )

message("=== SÉRIE MUNICIPAL ===")
message("Meses: ", nrow(serie_bh), " (", min(serie_bh$data), " a ", max(serie_bh$data), ")")
message("Ponto de intervenção: mes_num=29 = mai/2024 (Portaria GM/MS 3.493/2024)")
message("Meses pré-intervenção: ", sum(serie_bh$interv == 0),
        " | pós-intervenção: ", sum(serie_bh$interv == 1))
message("\nEstatísticas da taxa ICSAP (% internações):")
print(summary(serie_bh$taxa_pct))

# =============================================================================
# 2. Diagnóstico de autocorrelação (modelo OLS base)
# =============================================================================

message("\n--- Diagnóstico de autocorrelação (OLS base) ---")

mod_ols <- lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
              data = serie_bh)

resid_ols <- residuals(mod_ols)

# Durbin-Watson manual (d = Σ(e_t - e_{t-1})² / Σe_t²)
dw <- sum(diff(resid_ols)^2) / sum(resid_ols^2)
message("  Durbin-Watson = ", round(dw, 3),
        " (< 1.5 sugere autocorrelação positiva)")

# ACF dos resíduos — mostra se AR(1) é suficiente
acf_vals <- acf(resid_ols, plot = FALSE)
message("  ACF lag-1 = ", round(acf_vals$acf[2], 3),
        " | lag-2 = ", round(acf_vals$acf[3], 3),
        " | lag-12 = ", round(acf_vals$acf[13], 3))

# =============================================================================
# 3. Modelo ITS — GLS com AR(1) — BH municipal
# =============================================================================

message("\n=== MODELO ITS GLS AR(1) — BH MUNICIPAL ===")

mod_gls <- tryCatch(
  gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
      data        = serie_bh,
      correlation = corARMA(p = 1, q = 0, form = ~mes_num),
      method      = "ML"),
  error = function(e) {
    message("GLS AR(1) falhou: ", conditionMessage(e), " — usando OLS")
    NULL
  }
)

mod_final <- if (!is.null(mod_gls)) mod_gls else mod_ols
nome_mod  <- if (!is.null(mod_gls)) "GLS-AR1" else "OLS"

# Coeficientes e IC 95%
cf        <- coef(mod_final)
se_cf     <- if (!is.null(mod_gls)) sqrt(diag(mod_gls$varBeta)) else
               sqrt(diag(vcov(mod_ols)))
z_vals    <- cf / se_cf
p_vals    <- 2 * pnorm(-abs(z_vals))
ic_inf    <- cf - 1.96 * se_cf
ic_sup    <- cf + 1.96 * se_cf

message("\nModelo: ", nome_mod)
message(sprintf("%-22s %8s %8s %8s %8s", "Coeficiente", "Estimativa", "IC inf", "IC sup", "p-valor"))
message(strrep("-", 58))
for (nm in names(cf)) {
  message(sprintf("%-22s %8.4f %8.4f %8.4f %8.4f",
                  nm, cf[nm], ic_inf[nm], ic_sup[nm], p_vals[nm]))
}

# Correlação AR(1) estimada
if (!is.null(mod_gls)) {
  phi <- coef(mod_gls$modelStruct$corStruct, unconstrained = FALSE)
  message("\n  Correlação AR(1) estimada (φ): ", round(phi, 4))
}

# =============================================================================
# 4. Interpretação substantiva dos coeficientes
# =============================================================================

b1 <- cf["mes_num"]     # tendência pré (por mês, escala log)
b2 <- cf["interv"]      # mudança no nível em mai/2024
b3 <- cf["tempo_pos"]   # mudança na tendência pós

se_b1 <- se_cf["mes_num"]
se_b2 <- se_cf["interv"]
se_b3 <- se_cf["tempo_pos"]

# APC: (exp(12β) - 1) × 100 — variação anual percentual
apc_pre  <- (exp(12 * b1) - 1) * 100
apc_pos  <- (exp(12 * (b1 + b3)) - 1) * 100

# IC 95% para APC (delta method: Var(12β) = 144·Var(β))
apc_pre_inf <- (exp(12 * (b1 - 1.96 * se_b1)) - 1) * 100
apc_pre_sup <- (exp(12 * (b1 + 1.96 * se_b1)) - 1) * 100
apc_pos_inf <- (exp(12 * ((b1 + b3) - 1.96 * sqrt(se_b1^2 + se_b3^2))) - 1) * 100
apc_pos_sup <- (exp(12 * ((b1 + b3) + 1.96 * sqrt(se_b1^2 + se_b3^2))) - 1) * 100

# Mudança de nível: (exp(β₂) - 1) × 100
nivel_pct     <- (exp(b2) - 1) * 100
nivel_pct_inf <- (exp(b2 - 1.96 * se_b2) - 1) * 100
nivel_pct_sup <- (exp(b2 + 1.96 * se_b2) - 1) * 100

message("\n=== INTERPRETAÇÃO SUBSTANTIVA ===")
message(sprintf("  Tendência pré-portaria  (APC anual): %+.1f%% (IC95%%: %+.1f%% a %+.1f%%)  p=%.4f",
                apc_pre, apc_pre_inf, apc_pre_sup, p_vals["mes_num"]))
message(sprintf("  Mudança imediata nível  em mai/2024: %+.1f%% (IC95%%: %+.1f%% a %+.1f%%)  p=%.4f",
                nivel_pct, nivel_pct_inf, nivel_pct_sup, p_vals["interv"]))
message(sprintf("  Tendência pós-portaria  (APC anual): %+.1f%% (IC95%%: %+.1f%% a %+.1f%%)  p=%.4f",
                apc_pos, apc_pos_inf, apc_pos_sup, p_vals["tempo_pos"]))

# Interpretação qualitativa
message("\n  Diagnóstico:")
if (p_vals["interv"] < 0.05 && b2 < 0) {
  message("  ✓ β₂ < 0 (p<0,05): queda imediata e significativa no nível das ICSAP em mai/2024")
} else if (p_vals["interv"] < 0.05 && b2 > 0) {
  message("  ✗ β₂ > 0 (p<0,05): aumento imediato no nível — contrário ao esperado")
} else {
  message("  ~ β₂ não significativo (p=", round(p_vals["interv"], 3),
          "): sem mudança imediata no nível detectável")
}

if (p_vals["tempo_pos"] < 0.05 && b3 < 0) {
  message("  ✓ β₃ < 0 (p<0,05): mudança para tendência decrescente pós-portaria")
} else if (p_vals["tempo_pos"] < 0.05 && b3 > 0) {
  message("  ✗ β₃ > 0 (p<0,05): aceleração do crescimento pós-portaria — contrário ao esperado")
} else {
  message("  ~ β₃ não significativo (p=", round(p_vals["tempo_pos"], 3),
          "): sem mudança na tendência detectável")
}

# =============================================================================
# 5. Contrafactual: projeção SEM intervenção
# =============================================================================

serie_cf <- serie_bh %>%
  mutate(interv = 0L, tempo_pos = 0L)

# Predição no período pós com e sem intervenção
serie_bh <- serie_bh %>%
  mutate(
    fitted_obs = exp(predict(mod_final)),
    fitted_cf  = exp(predict(mod_final, newdata = serie_cf))
  )

# Efeito estimado no período pós (mai/2024–mar/2026)
pos <- filter(serie_bh, interv == 1)
efeito_medio <- mean((pos$fitted_obs - pos$fitted_cf) / pos$fitted_cf * 100, na.rm = TRUE)
message(sprintf("\n  Efeito médio estimado no período pós (mai/2024–mar/2026): %+.1f%%",
                efeito_medio))
message("  (observado vs contrafactual sem intervenção)")

# =============================================================================
# 6. Análise regional
# =============================================================================

message("\n=== ANÁLISE REGIONAL ===")

regional_bh <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                        col_types = cols(ano_cmpt = col_integer(),
                                         mes_cmpt = col_integer()),
                        show_col_types = FALSE) %>%
  filter(!is.na(regional))

variaveis <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                      show_col_types = FALSE)

pop_reg_ano <- variaveis %>%
  filter(mes_cmpt == 1L) %>%
  group_by(regional, ano_cmpt) %>%
  summarise(pop_anual = sum(as.numeric(populacao_referencia), na.rm = TRUE),
            .groups = "drop")

serie_reg <- regional_bh %>%
  count(ano_cmpt, mes_cmpt, regional, name = "n_icsap") %>%
  left_join(pop_reg_ano, by = c("regional", "ano_cmpt")) %>%
  filter(!is.na(pop_anual), pop_anual > 0) %>%
  arrange(regional, ano_cmpt, mes_cmpt) %>%
  group_by(regional) %>%
  mutate(
    data      = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num   = row_number(),
    taxa      = n_icsap / (pop_anual / 12) * 10000,
    log_taxa  = log(pmax(taxa, 0.001)),
    interv    = as.integer(mes_num >= 29L),
    tempo_pos = as.integer(pmax(0L, mes_num - 28L)),
    sin12     = sin(2 * pi * mes_num / 12),
    cos12     = cos(2 * pi * mes_num / 12)
  ) %>%
  ungroup()

regionais <- sort(unique(serie_reg$regional))

its_regional <- lapply(regionais, function(reg) {
  df <- filter(serie_reg, regional == reg)

  m <- tryCatch(
    gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
        data        = df,
        correlation = corARMA(p = 1, q = 0, form = ~mes_num),
        method      = "ML"),
    error = function(e) tryCatch(
      lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12, data = df),
      error = function(e2) NULL
    )
  )
  if (is.null(m)) return(NULL)

  cf_m  <- coef(m)
  se_m  <- if (inherits(m, "gls")) sqrt(diag(m$varBeta)) else sqrt(diag(vcov(m)))
  p_m   <- 2 * pnorm(-abs(cf_m / se_m))
  ic_i  <- cf_m - 1.96 * se_m
  ic_s  <- cf_m + 1.96 * se_m

  b1m <- cf_m["mes_num"];  b3m <- cf_m["tempo_pos"];  b2m <- cf_m["interv"]
  se1 <- se_m["mes_num"];  se3 <- se_m["tempo_pos"]

  tibble(
    regional      = reg,
    modelo        = if (inherits(m, "gls")) "GLS-AR1" else "OLS",
    n_meses       = nrow(df),
    # β₂ — nível
    beta_nivel    = round(b2m, 4),
    nivel_pct     = round((exp(b2m) - 1) * 100, 1),
    nivel_ic_inf  = round((exp(b2m - 1.96 * se_m["interv"]) - 1) * 100, 1),
    nivel_ic_sup  = round((exp(b2m + 1.96 * se_m["interv"]) - 1) * 100, 1),
    p_nivel       = round(p_m["interv"], 4),
    # APC pré
    apc_pre       = round((exp(12 * b1m) - 1) * 100, 1),
    apc_pre_inf   = round((exp(12 * (b1m - 1.96 * se1)) - 1) * 100, 1),
    apc_pre_sup   = round((exp(12 * (b1m + 1.96 * se1)) - 1) * 100, 1),
    p_pre         = round(p_m["mes_num"], 4),
    # APC pós
    apc_pos       = round((exp(12 * (b1m + b3m)) - 1) * 100, 1),
    apc_pos_inf   = round((exp(12 * ((b1m + b3m) - 1.96 * sqrt(se1^2 + se3^2))) - 1) * 100, 1),
    apc_pos_sup   = round((exp(12 * ((b1m + b3m) + 1.96 * sqrt(se1^2 + se3^2))) - 1) * 100, 1),
    p_pos         = round(p_m["tempo_pos"], 4)
  )
})

tab_regional <- bind_rows(its_regional)

message("\nResultados por regional:")
print(tab_regional %>% select(regional, modelo, nivel_pct, p_nivel, apc_pre, apc_pos, p_pos),
      n = Inf)

# =============================================================================
# 7. Tabela consolidada de resultados
# =============================================================================

tab_bh <- tibble(
  nivel         = "BH Municipal",
  regional      = NA_character_,
  modelo        = nome_mod,
  n_meses       = nrow(serie_bh),
  beta_nivel    = round(b2, 4),
  nivel_pct     = round(nivel_pct, 1),
  nivel_ic_inf  = round(nivel_pct_inf, 1),
  nivel_ic_sup  = round(nivel_pct_sup, 1),
  p_nivel       = round(p_vals["interv"], 4),
  apc_pre       = round(apc_pre, 1),
  apc_pre_inf   = round(apc_pre_inf, 1),
  apc_pre_sup   = round(apc_pre_sup, 1),
  p_pre         = round(p_vals["mes_num"], 4),
  apc_pos       = round(apc_pos, 1),
  apc_pos_inf   = round(apc_pos_inf, 1),
  apc_pos_sup   = round(apc_pos_sup, 1),
  p_pos         = round(p_vals["tempo_pos"], 4)
)

tab_completa <- bind_rows(
  tab_bh,
  tab_regional %>% mutate(nivel = "Regional") %>%
    dplyr::select(nivel, regional, modelo, n_meses,
                  beta_nivel, nivel_pct, nivel_ic_inf, nivel_ic_sup, p_nivel,
                  apc_pre, apc_pre_inf, apc_pre_sup, p_pre,
                  apc_pos, apc_pos_inf, apc_pos_sup, p_pos)
)

message("\n=== TABELA CONSOLIDADA ===")
print(tab_completa %>%
        dplyr::select(nivel, regional, nivel_pct, p_nivel, apc_pre, apc_pos, p_pos),
      n = Inf)

# =============================================================================
# 8. Gráfico BH — observado, ajustado, contrafactual
# =============================================================================

message("\nGerando gráficos...")

cores <- c("Observado" = "steelblue", "Ajustado" = "#d62728",
           "Contrafactual" = "#888888")

p_bh <- ggplot(serie_bh, aes(x = data)) +
  # Faixa pós-intervenção
  annotate("rect",
           xmin = as.Date("2024-05-01"), xmax = max(serie_bh$data),
           ymin = -Inf, ymax = Inf,
           fill = "#ffeeba", alpha = 0.5) +
  # Linha de intervenção
  geom_vline(xintercept = as.Date("2024-05-01"),
             linetype = "dashed", color = "#e67e22", linewidth = 0.8) +
  # Dados observados
  geom_point(aes(y = taxa_pct, color = "Observado"), alpha = 0.8, size = 2.2) +
  geom_line(aes(y = taxa_pct, color = "Observado"), alpha = 0.4, linewidth = 0.5) +
  # Tendência ajustada
  geom_line(aes(y = fitted_obs, color = "Ajustado"), linewidth = 1.2) +
  # Contrafactual (sem intervenção)
  geom_line(aes(y = fitted_cf, color = "Contrafactual"),
            linewidth = 1.0, linetype = "dashed") +
  scale_color_manual(values = cores, name = NULL) +
  annotate("text",
           x = as.Date("2024-05-15"), y = max(serie_bh$taxa_pct) * 0.97,
           label = "Portaria\n3.493/2024",
           color = "#e67e22", size = 3, hjust = 0, fontface = "bold") +
  labs(
    title    = "ITS — Taxas ICSAP em Belo Horizonte (jan/2022–mar/2026)",
    subtitle = sprintf(
      "Portaria GM/MS 3.493 (mai/2024) | Modelo: %s\nMudança no nível: %+.1f%% (p=%s) | APC pré: %+.1f%%/ano → pós: %+.1f%%/ano",
      nome_mod,
      nivel_pct, ifelse(p_vals["interv"] < 0.001, "<0,001", round(p_vals["interv"], 3)),
      apc_pre, apc_pos
    ),
    x       = NULL,
    y       = "Taxa ICSAP (% das internações)",
    caption = paste0(
      "Linha sólida vermelha = tendência ajustada | Linha tracejada cinza = contrafactual sem intervenção\n",
      "Área amarela = período pós-portaria | Fonte: SIHSUS/DATASUS · Método: ITS (Bernal et al., BMJ 2017)"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 8.5, hjust = 0.5, color = "gray30"),
    plot.caption  = element_text(size = 7, hjust = 0.5, color = "gray50"),
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(DIR_DOCS, "its_bh.png"), p_bh,
       width = 11, height = 6.5, dpi = 300, bg = "white")
message("  Gráfico BH salvo: docs/its_bh.png")

# Gráfico regional (fitted + observado por regional)
fitted_reg_list <- lapply(regionais, function(reg) {
  df <- filter(serie_reg, regional == reg)
  jp <- its_regional[[which(regionais == reg)]]
  if (is.null(jp)) return(NULL)

  m <- tryCatch(
    gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
        data = df, correlation = corARMA(p=1, q=0, form=~mes_num), method="ML"),
    error = function(e) lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12, data=df)
  )
  df_cf <- df %>% mutate(interv = 0L, tempo_pos = 0L)
  df %>% mutate(
    fitted_obs = exp(predict(m)),
    fitted_cf  = exp(predict(m, newdata = df_cf)),
    aapc_lbl   = sprintf("Nível: %+.0f%%\nAPC pós: %+.0f%%/ano",
                         jp$nivel_pct, jp$apc_pos)
  )
})
fitted_reg_df <- bind_rows(fitted_reg_list)

if (nrow(fitted_reg_df) > 0) {
  lbl_df <- fitted_reg_df %>%
    group_by(regional) %>%
    slice(1) %>%
    ungroup()

  p_reg <- ggplot(fitted_reg_df, aes(x = data)) +
    annotate("rect",
             xmin = as.Date("2024-05-01"), xmax = max(fitted_reg_df$data),
             ymin = -Inf, ymax = Inf, fill = "#ffeeba", alpha = 0.4) +
    geom_vline(xintercept = as.Date("2024-05-01"),
               linetype = "dashed", color = "#e67e22", linewidth = 0.5) +
    geom_point(aes(y = taxa), color = "steelblue", alpha = 0.5, size = 1) +
    geom_line(aes(y = fitted_obs), color = "#d62728", linewidth = 0.9) +
    geom_line(aes(y = fitted_cf),  color = "#888888", linewidth = 0.7, linetype = "dashed") +
    geom_text(data = lbl_df,
              aes(x = data, y = Inf, label = aapc_lbl),
              vjust = 1.3, hjust = 0, size = 2.4, color = "gray20") +
    facet_wrap(~regional, scales = "free_y", ncol = 3) +
    labs(
      title    = "ITS por Regional — Portaria GM/MS 3.493/2024",
      subtitle = "Linha vermelha = ajustado | Tracejada cinza = contrafactual | Área amarela = pós-portaria",
      x = NULL, y = "ICSAP por 10.000 hab.",
      caption  = "Fonte: SIHSUS/DATASUS · Método: ITS (Bernal et al., BMJ 2017)"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 8, hjust = 0.5, color = "gray40"),
      plot.caption  = element_text(size = 6, hjust = 0.5, color = "gray50"),
      strip.text    = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )

  ggsave(file.path(DIR_DOCS, "its_regional.png"), p_reg,
         width = 12, height = 10, dpi = 300, bg = "white")
  message("  Gráfico regional salvo: docs/its_regional.png")
}

# =============================================================================
# 9. Exporta resultados
# =============================================================================

write_csv(tab_completa, file.path(DIR_PROC, "its_resultados.csv"))

message("\n======================================")
message("ITS CONCLUÍDO")
message("")
message("BH (", nome_mod, "):")
message(sprintf("  Tendência pré-portaria : APC = %+.1f%%/ano (p=%.4f)",
                apc_pre, p_vals["mes_num"]))
message(sprintf("  Mudança imediata nível : %+.1f%% (p=%.4f)",
                nivel_pct, p_vals["interv"]))
message(sprintf("  Tendência pós-portaria : APC = %+.1f%%/ano (p=%.4f)",
                apc_pos, p_vals["tempo_pos"]))
message(sprintf("  Efeito médio pós (obs vs CF): %+.1f%%", efeito_medio))
message("")
message("Saídas:")
message("  data/processed/its_resultados.csv")
message("  docs/its_bh.png")
message("  docs/its_regional.png")
message("======================================")
