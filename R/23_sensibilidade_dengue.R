# =============================================================================
# 23_sensibilidade_dengue.R
#
# Análise de sensibilidade ITS — excluindo Dengue (grupo ICSAP G04)
#
# Motivação: o script 22 identificou que o grupo Dengue (G04) concentrou
#   parte da mudança de slope em mai/2024 (APC pré=+603%/ano, decorrente
#   do pico epidêmico de 2024). Esta análise verifica se o efeito da Portaria
#   GM/MS 3.493/2024 é robusto à exclusão de Dengue.
#
# Método: ITS GLS AR(1) idêntico ao script 11, aplicado a duas séries:
#   (a) Série completa (referência)
#   (b) Série excluindo internações com grupo ICSAP = "04" (Dengue)
#
# Se APC pós permanece significativamente negativo sem Dengue → efeito robusto.
# Se APC pós se torna NS após exclusão → efeito era parcialmente artefato.
#
# Saídas:
#   data/processed/its_sem_dengue_resultados.csv
#   docs/its_sem_dengue.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(nlme)
  library(patchwork)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_DOCS <- "docs"

MES_INTERV <- 29L   # mai/2024 (jan/2022 = mes_num 1)

# =============================================================================
# 1. Carrega e prepara ambas as séries
# =============================================================================

message("Carregando dados...")

icsap       <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                        col_types = cols(ano_cmpt = col_integer(),
                                         mes_cmpt = col_integer()),
                        show_col_types = FALSE)
internacoes <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"),
                        col_types = cols(ano_cmpt = col_integer(),
                                         mes_cmpt = col_integer()),
                        show_col_types = FALSE)

# Denominador: total mensal (idêntico ao script 11)
n_total_mes <- internacoes %>%
  count(ano_cmpt, mes_cmpt, name = "n_total")

# Numerador série completa
n_icsap_full <- icsap %>%
  count(ano_cmpt, mes_cmpt, name = "n_icsap_full")

# Numerador sem Dengue (grupo "04")
n_dengue_mes <- icsap %>%
  filter(grupo == "04") %>%
  count(ano_cmpt, mes_cmpt, name = "n_dengue")

n_icsap_nodg <- icsap %>%
  filter(grupo != "04") %>%
  count(ano_cmpt, mes_cmpt, name = "n_icsap_nodg")

message(sprintf("Total ICSAP: %d | Internações Dengue: %d (%.1f%%)",
                sum(n_icsap_full$n_icsap_full),
                sum(n_dengue_mes$n_dengue, na.rm = TRUE),
                sum(n_dengue_mes$n_dengue, na.rm = TRUE) /
                  sum(n_icsap_full$n_icsap_full) * 100))

# Série base para joins
base_mes <- n_total_mes %>%
  filter(n_total > 0) %>%
  arrange(ano_cmpt, mes_cmpt) %>%
  mutate(
    data      = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num   = row_number(),
    interv    = as.integer(mes_num >= MES_INTERV),
    tempo_pos = as.integer(pmax(0L, mes_num - (MES_INTERV - 1L))),
    sin12     = sin(2 * pi * mes_num / 12),
    cos12     = cos(2 * pi * mes_num / 12)
  )

serie_full <- base_mes %>%
  left_join(n_icsap_full, by = c("ano_cmpt", "mes_cmpt")) %>%
  replace_na(list(n_icsap_full = 0L)) %>%
  mutate(taxa_pct = n_icsap_full / n_total * 100,
         log_taxa = log(taxa_pct))

# Denominador sem Dengue: n_total - n_dengue
# (necessário para evitar artefato: queda de Dengue reduz n_total e
#  infla artificialmente a taxa das demais ICSAP se usarmos n_total)
serie_nodg <- base_mes %>%
  left_join(n_icsap_nodg, by = c("ano_cmpt", "mes_cmpt")) %>%
  replace_na(list(n_icsap_nodg = 0L)) %>%
  left_join(n_dengue_mes, by = c("ano_cmpt", "mes_cmpt")) %>%
  replace_na(list(n_dengue = 0L)) %>%
  mutate(
    n_total_nodg = n_total - n_dengue,
    taxa_pct     = n_icsap_nodg / pmax(n_total_nodg, 1L) * 100,
    log_taxa     = log(pmax(taxa_pct, 0.001))
  )

message(sprintf("Série: %d meses (%s – %s) | MES_INTERV=%d",
                nrow(base_mes), min(base_mes$data), max(base_mes$data), MES_INTERV))

# =============================================================================
# 2. Função ITS GLS AR(1) — idêntica ao script 11
# =============================================================================

fit_its_gls <- function(serie, label = "") {
  mod_ols <- lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
                data = serie)

  mod_gls <- tryCatch(
    gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
        data        = serie,
        correlation = corARMA(p = 1, q = 0, form = ~mes_num),
        method      = "ML"),
    error = function(e) {
      message("  [", label, "] GLS falhou, usando OLS: ", conditionMessage(e))
      NULL
    }
  )

  mod_final <- if (!is.null(mod_gls)) mod_gls else mod_ols
  nome_mod  <- if (!is.null(mod_gls)) "GLS-AR1" else "OLS"

  cf     <- coef(mod_final)
  se_cf  <- if (!is.null(mod_gls)) sqrt(diag(mod_gls$varBeta)) else sqrt(diag(vcov(mod_ols)))
  p_vals <- 2 * pnorm(-abs(cf / se_cf))

  b1 <- cf["mes_num"];   se_b1 <- se_cf["mes_num"]
  b2 <- cf["interv"];    se_b2 <- se_cf["interv"]
  b3 <- cf["tempo_pos"]; se_b3 <- se_cf["tempo_pos"]

  apc_pre  <- (exp(12 * b1) - 1) * 100
  apc_pos  <- (exp(12 * (b1 + b3)) - 1) * 100
  nivel_pct <- (exp(b2) - 1) * 100

  apc_pre_inf <- (exp(12 * (b1 - 1.96 * se_b1)) - 1) * 100
  apc_pre_sup <- (exp(12 * (b1 + 1.96 * se_b1)) - 1) * 100
  apc_pos_inf <- (exp(12 * ((b1+b3) - 1.96 * sqrt(se_b1^2 + se_b3^2))) - 1) * 100
  apc_pos_sup <- (exp(12 * ((b1+b3) + 1.96 * sqrt(se_b1^2 + se_b3^2))) - 1) * 100
  nivel_inf   <- (exp(b2 - 1.96 * se_b2) - 1) * 100
  nivel_sup   <- (exp(b2 + 1.96 * se_b2) - 1) * 100

  phi <- if (!is.null(mod_gls)) {
    round(coef(mod_gls$modelStruct$corStruct, unconstrained = FALSE), 4)
  } else NA_real_

  # Contrafactual
  cf_df <- serie %>% mutate(interv = 0L, tempo_pos = 0L)
  serie <- serie %>%
    mutate(fitted_obs = exp(predict(mod_final)),
           fitted_cf  = exp(predict(mod_final, newdata = cf_df)))

  list(
    modelo   = nome_mod,
    label    = label,
    phi      = phi,
    b1=b1, b2=b2, b3=b3,
    se_b1=se_b1, se_b2=se_b2, se_b3=se_b3,
    p_b1=p_vals["mes_num"], p_b2=p_vals["interv"], p_b3=p_vals["tempo_pos"],
    apc_pre=round(apc_pre,2), apc_pre_inf=round(apc_pre_inf,2), apc_pre_sup=round(apc_pre_sup,2),
    apc_pos=round(apc_pos,2), apc_pos_inf=round(apc_pos_inf,2), apc_pos_sup=round(apc_pos_sup,2),
    nivel_pct=round(nivel_pct,2), nivel_inf=round(nivel_inf,2), nivel_sup=round(nivel_sup,2),
    serie_com_fitted = serie
  )
}

# =============================================================================
# 3. Executa os dois modelos
# =============================================================================

message("\n=== MODELO COMPLETO (com Dengue) ===")
res_full <- fit_its_gls(serie_full, "COMPLETO")

message(sprintf("  APC pré:  %+.1f%% (p=%.4f)", res_full$apc_pre,  res_full$p_b1))
message(sprintf("  Nível:    %+.1f%% (p=%.4f)", res_full$nivel_pct, res_full$p_b2))
message(sprintf("  APC pós:  %+.1f%% (p=%.4f)", res_full$apc_pos,  res_full$p_b3))
message(sprintf("  φ AR(1) = %.4f",              res_full$phi))

message("\n=== MODELO SEM DENGUE ===")
res_nodg <- fit_its_gls(serie_nodg, "SEM DENGUE")

message(sprintf("  APC pré:  %+.1f%% (p=%.4f)", res_nodg$apc_pre,  res_nodg$p_b1))
message(sprintf("  Nível:    %+.1f%% (p=%.4f)", res_nodg$nivel_pct, res_nodg$p_b2))
message(sprintf("  APC pós:  %+.1f%% (p=%.4f)", res_nodg$apc_pos,  res_nodg$p_b3))
message(sprintf("  φ AR(1) = %.4f",              res_nodg$phi))

# =============================================================================
# 4. Tabela de comparação
# =============================================================================

tab_comp <- tibble(
  serie      = c("Completa (com Dengue)", "Sem Dengue (G04)"),
  n_meses    = nrow(base_mes),
  modelo     = c(res_full$modelo, res_nodg$modelo),
  phi_ar1    = c(res_full$phi,    res_nodg$phi),
  apc_pre    = c(res_full$apc_pre,  res_nodg$apc_pre),
  apc_pre_ic = c(sprintf("[%+.1f; %+.1f]", res_full$apc_pre_inf, res_full$apc_pre_sup),
                 sprintf("[%+.1f; %+.1f]", res_nodg$apc_pre_inf, res_nodg$apc_pre_sup)),
  p_pre      = c(round(res_full$p_b1, 4), round(res_nodg$p_b1, 4)),
  nivel_pct  = c(res_full$nivel_pct, res_nodg$nivel_pct),
  nivel_ic   = c(sprintf("[%+.1f; %+.1f]", res_full$nivel_inf, res_full$nivel_sup),
                 sprintf("[%+.1f; %+.1f]", res_nodg$nivel_inf, res_nodg$nivel_sup)),
  p_nivel    = c(round(res_full$p_b2, 4), round(res_nodg$p_b2, 4)),
  apc_pos    = c(res_full$apc_pos,  res_nodg$apc_pos),
  apc_pos_ic = c(sprintf("[%+.1f; %+.1f]", res_full$apc_pos_inf, res_full$apc_pos_sup),
                 sprintf("[%+.1f; %+.1f]", res_nodg$apc_pos_inf, res_nodg$apc_pos_sup)),
  p_pos      = c(round(res_full$p_b3, 4), round(res_nodg$p_b3, 4))
)

message("\n=== TABELA COMPARATIVA ===")
print(as.data.frame(tab_comp))

# Veredicto de robustez
rob_nivel <- res_nodg$p_b2 < 0.05 && res_nodg$nivel_pct < 0
rob_slope <- res_nodg$p_b3 < 0.05 && res_nodg$apc_pos < res_full$apc_pre
message("\n=== ROBUSTEZ ===")
message(sprintf("  Mudança de nível sig. sem Dengue: %s (p=%.4f, nivel=%+.1f%%)",
                if(rob_nivel) "SIM ✓" else "NÃO ✗", res_nodg$p_b2, res_nodg$nivel_pct))
message(sprintf("  Slope change sig. sem Dengue:     %s (p=%.4f, APC pós=%+.1f%%/ano)",
                if(rob_slope) "SIM ✓" else "NÃO ✗", res_nodg$p_b3, res_nodg$apc_pos))
if (rob_nivel || rob_slope) {
  message("  → Efeito ROBUSTO à exclusão de Dengue")
} else {
  message("  → Efeito NÃO robusto — depende das internações por Dengue")
}

write_csv(tab_comp, file.path(DIR_PROC, "its_sem_dengue_resultados.csv"))
message("\nSalvo: data/processed/its_sem_dengue_resultados.csv")

# =============================================================================
# 5. Gráficos
# =============================================================================

message("Gerando docs/its_sem_dengue.png...")

DATA_CORTE <- as.Date("2024-05-01")

theme_its <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle    = element_text(size = 9, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7.5, color = "gray50")
  )

vline <- geom_vline(xintercept = DATA_CORTE, linetype = "dashed",
                    color = "gray30", linewidth = 0.6)
ann_p <- annotate("text", x = DATA_CORTE + 5, y = Inf, label = "mai/2024",
                  hjust = 0, vjust = 1.5, size = 3, color = "gray30")

# --- Painel 1 & 2: séries + contrafactual ---

make_ts_plot <- function(res, titulo, cor_obs, cor_fit, cor_cf) {
  s <- res$serie_com_fitted
  ggplot(s, aes(x = data)) +
    geom_ribbon(
      data = s %>% filter(interv == 1),
      aes(ymin = pmin(fitted_obs, fitted_cf),
          ymax = pmax(fitted_obs, fitted_cf)),
      fill = cor_obs, alpha = 0.12
    ) +
    geom_point(aes(y = taxa_pct), color = cor_obs, alpha = 0.55, size = 1.8) +
    geom_line(aes(y = fitted_obs), color = cor_fit, linewidth = 1.0) +
    geom_line(aes(y = fitted_cf),  color = cor_cf,  linewidth = 0.7,
              linetype = "dashed") +
    vline + ann_p +
    labs(
      title    = titulo,
      subtitle = sprintf("APC pré=%+.1f%%/ano (p=%.4f) | Nível=%+.1f%% (p=%.4f) | APC pós=%+.1f%%/ano (p=%.4f)",
                         res$apc_pre, res$p_b1, res$nivel_pct, res$p_b2,
                         res$apc_pos, res$p_b3),
      x = NULL, y = "Taxa ICSAP (% internações)",
      caption = "Linha sólida = ajustado | tracejada = contrafactual sem intervenção"
    ) +
    theme_its
}

p1 <- make_ts_plot(res_full, "Série Completa (com Dengue)",
                   "#3498db", "#2c3e50", "#7f8c8d")
p2 <- make_ts_plot(res_nodg, "Série Sem Dengue (G04 excluído)",
                   "#e67e22", "#c0392b", "#7f8c8d")

# --- Painel 3: forest plot comparativo ---

forest_df <- tibble(
  serie     = rep(c("Com Dengue", "Sem Dengue"), 3),
  metrica   = rep(c("APC pré (%/ano)", "Mudança nível (%)", "APC pós (%/ano)"), each = 2),
  estimativa = c(res_full$apc_pre,  res_nodg$apc_pre,
                 res_full$nivel_pct, res_nodg$nivel_pct,
                 res_full$apc_pos,  res_nodg$apc_pos),
  ic_inf    = c(res_full$apc_pre_inf,  res_nodg$apc_pre_inf,
                res_full$nivel_inf,     res_nodg$nivel_inf,
                res_full$apc_pos_inf,  res_nodg$apc_pos_inf),
  ic_sup    = c(res_full$apc_pre_sup,  res_nodg$apc_pre_sup,
                res_full$nivel_sup,     res_nodg$nivel_sup,
                res_full$apc_pos_sup,  res_nodg$apc_pos_sup),
  p_val     = c(res_full$p_b1,  res_nodg$p_b1,
                res_full$p_b2,  res_nodg$p_b2,
                res_full$p_b3,  res_nodg$p_b3)
) %>%
  mutate(
    sig   = case_when(p_val < 0.001 ~ "***", p_val < 0.01 ~ "**",
                      p_val < 0.05  ~ "*",   TRUE ~ "NS"),
    lbl   = sprintf("%+.1f%%\n[%+.1f;%+.1f]\n%s", estimativa, ic_inf, ic_sup, sig),
    metrica = factor(metrica, levels = c("APC pré (%/ano)", "Mudança nível (%)", "APC pós (%/ano)"))
  )

p3 <- ggplot(forest_df,
             aes(x = estimativa, xmin = ic_inf, xmax = ic_sup,
                 y = serie, color = serie, shape = serie)) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "gray40") +
  geom_errorbarh(height = 0.2, linewidth = 0.8) +
  geom_point(size = 3.5) +
  geom_text(aes(label = lbl), nudge_y = 0.35, size = 2.6, lineheight = 0.9) +
  facet_wrap(~metrica, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("Com Dengue" = "#3498db", "Sem Dengue" = "#e67e22"),
                     name = NULL) +
  scale_shape_manual(values = c("Com Dengue" = 16, "Sem Dengue" = 17), name = NULL) +
  labs(
    title    = "Comparação ITS: Série Completa vs Série Sem Dengue",
    subtitle = "Ponto = estimativa | barras = IC 95%",
    x = "Estimativa (pp ou %/ano)", y = NULL
  ) +
  theme_its +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# Composição final
p_final <- (p1 / p2 / p3) +
  plot_annotation(
    title   = "Análise de Sensibilidade ITS — Exclusão do Grupo Dengue (G04)",
    caption = sprintf(
      "Fonte: SIHSUS/DATASUS · BH, jan/2022–mar/2026 (n=%d meses) · GLS AR(1) · mai/2026",
      nrow(base_mes)
    ),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.caption = element_text(size = 7.5, color = "gray50")
    )
  ) +
  plot_layout(heights = c(2, 2, 1.8))

ggsave(file.path(DIR_DOCS, "its_sem_dengue.png"),
       p_final, width = 14, height = 16, dpi = 300, bg = "white")

message("Salvo: docs/its_sem_dengue.png")
message("\n====================================")
message("ROBUSTEZ: ", if(rob_nivel || rob_slope) "CONFIRMADA" else "NÃO CONFIRMADA")
message("APC pós COM dengue:  ", sprintf("%+.1f%%/ano (p=%.4f)", res_full$apc_pos, res_full$p_b3))
message("APC pós SEM dengue:  ", sprintf("%+.1f%%/ano (p=%.4f)", res_nodg$apc_pos, res_nodg$p_b3))
message("====================================")
