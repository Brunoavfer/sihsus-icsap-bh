# =============================================================================
# 17_did_its.R
#
# Difference-in-Differences ITS Formal — BH × 4 Capitais Controle
#
# Objetivo: quantificar formalmente quanto a mudança de slope em BH (mai/2024)
#   diferiu das capitais controle, testando a especificidade do efeito.
#
# Lógica DiD-ITS (Dimick & Ryan, JAMA 2014; Bernal et al., BMJ 2017):
#   Modelo pooled com interação cidade × tempo:
#
#   log(taxa_t) = β₀ + β₁·T + β₂·X + β₃·P +
#                 γ·cidade +
#                 δ·cidade:X +      ← DiD nível: diferença de nível
#                 θ·cidade:P +      ← DiD slope: diferença de slope change
#                 sazonalidade + ε
#
#   Com BH como referência:
#     β₂  = mudança de nível em BH (mai/2024)
#     β₃  = mudança de slope em BH
#     θ_k = (slope change da capital k) − (slope change de BH)
#
#   Interpretação de θ_k:
#     θ_k > 0 → capital k desacelerou MENOS que BH → BH teve efeito mais forte
#     θ_k < 0 → capital k desacelerou MAIS que BH → não específico de BH
#     θ_k NS  → sem diferença detectável entre BH e capital k
#
# Pressupostos:
#   - Tendências paralelas pré-intervenção (verificado via gráfico)
#   - AR(1) dentro de cada cidade (GLS com corARMA por grupo)
#
# Saídas:
#   data/processed/did_its_resultados.csv
#   docs/did_its.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(nlme)
})

DIR_PROC <- "data/processed"
DIR_DOCS <- "docs"

# =============================================================================
# 1. Carrega e prepara dados
# =============================================================================

serie <- read_csv(file.path(DIR_PROC, "serie_controles.csv"),
                  col_types = cols(data = col_date()),
                  show_col_types = FALSE)

# Remove meses sem dados (garantia)
serie <- serie %>%
  filter(!is.na(taxa_pct), taxa_pct > 0, !is.na(log_taxa))

# BH como referência
serie$capital <- relevel(factor(serie$capital), ref = "Belo Horizonte")

capitais <- levels(serie$capital)
n_cidades <- length(capitais)

message("=== DiD-ITS FORMAL ===")
message("Cidades: ", paste(capitais, collapse = ", "))
message("Referência: Belo Horizonte")
message("Intervenção: mai/2024 (mes_num = 17)")
message("Observações: ", nrow(serie), " (", n_cidades, " cidades × ~",
        round(nrow(serie) / n_cidades), " meses)")

# =============================================================================
# 2. Verificação de tendências paralelas pré-intervenção (diagnóstico visual)
# =============================================================================

pre <- serie %>% filter(interv == 0)
message("\n--- Taxa ICSAP média pré-intervenção (jan/2023–abr/2024) ---")
pre %>%
  group_by(capital) %>%
  summarise(taxa_media = round(mean(taxa_pct), 2),
            n_meses    = n()) %>%
  print()

# =============================================================================
# 3. Modelo DiD-ITS — GLS com AR(1) por cidade
# =============================================================================

message("\n--- Ajustando modelo DiD-ITS GLS AR(1) ---")

mod_did <- tryCatch(
  gls(
    log_taxa ~ mes_num + interv + tempo_pos +
               capital + capital:interv + capital:tempo_pos +
               sin12 + cos12,
    data        = serie,
    correlation = corARMA(p = 1, q = 0, form = ~mes_num | capital),
    method      = "ML"
  ),
  error = function(e) {
    message("GLS AR(1) falhou: ", e$message, " — tentando OLS")
    tryCatch(
      lm(log_taxa ~ mes_num + interv + tempo_pos +
                    capital + capital:interv + capital:tempo_pos +
                    sin12 + cos12,
         data = serie),
      error = function(e2) { message("OLS também falhou: ", e2$message); NULL }
    )
  }
)

if (is.null(mod_did)) stop("Não foi possível ajustar o modelo DiD-ITS.")

tipo_mod <- if (inherits(mod_did, "gls")) "GLS-AR1" else "OLS"
message("Modelo ajustado: ", tipo_mod)

# Coeficientes e IC 95%
cf  <- coef(mod_did)
se  <- if (inherits(mod_did, "gls")) sqrt(diag(mod_did$varBeta)) else sqrt(diag(vcov(mod_did)))
pv  <- 2 * pnorm(-abs(cf / se))
ci_inf <- cf - 1.96 * se
ci_sup <- cf + 1.96 * se

# =============================================================================
# 4. Extrai coeficientes BH (referência) e DiD para cada cidade controle
# =============================================================================

# APC pré e pós de BH (referência)
b1_bh <- cf["mes_num"]
b2_bh <- cf["interv"]
b3_bh <- cf["tempo_pos"]
se1   <- se["mes_num"]
se2   <- se["interv"]
se3   <- se["tempo_pos"]

message("\n=== COEFICIENTES DE BH (referência) ===")
message(sprintf("  Tendência pré  (β₁): %.5f | APC = %+.1f%%/ano (p=%.4f)",
                b1_bh, (exp(12 * b1_bh) - 1) * 100, pv["mes_num"]))
message(sprintf("  Nível em mai/2024 (β₂): %+.4f → %+.1f%% (p=%.4f)",
                b2_bh, (exp(b2_bh) - 1) * 100, pv["interv"]))
message(sprintf("  Slope change (β₃): %+.5f | APC pós = %+.1f%%/ano (p=%.4f)",
                b3_bh, (exp(12 * (b1_bh + b3_bh)) - 1) * 100, pv["tempo_pos"]))

# DiD: interação capital × slope_change e capital × nível
cidades_ctrl <- setdiff(capitais, "Belo Horizonte")

did_rows <- lapply(cidades_ctrl, function(cid) {
  nm_nivel <- paste0("capital", cid, ":interv")
  nm_slope <- paste0("capital", cid, ":tempo_pos")

  # Tolerante a nomes inexistentes
  get_cf <- function(nm) if (nm %in% names(cf)) cf[nm] else NA_real_
  get_se <- function(nm) if (nm %in% names(se)) se[nm] else NA_real_
  get_pv <- function(nm) if (nm %in% names(pv)) pv[nm] else NA_real_

  did_n  <- get_cf(nm_nivel);  se_n  <- get_se(nm_nivel); pv_n <- get_pv(nm_nivel)
  did_s  <- get_cf(nm_slope);  se_s  <- get_se(nm_slope); pv_s <- get_pv(nm_slope)

  # slope change de cada cidade controle (β₃_BH + θ_k)
  b3_ctrl     <- b3_bh + did_s
  se_b3_ctrl  <- sqrt(se3^2 + se_s^2)   # delta method (sem covariância)

  message(sprintf(
    "\n  DiD [%s vs BH]:\n    θ_nivel: %+.4f → diferença de nível %+.1f%% (p=%.4f)\n    θ_slope: %+.5f (p=%.4f)\n    APC pós ctrl: %+.1f%%/ano",
    cid,
    did_n, (exp(did_n) - 1) * 100, pv_n,
    did_s, pv_s,
    (exp(12 * (b1_bh + b3_ctrl)) - 1) * 100
  ))

  tibble(
    capital          = cid,
    # BH slope change (referência)
    apc_pre_bh       = round((exp(12 * b1_bh) - 1) * 100, 1),
    nivel_bh_pct     = round((exp(b2_bh) - 1) * 100, 1),
    p_nivel_bh       = round(pv["interv"], 4),
    apc_pos_bh       = round((exp(12 * (b1_bh + b3_bh)) - 1) * 100, 1),
    p_slope_bh       = round(pv["tempo_pos"], 4),
    # DiD — diferença em relação a BH
    did_nivel        = round(did_n, 5),
    did_nivel_pct    = round((exp(did_n) - 1) * 100, 1),
    did_nivel_ic_inf = round((exp(did_n - 1.96 * se_n) - 1) * 100, 1),
    did_nivel_ic_sup = round((exp(did_n + 1.96 * se_n) - 1) * 100, 1),
    p_did_nivel      = round(pv_n, 4),
    did_slope        = round(did_s, 5),
    did_slope_ic_inf = round(ci_inf[nm_slope], 5),
    did_slope_ic_sup = round(ci_sup[nm_slope], 5),
    p_did_slope      = round(pv_s, 4),
    # APC pós da capital controle (β₁ + β₃_ctrl)
    apc_pos_ctrl     = round((exp(12 * (b1_bh + b3_ctrl)) - 1) * 100, 1)
  )
})

tab_did <- bind_rows(did_rows)

message("\n=== TABELA DiD ===")
print(tab_did %>% select(capital, did_nivel_pct, p_did_nivel,
                          did_slope, p_did_slope, apc_pos_ctrl),
      n = Inf)

# Interpretação global
sig_pos <- sum(tab_did$p_did_slope < 0.05 & tab_did$did_slope > 0, na.rm = TRUE)
sig_neg <- sum(tab_did$p_did_slope < 0.05 & tab_did$did_slope < 0, na.rm = TRUE)

message("\n=== INTERPRETAÇÃO ===")
message(sprintf(
  "BH slope change: %+.1f%%/ano\n  %d capital(is) com θ > 0 sig (BH desacelerou MAIS → efeito potencialmente específico)\n  %d capital(is) com θ < 0 sig (BH desacelerou MENOS → tendência nacional)",
  (exp(12 * (b1_bh + b3_bh)) - 1) * 100, sig_pos, sig_neg
))

# =============================================================================
# 5. Gráfico — forest plot DiD
# =============================================================================

message("\nGerando gráfico DiD...")

# Prepara dados para o forest plot
forest_df <- tab_did %>%
  mutate(
    sig  = case_when(
      p_did_slope < 0.05 & did_slope > 0 ~ "BH mais forte (p<0,05)",
      p_did_slope < 0.05 & did_slope < 0 ~ "Controle mais forte (p<0,05)",
      TRUE                                 ~ "Sem diferença sig."
    ),
    sig  = factor(sig, levels = c("BH mais forte (p<0,05)",
                                   "Controle mais forte (p<0,05)",
                                   "Sem diferença sig.")),
    # Converte did_slope para APC equivalente para legibilidade
    did_apc     = round((exp(12 * did_slope) - 1) * 100, 1),
    did_apc_inf = round((exp(12 * did_slope_ic_inf) - 1) * 100, 1),
    did_apc_sup = round((exp(12 * did_slope_ic_sup) - 1) * 100, 1),
    lbl         = sprintf("%+.1f%%/ano", did_apc),
    capital     = factor(capital, levels = rev(sort(unique(capital))))
  )

cores_did <- c("BH mais forte (p<0,05)"       = "#2196F3",
               "Controle mais forte (p<0,05)" = "#F44336",
               "Sem diferença sig."            = "#9E9E9E")

p_forest <- ggplot(forest_df,
                   aes(y = capital, x = did_apc,
                       xmin = did_apc_inf, xmax = did_apc_sup,
                       color = sig, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_errorbarh(height = 0.3, linewidth = 0.9) +
  geom_point(size = 4) +
  geom_text(aes(label = lbl, x = did_apc_sup), hjust = -0.2,
            size = 3.5, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = cores_did, name = NULL) +
  scale_shape_manual(values = c("BH mais forte (p<0,05)" = 16,
                                 "Controle mais forte (p<0,05)" = 16,
                                 "Sem diferença sig." = 1),
                     name = NULL) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.05, 0.25))) +
  labs(
    title    = "DiD-ITS: Mudança de Slope após Portaria GM/MS 3.493 (mai/2024)",
    subtitle = paste0(
      "θ = (slope change da capital controle) − (slope change de BH)\n",
      "θ > 0: BH desacelerou mais que o controle | ",
      sprintf("APC pós BH = %+.1f%%/ano", (exp(12 * (b1_bh + b3_bh)) - 1) * 100)
    ),
    x       = "Diferença DiD na APC pós-intervenção (%/ano)",
    y       = NULL,
    caption = paste0(
      "Modelo: GLS AR(1) pooled | IC 95% | Referência: Belo Horizonte\n",
      "Fonte: SIHSUS/DATASUS | Método: DiD-ITS (Dimick & Ryan, JAMA 2014)"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle    = element_text(size = 8.5, hjust = 0.5, color = "gray30"),
    plot.caption     = element_text(size = 7, hjust = 0.5, color = "gray50"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(file.path(DIR_DOCS, "did_its.png"), p_forest,
       width = 10, height = 5.5, dpi = 300, bg = "white")
message("  Gráfico salvo: docs/did_its.png")

# =============================================================================
# 6. Exporta resultados
# =============================================================================

# Adiciona linha BH para referência
tab_bh_ref <- tibble(
  capital          = "Belo Horizonte (referência)",
  apc_pre_bh       = round((exp(12 * b1_bh) - 1) * 100, 1),
  nivel_bh_pct     = round((exp(b2_bh) - 1) * 100, 1),
  p_nivel_bh       = round(pv["interv"], 4),
  apc_pos_bh       = round((exp(12 * (b1_bh + b3_bh)) - 1) * 100, 1),
  p_slope_bh       = round(pv["tempo_pos"], 4),
  did_nivel        = 0,    did_nivel_pct    = 0,
  did_nivel_ic_inf = 0,    did_nivel_ic_sup = 0,   p_did_nivel = NA_real_,
  did_slope        = 0,    did_slope_ic_inf = 0,    did_slope_ic_sup = 0,
  p_did_slope      = NA_real_, apc_pos_ctrl = round((exp(12 * (b1_bh + b3_bh)) - 1) * 100, 1)
)

tab_final <- bind_rows(tab_bh_ref, tab_did)
write_csv(tab_final, file.path(DIR_PROC, "did_its_resultados.csv"))

message("\n======================================")
message("DiD-ITS CONCLUÍDO")
message(sprintf("  BH APC pré: %+.1f%%/ano | APC pós: %+.1f%%/ano",
                (exp(12 * b1_bh) - 1) * 100, (exp(12 * (b1_bh + b3_bh)) - 1) * 100))
message("  Capitais com θ > 0 sig (BH mais forte): ", sig_pos)
message("  Capitais com θ < 0 sig (controle mais forte): ", sig_neg)
message("Saídas:")
message("  data/processed/did_its_resultados.csv")
message("  docs/did_its.png")
message("======================================")
