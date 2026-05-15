# =============================================================================
# 18_its_ivs.R
#
# ITS com Interação IVS × tempo_pos — Heterogeneidade da Resposta à Portaria
#
# Objetivo: testar se o efeito da Portaria GM/MS 3.493/2024 na mudança de
#   slope (tempo_pos) foi heterogêneo conforme a vulnerabilidade social do CS,
#   usando o IVS score como moderador contínuo.
#
# Hipótese:
#   ivs_score:tempo_pos > 0 → CS mais vulneráveis desaceleraram MENOS após a
#     Portaria → ampliação das desigualdades (consistente com script 15)
#   ivs_score:tempo_pos < 0 → CS mais vulneráveis desaceleraram MAIS → redução
#     das desigualdades
#   NS → efeito homogêneo entre estratos de vulnerabilidade
#
# Modelo GEE AR-1 (consistente com scripts 09 e 15):
#   taxa_cs ~ mes_num + interv + tempo_pos +
#             ivs_score + ivs_score:tempo_pos +
#             sin12 + cos12
#
#   Extensão com controle contextual (modelo completo):
#   taxa_cs ~ mes_num + interv + tempo_pos +
#             ivs_score + ivs_score:tempo_pos +
#             pct_sem_saneamento + ivs_score:interv +
#             sin12 + cos12
#
# Dados:
#   n_icsap_cs_mes_prop.csv  → n_icsap com alocação proporcional (script 14)
#   variaveis_cs.csv         → pop_total_censo, ivs_score, pct_sem_saneamento
#
# Saídas:
#   data/processed/its_ivs_resultados.csv
#   docs/its_ivs.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(geepack)   # geeglm
})

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

MES_INTERV <- 17L   # mai/2024

# =============================================================================
# 1. Carrega dados
# =============================================================================

n_prop <- read_csv(file.path(DIR_PROC, "n_icsap_cs_mes_prop.csv"),
                   col_types = cols(ano_cmpt = col_integer(),
                                    mes_cmpt = col_integer()),
                   show_col_types = FALSE)

variaveis <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                      col_types = cols(ano_cmpt = col_integer(),
                                       mes_cmpt = col_integer()),
                      show_col_types = FALSE)

message("n_icsap_cs_mes_prop: ", nrow(n_prop), " obs")
message("variaveis_cs: ", nrow(variaveis), " obs")

# =============================================================================
# 2. Monta painel CS × mês (jan/2023–dez/2025 = 36 meses)
# =============================================================================

# Grade completa: todos CS × todos meses (para AR-1 correto)
cs_lista <- unique(variaveis$nome_cs)
meses_df <- expand.grid(
  nome_cs  = cs_lista,
  ano_cmpt = 2023L:2025L,
  mes_cmpt = 1L:12L,
  stringsAsFactors = FALSE
) %>% as_tibble()

# Pop e covariáveis fixas por CS (constantes no tempo → usa primeiro mês)
cov_cs <- variaveis %>%
  filter(ano_cmpt == 2023L, mes_cmpt == 1L) %>%
  select(nome_cs, regional, pop_total_censo, ivs_score, ivs_predominante,
         pct_sem_saneamento, renda_media) %>%
  mutate(pop_total_censo = as.numeric(pop_total_censo),
         ivs_score       = as.numeric(ivs_score),
         pct_sem_saneamento = as.numeric(pct_sem_saneamento))

# Junta grade com covariáveis e n_icsap
painel <- meses_df %>%
  left_join(cov_cs, by = "nome_cs") %>%
  left_join(n_prop %>% select(nome_cs, ano_cmpt, mes_cmpt, n_icsap_prop),
            by = c("nome_cs", "ano_cmpt", "mes_cmpt")) %>%
  mutate(
    n_icsap_prop = replace_na(n_icsap_prop, 0),
    # Taxa por 10.000 hab/mês — pmax para evitar log(0) com Gamma
    taxa_cs = if_else(
      !is.na(pop_total_censo) & pop_total_censo > 0,
      pmax(n_icsap_prop / (pop_total_censo / 12) * 10000, 0.01),
      NA_real_
    )
  ) %>%
  filter(!is.na(taxa_cs), !is.na(ivs_score)) %>%
  arrange(nome_cs, ano_cmpt, mes_cmpt) %>%
  group_by(nome_cs) %>%
  mutate(
    mes_num   = row_number(),
    interv    = as.integer(mes_num >= MES_INTERV),
    tempo_pos = as.integer(pmax(0L, mes_num - (MES_INTERV - 1L))),
    sin12     = sin(2 * pi * mes_num / 12),
    cos12     = cos(2 * pi * mes_num / 12)
  ) %>%
  ungroup() %>%
  # CS completo: precisa de ≥ 30 meses para AR-1
  group_by(nome_cs) %>%
  filter(n() >= 30L) %>%
  ungroup()

n_cs  <- length(unique(painel$nome_cs))
n_obs <- nrow(painel)
message("\nPainel montado: ", n_obs, " obs | ", n_cs, " CS")

# ID numérico para GEE (requerido como integer)
painel <- painel %>%
  mutate(cs_id = as.integer(factor(nome_cs)))

# Centraliza ivs_score (média 0) para facilitar interpretação do intercepto
ivs_mean <- mean(painel$ivs_score, na.rm = TRUE)
ivs_sd   <- sd(painel$ivs_score, na.rm = TRUE)
painel   <- painel %>%
  mutate(ivs_z = (ivs_score - ivs_mean) / ivs_sd)

message(sprintf("IVS score: média=%.2f | DP=%.2f | centralizado como ivs_z",
                ivs_mean, ivs_sd))

# =============================================================================
# 3. Modelos GEE AR-1
# =============================================================================

ajusta_gee <- function(formula, dados, label) {
  message("\n--- ", label, " ---")
  mod <- tryCatch(
    geeglm(formula,
           family  = Gamma(link = "log"),
           data    = dados,
           id      = cs_id,
           waves   = mes_num,
           corstr  = "ar1"),
    error = function(e) {
      message("  AR-1 falhou: ", e$message, " — tentando exchangeable")
      tryCatch(
        geeglm(formula,
               family = Gamma(link = "log"),
               data   = dados,
               id     = cs_id,
               waves  = mes_num,
               corstr = "exchangeable"),
        error = function(e2) { message("  Também falhou: ", e2$message); NULL }
      )
    }
  )
  if (is.null(mod)) return(NULL)

  cf  <- coef(summary(mod))
  message(sprintf("  %-30s %8s %8s %8s %8s", "Coeficiente", "Est.", "EP", "RR", "p-valor"))
  for (nm in rownames(cf)) {
    est <- cf[nm, "Estimate"]
    ep  <- cf[nm, "Std.err"]
    pv  <- cf[nm, grep("Pr", colnames(cf), value = TRUE)]
    message(sprintf("  %-30s %+8.4f %8.4f %8.3f %8.4f", nm, est, ep, exp(est), pv))
  }
  mod
}

# M1: modelo ITS base (sem IVS)
f_base <- taxa_cs ~ mes_num + interv + tempo_pos + sin12 + cos12
mod_base <- ajusta_gee(f_base, painel, "M1 — ITS base (sem IVS)")

# M2: ITS + IVS score (nível) + interação ivs:tempo_pos
f_int <- taxa_cs ~ mes_num + interv + tempo_pos +
                   ivs_z + ivs_z:tempo_pos +
                   sin12 + cos12
mod_int <- ajusta_gee(f_int, painel, "M2 — ITS + ivs_z + ivs_z:tempo_pos")

# M3: modelo completo — inclui ivs_z:interv + pct_sem_saneamento
f_comp <- taxa_cs ~ mes_num + interv + tempo_pos +
                    ivs_z + ivs_z:interv + ivs_z:tempo_pos +
                    pct_sem_saneamento +
                    sin12 + cos12
mod_comp <- ajusta_gee(f_comp, painel, "M3 — Completo (ivs_z × interv + ivs_z × tempo_pos + sanea)")

# =============================================================================
# 4. Interpreta o coeficiente da interação ivs_z:tempo_pos
# =============================================================================

interpretar_interacao <- function(mod, label) {
  if (is.null(mod)) return(NULL)
  cf_s <- coef(summary(mod))
  p_col <- grep("Pr", colnames(cf_s), value = TRUE)[1]

  nm_int <- "ivs_z:tempo_pos"
  if (!nm_int %in% rownames(cf_s)) {
    nm_int <- "tempo_pos:ivs_z"
    if (!nm_int %in% rownames(cf_s)) { message("  Interação não encontrada em ", label); return(NULL) }
  }

  est <- cf_s[nm_int, "Estimate"]
  ep  <- cf_s[nm_int, "Std.err"]
  pv  <- cf_s[nm_int, p_col]
  rr  <- exp(est)
  ic_inf <- exp(est - 1.96 * ep)
  ic_sup <- exp(est + 1.96 * ep)

  message(sprintf(
    "\n  [%s] ivs_z:tempo_pos → RR=%+.3f (IC95%%: %.3f–%.3f) p=%.4f",
    label, rr, ic_inf, ic_sup, pv
  ))

  if (pv < 0.05) {
    if (est > 0) {
      message("  → CS mais vulneráveis (↑IVS) desaceleraram MENOS pós-Portaria")
      message("    → Portaria foi MENOS efetiva em CS vulneráveis → ampliação de desigualdades")
    } else {
      message("  → CS mais vulneráveis (↑IVS) desaceleraram MAIS pós-Portaria")
      message("    → Portaria foi MAIS efetiva em CS vulneráveis → redução de desigualdades")
    }
  } else {
    message("  → Efeito homogêneo — sem evidência de heterogeneidade por IVS (p=", round(pv, 3), ")")
  }

  tibble(
    modelo   = label,
    coef     = nm_int,
    estimate = round(est, 5),
    ep       = round(ep, 5),
    rr       = round(rr, 4),
    ic_inf   = round(ic_inf, 4),
    ic_sup   = round(ic_sup, 4),
    p_valor  = round(pv, 4)
  )
}

message("\n=== INTERPRETAÇÃO DA INTERAÇÃO IVS × SLOPE ===")
res_int <- bind_rows(
  interpretar_interacao(mod_int,  "M2"),
  interpretar_interacao(mod_comp, "M3")
)
print(res_int)

# =============================================================================
# 5. Extrai tabela completa de coeficientes
# =============================================================================

extrair_coefs <- function(mod, nome_mod) {
  if (is.null(mod)) return(NULL)
  cf_s  <- coef(summary(mod))
  p_col <- grep("Pr", colnames(cf_s), value = TRUE)[1]
  est   <- cf_s[, "Estimate"]
  ep    <- cf_s[, "Std.err"]
  pv    <- cf_s[, p_col]
  tibble(
    modelo     = nome_mod,
    coeficiente = rownames(cf_s),
    estimate   = round(est, 5),
    ep         = round(ep, 5),
    rr         = round(exp(est), 4),
    ic_inf     = round(exp(est - 1.96 * ep), 4),
    ic_sup     = round(exp(est + 1.96 * ep), 4),
    p_valor    = round(pv, 4)
  )
}

tab_coefs <- bind_rows(
  extrair_coefs(mod_base, "M1-base"),
  extrair_coefs(mod_int,  "M2-interacao"),
  extrair_coefs(mod_comp, "M3-completo")
)

# =============================================================================
# 6. Gráfico — efeito marginal da interação IVS × slope
# =============================================================================

message("\nGerando gráfico...")

if (!is.null(mod_int)) {
  cf_int <- coef(mod_int)

  # Predição do slope change (em taxa/mês) por quartis de IVS
  ivs_vals <- quantile(painel$ivs_z, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
  ivs_labels <- sprintf("IVS z=%+.1f\n(%.2f)", ivs_vals,
                        ivs_vals * ivs_sd + ivs_mean)

  # Para cada nível de IVS, APC pós líquida = exp(12*(β_mes + β_tempo_pos + IVS*β_int)) - 1
  b_mes   <- cf_int["mes_num"]
  b_tpos  <- cf_int["tempo_pos"]
  b_ivs   <- if ("ivs_z" %in% names(cf_int)) cf_int["ivs_z"] else 0
  b_int   <- if ("ivs_z:tempo_pos" %in% names(cf_int)) cf_int["ivs_z:tempo_pos"]
              else if ("tempo_pos:ivs_z" %in% names(cf_int)) cf_int["tempo_pos:ivs_z"]
              else 0

  pred_df <- tibble(ivs_z = ivs_vals) %>%
    mutate(
      apc_pre  = round((exp(12 * b_mes) - 1) * 100, 1),
      # slope pós = β_mes + β_tempo_pos + ivs_z * β_int
      slope_pos = b_mes + b_tpos + ivs_z * b_int,
      apc_pos  = round((exp(12 * slope_pos) - 1) * 100, 1),
      ivs_orig = round(ivs_z * ivs_sd + ivs_mean, 2),
      lbl      = sprintf("IVS=%.2f\n%+.1f%%/ano", ivs_orig, apc_pos)
    )

  message("\nEfeito marginal por quartil de IVS:")
  print(pred_df %>% select(ivs_orig, apc_pre, apc_pos))

  # Curva contínua do efeito
  ivs_seq <- seq(min(painel$ivs_z, na.rm = TRUE),
                 max(painel$ivs_z, na.rm = TRUE), length.out = 100)
  curva_df <- tibble(ivs_z = ivs_seq) %>%
    mutate(
      ivs_orig = ivs_z * ivs_sd + ivs_mean,
      apc_pos  = (exp(12 * (b_mes + b_tpos + ivs_z * b_int)) - 1) * 100
    )

  # Série temporal por estrato IVS (observado)
  painel_ivs <- painel %>%
    mutate(ivs_cat = cut(ivs_score,
                         breaks = quantile(ivs_score, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                         labels = c("Q1 Baixo", "Q2 Médio-Baixo",
                                    "Q3 Médio-Alto", "Q4 Alto"),
                         include.lowest = TRUE)) %>%
    filter(!is.na(ivs_cat)) %>%
    group_by(ivs_cat, mes_num) %>%
    summarise(taxa_media = mean(taxa_cs, na.rm = TRUE), .groups = "drop") %>%
    mutate(data = as.Date("2023-01-01") + months(mes_num - 1))

  p_serie <- ggplot(painel_ivs, aes(x = data, y = taxa_media, color = ivs_cat)) +
    annotate("rect",
             xmin = as.Date("2024-05-01"), xmax = max(painel_ivs$data),
             ymin = -Inf, ymax = Inf, fill = "#ffeeba", alpha = 0.4) +
    geom_vline(xintercept = as.Date("2024-05-01"),
               linetype = "dashed", color = "#e67e22", linewidth = 0.7) +
    geom_line(linewidth = 1.0) +
    geom_point(size = 1.5, alpha = 0.7) +
    scale_color_manual(
      values = c("Q1 Baixo"       = "#4CAF50",
                 "Q2 Médio-Baixo" = "#2196F3",
                 "Q3 Médio-Alto"  = "#FF9800",
                 "Q4 Alto"        = "#F44336"),
      name = "IVS (quartil)"
    ) +
    labs(
      title    = "Taxas ICSAP por Quartil de IVS — BH (jan/2023–dez/2025)",
      subtitle = "Área amarela = pós-Portaria GM/MS 3.493 | Taxa por 10.000 hab/mês",
      x = NULL, y = "Taxa ICSAP (por 10.000 hab/mês)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 8, hjust = 0.5, color = "gray40"),
          panel.grid.minor = element_blank(),
          legend.position = "right")

  p_curva <- ggplot(curva_df, aes(x = ivs_orig, y = apc_pos)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = apc_pos - 5, ymax = apc_pos + 5),
                fill = "#2196F3", alpha = 0.15) +
    geom_line(color = "#1565C0", linewidth = 1.2) +
    geom_point(data = pred_df,
               aes(x = ivs_orig, y = apc_pos),
               color = "#F44336", size = 3, shape = 18) +
    scale_y_continuous(labels = function(y) paste0(round(y, 0), "%")) +
    labs(
      title    = "APC Pós-Portaria por IVS Score (M2 — GEE AR-1)",
      subtitle = paste0(
        "Efeito marginal de ivs_z:tempo_pos\n",
        if (!is.null(res_int) && nrow(res_int) > 0 && res_int$modelo[1] == "M2")
          sprintf("RR interação = %.3f (p=%.4f)", res_int$rr[1], res_int$p_valor[1])
        else "ver tabela did_its_resultados.csv"
      ),
      x = "IVS Score (1 = baixa vulnerabilidade → 4 = muito elevada)",
      y = "APC pós-Portaria (%/ano)",
      caption = "Losangos vermelhos = quartis P10/P25/P50/P75/P90 | Faixa = ±5 unidades (ilustrativa)"
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 8, hjust = 0.5, color = "gray40"),
          plot.caption = element_text(size = 7, color = "gray50", hjust = 0.5),
          panel.grid.minor = element_blank())

  # Combina com patchwork se disponível
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    p_final <- p_serie / p_curva + plot_layout(heights = c(1.5, 1))
  } else {
    p_final <- p_curva   # fallback sem patchwork
  }

  ggsave(file.path(DIR_DOCS, "its_ivs.png"), p_final,
         width = 10, height = 10, dpi = 300, bg = "white")
  message("  Gráfico salvo: docs/its_ivs.png")
}

# =============================================================================
# 7. Exporta resultados
# =============================================================================

write_csv(tab_coefs, file.path(DIR_PROC, "its_ivs_resultados.csv"))

message("\n======================================")
message("ITS × IVS CONCLUÍDO")
message("  CS incluídos: ", n_cs, " | Observações: ", n_obs)
if (!is.null(res_int) && nrow(res_int) > 0) {
  ri <- res_int %>% filter(modelo == "M2")
  if (nrow(ri) > 0) {
    message(sprintf("  M2 ivs_z:tempo_pos → RR=%.3f (p=%.4f) — %s",
                    ri$rr, ri$p_valor,
                    ifelse(ri$p_valor < 0.05,
                           ifelse(ri$estimate > 0,
                                  "CS mais vulneráveis desaceleraram MENOS",
                                  "CS mais vulneráveis desaceleraram MAIS"),
                           "sem heterogeneidade sig.")))
  }
}
message("Saídas:")
message("  data/processed/its_ivs_resultados.csv")
message("  docs/its_ivs.png")
message("======================================")
