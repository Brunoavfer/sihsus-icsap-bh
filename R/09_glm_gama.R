# =============================================================================
# 09_glm_gama.R
#
# GLM-Gama e GEE AR-1 para taxas ICSAP por CS × ano
# Unidade de análise: Centro de Saúde (CS) × ano
#
# Modelo principal: GEE com família Gama (link log) e estrutura AR-1
#   Justificativa: Moran's I = 0,283 (p < 0,001) — script 08.
#
# NOTA SOBRE PREDITORES:
#   cobertura_aps_pct e n_esf_egestor são nível MUNICIPAL (mesmo valor para
#   todos os 153 CS em cada mês). Não podem ser usados como preditores CS-nível.
#   A única variável APS no nível CS disponível é n_esf (CNES EP, ~77% CS).
#
# Variável resposta: taxa_cs (por 10.000 hab.)
#   = n_icsap / pop_ref_media_cs × 10.000
#   Usa populacao_referencia do e-Gestor AB por CS × ano (denominador correto)
#
# Modelos:
#   M1 — GLM-Gama baseline: renda + saneamento + favela + tendência temporal
#   M2 — GEE AR-1 principal: idem (100% cobertura)
#   M3 — GEE AR-1 + n_esf: inclui equipes ESF CNES (cobertura ~77%)
#
# Preditores CS-nível (100% cobertura, Censo 2022):
#   renda_media        — renda per capita (salários mínimos)
#   pct_sem_saneamento — % domicílios sem rede geral de água
#   pct_area_favela    — % área de favela por CS
#   ano                — tendência temporal (contínua)
#
# Preditor CS-nível APS (~77% cobertura, CNES EP):
#   n_esf_media        — nº médio de equipes ESF por CS × ano
#
# Referências:
#   Zeger SL, Liang KY. Biometrics. 1986;42(1):121–30.
#   Liang KY, Zeger SL. Biometrika. 1986;73(1):13–22.
#   Pan W. Biometrics. 2001;57(1):120–5. [QIC]
#
# Saídas:
#   data/processed/glm_resultados.csv   — coeficientes, IC 95%, RR por modelo
#   data/processed/glm_diagnosticos.csv — QIC, correlação AR-1, N por modelo
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
})

for (pkg in c("geepack")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(geepack)

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# =============================================================================
# 1. Carrega dados
# =============================================================================

taxas <- read_csv(
  file.path(DIR_PROC, "taxas_padronizadas.csv"),
  show_col_types = FALSE
)

variaveis <- read_csv(
  file.path(DIR_REF, "variaveis_cs.csv"),
  show_col_types = FALSE
)

# =============================================================================
# 2. Agrega variaveis_cs de mensal → anual
#    Censo 2022 (constante por CS): primeiro valor não-NA
#    CNES EP (variável no tempo): média anual
# =============================================================================

variaveis_anuais <- variaveis %>%
  filter(ano_cmpt %in% c(2023, 2024, 2025)) %>%
  group_by(nome_cs, ano_cmpt) %>%
  summarise(
    pop_ref_media      = mean(as.numeric(populacao_referencia), na.rm = TRUE),
    # Censo 2022 — constantes por CS
    renda_media        = first(na.omit(renda_media)),
    pct_sem_saneamento = first(na.omit(pct_sem_saneamento)),
    pct_area_favela    = first(na.omit(pct_area_favela)),
    # CNES EP — varia por CS × mês → média anual
    n_esf_media        = mean(n_esf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(ano = ano_cmpt) %>%
  mutate(
    n_esf_media = ifelse(is.nan(n_esf_media), NA_real_, n_esf_media)
  )

# =============================================================================
# 3. Junta taxas × variaveis; calcula taxa com denominador CS-específico
# =============================================================================

dados_modelo <- taxas %>%
  filter(ano %in% c(2023, 2024, 2025)) %>%
  select(nome_cs, regional, ano, n_icsap) %>%
  left_join(variaveis_anuais, by = c("nome_cs", "ano")) %>%
  filter(
    !is.na(renda_media),
    !is.na(pct_sem_saneamento),
    !is.na(pct_area_favela),
    !is.na(pop_ref_media),
    pop_ref_media > 0
  ) %>%
  mutate(
    taxa_cs   = n_icsap / pop_ref_media * 10000,
    # ano centralizado em 2023 para melhor interpretação do intercepto
    ano_c     = ano - 2023,
    cs_id     = as.integer(factor(nome_cs)),
    ano_seq   = ano_c + 1L   # 1, 2, 3 para AR-1
  ) %>%
  arrange(cs_id, ano_seq)

n_obs <- nrow(dados_modelo)
n_cs  <- n_distinct(dados_modelo$nome_cs)

message("=== DADOS PARA MODELO ===")
message("Observações: ", n_obs, " (", n_cs, " CS × ", n_obs / n_cs, " anos)")
message("Anos: ", paste(sort(unique(dados_modelo$ano)), collapse = ", "))

n_zero <- sum(dados_modelo$taxa_cs <= 0, na.rm = TRUE)
if (n_zero > 0) {
  message("AVISO: ", n_zero, " taxa_cs <= 0 — excluindo")
  dados_modelo <- filter(dados_modelo, taxa_cs > 0)
}

message("\nEstatísticas da resposta (taxa_cs por 10.000 hab.):")
print(summary(dados_modelo$taxa_cs))

message("\nEstatísticas dos preditores:")
dados_modelo %>%
  select(renda_media, pct_sem_saneamento, pct_area_favela, ano_c) %>%
  summary() %>% print()

# =============================================================================
# 4. Modelos
# =============================================================================

formula_m1 <- taxa_cs ~ renda_media + pct_sem_saneamento + pct_area_favela + ano_c
formula_m3 <- taxa_cs ~ n_esf_media + renda_media + pct_sem_saneamento + pct_area_favela + ano_c

dados_cnes <- dados_modelo %>%
  filter(!is.na(n_esf_media)) %>%
  mutate(cs_id2 = as.integer(factor(nome_cs))) %>%
  arrange(cs_id2, ano_seq)

# --- M1: GLM-Gama baseline ---------------------------------------------------
message("\n=== M1: GLM-GAMA (baseline, ignora correlação temporal) ===")
mod_glm <- glm(formula_m1, family = Gamma(link = "log"), data = dados_modelo)
print(summary(mod_glm))

# --- M2: GEE AR-1 principal --------------------------------------------------
message("\n=== M2: GEE AR-1 (modelo principal, 100% cobertura) ===")
mod_gee_ar1 <- tryCatch(
  geeglm(
    formula_m1,
    family  = Gamma(link = "log"),
    data    = dados_modelo,
    id      = cs_id,
    corstr  = "ar1",
    waves   = ano_seq
  ),
  error = function(e) { message("GEE AR-1 falhou: ", conditionMessage(e)); NULL }
)
if (!is.null(mod_gee_ar1)) print(summary(mod_gee_ar1))

# --- M3: GEE AR-1 + n_esf (sensibilidade) -----------------------------------
message("\n=== M3: GEE AR-1 + n_esf CNES (sensibilidade, ~77% CS) ===")
message("N observações com n_esf: ", nrow(dados_cnes))
mod_gee_cnes <- NULL
if (nrow(dados_cnes) >= 30) {
  mod_gee_cnes <- tryCatch(
    geeglm(
      formula_m3,
      family  = Gamma(link = "log"),
      data    = dados_cnes,
      id      = cs_id2,
      corstr  = "ar1",
      waves   = ano_seq
    ),
    error = function(e) { message("GEE AR-1 CNES falhou: ", conditionMessage(e)); NULL }
  )
  if (!is.null(mod_gee_cnes)) print(summary(mod_gee_cnes))
}

# =============================================================================
# 5. Extrai coeficientes com RR e IC 95%
# =============================================================================

extrai_coef <- function(mod, nome_modelo) {
  if (is.null(mod)) return(tibble())
  cf <- tryCatch(coef(summary(mod)), error = function(e) NULL)
  if (is.null(cf)) return(tibble())

  se_col <- if ("Std.err" %in% colnames(cf)) "Std.err" else "Std. Error"
  p_col  <- if ("Pr(>|W|)" %in% colnames(cf)) "Pr(>|W|)" else {
    if ("Pr(>|z|)" %in% colnames(cf)) "Pr(>|z|)" else "Pr(>|t|)"
  }

  tibble(
    modelo    = nome_modelo,
    variavel  = rownames(cf),
    beta      = round(cf[, "Estimate"],    4),
    se        = round(cf[, se_col],        4),
    p_valor   = round(cf[, p_col],         4),
    RR        = round(exp(cf[, "Estimate"]),                              4),
    RR_ic_inf = round(exp(cf[, "Estimate"] - 1.96 * cf[, se_col]),       4),
    RR_ic_sup = round(exp(cf[, "Estimate"] + 1.96 * cf[, se_col]),       4)
  )
}

resultados <- bind_rows(
  extrai_coef(mod_glm,      "M1-GLM-Gama"),
  extrai_coef(mod_gee_ar1,  "M2-GEE-AR1"),
  extrai_coef(mod_gee_cnes, "M3-GEE-AR1-CNES")
)

message("\n=== TABELA DE RESULTADOS ===")
print(resultados, n = Inf)

# =============================================================================
# 6. QIC e diagnósticos
# =============================================================================

extrai_qic <- function(mod, nome_modelo, n) {
  if (is.null(mod)) return(tibble())

  qv <- tryCatch(QIC(mod), error = function(e) NULL)

  qic_val  <- if (!is.null(qv) && "QIC"  %in% names(qv)) round(qv["QIC"],  2) else NA_real_
  qicu_val <- if (!is.null(qv) && "QICu" %in% names(qv)) round(qv["QICu"], 2) else NA_real_

  corr_est <- tryCatch({
    if (inherits(mod, "geeglm")) round(mod$geese$alpha, 4) else NA_real_
  }, error = function(e) NA_real_)

  tibble(modelo = nome_modelo, n_obs = n, QIC = qic_val, QICu = qicu_val, corr_ar1 = corr_est)
}

diagnosticos <- bind_rows(
  extrai_qic(mod_glm,      "M1-GLM-Gama",       nrow(dados_modelo)),
  extrai_qic(mod_gee_ar1,  "M2-GEE-AR1",        nrow(dados_modelo)),
  extrai_qic(mod_gee_cnes, "M3-GEE-AR1-CNES",   nrow(dados_cnes))
)

message("\n=== DIAGNÓSTICOS ===")
print(diagnosticos)

# =============================================================================
# 7. Interpretação do modelo principal (M2)
# =============================================================================

if (!is.null(mod_gee_ar1)) {
  message("\n=== INTERPRETAÇÃO M2: GEE AR-1 ===")
  message("(RR por unidade de aumento em cada preditor)\n")

  cf_m2 <- resultados %>%
    filter(modelo == "M2-GEE-AR1", !str_detect(variavel, "Intercept"))

  for (i in seq_len(nrow(cf_m2))) {
    var <- cf_m2$variavel[i]
    rr  <- cf_m2$RR[i]
    inf <- cf_m2$RR_ic_inf[i]
    sup <- cf_m2$RR_ic_sup[i]
    p   <- cf_m2$p_valor[i]
    sig <- if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else "ns"
    message("  ", str_pad(var, 22), " RR=", rr,
            "  IC95%: ", inf, "–", sup,
            "  p=", p, " ", sig)
  }

  alpha_ar1 <- mod_gee_ar1$geese$alpha
  message("\n  Correlação AR-1 estimada: ", round(alpha_ar1, 4))

  if (alpha_ar1 > 0.3) {
    message("  → Correlação temporal moderada: GEE AR-1 preferível ao GLM padrão.")
  } else if (alpha_ar1 > 0.1) {
    message("  → Correlação temporal fraca-moderada; GEE AR-1 ainda mais conservador.")
  } else {
    message("  → Correlação temporal muito fraca; GLM pode ser suficiente.")
  }
}

# =============================================================================
# 8. Exporta
# =============================================================================

write_csv(resultados,   file.path(DIR_PROC, "glm_resultados.csv"))
write_csv(diagnosticos, file.path(DIR_PROC, "glm_diagnosticos.csv"))

message("\n======================================")
message("GLM-GAMA + GEE AR-1 CONCLUÍDO")
message("")
message("Modelo principal (M2): ", nrow(dados_modelo), " obs | ", n_cs, " CS")
message("Modelo sensibilidade (M3): ", nrow(dados_cnes), " obs")
message("")
message("Saídas:")
message("  ", file.path(DIR_PROC, "glm_resultados.csv"))
message("  ", file.path(DIR_PROC, "glm_diagnosticos.csv"))
message("======================================")
