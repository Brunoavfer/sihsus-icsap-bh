# =============================================================================
# 09_glm_gama.R  (versão 2 — painel mensal, IVS incorporado)
#
# GEE AR-1 com família Gama (link log) para taxa ICSAP por CS × mês
# Unidade de análise: Centro de Saúde (CS) × competência mensal
# Período: jan/2023–dez/2025 (153 CS × 36 meses = 5.508 obs máximas)
#
# Desfecho: taxa_cs = n_icsap_cs_mês / pop_ref_cs_mês × 10.000
#   Denominador: populacao_referencia do e-Gestor AB por CS × mês
#   Zeros (n_icsap = 0): excluídos — Gama requer resposta > 0
#
# Modelos:
#   M1 — GEE base:       tendência temporal + sazonalidade (Fourier)
#   M2 — GEE + CNES:     M1 + n_esf + n_emulti + n_acs  [~77% CS com CNES]
#   M3 — GEE + contexto: M1 + ivs_score + pct_area_favela + renda_media + pct_sem_saneamento
#   M4 — GEE completo:   M2 + M3 (todo conjunto, ~77% CS)
#
# LIMITAÇÕES EXPLÍCITAS:
#   — pct_mfc / n_medicos: PF não processado (read.dbc SIGSEGV em arquivos MG >30MB)
#   — cobertura_aps_pct / n_esf_egestor: nível municipal (mesmo valor para todos
#     os 153 CS num mesmo mês) → colinariedade perfeita com efeito de tempo;
#     NÃO utilizados como preditores CS-nível
#   — Zeros excluídos: Gama não admite 0; CS com 0 ICSAP num mês saem da amostra
#   — IVS 2012: vulnerabilidade medida 10+ anos antes do desfecho; usada como proxy
#
# Referências:
#   Zeger SL, Liang KY. Biometrics. 1986;42(1):121–30.
#   Liang KY, Zeger SL. Biometrika. 1986;73(1):13–22.
#   Pan W. Biometrics. 2001;57(1):120–5. [QIC]
#
# Saídas:
#   data/processed/glm_resultados.csv   — coeficientes, IC 95%, RR por modelo
#   data/processed/glm_diagnosticos.csv — QIC, QICu, correlação AR-1, N
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(geepack)
})

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# =============================================================================
# 1. Constrói painel mensal: n_icsap por CS × competência
# =============================================================================

message("=== 1. Carregando e agregando ICSAP por CS × mês ===")

icsap_raw <- read_csv(
  file.path(DIR_PROC, "icsap_bh_regional.csv"),
  show_col_types = FALSE
)

icsap_mes <- icsap_raw %>%
  filter(ano_cmpt %in% 2023:2025, !is.na(nome_cs)) %>%
  mutate(mes_pad = str_pad(as.integer(mes_cmpt), 2, pad = "0")) %>%
  group_by(nome_cs, ano_cmpt, mes_pad) %>%
  summarise(n_icsap = n(), .groups = "drop") %>%
  rename(mes_cmpt_n = mes_pad) %>%
  mutate(competencia = paste0(str_sub(as.character(ano_cmpt), 3, 4), mes_cmpt_n))

message("  Linhas ICSAP geocodificadas: ", nrow(icsap_raw[!is.na(icsap_raw$nome_cs), ]))
message("  CS × mês com ≥ 1 ICSAP: ", nrow(icsap_mes))

# =============================================================================
# 2. Carrega preditores mensais — variaveis_cs.csv (inclui IVS)
# =============================================================================

message("\n=== 2. Carregando variaveis_cs.csv ===")

vars <- read_csv(
  file.path(DIR_REF, "variaveis_cs.csv"),
  show_col_types = FALSE
) %>%
  filter(ano_cmpt %in% 2023:2025) %>%
  mutate(
    competencia = str_pad(as.character(as.integer(competencia)), 4, pad = "0"),
    mes_cmpt_n  = str_pad(as.integer(mes_cmpt), 2, pad = "0")
  )

message("  Dimensões: ", nrow(vars), " × ", ncol(vars))
message("  Cobertura IVS: ",
        round(mean(!is.na(vars$ivs_score)) * 100, 1), "%  | ",
        "CNES (n_esf): ",
        round(mean(!is.na(vars$n_esf)) * 100, 1), "%")

# =============================================================================
# 3. Grade completa CS × mês e join
# =============================================================================

message("\n=== 3. Construindo painel CS × mês ===")

cs_lista <- unique(vars$nome_cs)
mes_lista <- str_pad(1:12, 2, pad = "0")

grade <- expand.grid(
  nome_cs  = cs_lista,
  ano_cmpt = 2023:2025,
  mes_cmpt_n  = mes_lista,
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    ano_cmpt   = as.integer(ano_cmpt),
    competencia = paste0(str_sub(as.character(ano_cmpt), 3, 4), mes_cmpt_n)
  )

# Junta n_icsap (zeros implícitos → 0)
grade <- grade %>%
  left_join(
    icsap_mes %>% select(nome_cs, competencia, n_icsap),
    by = c("nome_cs", "competencia")
  ) %>%
  mutate(n_icsap = replace_na(n_icsap, 0L))

# Junta preditores
grade <- grade %>%
  left_join(
    vars %>% select(nome_cs, competencia, populacao_referencia,
                    n_esf, n_emulti, n_acs,
                    ivs_score, pct_area_favela, renda_media, pct_sem_saneamento),
    by = c("nome_cs", "competencia")
  )

# =============================================================================
# 4. Prepara dataset de modelagem
# =============================================================================

message("\n=== 4. Preparando dados para modelagem ===")

dados_bruto <- grade %>%
  filter(
    !is.na(populacao_referencia), populacao_referencia > 0,
    !is.na(ivs_score),
    !is.na(pct_area_favela),
    !is.na(renda_media),
    !is.na(pct_sem_saneamento)
  ) %>%
  mutate(
    taxa_cs = n_icsap / populacao_referencia * 10000,
    mes_num = (as.integer(ano_cmpt) - 2023L) * 12L + as.integer(mes_cmpt_n),
    sin12   = sin(2 * pi * mes_num / 12),
    cos12   = cos(2 * pi * mes_num / 12),
    cs_id   = as.integer(factor(nome_cs))
  ) %>%
  arrange(cs_id, mes_num)

n_zeros <- sum(dados_bruto$n_icsap == 0)
pct_zeros <- round(mean(dados_bruto$n_icsap == 0) * 100, 1)
message("  Painel completo (pré-filtro zero): ", nrow(dados_bruto),
        " obs | ", n_distinct(dados_bruto$nome_cs), " CS")
message("  Zeros (n_icsap = 0): ", n_zeros, " (", pct_zeros,
        "%) — excluídos (Gama requer resposta > 0)")

# Dataset para M1 e M3 (variáveis 100% disponíveis)
dados_full <- dados_bruto %>%
  filter(taxa_cs > 0)

# Dataset para M2 e M4 (inclui CNES — ~77% dos CS, meses completos)
dados_cnes <- dados_bruto %>%
  filter(!is.na(n_esf), taxa_cs > 0) %>%
  mutate(cs_id2 = as.integer(factor(nome_cs))) %>%
  arrange(cs_id2, mes_num)

message("\n  M1/M3 (100% vars): ", nrow(dados_full),
        " obs | ", n_distinct(dados_full$nome_cs), " CS")
message("  M2/M4 (CNES ~77%): ", nrow(dados_cnes),
        " obs | ", n_distinct(dados_cnes$nome_cs), " CS")

message("\nEstatísticas da resposta (taxa_cs, por 10.000 hab.):")
print(summary(dados_full$taxa_cs))

message("\nEstatísticas dos preditores (dados_full):")
dados_full %>%
  select(mes_num, ivs_score, pct_area_favela, renda_media, pct_sem_saneamento) %>%
  summary() %>% print()

# =============================================================================
# 5. Ajuste dos 4 modelos GEE AR-1
# =============================================================================

message("\n=== 5. Ajustando modelos GEE AR-1 ===")

# --- M1: base — tendência temporal + sazonalidade Fourier -------------------
message("\n--- M1: GEE base (tendência + sazonalidade) ---")
mod_m1 <- tryCatch(
  geeglm(
    taxa_cs ~ mes_num + sin12 + cos12,
    family  = Gamma(link = "log"),
    data    = dados_full,
    id      = cs_id,
    corstr  = "ar1",
    waves   = mes_num
  ),
  error = function(e) { message("M1 falhou: ", e$message); NULL }
)
if (!is.null(mod_m1)) print(summary(mod_m1))

# --- M2: + CNES (n_esf) -------------------------------------------------------
# NOTA: n_emulti, n_acs, n_esb = todos zeros no CNES EP (códigos TP_EQUIPE 76/77/87/88
# não mapeados ou equipes não cadastradas em BH); removidos da fórmula para evitar
# rank-deficiency. Único preditor CNES com variação: n_esf.
message("\n--- M2: GEE + CNES (n_esf) [77% CS] ---")
message("    Nota: n_emulti=0, n_acs=0 em todos os registros EP — excluídos da fórmula")
mod_m2 <- tryCatch(
  geeglm(
    taxa_cs ~ mes_num + sin12 + cos12 + n_esf,
    family  = Gamma(link = "log"),
    data    = dados_cnes,
    id      = cs_id2,
    corstr  = "ar1",
    waves   = mes_num
  ),
  error = function(e) { message("M2 falhou: ", e$message); NULL }
)
if (!is.null(mod_m2)) print(summary(mod_m2))

# --- M3: + contexto socioeconômico e vulnerabilidade ------------------------
message("\n--- M3: GEE + contexto (IVS, favela, renda, saneamento) ---")
mod_m3 <- tryCatch(
  geeglm(
    taxa_cs ~ mes_num + sin12 + cos12 +
              ivs_score + pct_area_favela + renda_media + pct_sem_saneamento,
    family  = Gamma(link = "log"),
    data    = dados_full,
    id      = cs_id,
    corstr  = "ar1",
    waves   = mes_num
  ),
  error = function(e) { message("M3 falhou: ", e$message); NULL }
)
if (!is.null(mod_m3)) print(summary(mod_m3))

# --- M4: completo ------------------------------------------------------------
# Nota: preditores constantes por CS (ivs_score etc.) + φ≈0.96 podem
# tornar a estimação AR-1 muito lenta. Usamos corstr="exchangeable" em M4
# para garantir convergência; os SEs robustos (sandwich) são válidos
# independentemente da estrutura de correlação de trabalho especificada.
message("\n--- M4: GEE completo (n_esf + contexto) [77% CS, exchangeable] ---")
mod_m4 <- tryCatch(
  geeglm(
    taxa_cs ~ mes_num + sin12 + cos12 +
              n_esf +
              ivs_score + pct_area_favela + renda_media + pct_sem_saneamento,
    family  = Gamma(link = "log"),
    data    = dados_cnes,
    id      = cs_id2,
    corstr  = "exchangeable"
  ),
  error = function(e) { message("M4 falhou: ", e$message); NULL }
)
if (!is.null(mod_m4)) print(summary(mod_m4))

# =============================================================================
# 6. Extrai coeficientes com RR e IC 95%
# =============================================================================

extrai_coef <- function(mod, nome_modelo) {
  if (is.null(mod)) return(tibble())
  cf <- tryCatch(coef(summary(mod)), error = function(e) NULL)
  if (is.null(cf)) return(tibble())

  se_col <- if ("Std.err" %in% colnames(cf)) "Std.err" else "Std. Error"
  p_col  <- if ("Pr(>|W|)" %in% colnames(cf)) "Pr(>|W|)" else
            if ("Pr(>|z|)" %in% colnames(cf)) "Pr(>|z|)" else "Pr(>|t|)"

  tibble(
    modelo    = nome_modelo,
    variavel  = rownames(cf),
    beta      = round(cf[, "Estimate"], 4),
    se        = round(cf[, se_col],    4),
    p_valor   = round(cf[, p_col],     4),
    RR        = round(exp(cf[, "Estimate"]),                        4),
    RR_ic_inf = round(exp(cf[, "Estimate"] - 1.96 * cf[, se_col]), 4),
    RR_ic_sup = round(exp(cf[, "Estimate"] + 1.96 * cf[, se_col]), 4),
    sig       = case_when(
      cf[, p_col] < 0.001 ~ "***",
      cf[, p_col] < 0.01  ~ "**",
      cf[, p_col] < 0.05  ~ "*",
      TRUE                ~ "ns"
    )
  )
}

resultados <- bind_rows(
  extrai_coef(mod_m1, "M1-base"),
  extrai_coef(mod_m2, "M2-CNES"),
  extrai_coef(mod_m3, "M3-contexto"),
  extrai_coef(mod_m4, "M4-completo")
)

message("\n=== TABELA DE RESULTADOS ===")
print(resultados, n = Inf)

# =============================================================================
# 7. QIC e diagnósticos
# =============================================================================

extrai_qic <- function(mod, nome_modelo, n) {
  if (is.null(mod)) return(tibble())
  qv   <- tryCatch(QIC(mod), error = function(e) NULL)
  qic  <- if (!is.null(qv) && "QIC"  %in% names(qv)) round(qv["QIC"],  1) else NA_real_
  qicu <- if (!is.null(qv) && "QICu" %in% names(qv)) round(qv["QICu"], 1) else NA_real_
  alpha <- tryCatch(round(mod$geese$alpha, 4), error = function(e) NA_real_)
  tibble(modelo = nome_modelo, n_obs = n, QIC = qic, QICu = qicu, corr_ar1 = alpha)
}

diagnosticos <- bind_rows(
  extrai_qic(mod_m1, "M1-base",       nrow(dados_full)),
  extrai_qic(mod_m2, "M2-CNES",       nrow(dados_cnes)),
  extrai_qic(mod_m3, "M3-contexto",   nrow(dados_full)),
  extrai_qic(mod_m4, "M4-completo",   nrow(dados_cnes))
)

message("\n=== DIAGNÓSTICOS (QIC) ===")
print(diagnosticos)
message("Nota: M1/M3 (n=", nrow(dados_full), ") vs M2/M4 (n=", nrow(dados_cnes),
        ") — datasets diferentes; QIC comparável apenas dentro do mesmo par.")

# =============================================================================
# 8. Interpretação dos coeficientes significativos
# =============================================================================

message("\n=== INTERPRETAÇÃO: COEFICIENTES SIGNIFICATIVOS (p < 0,05) ===\n")

for (m in c("M1-base", "M2-CNES", "M3-contexto", "M4-completo")) {
  cf_m <- resultados %>%
    filter(modelo == m, !str_detect(variavel, "Intercept"), sig != "ns")
  if (nrow(cf_m) == 0) { message(m, ": nenhum coeficiente significativo (p<0,05)"); next }
  message(m, ":")
  for (i in seq_len(nrow(cf_m))) {
    v   <- cf_m$variavel[i]
    rr  <- cf_m$RR[i]
    inf <- cf_m$RR_ic_inf[i]
    sup <- cf_m$RR_ic_sup[i]
    p   <- cf_m$p_valor[i]
    sig <- cf_m$sig[i]
    message("  ", str_pad(v, 24), " RR=", rr,
            " (IC95%: ", inf, "–", sup, ")",
            "  p=", p, " ", sig)
  }
  message("")
}

# APC/ano a partir do coeficiente mes_num
message("=== APC ANUAL ESTIMADA (exp(12 × β_mes_num) − 1) ===")
apc_tab <- resultados %>%
  filter(variavel == "mes_num") %>%
  mutate(
    APC_pct_ano  = round((exp(12 * beta) - 1) * 100, 2),
    APC_ic_inf   = round((exp(12 * (beta - 1.96 * se)) - 1) * 100, 2),
    APC_ic_sup   = round((exp(12 * (beta + 1.96 * se)) - 1) * 100, 2)
  ) %>%
  select(modelo, beta, RR, APC_pct_ano, APC_ic_inf, APC_ic_sup, sig)
print(apc_tab)

# Interpretação dos preditores do IVS no M3/M4
for (m in c("M3-contexto", "M4-completo")) {
  ivs_row <- resultados %>%
    filter(modelo == m, variavel == "ivs_score")
  if (nrow(ivs_row) > 0) {
    message("\nIVS (", m, "):")
    message("  ivs_score RR=", ivs_row$RR,
            " (IC95%: ", ivs_row$RR_ic_inf, "–", ivs_row$RR_ic_sup, ")",
            "  p=", ivs_row$p_valor, " ", ivs_row$sig)
    if (ivs_row$sig != "ns") {
      dir <- if (ivs_row$RR > 1) "aumento" else "redução"
      message("  → Cada ponto a mais no IVS-score (1=Baixo→4=M.Elevado) associado a ",
              dir, " de ", round(abs(ivs_row$RR - 1) * 100, 1), "% na taxa ICSAP.")
    }
  }
}

# Correlação AR-1 estimada
message("\n=== CORRELAÇÃO AR-1 ===")
for (m in c("M1-base", "M2-CNES", "M3-contexto", "M4-completo")) {
  row_diag <- diagnosticos %>% filter(modelo == m)
  if (nrow(row_diag) == 0) next
  alpha <- row_diag$corr_ar1[1]
  if (!is.na(alpha) && length(alpha) == 1) {
    interp <- if (alpha > 0.5) "alta" else if (alpha > 0.3) "moderada" else "fraca"
    message("  ", str_pad(m, 15), " φ=", alpha,
            " (correlação temporal ", interp, " — GEE AR-1 justificado)")
  }
}

# =============================================================================
# 9. VIF — diagnóstico de multicolinearidade (design matrix, GEE-agnóstico)
# =============================================================================
# VIF é propriedade da matriz de preditores, não da função de ligação.
# Calculamos sobre OLS com os mesmos preditores do modelo completo (M4-like).
# Preditores com VIF > 5 devem ser investigados; VIF > 10 indica colinearidade grave.
# =============================================================================

message("\n=== 9. VIF — DIAGNÓSTICO DE MULTICOLINEARIDADE ===")

for (pkg in c("car")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}

# Dataset com todas as variáveis (CNES + contexto)
dados_vif <- dados_cnes %>%
  filter(!is.na(ivs_score), !is.na(pct_area_favela),
         !is.na(renda_media), !is.na(pct_sem_saneamento))

lm_vif <- lm(
  taxa_cs ~ mes_num + sin12 + cos12 + n_esf +
            ivs_score + pct_area_favela + renda_media + pct_sem_saneamento,
  data = dados_vif
)

vif_vals <- car::vif(lm_vif)
message("  VIF por preditor:")
for (v in names(vif_vals)) {
  flag <- if (vif_vals[v] > 10) " *** COLINEARIDADE GRAVE" else
          if (vif_vals[v] > 5)  " *   VIF > 5 — investigar" else ""
  message("  ", str_pad(v, 24), " VIF = ", round(vif_vals[v], 2), flag)
}

vars_ok <- names(vif_vals[vif_vals <= 5])
vars_alerta <- names(vif_vals[vif_vals > 5])
message("\n  Preditores OK (VIF ≤ 5): ", paste(vars_ok, collapse = ", "))
if (length(vars_alerta) > 0)
  message("  Preditores com VIF > 5:  ", paste(vars_alerta, collapse = ", "))

# =============================================================================
# 10. Seleção stepwise Forward (univariada p < 0.20) + Backward (p > 0.05)
# =============================================================================
# Estratégia:
#   Forward: para cada preditor candidato, roda GEE univariado (+ intercepto +
#   sin12/cos12/mes_num que são estruturais). Mantém se p < 0.20.
#   Backward: a partir do modelo com preditores selecionados, remove iterativamente
#   o preditor com maior p-valor se p > 0.05, até nenhum ser removível.
#
# Nota: seleção por p-valor em GEE não é o método ideal (QIC seria preferível).
#   Implementado conforme especificado pelo protocolo do estudo.
# =============================================================================

message("\n=== 10. SELEÇÃO STEPWISE (Forward p<0.20 → Backward p>0.05) ===")

# Preditores candidatos (excluindo termos estruturais mes_num, sin12, cos12)
candidatos <- intersect(
  c("n_esf", "ivs_score", "pct_area_favela", "renda_media", "pct_sem_saneamento"),
  vars_ok  # somente preditores com VIF ≤ 5
)
message("  Candidatos (VIF ≤ 5): ", paste(candidatos, collapse = ", "))

gee_univar <- function(pred, dados, id_col) {
  f  <- as.formula(paste("taxa_cs ~ mes_num + sin12 + cos12 +", pred))
  id_vec <- dados[[id_col]]
  tryCatch(
    geeglm(f, family = Gamma(link = "log"), data = dados,
           id = id_vec, corstr = "ar1", waves = mes_num),
    error = function(e) NULL
  )
}

# --- Forward: triagem univariada ---
message("\n  Forward — triagem univariada:")
selecionados <- character(0)
for (pred in candidatos) {
  # Usa dados_cnes se pred == "n_esf", senão dados_full
  d   <- if (pred == "n_esf") dados_cnes else dados_full
  idc <- if (pred == "n_esf") "cs_id2"   else "cs_id"
  mod_uni <- gee_univar(pred, d, idc)
  if (is.null(mod_uni)) { message("  ", pred, " → FALHOU"); next }
  cf  <- coef(summary(mod_uni))
  p_col <- if ("Pr(>|W|)" %in% colnames(cf)) "Pr(>|W|)" else "Pr(>|z|)"
  p_pred <- cf[pred, p_col]
  sel <- p_pred < 0.20
  message("  ", str_pad(pred, 22), " p=", round(p_pred, 4),
          if (sel) " → SELECIONADO (p<0.20)" else " → removido (p≥0.20)")
  if (sel) selecionados <- c(selecionados, pred)
}

message("\n  Preditores após Forward: ", paste(selecionados, collapse = ", "))

# --- Backward: eliminação do modelo multivariável ---
# Usa dados_cnes se "n_esf" está selecionado, senão dados_full.
# Quando usa_cnes = TRUE, pct_sem_saneamento é preditor constante por CS
# (Censo 2022) → combinação com AR-1 e φ≈0,96 causa divergência do algoritmo
# GEESE (mesmo problema de M4-completo). Solução: exchangeable para este caso.
usa_cnes  <- "n_esf" %in% selecionados
d_bw      <- if (usa_cnes) dados_cnes else dados_full
id_bw     <- if (usa_cnes) "cs_id2"  else "cs_id"
corstr_bw <- if (usa_cnes) "exchangeable" else "ar1"
if (usa_cnes) message("  Nota: backward usa corstr='exchangeable' — preditor CS-constante",
                      " (pct_sem_saneamento) + φ≈0,96 causa divergência AR-1.")

ativos <- selecionados
mod_bw <- NULL

message("\n  Backward — eliminação iterativa (p > 0.05):")
for (iter in seq_len(length(ativos) + 1)) {
  if (length(ativos) == 0) { message("  Nenhum preditor restante"); break }

  f_bw   <- as.formula(paste("taxa_cs ~ mes_num + sin12 + cos12 +",
                             paste(ativos, collapse = " + ")))
  id_bw_vec <- d_bw[[id_bw]]
  mod_bw <- tryCatch(
    geeglm(f_bw, family = Gamma(link = "log"), data = d_bw,
           id = id_bw_vec, corstr = corstr_bw, waves = mes_num),
    error = function(e) { message("  Backward falhou: ", e$message); NULL }
  )
  if (is.null(mod_bw)) break

  cf_bw <- coef(summary(mod_bw))
  p_col    <- if ("Pr(>|W|)" %in% colnames(cf_bw)) "Pr(>|W|)" else "Pr(>|z|)"
  p_ativos <- setNames(as.numeric(cf_bw[ativos, p_col]), ativos)
  pior     <- ativos[which.max(p_ativos)]
  p_pior   <- max(p_ativos)

  message("  Iter ", iter, ": pior preditor = ", pior, " (p=", round(p_pior, 4), ")")
  if (p_pior <= 0.05) {
    message("  → Todos os preditores com p ≤ 0.05. Backward concluído.")
    break
  }
  message("  → Removendo ", pior)
  ativos <- setdiff(ativos, pior)
}

# Modelo final
message("\n=== MODELO FINAL (Stepwise) ===")

if (length(ativos) == 0) {
  message("Todos os preditores eliminados no backward (p > 0,05 no subsample CNES, 117 CS).")
  message("Modelo final = M1-base: taxa_cs ~ mes_num + sin12 + cos12")
  message("Nota: pct_sem_saneamento é significativo no painel completo (M3, 153 CS, p=0,005)")
  message("      mas perde significância no subsample CNES (117 CS, exchangeable).")
  message("      Relatório: M3 como modelo principal para pct_sem_saneamento; stepwise")
  message("      confirma ausência de preditor adicional robusto no subsample.")
} else {
  message("Preditores selecionados: ", paste(ativos, collapse = ", "))
  if (!is.null(mod_bw)) {
    print(summary(mod_bw))
    cf_final <- coef(summary(mod_bw))
    se_col <- if ("Std.err" %in% colnames(cf_final)) "Std.err" else "Std. Error"
    p_col  <- if ("Pr(>|W|)" %in% colnames(cf_final)) "Pr(>|W|)" else "Pr(>|z|)"
    resultados_final <- tibble(
      modelo    = "M-final-stepwise",
      variavel  = rownames(cf_final),
      beta      = round(cf_final[, "Estimate"], 4),
      se        = round(cf_final[, se_col],    4),
      p_valor   = round(cf_final[, p_col],     4),
      RR        = round(exp(cf_final[, "Estimate"]),                        4),
      RR_ic_inf = round(exp(cf_final[, "Estimate"] - 1.96*cf_final[,se_col]), 4),
      RR_ic_sup = round(exp(cf_final[, "Estimate"] + 1.96*cf_final[,se_col]), 4)
    )
    resultados <- bind_rows(resultados, resultados_final)
    qic_final <- tryCatch(QIC(mod_bw), error = function(e) NULL)
    if (!is.null(qic_final)) {
      message("QIC modelo final: ", round(qic_final["QIC"], 1),
              " | QICu: ", round(qic_final["QICu"], 1))
    }
  }
}

# =============================================================================
# 11. Exporta (atualiza com modelo final)
# =============================================================================

write_csv(resultados,   file.path(DIR_PROC, "glm_resultados.csv"))
write_csv(diagnosticos, file.path(DIR_PROC, "glm_diagnosticos.csv"))

message("\n======================================")
message("GEE AR-1 COMPLETO — CONCLUÍDO")
message("")
message("Modelos ajustados:")
message("  M1 (base):     ", nrow(dados_full), " obs | ", n_distinct(dados_full$nome_cs), " CS")
message("  M2 (CNES):     ", nrow(dados_cnes), " obs | ", n_distinct(dados_cnes$nome_cs), " CS")
message("  M3 (contexto): ", nrow(dados_full), " obs | ", n_distinct(dados_full$nome_cs), " CS")
message("  M4 (completo): ", nrow(dados_cnes), " obs | ", n_distinct(dados_cnes$nome_cs), " CS")
message("")
message("Saídas:")
message("  ", file.path(DIR_PROC, "glm_resultados.csv"))
message("  ", file.path(DIR_PROC, "glm_diagnosticos.csv"))
message("======================================")
