# =============================================================================
# 27_diagnosticos_auditoria.R
# Diagnósticos das Perguntas P1, P2, P6, P7 — mostrar resultados ANTES
# de qualquer correção de texto
#
# P1: Sensibilidade ao pico (excluir abr/2024 em 3 versões)
# P2: Distribuição geográfica do MNAR (% missing por ano/grupo/CEP)
# P6: Teste ADF de estacionaridade + ACF/PACF dos resíduos ITS
# P7: IVS Pooled sem FE de CS (Poisson com GLM base R)
# =============================================================================

suppressPackageStartupMessages({
  library(nlme)
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# =============================================================================
# UTILIDADE: construir série ITS BH
# =============================================================================

build_serie <- function() {
  icsap      <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                         col_types = cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                         show_col_types = FALSE)
  internacoes <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"),
                          col_types = cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                          show_col_types = FALSE)

  icsap |>
    count(ano_cmpt, mes_cmpt, name = "n_icsap") |>
    left_join(internacoes |> count(ano_cmpt, mes_cmpt, name = "n_total"),
              by = c("ano_cmpt","mes_cmpt")) |>
    filter(!is.na(n_total), n_total > 0) |>
    arrange(ano_cmpt, mes_cmpt) |>
    mutate(
      data      = as.Date(paste(ano_cmpt, sprintf("%02d", mes_cmpt), "01", sep="-")),
      mes_num   = row_number(),
      taxa_pct  = n_icsap / n_total * 100,
      log_taxa  = log(taxa_pct),
      interv    = as.integer(mes_num >= 29L),
      tempo_pos = as.integer(pmax(0L, mes_num - 28L)),
      sin12     = sin(2 * pi * mes_num / 12),
      cos12     = cos(2 * pi * mes_num / 12)
    )
}

# =============================================================================
# UTILIDADE: ajustar GLS AR(1) e extrair APC pós com delta method
# =============================================================================

fit_gls <- function(data, label) {
  m <- tryCatch(
    nlme::gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
              data        = data,
              correlation = nlme::corARMA(p=1, q=0, form=~mes_num),
              method      = "ML"),
    error = function(e) {
      cat(sprintf("  [AVISO] GLS falhou para '%s': %s\n", label, e$message))
      NULL
    }
  )
  if (is.null(m)) return(NULL)

  b  <- coef(m)
  V  <- m$varBeta
  b1 <- b["mes_num"]
  b3 <- b["tempo_pos"]
  se_b3  <- sqrt(V["tempo_pos","tempo_pos"])
  var_pos <- V["mes_num","mes_num"] + V["tempo_pos","tempo_pos"] +
             2 * V["mes_num","tempo_pos"]
  se_pos  <- sqrt(var_pos)

  apc     <- (exp(12*(b1+b3)) - 1) * 100
  apc_inf <- (exp(12*((b1+b3) - 1.96*se_pos)) - 1) * 100
  apc_sup <- (exp(12*((b1+b3) + 1.96*se_pos)) - 1) * 100
  p_b3    <- 2 * pnorm(-abs(b3 / se_b3))

  apc_pre     <- (exp(12*b1) - 1) * 100
  se_b1       <- sqrt(V["mes_num","mes_num"])
  apc_pre_inf <- (exp(12*(b1 - 1.96*se_b1)) - 1) * 100
  apc_pre_sup <- (exp(12*(b1 + 1.96*se_b1)) - 1) * 100

  phi <- tryCatch(
    coef(m$modelStruct$corStruct, unconstrained=FALSE),
    error = function(e) NA_real_
  )

  list(
    label=label, n=nrow(data), phi=round(phi,3),
    apc_pre=round(apc_pre,1),
    apc_pre_ic=sprintf("(%+.1f; %+.1f)", apc_pre_inf, apc_pre_sup),
    apc=round(apc,1),
    ic=sprintf("(%+.1f; %+.1f)", apc_inf, apc_sup),
    p_b3=round(p_b3, 4),
    model=m
  )
}

# =============================================================================
# P1 — SENSIBILIDADE AO PICO
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("P1 — SENSIBILIDADE AO PICO DE INTERNAÇÕES (abr/2024)\n")
cat(strrep("=", 60), "\n\n")

serie <- build_serie()
cat(sprintf("Série completa: %d meses (%s a %s)\n",
            nrow(serie), min(serie$data), max(serie$data)))

# Verificar os meses do pico
pico <- serie |> filter(data >= as.Date("2024-01-01"),
                         data <= as.Date("2024-06-30")) |>
  select(data, n_icsap, n_total, taxa_pct)
cat("\nTaxas nos meses em torno da intervenção:\n")
print(as.data.frame(mutate(pico, taxa_pct=round(taxa_pct,2))), row.names=FALSE)

cat("\n--- Modelo 1: COMPLETO (n=51) ---\n")
r1 <- fit_gls(serie, "Completo")
cat(sprintf("  APC pré  = %+.1f%% %s\n", r1$apc_pre, r1$apc_pre_ic))
cat(sprintf("  APC pós  = %+.1f%% %s | p(β₃)=%.4f | φ=%.3f\n",
            r1$apc, r1$ic, r1$p_b3, r1$phi))

cat("\n--- Modelo 2: SEM abr/2024 (n=50) ---\n")
s2 <- serie |> filter(format(data, "%Y-%m") != "2024-04")
r2 <- fit_gls(s2, "Sem abr/2024")
cat(sprintf("  APC pré  = %+.1f%% %s\n", r2$apc_pre, r2$apc_pre_ic))
cat(sprintf("  APC pós  = %+.1f%% %s | p(β₃)=%.4f | φ=%.3f\n",
            r2$apc, r2$ic, r2$p_b3, r2$phi))

cat("\n--- Modelo 3: SEM mar-abr/2024 (n=49) ---\n")
s3 <- serie |> filter(!(format(data, "%Y-%m") %in% c("2024-03","2024-04")))
r3 <- fit_gls(s3, "Sem mar-abr/2024")
cat(sprintf("  APC pré  = %+.1f%% %s\n", r3$apc_pre, r3$apc_pre_ic))
cat(sprintf("  APC pós  = %+.1f%% %s | p(β₃)=%.4f | φ=%.3f\n",
            r3$apc, r3$ic, r3$p_b3, r3$phi))

cat("\n--- Modelo 4: SEM jan-abr/2024 (n=47) ---\n")
s4 <- serie |> filter(!(data >= as.Date("2024-01-01") &
                         data <= as.Date("2024-04-30")))
r4 <- fit_gls(s4, "Sem jan-abr/2024")
cat(sprintf("  APC pré  = %+.1f%% %s\n", r4$apc_pre, r4$apc_pre_ic))
cat(sprintf("  APC pós  = %+.1f%% %s | p(β₃)=%.4f | φ=%.3f\n",
            r4$apc, r4$ic, r4$p_b3, r4$phi))

resultados_p1 <- tibble(
  Especificação  = c(r1$label, r2$label, r3$label, r4$label),
  n_meses        = c(r1$n,  r2$n,  r3$n,  r4$n),
  APC_pos        = c(r1$apc, r2$apc, r3$apc, r4$apc),
  IC95           = c(r1$ic,  r2$ic,  r3$ic,  r4$ic),
  p_beta3        = c(r1$p_b3, r2$p_b3, r3$p_b3, r4$p_b3),
  phi_AR1        = c(r1$phi,  r2$phi,  r3$phi,  r4$phi)
)

cat("\n>>> RESUMO P1:\n")
print(as.data.frame(resultados_p1), row.names=FALSE)

# =============================================================================
# P2 — DISTRIBUIÇÃO GEOGRÁFICA DO MISSING (MNAR)
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("P2 — DISTRIBUIÇÃO GEOGRÁFICA DO MISSING\n")
cat(strrep("=", 60), "\n\n")

reg_df <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                   col_types = cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                   show_col_types = FALSE)

n_total_p2 <- nrow(reg_df)
n_sem_geo  <- sum(is.na(reg_df$regional))
cat(sprintf("Total ICSAP: %s | Sem regional: %s (%.1f%%)\n\n",
            format(n_total_p2, big.mark=","),
            format(n_sem_geo,  big.mark=","),
            100*n_sem_geo/n_total_p2))

cat("--- % sem geocodificação por ANO ---\n")
reg_df |>
  mutate(geocod = !is.na(regional)) |>
  group_by(ano_cmpt) |>
  summarise(n=n(), n_sem=sum(!geocod),
            pct_sem=round(mean(!geocod)*100,1), .groups="drop") |>
  print(n=Inf)

cat("\n--- % sem geocodificação por GRUPO CLÍNICO (top 10) ---\n")
reg_df |>
  mutate(geocod = !is.na(regional)) |>
  group_by(grupo) |>
  summarise(n=n(), n_sem=sum(!geocod),
            pct_sem=round(mean(!geocod)*100,1), .groups="drop") |>
  arrange(desc(pct_sem)) |>
  head(10) |>
  print(n=Inf)

cat("\n--- % sem geocodificação por PREFIXO CEP 2-dígitos (>=500 reg.) ---\n")
reg_df |>
  mutate(geocod=!is.na(regional),
         cep2=substr(as.character(cep), 1, 2)) |>
  group_by(cep2) |>
  summarise(n=n(), n_sem=sum(!geocod),
            pct_sem=round(mean(!geocod)*100,1), .groups="drop") |>
  filter(n >= 500) |>
  arrange(desc(pct_sem)) |>
  print(n=Inf)

cat("\n--- Correlação CEP-nível: % missing vs taxa total ---\n")
cep_stats <- reg_df |>
  mutate(geocod=!is.na(regional)) |>
  group_by(cep) |>
  summarise(n=n(), pct_sem=mean(!geocod)*100, .groups="drop") |>
  filter(n >= 10)
cat(sprintf("CEPs com ≥10 registros: n=%d\n", nrow(cep_stats)))
cat(sprintf("CEPs 100%% missing: n=%d (%.1f%% dos CEPs)\n",
            sum(cep_stats$pct_sem == 100),
            100*mean(cep_stats$pct_sem == 100)))
cat(sprintf("Mediana pct_missing por CEP: %.1f%%\n",
            median(cep_stats$pct_sem)))

# =============================================================================
# P6 — TESTE ADF (ESTACIONARIDADE) + ACF/PACF RESÍDUOS ITS
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("P6 — TESTE ADF + ACF/PACF DOS RESÍDUOS ITS\n")
cat(strrep("=", 60), "\n\n")

# ADF manual via regressão OLS (Dickey-Fuller aumentado, k=1 lag)
adf_test <- function(x, k=1, label="série") {
  n   <- length(x)
  dx  <- diff(x)
  xl  <- x[-n]
  nl  <- length(dx)
  if (k > 0) {
    # adiciona k lags de dx como regressores
    lag_mat <- matrix(NA_real_, nl, k)
    for (i in seq_len(k)) {
      if ((i+1) <= nl) lag_mat[(i+1):nl, i] <- dx[seq_len(nl-i)]
    }
    df_dat <- data.frame(dy=dx, xl=xl[seq_len(nl)], lag_mat)
  } else {
    df_dat <- data.frame(dy=dx, xl=xl[seq_len(nl)])
  }
  mod    <- lm(dy ~ ., data=df_dat)
  t_stat <- summary(mod)$coefficients["xl","t value"]
  # Valores críticos MacKinnon 1994 (sem constante): 1%=-3.51, 5%=-2.89, 10%=-2.58
  sig <- dplyr::case_when(
    t_stat < -3.51 ~ "p < 0.01 → ESTACIONÁRIA",
    t_stat < -2.89 ~ "p < 0.05 → ESTACIONÁRIA",
    t_stat < -2.58 ~ "p < 0.10 → provavelmente estacionária",
    TRUE           ~ "p > 0.10 → NÃO REJEITA H0 (raiz unitária provável)"
  )
  cat(sprintf("  ADF (%s, k=%d): t=%.3f → %s\n", label, k, t_stat, sig))
  invisible(t_stat)
}

cat("=== Teste ADF (H0: raiz unitária / não-estacionária) ===\n")
adf_test(serie$taxa_pct,  k=0, label="taxa_pct k=0")
adf_test(serie$taxa_pct,  k=1, label="taxa_pct k=1")
adf_test(serie$log_taxa,  k=0, label="log_taxa k=0")
adf_test(serie$log_taxa,  k=1, label="log_taxa k=1")
adf_test(diff(serie$log_taxa), k=0, label="Δlog_taxa k=0")

cat("\n=== ACF/PACF dos resíduos normalizados (modelo completo) ===\n")
res <- tryCatch(residuals(r1$model, type="normalized"), error=function(e) NA)
if (!anyNA(res)) {
  acf_v  <- acf(res,  plot=FALSE, lag.max=14)
  pacf_v <- pacf(res, plot=FALSE, lag.max=14)
  cat(sprintf("  ACF  lag 1=%+.3f  lag 2=%+.3f  lag 3=%+.3f  lag 6=%+.3f  lag 12=%+.3f\n",
              acf_v$acf[2], acf_v$acf[3], acf_v$acf[4],
              acf_v$acf[7], acf_v$acf[13]))
  cat(sprintf("  PACF lag 1=%+.3f  lag 2=%+.3f  lag 3=%+.3f\n",
              pacf_v$acf[1], pacf_v$acf[2], pacf_v$acf[3]))
  lb12 <- Box.test(res, type="Ljung-Box", lag=12)
  lb6  <- Box.test(res, type="Ljung-Box", lag=6)
  cat(sprintf("  Ljung-Box lag=6:  χ²=%.3f (df=6)  p=%.4f\n",
              lb6$statistic, lb6$p.value))
  cat(sprintf("  Ljung-Box lag=12: χ²=%.3f (df=12) p=%.4f\n",
              lb12$statistic, lb12$p.value))
} else {
  cat("  [ERRO] Não foi possível extrair resíduos normalizados\n")
}

# Resíduos do modelo OLS (pré-GLS) para comparação
mod_ols <- lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
              data = serie)
res_ols <- residuals(mod_ols)
acf_ols <- acf(res_ols, plot=FALSE, lag.max=3)
dw <- sum(diff(res_ols)^2) / sum(res_ols^2)
cat(sprintf("\n  OLS (diagnóstico pré-GLS): DW=%.3f | ACF lag1=%.3f\n",
            dw, acf_ols$acf[2]))

# =============================================================================
# P7 — IVS POOLED SEM FE DE CS (base R glm)
# =============================================================================

cat("\n")
cat(strrep("=", 60), "\n")
cat("P7 — IVS POOLED / NAIVE SEM FE (base R glm)\n")
cat(strrep("=", 60), "\n\n")

cat("Construindo painel 153 CS × 51 meses...\n")

icsap_reg  <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                       col_types = cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                       show_col_types = FALSE)
variaveis  <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                       show_col_types = FALSE)
pop_faixas <- tryCatch(
  read_csv(file.path(DIR_REF, "pop_cs_faixas.csv"), show_col_types=FALSE),
  error = function(e) NULL
)

# Covariáveis estáticas (1 linha por CS)
static_cs <- variaveis |>
  group_by(nome_cs) |>
  slice(1) |>
  ungroup() |>
  select(nome_cs, regional, pop_total_censo, pct_sem_saneamento,
         ivs_score, renda_media, pct_area_favela)

# Offset populacional
if (!is.null(pop_faixas)) {
  pop_cs <- pop_faixas |>
    group_by(nome_cs) |>
    summarise(pop_censo=sum(pop_faixa, na.rm=TRUE), .groups="drop")
  static_cs <- left_join(static_cs, pop_cs, by="nome_cs")
  static_cs <- mutate(static_cs,
    pop_cs = if_else(!is.na(pop_censo) & pop_censo > 0,
                     pop_censo, pop_total_censo))
} else {
  static_cs <- mutate(static_cs, pop_cs = pop_total_censo)
}

# Meses de referência (51)
meses_ref <- tibble(
  ano_cmpt = c(rep(2022L,12), rep(2023L,12), rep(2024L,12),
               rep(2025L,12), rep(2026L,3)),
  mes_cmpt = c(1:12, 1:12, 1:12, 1:12, 1:3)
) |> mutate(mes_num=row_number())

cs_ref <- static_cs |> select(nome_cs, regional)
skeleton <- tidyr::crossing(cs_ref, meses_ref)

# Contagem n_icsap por CS × mês
n_cs_mes <- icsap_reg |>
  filter(!is.na(nome_cs)) |>
  mutate(mes_cmpt=as.integer(mes_cmpt)) |>
  group_by(nome_cs, ano_cmpt, mes_cmpt) |>
  summarise(n_icsap=n(), .groups="drop")

# Painel completo
painel <- skeleton |>
  left_join(n_cs_mes, by=c("nome_cs","ano_cmpt","mes_cmpt")) |>
  mutate(n_icsap = replace_na(n_icsap, 0L)) |>
  left_join(static_cs |> select(-regional), by="nome_cs") |>
  filter(!is.na(ivs_score), !is.na(pct_sem_saneamento),
         !is.na(pop_cs), pop_cs > 0) |>
  mutate(
    ano_fac   = factor(ano_cmpt),
    log_pop   = log(pop_cs)
  )

cat(sprintf("Painel: %s obs | %d CS únicos | %d meses\n",
            format(nrow(painel), big.mark=","),
            length(unique(painel$nome_cs)),
            length(unique(painel$mes_num))))
cat(sprintf("n_icsap total: %s | zeros: %.1f%%\n\n",
            format(sum(painel$n_icsap), big.mark=","),
            100*mean(painel$n_icsap==0)))

extract_irr <- function(mod, var) {
  b  <- coef(mod)[var]
  se <- summary(mod)$coefficients[var, "Std. Error"]
  p  <- summary(mod)$coefficients[var, "Pr(>|z|)"]
  cat(sprintf("  %-14s: IRR=%.3f (%.3f; %.3f)  p=%.4f\n",
              var, exp(b), exp(b-1.96*se), exp(b+1.96*se), p))
}

# M2 referência: FE regional + ano
cat("--- M2 Referência: FE Regional (9 cat) + Ano ---\n")
m2 <- glm(n_icsap ~ ivs_score + pct_sem_saneamento +
           regional + ano_fac,
          family=poisson(), offset=log_pop, data=painel)
extract_irr(m2, "ivs_score")
extract_irr(m2, "pct_sem_saneamento")
cat(sprintf("  Dispersão Pearson: %.2f\n\n", sum(residuals(m2,"pearson")^2)/m2$df.residual))

# Pooled: FE apenas ano
cat("--- Pooled: FE apenas Ano (sem regional) ---\n")
m_pooled <- glm(n_icsap ~ ivs_score + pct_sem_saneamento + ano_fac,
                family=poisson(), offset=log_pop, data=painel)
extract_irr(m_pooled, "ivs_score")
extract_irr(m_pooled, "pct_sem_saneamento")
cat(sprintf("  Dispersão Pearson: %.2f\n\n", sum(residuals(m_pooled,"pearson")^2)/m_pooled$df.residual))

# Naive: sem qualquer FE
cat("--- Naive: sem qualquer FE ---\n")
m_naive <- glm(n_icsap ~ ivs_score + pct_sem_saneamento,
               family=poisson(), offset=log_pop, data=painel)
extract_irr(m_naive, "ivs_score")
extract_irr(m_naive, "pct_sem_saneamento")
cat(sprintf("  Dispersão Pearson: %.2f\n\n", sum(residuals(m_naive,"pearson")^2)/m_naive$df.residual))

# Salvando resultados P1 e P7 em CSV
write_csv(resultados_p1,
          file.path(DIR_PROC, "sensibilidade_pico_resultados.csv"))
cat(sprintf("Resultados P1 salvos em: %s\n",
            file.path(DIR_PROC, "sensibilidade_pico_resultados.csv")))

cat("\n=== Script 27 concluído ===\n")
