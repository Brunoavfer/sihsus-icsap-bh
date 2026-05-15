# =============================================================================
# 10_joinpoint.R
#
# Joinpoint regression — APC e AAPC para análise de tendência temporal ICSAP
#
# Metodologia: regressão segmentada log-linear (equivalente ao software
#   NCI Joinpoint), implementada via pacote `segmented` (Muggeo, 2003).
#   Testa 0 a 2 joinpoints; seleciona pelo menor BIC.
#
# Níveis de análise:
#   1. BH municipal (mensal, série completa — 100% dos dados)
#      Numerador : n_icsap por mês (icsap_bh.csv)
#      Denominador: n_internações total por mês (internacoes_bh.csv)
#      Taxa: % ICSAP entre todas as internações
#
#   2. Regional (mensal, ~86% geocodificados)
#      Numerador : n_icsap por regional × mês (icsap_bh_regional.csv)
#      Denominador: população referência por regional × ano (variaveis_cs.csv)
#      Taxa: ICSAP por 10.000 hab.
#
# Métricas:
#   APC  — Annual Percent Change por segmento
#   AAPC — Average Annual Percent Change (período completo)
#
# Referências:
#   Muggeo VMR. Estimating regression models with unknown break-points.
#   Stat Med. 2003;22(19):3055–71. doi:10.1002/sim.1545
#
#   Kim HJ, et al. Permutation tests for joinpoint regression with applications
#   to cancer rates. Stat Med. 2000;19(3):335–51.
#
# Saídas:
#   data/processed/joinpoint_bh.csv        — APC/AAPC nível municipal
#   data/processed/joinpoint_regional.csv  — APC/AAPC por regional
#   docs/tendencia_bh.png                  — gráfico BH com joinpoints
#   docs/tendencia_regional.png            — gráfico por regional
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
})

for (pkg in c("segmented")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(segmented)

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

# =============================================================================
# Função auxiliar: ajusta joinpoint e retorna APC/AAPC
# =============================================================================

fit_joinpoint <- function(df, y_col = "log_taxa", x_col = "mes_num",
                          max_jp = 2, label = "") {
  df <- df %>% filter(!is.na(.data[[y_col]]), !is.infinite(.data[[y_col]]))
  n  <- nrow(df)

  if (n < 6) {
    message("  [", label, "] poucos pontos (n=", n, ") — pulando")
    return(NULL)
  }

  formula0 <- as.formula(paste(y_col, "~", x_col))
  mod0     <- lm(formula0, data = df)

  # Seleciona número de joinpoints por BIC com comprimento mínimo de 6 meses
  MIN_SEG <- 6L
  best_mod <- mod0
  best_bic <- BIC(mod0)
  best_njp <- 0L

  for (k in seq_len(min(max_jp, floor((n - 2) / 2)))) {
    mk <- tryCatch(
      suppressWarnings(
        selgmented(mod0, seg.Z = as.formula(paste("~", x_col)),
                   Kmax = k, type = "bic", msg = FALSE)
      ),
      error = function(e) NULL
    )
    if (is.null(mk) || !inherits(mk, "segmented")) next

    # Valida comprimento mínimo dos segmentos
    psi_k  <- mk$psi[, "Est."]
    brks_k <- c(min(df[[x_col]]), psi_k, max(df[[x_col]]))
    if (any(diff(brks_k) < MIN_SEG)) next

    bic_k <- tryCatch(BIC(mk), error = function(e) Inf)
    if (!is.na(bic_k) && bic_k < best_bic) {
      best_bic <- bic_k
      best_mod <- mk
      best_njp <- length(psi_k)   # nº real de joinpoints do modelo
    }
  }

  message("  [", label, "] n=", n, " | joinpoints selecionados: ", best_njp,
          " | BIC=", round(best_bic, 1))

  # Extrai segmentos
  if (best_njp == 0 || !inherits(best_mod, "segmented")) {
    beta_seg <- coef(best_mod)[x_col]
    breaks   <- c(min(df[[x_col]]), max(df[[x_col]]))
    slopes   <- beta_seg
    n_segs   <- 1L
  } else {
    psi    <- best_mod$psi[, "Est."]
    breaks <- c(min(df[[x_col]]), psi, max(df[[x_col]]))
    sl     <- slope(best_mod, conf.level = 0.95)[[x_col]]
    slopes <- sl[, "Est."]
    n_segs <- length(slopes)
  }

  # APC = (exp(12 * slope) - 1) × 100  [mensal → anual]
  # IC 95%: propagado da incerteza do slope via delta-method (sem SE aqui,
  #         usamos IC95% do slope do `slope()` quando disponível)
  segmentos <- tibble(
    segmento   = seq_len(n_segs),
    mes_inicio = round(breaks[-length(breaks)]),
    mes_fim    = round(breaks[-1]),
    slope      = slopes,
    APC        = round((exp(12 * slopes) - 1) * 100, 2)
  )

  # AAPC: média ponderada dos APCs pelo comprimento do segmento
  comprimentos <- segmentos$mes_fim - segmentos$mes_inicio
  aapc <- round(
    sum(segmentos$APC * comprimentos) / sum(comprimentos), 2
  )

  list(
    modelo     = best_mod,
    n_jp       = best_njp,
    bic        = round(best_bic, 2),
    segmentos  = segmentos,
    aapc       = aapc,
    dados      = df
  )
}

# =============================================================================
# 1. NÍVEL MUNICIPAL — BH (mensal, série completa)
# =============================================================================

message("=== NÍVEL MUNICIPAL ===")

icsap_bh     <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                         show_col_types = FALSE)
internacoes  <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"),
                         show_col_types = FALSE)

serie_bh <- icsap_bh %>%
  mutate(ano_cmpt = as.integer(ano_cmpt), mes_cmpt = as.integer(mes_cmpt)) %>%
  count(ano_cmpt, mes_cmpt, name = "n_icsap") %>%
  full_join(
    internacoes %>%
      mutate(ano_cmpt = as.integer(ano_cmpt), mes_cmpt = as.integer(mes_cmpt)) %>%
      count(ano_cmpt, mes_cmpt, name = "n_total"),
    by = c("ano_cmpt", "mes_cmpt")
  ) %>%
  replace_na(list(n_icsap = 0, n_total = 0)) %>%
  filter(n_total > 0) %>%
  arrange(ano_cmpt, mes_cmpt) %>%
  mutate(
    data       = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num    = as.integer((ano_cmpt - min(ano_cmpt)) * 12L + mes_cmpt -
                              min(mes_cmpt[ano_cmpt == min(ano_cmpt)]) + 1L),
    taxa_pct   = n_icsap / n_total * 100,
    log_taxa   = log(taxa_pct)
  )

message("Série BH: ", nrow(serie_bh), " meses | ",
        min(serie_bh$data), " a ", max(serie_bh$data))

jp_bh <- fit_joinpoint(serie_bh, label = "BH")

if (!is.null(jp_bh)) {
  message("\n  Segmentos:")
  print(jp_bh$segmentos)
  message("  AAPC = ", jp_bh$aapc, "% ao ano")
}

# =============================================================================
# 2. NÍVEL REGIONAL (mensal)
# =============================================================================

message("\n=== NÍVEL REGIONAL ===")

regional_bh <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                        show_col_types = FALSE) %>%
  filter(!is.na(regional))

# Denominador: população referência por regional × ano (e-Gestor, jan de cada ano)
variaveis <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                      show_col_types = FALSE)

pop_regional_ano <- variaveis %>%
  filter(mes_cmpt == 1) %>%
  group_by(regional, ano_cmpt) %>%
  summarise(pop_regional = sum(as.numeric(populacao_referencia), na.rm = TRUE),
            .groups = "drop")

serie_regional <- regional_bh %>%
  mutate(ano_cmpt = as.integer(ano_cmpt), mes_cmpt = as.integer(mes_cmpt)) %>%
  count(ano_cmpt, mes_cmpt, regional, name = "n_icsap") %>%
  left_join(pop_regional_ano, by = c("regional", "ano_cmpt" = "ano_cmpt")) %>%
  filter(!is.na(pop_regional), pop_regional > 0) %>%
  arrange(regional, ano_cmpt, mes_cmpt) %>%
  group_by(regional) %>%
  mutate(
    data      = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num   = as.integer((ano_cmpt - min(ano_cmpt)) * 12L + mes_cmpt -
                             min(mes_cmpt[ano_cmpt == min(ano_cmpt)]) + 1L),
    taxa      = n_icsap / (pop_regional / 12) * 10000,
    log_taxa  = log(pmax(taxa, 0.0001))
  ) %>%
  ungroup()

regionais <- sort(unique(serie_regional$regional))
message("Regionais: ", paste(regionais, collapse = ", "))

jp_regional_list <- lapply(regionais, function(reg) {
  df_reg <- filter(serie_regional, regional == reg)
  fit_joinpoint(df_reg, label = reg)
})
names(jp_regional_list) <- regionais

# =============================================================================
# 3. Tabelas de resultados
# =============================================================================

# BH municipal
if (!is.null(jp_bh)) {
  tab_bh <- jp_bh$segmentos %>%
    mutate(
      nivel     = "BH Municipal",
      regional  = NA_character_,
      n_meses   = jp_bh$dados %>% nrow(),
      n_jp      = jp_bh$n_jp,
      aapc      = jp_bh$aapc,
      unidade   = "% internações ICSAP"
    )
} else {
  tab_bh <- tibble()
}

# Regional
tab_regional <- bind_rows(lapply(regionais, function(reg) {
  jp <- jp_regional_list[[reg]]
  if (is.null(jp)) return(tibble())
  jp$segmentos %>%
    mutate(
      nivel    = "Regional",
      regional = reg,
      n_meses  = nrow(jp$dados),
      n_jp     = jp$n_jp,
      aapc     = jp$aapc,
      unidade  = "ICSAP por 10.000 hab."
    )
}))

tab_completa <- bind_rows(tab_bh, tab_regional) %>%
  dplyr::select(nivel, regional, n_meses, n_jp, segmento, mes_inicio, mes_fim,
                slope, APC, aapc, unidade)

message("\n=== TABELA DE RESULTADOS ===")
print(tab_completa, n = Inf)

# =============================================================================
# 4. Gráfico BH municipal
# =============================================================================

message("\nGerando gráficos...")

plot_bh <- function(jp, serie) {
  if (is.null(jp)) return(NULL)

  # Valores ajustados pelo modelo
  serie$fitted <- exp(fitted(jp$modelo))

  # Pontos de inflexão
  if (jp$n_jp > 0) {
    psi_mes <- jp$modelo$psi[, "Est."]
    jp_dates <- serie %>%
      mutate(dist = abs(mes_num - psi_mes[1])) %>%
      slice_min(dist, n = 1) %>%
      pull(data)
  } else {
    jp_dates <- NULL
  }

  p <- ggplot(serie, aes(x = data)) +
    geom_point(aes(y = taxa_pct), color = "steelblue", alpha = 0.7, size = 2) +
    geom_line(aes(y = fitted), color = "#d62728", linewidth = 1) +
    labs(
      title    = "Tendência Temporal das Internações ICSAP — Belo Horizonte",
      subtitle = paste0(
        "Joinpoint regression | Joinpoints selecionados: ", jp$n_jp,
        " | AAPC = ", jp$aapc, "% ao ano"
      ),
      x       = NULL,
      y       = "Taxa ICSAP (% das internações)",
      caption = paste0(
        "Fonte: SIHSUS/DATASUS · Método: regressão segmentada log-linear (Muggeo, 2003)\n",
        "Linha vermelha = tendência ajustada; pontos = observações mensais"
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 9,  hjust = 0.5, color = "gray40"),
      plot.caption  = element_text(size = 7,  hjust = 0.5, color = "gray50"),
      panel.grid.minor = element_blank()
    )

  if (!is.null(jp_dates)) {
    for (d in jp_dates) {
      p <- p + geom_vline(xintercept = as.Date(d), linetype = "dashed",
                           color = "gray40", linewidth = 0.6)
    }
  }
  p
}

p_bh <- plot_bh(jp_bh, serie_bh)
if (!is.null(p_bh)) {
  ggsave(file.path(DIR_DOCS, "tendencia_bh.png"), p_bh,
         width = 10, height = 6, dpi = 300, bg = "white")
  message("  Gráfico BH salvo: docs/tendencia_bh.png")
}

# Gráfico regional (facet_wrap)
if (nrow(serie_regional) > 0 && !is.null(jp_regional_list[[regionais[1]]])) {

  fitted_regional <- bind_rows(lapply(regionais, function(reg) {
    jp <- jp_regional_list[[reg]]
    if (is.null(jp)) return(tibble())
    df <- jp$dados %>%
      mutate(fitted = exp(fitted(jp$modelo)),
             regional = reg,
             aapc_lbl = paste0("AAPC=", jp$aapc, "%"))
    df
  }))

  if (nrow(fitted_regional) > 0) {
    p_reg <- ggplot(fitted_regional, aes(x = data)) +
      geom_point(aes(y = taxa), color = "steelblue", alpha = 0.5, size = 1) +
      geom_line(aes(y = fitted), color = "#d62728", linewidth = 0.8) +
      geom_text(
        data = fitted_regional %>% group_by(regional) %>% slice(1),
        aes(x = data, y = Inf, label = aapc_lbl),
        vjust = 1.5, hjust = 0, size = 2.5, color = "gray30"
      ) +
      facet_wrap(~regional, scales = "free_y", ncol = 3) +
      labs(
        title   = "Tendência Temporal das Internações ICSAP por Regional — BH",
        subtitle = "Joinpoint regression | ICSAP por 10.000 hab.",
        x = NULL, y = "ICSAP por 10.000 hab.",
        caption = "Fonte: SIHSUS/DATASUS · Método: regressão segmentada log-linear"
      ) +
      theme_minimal(base_size = 9) +
      theme(
        plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 8, hjust = 0.5, color = "gray40"),
        plot.caption  = element_text(size = 6, hjust = 0.5, color = "gray50"),
        strip.text    = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )

    ggsave(file.path(DIR_DOCS, "tendencia_regional.png"), p_reg,
           width = 12, height = 10, dpi = 300, bg = "white")
    message("  Gráfico regional salvo: docs/tendencia_regional.png")
  }
}

# =============================================================================
# 5. Exporta
# =============================================================================

write_csv(tab_completa, file.path(DIR_PROC, "joinpoint_resultados.csv"))

message("\n======================================")
message("JOINPOINT CONCLUÍDO")
if (!is.null(jp_bh)) {
  message("BH: AAPC = ", jp_bh$aapc, "% ao ano | ",
          jp_bh$n_jp, " joinpoint(s)")
}
for (reg in regionais) {
  jp <- jp_regional_list[[reg]]
  if (!is.null(jp)) {
    message("  ", formatC(reg, width = 12, flag = "-"), ": AAPC = ", jp$aapc,
            "% ao ano | ", jp$n_jp, " joinpoint(s)")
  }
}
message("")
message("Saídas:")
message("  data/processed/joinpoint_resultados.csv")
message("  docs/tendencia_bh.png")
message("  docs/tendencia_regional.png")
message("======================================")
