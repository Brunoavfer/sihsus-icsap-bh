# =============================================================================
# 12_its_controle.R
#
# ITS com Grupo Controle — Comparação BH × Outras Capitais
#
# Objetivo: verificar se a inflexão de mai/2024 detectada em BH é específica
#   de BH / Portaria GM/MS 3.493/2024, ou reflete tendência nacional
#   (ex.: recuperação pós-COVID, mudança de codificação SIHSUS, sazonalidade
#   pós-pandemia).
#
# Hipóteses:
#   H0 (tendência nacional): capitais controle apresentam β₃ negativo similar
#      → inflexão de mai/2024 não é específica de BH
#   H1 (efeito específico BH / Portaria): capitais controle não apresentam
#      β₃ negativo significativo em mai/2024
#
# Capitais controle:
#   São Paulo      (SP) — 355030
#   Rio de Janeiro (RJ) — 330455
#   Curitiba       (PR) — 410690
#   Fortaleza      (CE) — 230440
#
# Métrica: % das internações totais que são ICSAP (n_icsap / n_total × 100)
#   — mesma métrica do script 11 para BH municipal
#   — evita necessidade de estimativas populacionais por capital
#
# Modelo ITS (idêntico ao script 11, Bernal et al., BMJ 2017):
#   log(taxa_pct_t) = β₀ + β₁·T + β₂·X_t + β₃·P_t + sin12 + cos12 + ε_t
#
#   T   = mês sequencial (1 = jan/2023 … 39 = mar/2026)
#   X_t = 0 antes de mai/2024; 1 a partir de mai/2024 (mudança de nível)
#   P_t = 0 antes de mai/2024; 1,2,3,… após mai/2024 (mudança de tendência)
#   β₁  = tendência pré (APC mensal)
#   β₂  = mudança imediata de nível em mai/2024
#   β₃  = mudança de slope — chave comparativa entre cidades
#   Autocorrelação: GLS AR(1) via nlme::gls
#
# Saídas:
#   data/processed/its_controle_resultados.csv — coef., IC 95%, p-valor
#   data/processed/serie_controles.csv         — séries mensais por capital
#   docs/its_comparativo.png                   — painel 5 cidades + forest plot
# =============================================================================

suppressPackageStartupMessages({
  library(read.dbc)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  library(nlme)
  library(patchwork)
})

DIR_RAW      <- "data/raw"
DIR_RAW_CTRL <- "data/raw/controle"
DIR_PROC     <- "data/processed"
DIR_REF      <- "data/ref"
DIR_DOCS     <- "docs"

dir.create(DIR_RAW_CTRL, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. Parâmetros
# =============================================================================

CAPITAIS <- list(
  list(nome = "Belo Horizonte", uf = "MG", cod = "310620", usar_existente = TRUE),
  list(nome = "São Paulo",      uf = "SP", cod = "355030", usar_existente = FALSE),
  list(nome = "Rio de Janeiro", uf = "RJ", cod = "330455", usar_existente = FALSE),
  list(nome = "Curitiba",       uf = "PR", cod = "410690", usar_existente = FALSE),
  list(nome = "Fortaleza",      uf = "CE", cod = "230440", usar_existente = FALSE)
)

ANO_INICIO <- 2023L
ANO_FIM    <- 2026L
# Intervencão: Portaria GM/MS 3.493 — vigente a partir de mai/2024 = mes_num 17
MES_INTERV <- 17L

# =============================================================================
# 2. Carrega lista ICSAP
# =============================================================================

lista_icsap <- read_csv(file.path(DIR_REF, "lista_icsap.csv"), show_col_types = FALSE)
cids_icsap  <- lista_icsap$cid
message("Lista ICSAP: ", length(cids_icsap), " CIDs")

# =============================================================================
# 3. Funções auxiliares
# =============================================================================

baixar_dbc <- function(uf, ano, mes, dir_destino = DIR_RAW_CTRL) {
  ano_curto <- str_sub(as.character(ano), 3, 4)
  arquivo   <- paste0("RD", uf, ano_curto, mes, ".dbc")
  destino   <- file.path(dir_destino, arquivo)
  if (file.exists(destino)) {
    message("  Já existe: ", arquivo)
    return(invisible(destino))
  }
  url <- paste0("ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/", arquivo)
  message("  Baixando: ", arquivo, " ...")
  tryCatch(
    {
      download.file(url, destino, mode = "wb", quiet = TRUE)
      message("  OK: ", arquivo)
    },
    error = function(e) message("  Falhou: ", arquivo, " (", e$message, ")")
  )
  invisible(destino)
}

# Lê um .dbc, filtra cidade (MUNIC_RES == MUNIC_MOV == cod), retorna resumo mensal
processar_dbc_cidade <- function(arquivo, cod_cidade, cids_icsap) {
  dados <- tryCatch(
    read.dbc(arquivo),
    error = function(e) {
      message("    Erro ao ler: ", basename(arquivo), " — ", e$message)
      return(NULL)
    }
  )
  if (is.null(dados) || nrow(dados) == 0) return(NULL)

  names(dados) <- tolower(names(dados))

  # Filtro duplo: reside E foi internado na capital
  dados_cid <- dados %>%
    filter(
      as.character(munic_res) == cod_cidade,
      as.character(munic_mov) == cod_cidade
    )

  if (nrow(dados_cid) == 0) return(NULL)

  dados_cid %>%
    mutate(
      ano_cmpt = as.integer(as.character(ano_cmpt)),
      mes_cmpt = as.integer(as.character(mes_cmpt)),
      cid3     = str_sub(diag_princ, 1, 3),
      icsap    = cid3 %in% cids_icsap
    ) %>%
    group_by(ano_cmpt, mes_cmpt) %>%
    summarise(
      n_icsap = sum(icsap, na.rm = TRUE),
      n_total = n(),
      .groups = "drop"
    )
}

# Monta série mensal completa (jan/2023 – mar/2026) para uma capital
serie_capital <- function(cap) {
  nome_cap <- cap$nome
  uf       <- cap$uf
  cod      <- cap$cod
  message("\n=== ", toupper(nome_cap), " (", uf, " / ", cod, ") ===")

  # BH: usa dados já processados (evita re-download do MG inteiro)
  if (isTRUE(cap$usar_existente)) {
    message("  Usando dados existentes de data/processed/")
    icsap_bh       <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                               col_types = cols(ano_cmpt = col_integer(),
                                                mes_cmpt = col_integer()),
                               show_col_types = FALSE)
    internacoes_bh <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"),
                               col_types = cols(ano_cmpt = col_integer(),
                                                mes_cmpt = col_integer()),
                               show_col_types = FALSE)
    serie <- icsap_bh %>%
      count(ano_cmpt, mes_cmpt, name = "n_icsap") %>%
      left_join(
        internacoes_bh %>% count(ano_cmpt, mes_cmpt, name = "n_total"),
        by = c("ano_cmpt", "mes_cmpt")
      ) %>%
      filter(!is.na(n_total), n_total > 0)

    message("  Meses disponíveis: ", nrow(serie))
    return(serie %>% mutate(capital = nome_cap))
  }

  # Capitais controle: baixa e processa arquivos .dbc
  meses_grid <- expand.grid(
    ano = ANO_INICIO:ANO_FIM,
    mes = str_pad(1:12, 2, pad = "0"),
    stringsAsFactors = FALSE
  ) %>%
    mutate(data_ref = make_date(ano, as.integer(mes), 1L)) %>%
    filter(data_ref >= as.Date("2023-01-01"),
           data_ref <= as.Date("2026-03-01")) %>%
    arrange(ano, mes)

  message("  Baixando ", nrow(meses_grid), " arquivos para ", uf, "...")

  resumos <- lapply(seq_len(nrow(meses_grid)), function(i) {
    ano_i <- meses_grid$ano[i]
    mes_i <- meses_grid$mes[i]
    destino <- baixar_dbc(uf, ano_i, mes_i, DIR_RAW_CTRL)
    if (!file.exists(destino)) return(NULL)
    res <- processar_dbc_cidade(destino, cod, cids_icsap)
    if (!is.null(res)) {
      message("    ", uf, ano_i, mes_i, ": ", res$n_icsap, "/", res$n_total,
              " (", round(res$n_icsap / res$n_total * 100, 1), "%)")
    }
    res
  })

  serie <- bind_rows(resumos)
  if (nrow(serie) == 0) {
    message("  AVISO: nenhum dado encontrado para ", nome_cap)
    return(NULL)
  }
  message("  Meses com dados: ", nrow(serie))
  serie %>% mutate(capital = nome_cap)
}

# =============================================================================
# 4. Monta séries para todas as capitais
# =============================================================================

series_lista <- lapply(CAPITAIS, serie_capital)
names(series_lista) <- sapply(CAPITAIS, `[[`, "nome")

# Remove capitais sem dados
series_lista <- Filter(Negate(is.null), series_lista)

# Combina em um único dataframe com variáveis ITS
series_todas <- bind_rows(series_lista) %>%
  filter(!is.na(n_total), n_total > 0) %>%
  group_by(capital) %>%
  arrange(ano_cmpt, mes_cmpt, .by_group = TRUE) %>%
  mutate(
    data      = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num   = row_number(),
    taxa_pct  = n_icsap / n_total * 100,
    log_taxa  = log(pmax(taxa_pct, 0.001)),
    interv    = as.integer(mes_num >= MES_INTERV),
    tempo_pos = as.integer(pmax(0L, mes_num - (MES_INTERV - 1L))),
    sin12     = sin(2 * pi * mes_num / 12),
    cos12     = cos(2 * pi * mes_num / 12)
  ) %>%
  ungroup()

message("\nSéries compiladas:")
series_todas %>%
  group_by(capital) %>%
  summarise(n_meses = n(), taxa_media = round(mean(taxa_pct), 1),
            data_ini = min(data), data_fim = max(data)) %>%
  print()

# Salva séries mensais
write_csv(series_todas, file.path(DIR_PROC, "serie_controles.csv"))
message("Séries salvas: data/processed/serie_controles.csv")

# =============================================================================
# 5. Modelo ITS GLS AR(1) — uma função para todas as capitais
# =============================================================================

ajusta_its <- function(df, nome) {
  if (nrow(df) < 20) {
    message("  ", nome, ": menos de 20 observações — pulando")
    return(NULL)
  }

  mod_gls <- tryCatch(
    gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
        data        = df,
        correlation = corARMA(p = 1, q = 0, form = ~mes_num),
        method      = "ML"),
    error = function(e) {
      message("  ", nome, ": GLS AR(1) falhou — tentando OLS: ", e$message)
      tryCatch(
        lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12, data = df),
        error = function(e2) NULL
      )
    }
  )

  if (is.null(mod_gls)) return(NULL)

  cf  <- coef(mod_gls)
  se  <- if (inherits(mod_gls, "gls")) sqrt(diag(mod_gls$varBeta)) else sqrt(diag(vcov(mod_gls)))
  pv  <- 2 * pnorm(-abs(cf / se))
  ci_inf <- cf - 1.96 * se
  ci_sup <- cf + 1.96 * se

  b1 <- cf["mes_num"];    se1 <- se["mes_num"]
  b2 <- cf["interv"];     se2 <- se["interv"]
  b3 <- cf["tempo_pos"];  se3 <- se["tempo_pos"]

  phi <- NA_real_
  if (inherits(mod_gls, "gls") && !is.null(mod_gls$modelStruct$corStruct)) {
    phi <- tryCatch(
      coef(mod_gls$modelStruct$corStruct, unconstrained = FALSE),
      error = function(e) NA_real_
    )
  }

  modelo_tipo <- if (inherits(mod_gls, "gls")) "GLS-AR1" else "OLS"
  message("  ", nome, " [", modelo_tipo, "] φ=", round(phi, 3),
          " | β₂=", round(b2, 4), " (p=", round(pv["interv"], 3), ")",
          " | β₃=", round(b3, 4), " (p=", round(pv["tempo_pos"], 3), ")")

  tibble(
    capital       = nome,
    modelo        = modelo_tipo,
    phi_ar1       = round(phi, 4),
    n_meses       = nrow(df),
    # Coeficientes brutos
    beta_pre      = round(b1, 5),
    beta_nivel    = round(b2, 5),
    beta_slope    = round(b3, 5),
    # Mudança de nível (β₂)
    nivel_pct     = round((exp(b2) - 1) * 100, 1),
    nivel_ic_inf  = round((exp(ci_inf["interv"]) - 1) * 100, 1),
    nivel_ic_sup  = round((exp(ci_sup["interv"]) - 1) * 100, 1),
    p_nivel       = round(pv["interv"], 4),
    # APC pré (β₁ anualizando)
    apc_pre       = round((exp(12 * b1) - 1) * 100, 1),
    apc_pre_inf   = round((exp(12 * ci_inf["mes_num"]) - 1) * 100, 1),
    apc_pre_sup   = round((exp(12 * ci_sup["mes_num"]) - 1) * 100, 1),
    p_pre         = round(pv["mes_num"], 4),
    # APC pós LÍQUIDA (β₁ + β₃)
    apc_pos       = round((exp(12 * (b1 + b3)) - 1) * 100, 1),
    apc_pos_inf   = round((exp(12 * ((b1 + b3) - 1.96 * sqrt(se1^2 + se3^2))) - 1) * 100, 1),
    apc_pos_sup   = round((exp(12 * ((b1 + b3) + 1.96 * sqrt(se1^2 + se3^2))) - 1) * 100, 1),
    p_pos         = round(pv["tempo_pos"], 4),
    # Só β₃ (mudança de slope)
    slope_change_pct = round((exp(b3) - 1) * 100, 1)
  )
}

# =============================================================================
# 6. Ajusta ITS para cada capital
# =============================================================================

message("\n=== MODELOS ITS ===")

capitais_unicas <- unique(series_todas$capital)
resultados_lista <- lapply(capitais_unicas, function(cap) {
  df <- filter(series_todas, capital == cap)
  ajusta_its(df, cap)
})
names(resultados_lista) <- capitais_unicas

tab_resultados <- bind_rows(resultados_lista)

message("\n=== TABELA COMPARATIVA ===")
print(tab_resultados %>%
        select(capital, modelo, phi_ar1, nivel_pct, p_nivel, apc_pre, apc_pos, p_pos),
      n = Inf)

# =============================================================================
# 7. Interpretação comparativa
# =============================================================================

message("\n=== INTERPRETAÇÃO ===")
bh_res <- tab_resultados %>% filter(capital == "Belo Horizonte")

if (nrow(bh_res) == 1) {
  message(sprintf("BH: β₃ = %.4f | APC pós = %+.1f%%/ano (p=%.4f)",
                  bh_res$beta_slope, bh_res$apc_pos, bh_res$p_pos))
}

controles_sig <- tab_resultados %>%
  filter(capital != "Belo Horizonte", p_pos < 0.05, beta_slope < 0)

if (nrow(controles_sig) == 0) {
  message("→ NENHUMA capital controle apresenta β₃ < 0 significativo em mai/2024")
  message("→ Inflexão de mai/2024 é ESPECÍFICA de BH — consistente com efeito da Portaria 3.493")
} else {
  message("→ ", nrow(controles_sig), " capital(is) controle(s) apresenta(m) β₃ < 0 sig. em mai/2024:")
  message(paste("  -", controles_sig$capital, collapse = "\n"))
  message("→ Inflexão compartilhada — pode refletir tendência nacional")
}

# =============================================================================
# 8. Gráficos
# =============================================================================

message("\nGerando gráficos...")

# --- 8a. Painel séries temporais (1 linha por capital) ---

# Predições ajustadas + contrafactual
pred_lista <- lapply(capitais_unicas, function(cap) {
  df <- filter(series_todas, capital == cap)
  if (nrow(df) < 20) return(NULL)

  mod <- tryCatch(
    gls(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
        data = df, correlation = corARMA(p=1, q=0, form=~mes_num), method="ML"),
    error = function(e)
      tryCatch(lm(log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12, data=df),
               error = function(e2) NULL)
  )
  if (is.null(mod)) return(NULL)

  df_cf <- df %>% mutate(interv = 0L, tempo_pos = 0L)
  res   <- tab_resultados %>% filter(capital == cap)

  df %>% mutate(
    fitted_obs = exp(predict(mod)),
    fitted_cf  = exp(predict(mod, newdata = df_cf)),
    lbl_cap    = sprintf("Nível: %+.0f%% (p=%s)\nAPC pós: %+.0f%%/ano",
                         res$nivel_pct,
                         ifelse(res$p_nivel < 0.05, sprintf("%.3f*", res$p_nivel),
                                sprintf("%.3f", res$p_nivel)),
                         res$apc_pos)
  )
})
pred_df <- bind_rows(pred_lista)

# Ordem: BH primeiro, depois controles
ordem_caps <- c("Belo Horizonte", setdiff(capitais_unicas, "Belo Horizonte"))
pred_df$capital <- factor(pred_df$capital, levels = ordem_caps)

lbl_df <- pred_df %>%
  group_by(capital) %>%
  arrange(data) %>%
  slice(1) %>%
  ungroup()

p_series <- ggplot(pred_df, aes(x = data)) +
  annotate("rect",
           xmin = as.Date("2024-05-01"), xmax = max(pred_df$data, na.rm = TRUE),
           ymin = -Inf, ymax = Inf, fill = "#ffeeba", alpha = 0.4) +
  geom_vline(xintercept = as.Date("2024-05-01"),
             linetype = "dashed", color = "#e67e22", linewidth = 0.5) +
  geom_point(aes(y = taxa_pct), color = "steelblue", alpha = 0.5, size = 1) +
  geom_line(aes(y = taxa_pct), color = "steelblue", alpha = 0.3, linewidth = 0.3) +
  geom_line(aes(y = fitted_obs), color = "#d62728", linewidth = 1.0) +
  geom_line(aes(y = fitted_cf),  color = "#888888", linewidth = 0.7, linetype = "dashed") +
  geom_text(data = lbl_df,
            aes(x = data, y = Inf, label = lbl_cap),
            vjust = 1.2, hjust = 0, size = 2.2, color = "gray20") +
  facet_wrap(~capital, scales = "free_y", ncol = 1) +
  labs(
    title    = "ITS — Comparação BH × Capitais Controle (jan/2023–mar/2026)",
    subtitle = paste0(
      "Portaria GM/MS 3.493 (mai/2024) | Linha vermelha = ajustado | ",
      "Tracejada cinza = contrafactual\n",
      "Se β₃ < 0 apenas em BH → efeito específico de BH/Portaria"
    ),
    x = NULL, y = "Taxa ICSAP (% das internações)",
    caption = "Fonte: SIHSUS/DATASUS · Método: ITS GLS AR(1) (Bernal et al., BMJ 2017)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5),
    plot.subtitle = element_text(size = 7.5, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(size = 6, hjust = 0.5, color = "gray50"),
    strip.text    = element_text(face = "bold", size = 8),
    panel.grid.minor = element_blank()
  )

# --- 8b. Forest plot: β₃ (mudança de slope) por capital ---

forest_df <- tab_resultados %>%
  mutate(
    capital  = factor(capital, levels = rev(ordem_caps)),
    sig      = ifelse(p_pos < 0.05, "p < 0,05", "p ≥ 0,05"),
    lbl      = sprintf("%+.0f%%/ano", apc_pos)
  )

# IC para APC pós (já calculado em tab_resultados)
p_forest <- ggplot(forest_df, aes(y = capital, x = apc_pos,
                                   xmin = apc_pos_inf, xmax = apc_pos_sup,
                                   color = sig, shape = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(height = 0.25, linewidth = 0.8) +
  geom_point(size = 3.5) +
  geom_text(aes(label = lbl), hjust = -0.3, size = 3, fontface = "bold") +
  scale_color_manual(values = c("p < 0,05" = "#d62728", "p ≥ 0,05" = "#888888"),
                     name = NULL) +
  scale_shape_manual(values = c("p < 0,05" = 16, "p ≥ 0,05" = 1), name = NULL) +
  labs(
    title    = "APC pós-intervenção (β₁ + β₃) por capital",
    subtitle = "Portaria GM/MS 3.493 — mai/2024 | ITS GLS AR(1) | IC 95%",
    x        = "APC pós (% por ano)",
    y        = NULL,
    caption  = "APC pós líquida = (exp(12·(β₁+β₃)) – 1) × 100 | Fonte: SIHSUS/DATASUS"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 8, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(size = 7, hjust = 0.5, color = "gray50"),
    legend.position   = "bottom",
    panel.grid.minor  = element_blank(),
    panel.grid.major.y = element_blank()
  )

# Combina os dois gráficos em layout coluna
p_comparativo <- p_series / p_forest +
  plot_layout(heights = c(3, 1.5))

ggsave(file.path(DIR_DOCS, "its_comparativo.png"), p_comparativo,
       width = 11, height = 16, dpi = 300, bg = "white")
message("  Figura salva: docs/its_comparativo.png")

# =============================================================================
# 9. Exporta resultados
# =============================================================================

write_csv(tab_resultados, file.path(DIR_PROC, "its_controle_resultados.csv"))

message("\n======================================")
message("ITS CONTROLE CONCLUÍDO")
message("")
message("Capitais analisadas: ", nrow(tab_resultados))
message("")
print(tab_resultados %>%
        select(capital, apc_pre, nivel_pct, p_nivel, apc_pos, p_pos) %>%
        arrange(desc(capital == "Belo Horizonte"), capital),
      n = Inf)
message("")
message("Saídas:")
message("  data/processed/its_controle_resultados.csv")
message("  data/processed/serie_controles.csv")
message("  docs/its_comparativo.png")
message("======================================")
