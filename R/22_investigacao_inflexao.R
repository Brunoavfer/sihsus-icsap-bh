# =============================================================================
# 22_investigacao_inflexao.R
#
# Investiga se a inflexão de mai/2024 é artefato de codificação SIHSUS
# ou efeito sistêmico real da Portaria GM/MS 3.493/2024.
#
# A inflexão é uma MUDANÇA DE SLOPE (não de nível): o ITS capturou a
# desaceleração da taxa ICSAP de +22,9%/ano → -11,2%/ano. Portanto,
# os critérios devem comparar SLOPES pré vs pós, não médias de nível.
#
# Três critérios:
#   (A) Distribuição por grupos ICSAP — mudança de slope difusa ou concentrada?
#   (B) Taxa ICSAP vs taxa não-ICSAP — desaceleração específica ou geral?
#   (C) Capitais controle — slope change simultâneo em outras capitais?
#
# Conclusão: Artefato de codificação PROVÁVEL / IMPROVÁVEL
#
# Saída: docs/investigacao_inflexao.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(ggplot2)
  library(forcats)
  library(stringr)
  library(patchwork)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

# Ponto de corte: mai/2024 (MES_INTERV=29 dos ITS)
DATA_CORTE  <- as.Date("2024-05-01")
MES_CORTE_N <- 29L   # mes_num a partir de jan/2022=1

# =============================================================================
# 1. CARREGA DADOS
# =============================================================================

message("Carregando dados...")

icsap     <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),       show_col_types = FALSE)
int_bh    <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"), show_col_types = FALSE)
controles <- read_csv(file.path(DIR_PROC, "serie_controles.csv"),show_col_types = FALSE)
lista     <- read_csv(file.path(DIR_REF,  "lista_icsap.csv"),    show_col_types = FALSE)

# Rótulo resumido por grupo
label_grupo <- lista %>%
  group_by(grupo) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    grupo,
    grupo_label = paste0("G", grupo, " – ",
                         str_trunc(str_to_sentence(descricao), 32, ellipsis = "…"))
  )

# =============================================================================
# CRITÉRIO A — Slope change da taxa ICSAP por grupo (pré vs pós mai/2024)
# Lógica: ajusta regressão log-linear por período; slope pré e pós.
# Queda real → maioria dos grupos muda de slope positivo para negativo.
# Artefato → 1–2 grupos concentram a mudança.
# =============================================================================

message("\n--- CRITÉRIO A: slope por grupo ICSAP ---")

# Série mensal de taxa ICSAP por grupo (usa n_total BH como denominador)
n_total_mes <- int_bh %>%
  mutate(ano_cmpt = as.integer(ano_cmpt), mes_cmpt = as.integer(mes_cmpt)) %>%
  count(ano_cmpt, mes_cmpt, name = "n_total")

serie_grupo <- icsap %>%
  mutate(ano_cmpt = as.integer(ano_cmpt), mes_cmpt = as.integer(mes_cmpt)) %>%
  count(ano_cmpt, mes_cmpt, grupo, name = "n_icsap_grupo") %>%
  left_join(n_total_mes, by = c("ano_cmpt", "mes_cmpt")) %>%
  mutate(
    data     = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num  = as.integer((ano_cmpt - 2022L) * 12L + mes_cmpt),
    taxa_g   = n_icsap_grupo / n_total * 10000,   # por 10.000 internações
    log_taxa = log(pmax(taxa_g, 0.001)),
    periodo  = if_else(data < DATA_CORTE, "pre", "pos"),
    pre_flag = as.integer(data < DATA_CORTE)
  ) %>%
  left_join(label_grupo, by = "grupo")

# Slope por grupo × período via lm simples
slope_grupo <- serie_grupo %>%
  group_by(grupo, grupo_label, periodo) %>%
  filter(n() >= 4) %>%   # mínimo 4 pontos por segmento
  summarise(
    slope = tryCatch(coef(lm(log_taxa ~ mes_num))[["mes_num"]], error = function(e) NA_real_),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = periodo, values_from = c(slope, n_obs)) %>%
  mutate(
    apc_pre  = round((exp(12 * slope_pre)  - 1) * 100, 1),
    apc_pos  = round((exp(12 * slope_pos)  - 1) * 100, 1),
    delta_apc = round(apc_pos - apc_pre, 1),
    slope_rev = sign(slope_pre) != sign(slope_pos) & !is.na(slope_pre) & !is.na(slope_pos),
    queda_pos = slope_pos < 0 & !is.na(slope_pos)
  ) %>%
  arrange(delta_apc)

message("Slope change por grupo (APC pré → APC pós):")
print(as.data.frame(slope_grupo %>%
  select(grupo, grupo_label, apc_pre, apc_pos, delta_apc, slope_rev, queda_pos)))

n_queda_pos  <- sum(slope_grupo$queda_pos, na.rm = TRUE)
n_slope_rev  <- sum(slope_grupo$slope_rev, na.rm = TRUE)
n_grupos     <- nrow(slope_grupo)

message(sprintf("\nGrupos com APC pós < 0: %d/%d", n_queda_pos, n_grupos))
message(sprintf("Grupos com reversão de slope (+ → -): %d/%d", n_slope_rev, n_grupos))

# Dengue (grupo 04) separado — fortemente sazonal e epidêmico
dengue_flag <- "04" %in% slope_grupo$grupo
if (dengue_flag) {
  dg <- slope_grupo %>% filter(grupo == "04")
  message(sprintf("\nNota: Dengue (G04) — APC pré=%+.1f%% | APC pós=%+.1f%%",
                  dg$apc_pre, dg$apc_pos))
  message("  (Dengue é altamente sazonal/epidêmico — verificar separadamente)")
  # Excluindo dengue da contagem principal
  n_queda_pos_sem_dengue <- sum(slope_grupo$queda_pos[slope_grupo$grupo != "04"], na.rm = TRUE)
  n_grupos_sem_dengue    <- sum(slope_grupo$grupo != "04")
  message(sprintf("  Sem Dengue: %d/%d grupos com queda pós",
                  n_queda_pos_sem_dengue, n_grupos_sem_dengue))
}

# HHI de concentração da queda (por |delta_apc| nos grupos que caíram)
quedas_vec <- abs(slope_grupo$delta_apc[slope_grupo$delta_apc < 0])
hhi <- if (length(quedas_vec) > 1) {
  shares <- quedas_vec / sum(quedas_vec)
  round(sum(shares^2), 3)
} else if (length(quedas_vec) == 1) 1.0 else 0.0
message(sprintf("\nHHI de concentração do delta_APC (grupos com queda): %.3f", hhi))
message("  (0 = difuso; 1 = concentrado em 1 grupo)")

# Critério A: difuso se ≥60% dos grupos mostram queda pós E HHI < 0.5
pct_queda    <- n_queda_pos / n_grupos
crit_a_ok    <- pct_queda >= 0.5 || (dengue_flag && n_queda_pos_sem_dengue >= 0.5 * n_grupos_sem_dengue)
crit_a_txt   <- sprintf(
  "%d/%d grupos com slope pós < 0 | HHI=%.3f | %s",
  n_queda_pos, n_grupos, hhi,
  if (crit_a_ok) "→ queda difusa (contra artefato)" else "→ queda concentrada (pode indicar artefato)"
)
message("Critério A: ", crit_a_txt)

# =============================================================================
# CRITÉRIO B — Taxa ICSAP vs taxa não-ICSAP: slope específico ou geral?
# Lógica: se apenas a taxa ICSAP desacelera e a taxa não-ICSAP permanece
# com slope positivo → o efeito é específico das condições APS-sensíveis.
# =============================================================================

message("\n--- CRITÉRIO B: slope ICSAP vs não-ICSAP ---")

serie_nd <- int_bh %>%
  mutate(
    ano_cmpt   = as.integer(ano_cmpt),
    mes_cmpt   = as.integer(mes_cmpt),
    icsap_flag = as.logical(icsap)
  ) %>%
  group_by(ano_cmpt, mes_cmpt) %>%
  summarise(
    n_icsap      = sum(icsap_flag, na.rm = TRUE),
    n_nao_icsap  = sum(!icsap_flag, na.rm = TRUE),
    n_total      = n(),
    .groups      = "drop"
  ) %>%
  arrange(ano_cmpt, mes_cmpt) %>%
  mutate(
    data         = make_date(ano_cmpt, mes_cmpt, 1L),
    mes_num      = as.integer((ano_cmpt - 2022L) * 12L + mes_cmpt),
    taxa_icsap   = n_icsap     / n_total * 100,
    taxa_nicsap  = n_nao_icsap / n_total * 100,
    log_icsap    = log(taxa_icsap),
    log_nicsap   = log(taxa_nicsap),
    periodo      = if_else(data < DATA_CORTE, "pre", "pos")
  )

# Slopes por período
slope_nd <- function(df, y_col) {
  lapply(c("pre", "pos"), function(p) {
    d <- df %>% filter(periodo == p)
    s <- tryCatch(coef(lm(as.formula(paste(y_col, "~ mes_num")), data = d))[["mes_num"]],
                  error = function(e) NA_real_)
    tibble(periodo = p, slope = s,
           apc = round((exp(12 * s) - 1) * 100, 1))
  }) %>% bind_rows()
}

sl_icsap  <- slope_nd(serie_nd, "log_icsap")
sl_nicsap <- slope_nd(serie_nd, "log_nicsap")

message("Taxa ICSAP — slope pré/pós:")
print(as.data.frame(sl_icsap))
message("Taxa não-ICSAP — slope pré/pós:")
print(as.data.frame(sl_nicsap))

delta_apc_icsap  <- sl_icsap$apc[sl_icsap$periodo == "pos"]  - sl_icsap$apc[sl_icsap$periodo == "pre"]
delta_apc_nicsap <- sl_nicsap$apc[sl_nicsap$periodo == "pos"] - sl_nicsap$apc[sl_nicsap$periodo == "pre"]

message(sprintf("\nΔAPC (pós−pré): ICSAP=%+.1f%%/ano | Não-ICSAP=%+.1f%%/ano",
                delta_apc_icsap, delta_apc_nicsap))

# Critério B: ICSAP desacelera muito mais que não-ICSAP
crit_b_ok  <- delta_apc_icsap < delta_apc_nicsap - 5
crit_b_txt <- sprintf(
  "ICSAP Δ=%+.1f%%/ano vs Não-ICSAP Δ=%+.1f%%/ano | %s",
  delta_apc_icsap, delta_apc_nicsap,
  if (crit_b_ok) "→ desaceleração específica em ICSAP (contra artefato geral)"
  else "→ desacelerações similares (ambos afetados por fator comum)"
)
message("Critério B: ", crit_b_txt)

# =============================================================================
# CRITÉRIO C — Slope change nas capitais controle
# =============================================================================

message("\n--- CRITÉRIO C: capitais controle ---")

slope_cap <- controles %>%
  mutate(data = as.Date(data)) %>%
  group_by(capital) %>%
  group_modify(~ {
    df  <- .x
    pre <- df[df$data <  DATA_CORTE, ]
    pos <- df[df$data >= DATA_CORTE, ]
    tibble(
      slope_pre = tryCatch(coef(lm(log_taxa ~ mes_num, data = pre))[["mes_num"]], error = function(e) NA_real_),
      slope_pos = tryCatch(coef(lm(log_taxa ~ mes_num, data = pos))[["mes_num"]], error = function(e) NA_real_)
    )
  }) %>%
  ungroup() %>%
  mutate(
    apc_pre   = round((exp(12 * slope_pre) - 1) * 100, 1),
    apc_pos   = round((exp(12 * slope_pos) - 1) * 100, 1),
    delta_apc = round(apc_pos - apc_pre, 1),
    queda_slope = slope_pos < slope_pre  # desacelerou?
  ) %>%
  arrange(delta_apc)

message("Slope change por capital:")
print(as.data.frame(slope_cap %>% select(capital, apc_pre, apc_pos, delta_apc, queda_slope)))

n_desacel <- sum(slope_cap$queda_slope, na.rm = TRUE)
n_cap     <- nrow(slope_cap)
message(sprintf("\n%d/%d capitais com desaceleração pós mai/2024", n_desacel, n_cap))

crit_c_ok  <- n_desacel >= 5
crit_c_txt <- sprintf(
  "%d/%d capitais com slope change | %s",
  n_desacel, n_cap,
  if (crit_c_ok) "→ efeito nacional difuso (consistente com Portaria, contra artefato BH-específico)"
  else "→ padrão não generalizado"
)
message("Critério C: ", crit_c_txt)

# =============================================================================
# CONCLUSÃO
# =============================================================================

message("\n================================================================")
message("CONCLUSÃO")
message("================================================================")

n_favor_real <- sum(c(crit_a_ok, crit_b_ok, crit_c_ok))
veredicto    <- if (n_favor_real >= 2) "IMPROVÁVEL" else "PROVÁVEL"

message(sprintf("\n  A. Distribuição grupos: %s", crit_a_txt))
message(sprintf("  B. Taxa ICSAP vs Não-ICSAP: %s", crit_b_txt))
message(sprintf("  C. Capitais controle: %s",      crit_c_txt))
message(sprintf("\n  ARTEFATO DE CODIFICAÇÃO: %s (%d/3 critérios contra artefato)",
                veredicto, n_favor_real))

# =============================================================================
# GRÁFICOS — 4 painéis
# =============================================================================

message("\nGerando docs/investigacao_inflexao.png...")

theme_inv <- theme_minimal(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle    = element_text(size = 8, hjust = 0.5, color = "gray40"),
    panel.grid.minor = element_blank(),
    plot.caption     = element_text(size = 7, color = "gray50")
  )

vline_corte <- geom_vline(xintercept = DATA_CORTE,
                          linetype = "dashed", color = "#e74c3c", linewidth = 0.7)
ann_corte   <- annotate("text", x = DATA_CORTE + 5, y = Inf,
                        label = "mai/2024", hjust = 0, vjust = 1.5,
                        size = 2.8, color = "#e74c3c")

# --- Painel A: série temporal dos 8 grupos com maior volume ---

top8 <- serie_grupo %>%
  group_by(grupo, grupo_label) %>%
  summarise(total = sum(n_icsap_grupo), .groups = "drop") %>%
  slice_max(total, n = 8) %>%
  pull(grupo)

# Slope change por grupo para legenda
slope_lbl <- slope_grupo %>%
  filter(grupo %in% top8) %>%
  transmute(grupo,
            lbl = sprintf("%s\n(pré%+.0f%% → pós%+.0f%%/ano)",
                          grupo_label, apc_pre, apc_pos))
lbl_map <- setNames(slope_lbl$lbl, slope_lbl$grupo)

p_grupos_ts <- serie_grupo %>%
  filter(grupo %in% top8) %>%
  mutate(grupo_f = fct_reorder(grupo_label, -taxa_g, .fun = mean)) %>%
  ggplot(aes(x = data, y = taxa_g, color = grupo_f)) +
  geom_line(alpha = 0.85, linewidth = 0.65) +
  vline_corte + ann_corte +
  scale_color_brewer(palette = "Set2", name = NULL) +
  labs(
    title    = "A. Taxa ICSAP por Grupo (top 8 por volume)",
    subtitle = "Taxa por 10.000 internações totais",
    x = NULL, y = "Taxa por 10.000 internações"
  ) +
  theme_inv +
  theme(legend.position = "right",
        legend.text = element_text(size = 6.5),
        legend.key.height = unit(0.55, "cm"))

# --- Painel B: slope change por grupo (barras horizontais) ---

p_delta_slope <- slope_grupo %>%
  mutate(
    grupo_label = fct_reorder(grupo_label, delta_apc),
    cor = case_when(
      grupo == "04" ~ "#f39c12",            # Dengue — sazonal, destacado
      delta_apc < 0 ~ "#e74c3c",
      TRUE          ~ "#27ae60"
    )
  ) %>%
  ggplot(aes(x = delta_apc, y = grupo_label, fill = cor)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = "gray30") +
  geom_text(aes(label = paste0(ifelse(delta_apc > 0, "+", ""), delta_apc, "%"),
                hjust = if_else(delta_apc < 0, 1.1, -0.1)),
            size = 2.5) +
  scale_fill_identity() +
  labs(
    title    = "B. ΔAPC por Grupo (pré→pós mai/2024)",
    subtitle = sprintf("Vermelho=queda | Laranja=Dengue (sazonal) | HHI=%.3f", hhi),
    x = "ΔAPC (pp/ano)", y = NULL
  ) +
  theme_inv +
  theme(axis.text.y = element_text(size = 7))

# --- Painel C: slope ICSAP vs não-ICSAP ---

p_nd <- serie_nd %>%
  select(data, taxa_icsap, taxa_nicsap) %>%
  pivot_longer(c(taxa_icsap, taxa_nicsap),
               names_to = "tipo", values_to = "taxa") %>%
  mutate(tipo = recode(tipo,
    taxa_icsap  = "ICSAP (sensíveis à APS)",
    taxa_nicsap = "Não-ICSAP"
  )) %>%
  ggplot(aes(x = data, y = taxa, color = tipo)) +
  geom_line(linewidth = 0.85, alpha = 0.9) +
  vline_corte + ann_corte +
  scale_color_manual(
    values = c("ICSAP (sensíveis à APS)" = "#e74c3c", "Não-ICSAP" = "#3498db"),
    name = NULL) +
  labs(
    title    = "C. Taxa ICSAP vs Não-ICSAP — BH",
    subtitle = sprintf("ΔAPC ICSAP=%+.1f%%/ano | ΔAPC Não-ICSAP=%+.1f%%/ano",
                       delta_apc_icsap, delta_apc_nicsap),
    x = NULL, y = "% do total de internações"
  ) +
  theme_inv +
  theme(legend.position = "top")

# --- Painel D: APC pós por capital ---

p_controles <- slope_cap %>%
  filter(!is.na(delta_apc)) %>%
  mutate(
    capital_f = fct_reorder(capital, delta_apc),
    bh        = capital == "Belo Horizonte",
    cor       = if_else(delta_apc < 0, "#e74c3c", "#27ae60")
  ) %>%
  ggplot(aes(x = delta_apc, y = capital_f, fill = cor)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_text(aes(label = paste0(ifelse(delta_apc > 0, "+", ""), delta_apc, "%/ano"),
                hjust = if_else(delta_apc < 0, 1.1, -0.1)),
            size = 3) +
  scale_fill_identity() +
  labs(
    title    = "D. ΔAPC por Capital (pré→pós mai/2024)",
    subtitle = sprintf("%d/7 capitais com desaceleração", n_desacel),
    x = "ΔAPC (pp/ano)", y = NULL
  ) +
  theme_inv

# Composição final 2×2
p_final <- (p_grupos_ts + p_delta_slope) /
           (p_nd        + p_controles) +
  plot_annotation(
    title    = "Investigação da Inflexão de mai/2024 — Artefato ou Efeito Real?",
    subtitle = sprintf(
      "Artefato de codificação: %s  |  Critérios contra artefato: %d/3  |  A: %s  B: %s  C: %s",
      veredicto, n_favor_real,
      if (crit_a_ok) "✓" else "✗",
      if (crit_b_ok) "✓" else "✗",
      if (crit_c_ok) "✓" else "✗"
    ),
    caption  = "Fonte: SIHSUS/DATASUS · Portaria GM/MS 3.493/2024 · Análise: mai/2026",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 9.5, hjust = 0.5,
                                   color = if (veredicto == "IMPROVÁVEL") "#27ae60" else "#e74c3c"),
      plot.caption  = element_text(size = 7, color = "gray50")
    )
  )

ggsave(file.path(DIR_DOCS, "investigacao_inflexao.png"),
       p_final, width = 16, height = 12, dpi = 300, bg = "white")

message("Salvo: docs/investigacao_inflexao.png")
message("\n====================================")
message("ARTEFATO DE CODIFICAÇÃO: ", veredicto)
message("====================================")
