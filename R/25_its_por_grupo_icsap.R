# =============================================================================
# 25_its_por_grupo_icsap.R
#
# P9/P17 — ITS GLS AR(1) por grupo ICSAP — dois desfechos complementares:
#
# ANÁLISE 1 (contagem absoluta): log(n_grupo_t) — idêntico ao script 11
#   Detecta redução no VOLUME absoluto de cada grupo após a Portaria.
#   APC pós em %/ano (escala log-linear).
#
# ANÁLISE 2 (proporção no total, P17): taxa_pct = n_grupo / n_total_mes × 100
#   GLS linear (não log) — detecta mudanças na COMPOSIÇÃO do mix ICSAP.
#   Slope change em p.p./ano (escala linear).
#   Interpreta se algum grupo responde MAIS que os outros (efeito seletivo).
#
# INTERPRETAÇÃO CRÍTICA (P21/P22):
#   Se Análise 1 → sig mas Análise 2 → NS: redução proporcional ao total
#   (efeito não seletivo — todos os grupos caíram igualmente).
#   Se Análise 1 → sig E Análise 2 → sig: redução seletiva do grupo
#   (Portaria afetou especificamente aquele grupo clínico).
#
# Resultado jun/2026:
#   Diabetes (grupo 13): Análise 1 p<0,001 (vol. absoluto), Análise 2 p=0,571 (NS)
#   → redução proporcional ao total, não seletiva. Moderação obrigatória.
#
# MES_INTERV = 29 (maio/2024 = Portaria GM/MS 3.493/2024)
#
# Saídas:
#   data/processed/its_por_grupo.csv          (contagens absolutas)
#   data/processed/its_por_grupo_prop.csv     (proporções)
#   docs/its_por_grupo.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(nlme)
  library(ggplot2)
  library(tidyr)
  library(ragg)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

MES_INTERV <- 29L

# =============================================================================
# 1. Carrega dados
# =============================================================================

message("=== 1. Carregando dados ===")

icsap_reg <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                      show_col_types = FALSE) |>
  mutate(mes_cmpt = as.integer(mes_cmpt))

message(sprintf("  N total: %s | grupos disponíveis: %s",
                format(nrow(icsap_reg), big.mark = ","),
                paste(sort(unique(icsap_reg$grupo)), collapse = ", ")))

# =============================================================================
# 2. Constrói séries mensais
# =============================================================================

message("=== 2. Construindo séries ===")

meses_ref <- tibble(
  ano_cmpt = c(rep(2022L,12),rep(2023L,12),rep(2024L,12),rep(2025L,12),rep(2026L,3)),
  mes_cmpt = c(1:12,1:12,1:12,1:12,1:3)
) |> mutate(
  mes_num   = row_number(),
  interv    = as.integer(mes_num >= MES_INTERV),
  tempo_pos = pmax(mes_num - MES_INTERV + 1L, 0L),
  sin12     = sin(2 * pi * mes_num / 12),
  cos12     = cos(2 * pi * mes_num / 12)
)

GRUPOS_ALVO <- c("09", "10", "13")

# Total ICSAP municipal por mês (denominador da taxa proporcional)
total_mes <- icsap_reg |>
  group_by(ano_cmpt, mes_cmpt) |>
  summarise(n_total = n(), .groups = "drop")

# Série municipal completa ("Todos")
serie_total <- icsap_reg |>
  group_by(ano_cmpt, mes_cmpt) |>
  summarise(n_icsap = n(), .groups = "drop") |>
  right_join(meses_ref, by = c("ano_cmpt","mes_cmpt")) |>
  mutate(n_icsap = replace_na(n_icsap, 0L), grupo = "Todos")

# Séries por grupo + proporção
skeleton_grupos <- tidyr::crossing(grupo = GRUPOS_ALVO, meses_ref)

series <- skeleton_grupos |>
  left_join(
    icsap_reg |>
      filter(grupo %in% GRUPOS_ALVO) |>
      group_by(grupo, ano_cmpt, mes_cmpt) |>
      summarise(n_icsap = n(), .groups = "drop"),
    by = c("grupo","ano_cmpt","mes_cmpt")
  ) |>
  mutate(n_icsap = replace_na(n_icsap, 0L)) |>
  left_join(total_mes, by = c("ano_cmpt","mes_cmpt")) |>
  mutate(taxa_pct = n_icsap / n_total * 100) |>
  bind_rows(serie_total |> left_join(total_mes, by=c("ano_cmpt","mes_cmpt")) |>
              mutate(taxa_pct = 100)) |>
  arrange(grupo, mes_num)

# Diagnóstico
for (g in c("Todos", GRUPOS_ALVO)) {
  s <- series |> filter(grupo == g)
  message(sprintf("  Grupo %-5s: %s ICSAP | média %.0f/mês | zeros: %d | taxa: %.1f%% (pré) / %.1f%% (pós)",
                  g, format(sum(s$n_icsap), big.mark=","),
                  mean(s$n_icsap),
                  sum(s$n_icsap == 0),
                  mean(s$taxa_pct[s$interv==0], na.rm=TRUE),
                  mean(s$taxa_pct[s$interv==1], na.rm=TRUE)))
}

# =============================================================================
# 3. Função ITS — ANÁLISE 1: contagem absoluta (log-linear, idêntico script 11)
# =============================================================================

ajusta_its_abs <- function(dados, grupo_id) {
  d <- dados |>
    filter(n_icsap > 0) |>
    mutate(log_n = log(n_icsap))

  if (nrow(d) < 15) {
    message(sprintf("    AVISO: grupo %s, apenas %d meses não-zero", grupo_id, nrow(d)))
    return(NULL)
  }

  mod_gls <- tryCatch(
    nlme::gls(
      log_n ~ mes_num + interv + tempo_pos + sin12 + cos12,
      data = d, correlation = nlme::corAR1(form = ~mes_num), method = "ML",
      control = nlme::glsControl(maxIter = 200, msMaxIter = 200, tolerance = 1e-6)
    ),
    error = function(e) { message(sprintf("    GLS falhou (%s) — OLS", conditionMessage(e))); NULL }
  )
  mod_ols <- lm(log_n ~ mes_num + interv + tempo_pos + sin12 + cos12, data = d)
  mod <- if (!is.null(mod_gls)) mod_gls else mod_ols
  V   <- if (!is.null(mod_gls)) mod_gls$varBeta else vcov(mod_ols)
  b   <- coef(mod)

  b1 <- b["mes_num"]; b3 <- b["tempo_pos"]
  se_b1   <- sqrt(V["mes_num","mes_num"])
  var_pos <- V["mes_num","mes_num"] + V["tempo_pos","tempo_pos"] + 2*V["mes_num","tempo_pos"]
  se_pos  <- sqrt(var_pos)

  if (!is.null(mod_gls)) {
    p_pos <- summary(mod_gls)$tTable["tempo_pos","p-value"]
    p_niv <- summary(mod_gls)$tTable["interv",   "p-value"]
  } else {
    st <- summary(mod_ols)$coefficients
    p_pos <- st["tempo_pos","Pr(>|t|)"]; p_niv <- st["interv","Pr(>|t|)"]
  }

  phi <- tryCatch(coef(mod_gls$modelStruct$corStruct, unconstrained=FALSE), error=function(e) NA_real_)

  tibble(
    grupo        = grupo_id,
    analise      = "Absoluta (log-linear)",
    n_meses      = nrow(d),
    n_icsap_tot  = sum(dados$n_icsap),
    nivel_pct    = (exp(b["interv"]) - 1) * 100,
    p_nivel      = p_niv,
    apc_pre      = (exp(12*b1) - 1) * 100,
    apc_pre_inf  = (exp(12*(b1 - 1.96*se_b1)) - 1) * 100,
    apc_pre_sup  = (exp(12*(b1 + 1.96*se_b1)) - 1) * 100,
    apc_pos      = (exp(12*(b1+b3)) - 1) * 100,
    apc_pos_inf  = (exp(12*((b1+b3) - 1.96*se_pos)) - 1) * 100,
    apc_pos_sup  = (exp(12*((b1+b3) + 1.96*se_pos)) - 1) * 100,
    p_pos        = p_pos,
    phi_ar1      = phi,
    modelo       = if (!is.null(mod_gls)) "GLS AR(1)" else "OLS"
  )
}

# =============================================================================
# 4. Função ITS — ANÁLISE 2: proporção (linear, p.p./ano, P17)
# =============================================================================

ajusta_its_prop <- function(dados, grupo_id) {
  d <- dados |> filter(!is.na(taxa_pct))

  if (nrow(d) < 15) return(NULL)

  mod_gls <- tryCatch(
    nlme::gls(
      taxa_pct ~ mes_num + interv + tempo_pos + sin12 + cos12,
      data = d, correlation = nlme::corAR1(form = ~mes_num), method = "ML",
      control = nlme::glsControl(maxIter = 200, msMaxIter = 200, tolerance = 1e-6)
    ),
    error = function(e) { message(sprintf("    GLS prop falhou — OLS")); NULL }
  )
  mod_ols <- lm(taxa_pct ~ mes_num + interv + tempo_pos + sin12 + cos12, data = d)
  mod <- if (!is.null(mod_gls)) mod_gls else mod_ols
  V   <- if (!is.null(mod_gls)) mod_gls$varBeta else vcov(mod_ols)
  b   <- coef(mod)

  b1 <- b["mes_num"]; b3 <- b["tempo_pos"]
  se_b1   <- sqrt(V["mes_num","mes_num"])
  var_pos <- V["mes_num","mes_num"] + V["tempo_pos","tempo_pos"] + 2*V["mes_num","tempo_pos"]
  se_pos  <- sqrt(var_pos)

  if (!is.null(mod_gls)) {
    p_pos <- summary(mod_gls)$tTable["tempo_pos","p-value"]
    p_niv <- summary(mod_gls)$tTable["interv",   "p-value"]
  } else {
    st <- summary(mod_ols)$coefficients
    p_pos <- st["tempo_pos","Pr(>|t|)"]; p_niv <- st["interv","Pr(>|t|)"]
  }
  phi <- tryCatch(coef(mod_gls$modelStruct$corStruct, unconstrained=FALSE), error=function(e) NA_real_)

  # p.p./ano (linear, não %)
  tibble(
    grupo        = grupo_id,
    analise      = "Proporcional (linear, p.p./ano)",
    n_meses      = nrow(d),
    n_icsap_tot  = sum(dados$n_icsap),
    nivel_pct    = b["interv"],           # p.p. (nível)
    p_nivel      = p_niv,
    apc_pre      = b1 * 12,               # p.p./ano pré
    apc_pre_inf  = (b1 - 1.96*se_b1) * 12,
    apc_pre_sup  = (b1 + 1.96*se_b1) * 12,
    apc_pos      = (b1 + b3) * 12,       # p.p./ano pós
    apc_pos_inf  = ((b1+b3) - 1.96*se_pos) * 12,
    apc_pos_sup  = ((b1+b3) + 1.96*se_pos) * 12,
    p_pos        = p_pos,
    phi_ar1      = phi,
    modelo       = if (!is.null(mod_gls)) "GLS AR(1) linear" else "OLS linear"
  )
}

# =============================================================================
# 5. Ajusta modelos para todos os grupos
# =============================================================================

message("=== 3. Ajustando ITS — Análise 1 (absoluta) ===")

grupos_lista <- c("Todos", GRUPOS_ALVO)
res_abs  <- vector("list", length(grupos_lista))
res_prop <- vector("list", length(GRUPOS_ALVO))

for (i in seq_along(grupos_lista)) {
  g <- grupos_lista[i]
  message(sprintf("  >> %s", g))
  d_g <- series |> filter(grupo == g)
  res_abs[[i]] <- ajusta_its_abs(d_g, g)
}

message("\n=== 4. Ajustando ITS — Análise 2 (proporcional, P17) ===")

for (i in seq_along(GRUPOS_ALVO)) {
  g <- GRUPOS_ALVO[i]
  message(sprintf("  >> %s", g))
  d_g <- series |> filter(grupo == g)
  res_prop[[i]] <- ajusta_its_prop(d_g, g)
}

res_abs_df  <- bind_rows(res_abs)
res_prop_df <- bind_rows(res_prop)

# Adiciona rótulos
grupo_labels <- tibble(
  grupo = c("Todos","09","10","13"),
  grupo_nome = c(
    "Todos os grupos ICSAP",
    "Doenças cardiovasculares (HAS, angina, IC, AVC)",
    "Doenças respiratórias (IVAS, pneumonias, bronquite, asma)",
    "Diabetes mellitus"
  )
)

res_abs_df  <- res_abs_df  |> left_join(grupo_labels, by="grupo") |> relocate(grupo, grupo_nome)
res_prop_df <- res_prop_df |> left_join(grupo_labels, by="grupo") |> relocate(grupo, grupo_nome)

# =============================================================================
# 6. Imprime resultados
# =============================================================================

n_total_geral <- sum(series$n_icsap[series$grupo == "Todos"])

message("\n=== ANÁLISE 1 — CONTAGEM ABSOLUTA (APC %/ano) ===")
for (r in split(res_abs_df, seq_len(nrow(res_abs_df)))) {
  pct_total <- if (r$grupo != "Todos") r$n_icsap_tot / n_total_geral * 100 else 100
  message(sprintf("  %-50s n=%s (%.1f%%) | APC_pre=%+.1f%% | APC_pos=%+.1f%% (%.1f;%.1f) | p=%.4f",
                  r$grupo_nome, format(r$n_icsap_tot, big.mark=","), pct_total,
                  r$apc_pre, r$apc_pos, r$apc_pos_inf, r$apc_pos_sup, r$p_pos))
}

message("\n=== ANÁLISE 2 — PROPORÇÃO (slope change em p.p./ano) ===")
for (r in split(res_prop_df, seq_len(nrow(res_prop_df)))) {
  message(sprintf("  %-50s slope_pre=%+.2f pp/ano | slope_pos=%+.2f pp/ano (%.2f;%.2f) | p=%.4f",
                  r$grupo_nome, r$apc_pre, r$apc_pos, r$apc_pos_inf, r$apc_pos_sup, r$p_pos))
}

message("\n=== INTERPRETAÇÃO P17/P21 ===")
for (g in GRUPOS_ALVO) {
  nome <- grupo_labels$grupo_nome[grupo_labels$grupo == g]
  p_abs  <- res_abs_df$p_pos[res_abs_df$grupo == g]
  p_prop <- res_prop_df$p_pos[res_prop_df$grupo == g]
  interp <- dplyr::case_when(
    p_abs < 0.05 & p_prop < 0.05 ~ "REDUÇÃO SELETIVA — grupo caiu mais que os outros",
    p_abs < 0.05 & p_prop >= 0.05 ~ "REDUÇÃO PROPORCIONAL — acompanha o total, não seletiva",
    p_abs >= 0.05 ~ "SEM EFEITO SIGNIFICATIVO no volume absoluto"
  )
  message(sprintf("  %s (abs p=%.4f, prop p=%.4f): %s", nome, p_abs, p_prop, interp))
}

# =============================================================================
# 7. Salva CSVs
# =============================================================================

write_csv(res_abs_df,  file.path(DIR_PROC, "its_por_grupo.csv"))
write_csv(res_prop_df, file.path(DIR_PROC, "its_por_grupo_prop.csv"))
message(sprintf("\nSalvos: its_por_grupo.csv, its_por_grupo_prop.csv"))

# =============================================================================
# 8. Figura: forest plot APC pós (análise 1 — absoluta)
# =============================================================================

message("=== 5. Gerando figura ===")

fig_data <- res_abs_df |>
  mutate(
    grupo_nome = factor(grupo_nome, levels = rev(grupo_nome)),
    sig = ifelse(p_pos < 0.05, "Significativo (p<0,05)", "Não significativo"),
    label_apc = sprintf("%+.1f%%/ano\n(%+.1f; %+.1f)",
                        apc_pos, apc_pos_inf, apc_pos_sup)
  )

p_fig <- ggplot(fig_data, aes(x = apc_pos, y = grupo_nome, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_errorbar(
    aes(xmin = apc_pos_inf, xmax = apc_pos_sup),
    width = 0.25, linewidth = 0.8, orientation = "y"
  ) +
  geom_point(size = 3) +
  geom_text(aes(label = label_apc), hjust = -0.1, size = 3, show.legend = FALSE) +
  scale_color_manual(
    values = c("Significativo (p<0,05)" = "#c0392b", "Não significativo" = "#7f8c8d")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), x, "%"),
    expand = expansion(mult = c(0.05, 0.4))
  ) +
  labs(
    title    = "Mudança de tendência pós-Portaria GM/MS 3.493/2024 por grupo ICSAP",
    subtitle = "ITS GLS AR(1) — contagem absoluta — BH municipal, jan/2022–mar/2026",
    x        = "APC pós-intervenção (% ao ano, IC 95%)",
    y        = NULL, color = NULL,
    caption  = paste(
      "APC pós = (β₁+β₃)×12, IC 95% via método delta (Var(β₁+β₃) inclui Cov(β₁,β₃)).",
      "Diabetes (APC abs = −11,4%, p<0,001) mas sem mudança na composição proporcional (p=0,571):",
      "a queda reflete redução proporcional ao total ICSAP, não efeito clínico seletivo.",
      sep = "\n"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 8, color = "grey50"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

fig_path <- file.path(DIR_DOCS, "its_por_grupo.png")
agg_png(fig_path, width = 170, height = 110, units = "mm", res = 300)
print(p_fig)
dev.off()
message(sprintf("Figura salva: %s", fig_path))

message("\nScript 25 concluído.")
