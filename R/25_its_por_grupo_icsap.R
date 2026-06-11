# =============================================================================
# 25_its_por_grupo_icsap.R
#
# P9 — ITS GLS AR(1) estratificado por grupo ICSAP (Portaria GM/MS 3.493/2024)
#
# Objetivo: verificar se o efeito da Portaria 3.493/2024 é heterogêneo entre
# os principais grupos clínicos de ICSAP. Grupos analisados:
#   09 — Doenças cardiovasculares (HAS, angina, IC, AVC)
#   10 — Doenças respiratórias (IVAS, pneumonias, bronquite, asma)
#   13 — Diabetes mellitus
#   + Todos os grupos combinados (referência = script 11)
#
# Método: GLS AR(1) ITS — idêntico ao script 11 (série BH municipal)
#   log(Y_t) = β₀ + β₁·mes_num + β₂·interv + β₃·tempo_pos + ε_t
#   MES_INTERV = 29 (maio/2024)
#
# Saídas:
#   data/processed/its_por_grupo.csv
#   docs/its_por_grupo.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(nlme)
  library(ggplot2)
  library(tidyr)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

MES_INTERV <- 29L   # maio/2024 = mês 29 da série jan/2022–mar/2026

# =============================================================================
# 1. Carrega dados
# =============================================================================

message("=== 1. Carregando dados ===")

icsap_reg <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                      show_col_types = FALSE)

# Verifica colunas disponíveis
message(sprintf("  Colunas: %s", paste(names(icsap_reg), collapse = ", ")))
message(sprintf("  N total: %s", format(nrow(icsap_reg), big.mark=",")))

# Verifica coluna de grupo ICSAP
if (!"grupo" %in% names(icsap_reg)) {
  # Tenta carregar lista_icsap para fazer join pelo CID
  message("  Coluna 'grupo' não encontrada — carregando lista_icsap.csv para join")
  lista_icsap <- read_csv(file.path(DIR_REF, "lista_icsap.csv"),
                          show_col_types = FALSE)
  message(sprintf("  lista_icsap: %s linhas | colunas: %s",
                  nrow(lista_icsap), paste(names(lista_icsap), collapse = ", ")))

  # O arquivo icsap_bh_regional já é subset de ICSAP (todos são ICSAP)
  # Identifica coluna do CID (geralmente DIAG_PRINC ou CID_PRINC)
  cid_col <- intersect(c("DIAG_PRINC", "CID_PRINC", "diag_princ", "cid"), names(icsap_reg))
  if (length(cid_col) == 0) stop("Coluna CID não encontrada em icsap_bh_regional.csv")
  cid_col <- cid_col[1]
  message(sprintf("  Usando coluna CID: %s", cid_col))

  # Join com lista_icsap pelo CID (match primeiros 3 caracteres = CID-10 3 dígitos)
  lista_join <- lista_icsap %>%
    select(grupo, cid) %>%
    distinct() %>%
    mutate(cid3 = str_sub(cid, 1, 3))

  icsap_reg <- icsap_reg %>%
    mutate(cid3 = str_sub(.data[[cid_col]], 1, 3)) %>%
    left_join(lista_join %>% select(grupo, cid3), by = "cid3") %>%
    select(-cid3)

  n_sem_grupo <- sum(is.na(icsap_reg$grupo))
  message(sprintf("  Registros sem grupo após join: %d (%.1f%%)",
                  n_sem_grupo, 100 * n_sem_grupo / nrow(icsap_reg)))
} else {
  message("  Coluna 'grupo' encontrada diretamente")
}

# =============================================================================
# 2. Constrói séries mensais por grupo
# =============================================================================

message("=== 2. Construindo séries mensais por grupo ===")

# Skeleton completo: jan/2022–mar/2026 (51 meses)
meses_ref <- tibble(
  ano_cmpt = c(rep(2022L, 12), rep(2023L, 12), rep(2024L, 12),
               rep(2025L, 12), rep(2026L,  3)),
  mes_cmpt = c(1:12, 1:12, 1:12, 1:12, 1:3)
) %>%
  mutate(
    mes_num  = row_number(),
    data     = make_date(ano_cmpt, mes_cmpt, 1L),
    interv   = as.integer(mes_num >= MES_INTERV),
    tempo_pos = pmax(mes_num - MES_INTERV + 1L, 0L),
    sin12    = sin(2 * pi * mes_num / 12),
    cos12    = cos(2 * pi * mes_num / 12)
  )

# Grupos a analisar: 09, 10, 13 + "Todos"
GRUPOS_ALVO <- c("09", "10", "13")

# Série municipal total (referência)
serie_total <- icsap_reg %>%
  mutate(mes_cmpt = as.integer(mes_cmpt)) %>%
  group_by(ano_cmpt, mes_cmpt) %>%
  summarise(n_icsap = n(), .groups = "drop") %>%
  right_join(meses_ref, by = c("ano_cmpt", "mes_cmpt")) %>%
  mutate(n_icsap = replace_na(n_icsap, 0L),
         grupo = "Todos")

# Séries por grupo
serie_grupos <- icsap_reg %>%
  filter(!is.na(grupo), grupo %in% GRUPOS_ALVO) %>%
  mutate(mes_cmpt = as.integer(mes_cmpt)) %>%
  group_by(grupo, ano_cmpt, mes_cmpt) %>%
  summarise(n_icsap = n(), .groups = "drop")

# Skeleton por grupo
skeleton_grupos <- tidyr::crossing(grupo = GRUPOS_ALVO, meses_ref)

series <- skeleton_grupos %>%
  left_join(serie_grupos, by = c("grupo", "ano_cmpt", "mes_cmpt")) %>%
  mutate(n_icsap = replace_na(n_icsap, 0L)) %>%
  bind_rows(serie_total) %>%
  arrange(grupo, mes_num)

# Verifica totais
for (g in c("Todos", GRUPOS_ALVO)) {
  s <- series %>% filter(grupo == g)
  message(sprintf("  Grupo %-5s: %s ICSAP em 51 meses (média %.0f/mês)",
                  g, format(sum(s$n_icsap), big.mark=","), mean(s$n_icsap)))
}

# =============================================================================
# 3. Função ITS GLS AR(1) — idêntica ao script 11
# =============================================================================

ajusta_its <- function(dados, grupo_id) {
  d <- dados %>%
    filter(n_icsap > 0) %>%
    mutate(log_n = log(n_icsap))

  if (nrow(d) < 15) {
    message(sprintf("    AVISO: grupo %s com apenas %d meses não-zero — pulando", grupo_id, nrow(d)))
    return(NULL)
  }

  # Tenta GLS AR(1)
  mod_gls <- tryCatch(
    nlme::gls(
      log_n ~ mes_num + interv + tempo_pos + sin12 + cos12,
      data          = d,
      correlation   = nlme::corAR1(form = ~mes_num),
      method        = "ML",
      control       = nlme::glsControl(maxIter = 200, msMaxIter = 200, tolerance = 1e-6)
    ),
    error = function(e) {
      message(sprintf("    GLS AR(1) falhou para grupo %s: %s — usando OLS", grupo_id, conditionMessage(e)))
      NULL
    }
  )

  mod_ols <- lm(log_n ~ mes_num + interv + tempo_pos + sin12 + cos12, data = d)

  mod <- if (!is.null(mod_gls)) mod_gls else mod_ols

  cf  <- coef(mod)
  V   <- if (!is.null(mod_gls)) mod_gls$varBeta else vcov(mod_ols)

  b1  <- cf["mes_num"]
  b2  <- cf["interv"]
  b3  <- cf["tempo_pos"]

  # APC pré: β₁ × 12
  apc_pre     <- (exp(12 * b1) - 1) * 100
  se_b1       <- sqrt(V["mes_num", "mes_num"])
  apc_pre_inf <- (exp(12 * (b1 - 1.96 * se_b1)) - 1) * 100
  apc_pre_sup <- (exp(12 * (b1 + 1.96 * se_b1)) - 1) * 100

  # p-valor β₃ (slope change)
  if (!is.null(mod_gls)) {
    p_pos <- summary(mod_gls)$tTable["tempo_pos", "p-value"]
    p_niv <- summary(mod_gls)$tTable["interv",    "p-value"]
  } else {
    st <- summary(mod_ols)$coefficients
    p_pos <- st["tempo_pos", "Pr(>|t|)"]
    p_niv <- st["interv",    "Pr(>|t|)"]
  }

  # APC pós: (β₁+β₃) × 12 — delta method com covariância
  var_pos     <- V["mes_num","mes_num"] + V["tempo_pos","tempo_pos"] +
                 2 * V["mes_num","tempo_pos"]
  se_pos      <- sqrt(var_pos)
  apc_pos     <- (exp(12 * (b1 + b3)) - 1) * 100
  apc_pos_inf <- (exp(12 * ((b1 + b3) - 1.96 * se_pos)) - 1) * 100
  apc_pos_sup <- (exp(12 * ((b1 + b3) + 1.96 * se_pos)) - 1) * 100

  # Nível change (%)
  nivel_pct     <- (exp(b2) - 1) * 100

  # AR(1) phi
  phi <- tryCatch(
    coef(mod_gls$modelStruct$corStruct, unconstrained = FALSE),
    error = function(e) NA_real_
  )

  tibble(
    grupo        = grupo_id,
    n_meses      = nrow(d),
    n_icsap_tot  = sum(dados$n_icsap),
    nivel_pct    = nivel_pct,
    p_nivel      = p_niv,
    apc_pre      = apc_pre,
    apc_pre_inf  = apc_pre_inf,
    apc_pre_sup  = apc_pre_sup,
    apc_pos      = apc_pos,
    apc_pos_inf  = apc_pos_inf,
    apc_pos_sup  = apc_pos_sup,
    p_pos        = p_pos,
    phi_ar1      = phi,
    modelo       = if (!is.null(mod_gls)) "GLS AR(1)" else "OLS"
  )
}

# =============================================================================
# 4. Aplica ITS a cada grupo
# =============================================================================

message("=== 3. Ajustando ITS por grupo ===")

grupos_lista <- c("Todos", GRUPOS_ALVO)
resultados   <- vector("list", length(grupos_lista))

for (i in seq_along(grupos_lista)) {
  g <- grupos_lista[i]
  message(sprintf("\n  >> Grupo: %s", g))
  d_g <- series %>% filter(grupo == g)
  resultados[[i]] <- ajusta_its(d_g, g)
}

res_df <- bind_rows(resultados)

# Adiciona rótulos dos grupos
grupo_labels <- tibble(
  grupo = c("Todos", "09", "10", "13"),
  grupo_nome = c(
    "Todos os grupos ICSAP",
    "Doenças cardiovasculares (HAS, angina, IC, AVC)",
    "Doenças respiratórias (IVAS, pneumonias, bronquite, asma)",
    "Diabetes mellitus"
  )
)

res_df <- res_df %>%
  left_join(grupo_labels, by = "grupo") %>%
  relocate(grupo, grupo_nome)

# =============================================================================
# 5. Salva resultados
# =============================================================================

write_csv(res_df, file.path(DIR_PROC, "its_por_grupo.csv"))
message(sprintf("\nSalvo: %s", file.path(DIR_PROC, "its_por_grupo.csv")))

# Imprime tabela resumida
message("\n=== RESULTADOS ITS POR GRUPO ===")
message(sprintf("%-50s  APC_pré   APC_pós   p(β₃)   Modelo",
                "Grupo"))
message(strrep("-", 85))
for (i in seq_len(nrow(res_df))) {
  r <- res_df[i, ]
  message(sprintf("%-50s  %+6.1f%%   %+6.1f%%   %.4f  %s",
                  r$grupo_nome,
                  r$apc_pre, r$apc_pos, r$p_pos, r$modelo))
}

# =============================================================================
# 6. Figura: forest plot APC pós por grupo
# =============================================================================

message("\n=== 4. Gerando figura ===")

library(ragg)

fig_data <- res_df %>%
  mutate(
    grupo_nome = factor(grupo_nome, levels = rev(grupo_nome)),
    sig = ifelse(p_pos < 0.05, "Significativo (p<0,05)", "Não significativo"),
    label_apc = sprintf("%+.1f%%/ano\n(%.1f; %.1f)",
                        apc_pos, apc_pos_inf, apc_pos_sup)
  )

p <- ggplot(fig_data, aes(x = apc_pos, y = grupo_nome, color = sig)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = apc_pos_inf, xmax = apc_pos_sup),
    height = 0.25, linewidth = 0.8
  ) +
  geom_point(size = 3) +
  geom_text(
    aes(label = label_apc),
    hjust = -0.1, size = 3, show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("Significativo (p<0,05)" = "#c0392b",
               "Não significativo"       = "#7f8c8d")
  ) +
  scale_x_continuous(
    labels = function(x) paste0(ifelse(x >= 0, "+", ""), x, "%"),
    expand = expansion(mult = c(0.05, 0.35))
  ) +
  labs(
    title    = "Mudança de tendência pós-Portaria GM/MS 3.493/2024 por grupo ICSAP",
    subtitle = "ITS GLS AR(1) — BH municipal, jan/2022–mar/2026 (MES_INTERV = 29)",
    x        = "APC pós-intervenção (% ao ano, IC 95%)",
    y        = NULL,
    color    = NULL,
    caption  = paste(
      "APC pós = (β₁+β₃)×12, IC 95% via método delta (matriz vcov completa).",
      "Grupos 09, 10 e 13 correspondem às categorias da Portaria SAS/MS nº 221/2008.",
      sep = "\n"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

fig_path <- file.path(DIR_DOCS, "its_por_grupo.png")
agg_png(fig_path, width = 170, height = 100, units = "mm", res = 300)
print(p)
dev.off()
message(sprintf("Figura salva: %s", fig_path))

message("\nScript 25 concluído.")
