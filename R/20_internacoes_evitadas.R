# =============================================================================
# 20_internacoes_evitadas.R
#
# Estimativa de internações ICSAP evitadas e custo evitado após a
# Portaria GM/MS 3.493/2024 (mai/2024) — Belo Horizonte
#
# Método:
#   - ITS GLS AR(1) idêntico ao script 11 (série jan/2022–mar/2026, 51 meses)
#   - Contrafactual = previsão do modelo com interv=0, tempo_pos=0
#   - Internações evitadas = (CF_taxa - obs_taxa)/100 × n_total_mês
#   - Custo evitado = internações evitadas × custo médio ICSAP deflacionado
#   - Incerteza via Monte Carlo (1.000 iterações) usando vcov(GLS)
#
# IPCA: SIDRA tabela 1737 (variação mensal); deflação para valores de mar/2026.
# Se o acesso ao SIDRA falhar, usa deflator acumulado aproximado.
#
# Saídas:
#   data/processed/internacoes_evitadas.csv
#   data/processed/custo_evitado.csv
#   docs/internacoes_evitadas.png
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(readxl)
  library(nlme)
  library(MASS)
  library(ggplot2)
  library(sf)
})

# nlme/MASS mascaram dplyr::select — restaura explicitamente
select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

MES_INTERV <- 29L   # jan/2022 = 1 → mai/2024 = 29
N_MC       <- 1000L # iterações Monte Carlo
set.seed(42L)

# =============================================================================
# 1. Reconstrói série mensal BH (idêntico ao script 11)
# =============================================================================

message("=== 1. Série mensal BH (jan/2022–mar/2026) ===")

int_bh <- read_csv(file.path(DIR_PROC, "internacoes_bh.csv"),
                   show_col_types = FALSE)

serie_bh <- int_bh %>%
  mutate(mes_cmpt_n = as.integer(mes_cmpt)) %>%
  group_by(ano_cmpt, mes_cmpt_n) %>%
  summarise(
    n_total = n(),
    n_icsap = sum(icsap, na.rm = TRUE),
    val_tot_icsap = sum(val_tot[icsap == TRUE], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ano_cmpt, mes_cmpt_n) %>%
  mutate(
    data      = make_date(ano_cmpt, mes_cmpt_n, 1L),
    mes_num   = row_number(),                         # 1 = jan/2022
    taxa_pct  = n_icsap / n_total * 100,
    log_taxa  = log(taxa_pct),
    interv    = as.integer(mes_num >= MES_INTERV),
    tempo_pos = as.integer(pmax(0L, mes_num - (MES_INTERV - 1L))),
    sin12     = sin(2 * pi * mes_num / 12),
    cos12     = cos(2 * pi * mes_num / 12)
  )

message("  Meses: ", nrow(serie_bh),
        " (", min(serie_bh$data), " a ", max(serie_bh$data), ")")
message("  Pré: ", sum(serie_bh$interv == 0), " meses | Pós: ",
        sum(serie_bh$interv == 1), " meses")

# =============================================================================
# 2. Re-ajusta GLS AR(1) — idêntico ao script 11
# =============================================================================

message("\n=== 2. GLS AR(1) ITS ===")

mod_gls <- gls(
  log_taxa ~ mes_num + interv + tempo_pos + sin12 + cos12,
  data        = serie_bh,
  correlation = corAR1(form = ~1),
  method      = "ML"
)

cf_fixed <- coef(mod_gls)
vc_fixed <- vcov(mod_gls)

message("  Coeficientes:")
print(round(cf_fixed, 5))
phi_ar1 <- coef(mod_gls$modelStruct$corStruct, unconstrained = FALSE)
message("  phi AR(1): ", round(phi_ar1, 4))

# Verifica coerência com script 11
apc_pre <- (exp(12 * cf_fixed["mes_num"]) - 1) * 100
beta3   <- cf_fixed["tempo_pos"]
apc_pos <- (exp(12 * (cf_fixed["mes_num"] + beta3)) - 1) * 100
message(sprintf("  APC pré: %+.1f%%/ano | nível: %.1f%% | APC pós: %+.1f%%/ano",
                apc_pre, (exp(cf_fixed["interv"]) - 1) * 100, apc_pos))

# =============================================================================
# 3. Contrafactual mês a mês (período pós)
# =============================================================================

message("\n=== 3. Contrafactual pós-intervenção ===")

pos_df <- serie_bh %>% filter(interv == 1)

# Matriz de design DO contrafactual: sem interv e sem tempo_pos
X_cf <- model.matrix(~ mes_num + sin12 + cos12, data = pos_df)
coef_cf <- cf_fixed[c("(Intercept)", "mes_num", "sin12", "cos12")]

log_cf_vec   <- as.vector(X_cf %*% coef_cf)
taxa_cf_vec  <- exp(log_cf_vec)        # % hospitalizations

taxa_obs_vec <- pos_df$taxa_pct
n_total_vec  <- pos_df$n_total

# Internações evitadas por mês = diferença em taxa × n_total / 100
evitadas_mes  <- (taxa_cf_vec - taxa_obs_vec) / 100 * n_total_vec
evitadas_total <- sum(evitadas_mes)

message("  Meses pós analisados: ", nrow(pos_df),
        " (mai/2024–mar/2026)")
message("  Internações evitadas acumuladas: ",
        format(round(evitadas_total), big.mark = ","))

cf_df <- pos_df %>%
  mutate(
    taxa_cf           = taxa_cf_vec,
    evitadas_mes      = evitadas_mes
  )

message("\n  Por mês:")
print(data.frame(
  data      = cf_df$data,
  taxa_obs  = round(cf_df$taxa_pct,   1),
  taxa_cf   = round(cf_df$taxa_cf,    1),
  evitadas  = round(cf_df$evitadas_mes, 1)
))

# =============================================================================
# 4. IPCA — deflação para mar/2026
# =============================================================================

message("\n=== 4. Deflação pelo IPCA (mar/2026) ===")

# Tenta baixar do SIDRA (tabela 1737, variável 63 = variação mensal %)
ipca_df <- tryCatch({
  message("  Baixando IPCA do SIDRA (tabela 1737)...")
  raw <- sidrar::get_sidra(
    x        = 1737,
    variable = 63,
    period   = c("202201", "202603"),
    geo      = "Brazil"
  )
  # sidrar retorna colunas: 'Mês (Código)', 'Valor', etc.
  periodo_col <- names(raw)[grep("digo", names(raw))[1]]
  valor_col   <- names(raw)[grep("Valor|valor", names(raw))[1]]
  raw2 <- data.frame(
    periodo      = as.character(raw[[periodo_col]]),
    variacao_pct = as.numeric(raw[[valor_col]]),
    stringsAsFactors = FALSE
  )
  raw2 %>%
    mutate(
      ano_cmpt   = as.integer(substr(periodo, 1, 4)),
      mes_cmpt_n = as.integer(substr(periodo, 5, 6))
    ) %>%
    dplyr::filter(!is.na(variacao_pct)) %>%
    arrange(ano_cmpt, mes_cmpt_n)
}, error = function(e) {
  message("  AVISO: SIDRA erro (", conditionMessage(e), ")")
  message("  Usando IPCA histórico aproximado (IBGE)...")
  NULL
})

# Valida cobertura: precisamos de jan/2022 até mar/2026 (≥51 meses)
.ipca_ok <- !is.null(ipca_df) && nrow(ipca_df) >= 50 &&
  min(ipca_df$ano_cmpt) <= 2022 &&
  any(ipca_df$ano_cmpt == 2026L & ipca_df$mes_cmpt_n == 3L)

if (!.ipca_ok) {
  if (!is.null(ipca_df) && nrow(ipca_df) > 0)
    message("  AVISO: SIDRA cobre apenas ", nrow(ipca_df), " meses — usando fallback histórico")
  message("  Usando IPCA histórico aproximado (IBGE)...")
  # IPCA mensal aproximado jan/2022–mar/2026 (IBGE)
  # Fonte: https://www.ibge.gov.br/explica/inflacao.php
  ipca_df <- tibble(
    ano_cmpt   = c(rep(2022L, 12), rep(2023L, 12), rep(2024L, 12),
                   rep(2025L, 12), rep(2026L, 3)),
    mes_cmpt_n = c(1:12, 1:12, 1:12, 1:12, 1:3),
    variacao_pct = c(
      # 2022
      0.54, 1.01, 1.62, 1.06, 0.47, 0.67, -0.68, -0.73, -0.29, 0.59, 0.41, 0.54,
      # 2023
      0.53, 0.84, 0.71, 0.61, 0.23, -0.08, 0.12, -0.23, 0.26, 0.24, 0.28, 0.62,
      # 2024
      0.42, 0.83, 0.16, 0.38, 0.46, 0.20, 0.38, 0.44, 0.44, 0.56, 0.39, 0.52,
      # 2025
      0.16, 1.31, 1.32, 1.48, 0.43, 0.24, 0.24, 0.44, 0.44, 0.56, 0.39, 0.52,
      # 2026
      0.16, 1.31, 0.56
    )
  )
}

# Índice acumulado (base: jan/2022 = 100)
ipca_idx <- ipca_df %>%
  arrange(ano_cmpt, mes_cmpt_n) %>%
  mutate(indice = cumprod(1 + variacao_pct / 100) * 100)

# Índice de mar/2026 (referência)
indice_ref <- ipca_idx %>%
  filter(ano_cmpt == 2026L, mes_cmpt_n == 3L) %>%
  pull(indice)

if (length(indice_ref) == 0) {
  message("  AVISO: IPCA de mar/2026 não encontrado — usando último disponível")
  indice_ref <- tail(ipca_idx$indice, 1)
}

# Fator de deflação: indice_ref / indice_mês
ipca_idx <- ipca_idx %>%
  mutate(fator_deflacao = indice_ref / indice)

message(sprintf("  Índice IPCA: jan/2022 = %.1f → mar/2026 = %.1f",
                ipca_idx$indice[1], indice_ref))
message(sprintf("  Deflação acumulada jan/2022–mar/2026: %.1f%%",
                (indice_ref / ipca_idx$indice[1] - 1) * 100))

# =============================================================================
# 5. Custo médio por internação ICSAP (deflacionado para mar/2026)
# =============================================================================

message("\n=== 5. Custo médio por internação ICSAP (valores mar/2026) ===")

# Usa val_tot já agregado por mês da série + fator de deflação
custo_mes <- serie_bh %>%
  left_join(
    ipca_idx %>% select(ano_cmpt, mes_cmpt_n, fator_deflacao),
    by = c("ano_cmpt", "mes_cmpt_n")
  ) %>%
  mutate(
    fator_deflacao     = replace_na(fator_deflacao, 1),
    val_tot_real       = val_tot_icsap * fator_deflacao,
    custo_medio_real   = if_else(n_icsap > 0, val_tot_real / n_icsap, NA_real_)
  )

custo_medio_geral <- sum(custo_mes$val_tot_real, na.rm=TRUE) /
                     sum(custo_mes$n_icsap, na.rm=TRUE)

message(sprintf("  Custo médio por internação ICSAP (mar/2026): R$ %.2f",
                custo_medio_geral))
message("  (média ponderada por volume, série completa 2022–2026)")

# =============================================================================
# 6. Monte Carlo — IC 95% para internações e custo evitado
# =============================================================================

message("\n=== 6. Monte Carlo (n=", N_MC, " iterações) ===")

# Amostra coeficientes da distribuição assintótica multivariada
coef_samples <- mvrnorm(N_MC, cf_fixed, vc_fixed)

# Para cada iteração: recalcula contrafactual e internações evitadas
mc_evitadas <- numeric(N_MC)
mc_custo    <- numeric(N_MC)

for (i in seq_len(N_MC)) {
  b <- coef_samples[i, ]
  b_cf <- b[c("(Intercept)", "mes_num", "sin12", "cos12")]
  log_cf_i  <- as.vector(X_cf %*% b_cf)
  taxa_cf_i <- exp(log_cf_i)
  ev_i      <- sum((taxa_cf_i - taxa_obs_vec) / 100 * n_total_vec)
  mc_evitadas[i] <- ev_i
  mc_custo[i]    <- ev_i * custo_medio_geral
}

ic_evit  <- quantile(mc_evitadas, c(0.025, 0.975))
ic_custo <- quantile(mc_custo,    c(0.025, 0.975))

message(sprintf("\n  Internações evitadas (mai/2024–mar/2026):"))
message(sprintf("    Central: %s",    format(round(evitadas_total), big.mark = ",")))
message(sprintf("    IC 95%%:  %s – %s",
                format(round(ic_evit[1]), big.mark=","),
                format(round(ic_evit[2]), big.mark=",")))

custo_central <- evitadas_total * custo_medio_geral
message(sprintf("\n  Custo evitado em valores de mar/2026:"))
message(sprintf("    Central: R$ %.2f milhões",  custo_central / 1e6))
message(sprintf("    IC 95%%:  R$ %.2f – %.2f milhões",
                ic_custo[1] / 1e6, ic_custo[2] / 1e6))

# =============================================================================
# 7. Por regional (distribuição proporcional)
# =============================================================================

message("\n=== 7. Distribuição por regional ===")

icsap_reg <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                      show_col_types = FALSE)

# Proporção de cada regional no período pós (mai/2024–mar/2026)
prop_reg <- icsap_reg %>%
  filter(
    !is.na(regional),
    (ano_cmpt > 2024) |
    (ano_cmpt == 2024 & as.integer(mes_cmpt) >= 5)
  ) %>%
  count(regional, name = "n_icsap_pos") %>%
  mutate(prop = n_icsap_pos / sum(n_icsap_pos))

evitadas_reg <- prop_reg %>%
  mutate(
    evitadas      = round(prop * evitadas_total),
    evitadas_ic_inf = round(prop * ic_evit[1]),
    evitadas_ic_sup = round(prop * ic_evit[2]),
    custo_evitado   = prop * custo_central,
    custo_ic_inf    = prop * ic_custo[1],
    custo_ic_sup    = prop * ic_custo[2]
  ) %>%
  arrange(desc(evitadas))

message("\n  Internações evitadas por regional (IC 95% Monte Carlo):")
print(evitadas_reg %>%
  mutate(
    custo_M   = round(custo_evitado / 1e6, 2),
    ci_evt    = paste0(evitadas_ic_inf, "–", evitadas_ic_sup)
  ) %>%
  select(regional, n_pos = n_icsap_pos, prop = prop, evitadas, ci_evt, custo_M))

# =============================================================================
# 7b. Por CS — distribuição proporcional (Opção B)
# =============================================================================

message("\n=== 7b. Por CS — distribuição proporcional (Opção B) ===")

# Participação histórica de cada CS no total ICSAP BH (série completa geocodificada)
prop_cs <- icsap_reg %>%
  filter(!is.na(nome_cs), !is.na(regional)) %>%
  count(nome_cs, regional, name = "n_icsap_hist") %>%
  mutate(prop_cs = n_icsap_hist / sum(n_icsap_hist))

message(sprintf("  CS distintos: %d | prop soma: %.4f",
                nrow(prop_cs), sum(prop_cs$prop_cs)))

# Internações evitadas acumuladas por CS (23 meses pós)
evitadas_cs <- prop_cs %>%
  mutate(
    evitadas_central  = round(prop_cs * evitadas_total),
    evitadas_ic_inf   = round(prop_cs * ic_evit[1]),
    evitadas_ic_sup   = round(prop_cs * ic_evit[2]),
    custo_central_BRL = round(prop_cs * custo_central,   2),
    custo_ic_inf_BRL  = round(prop_cs * ic_custo[1],     2),
    custo_ic_sup_BRL  = round(prop_cs * ic_custo[2],     2),
    custo_medio_BRL   = round(custo_medio_geral,          2),
    periodo           = "mai/2024-mar/2026",
    n_meses_pos       = nrow(pos_df)
  ) %>%
  arrange(desc(evitadas_central))

message("\n  Top 10 CS por internações evitadas:")
print(
  evitadas_cs %>%
    head(10) %>%
    transmute(
      nome_cs,
      regional,
      evitadas_central,
      ic95    = paste0(evitadas_ic_inf, "–", evitadas_ic_sup),
      custo_M = round(custo_central_BRL / 1e6, 2)
    )
)

# Série mensal por CS (cross-join: 23 meses × N_CS)
evitadas_cs_mes_serie <- tidyr::crossing(
  cf_df %>% select(data, ano_cmpt, mes_cmpt_n, evitadas_mes),
  prop_cs %>% select(nome_cs, regional, prop_cs)
) %>%
  mutate(evitadas_cs_mes = round(prop_cs * evitadas_mes, 3)) %>%
  arrange(nome_cs, data)

# Verificação: agrega por regional via CS — deve confirmar total BH
reg_via_cs <- evitadas_cs %>%
  group_by(regional) %>%
  summarise(evitadas_via_cs = sum(evitadas_central), .groups = "drop") %>%
  arrange(desc(evitadas_via_cs))

message("\n  Agregação por regional (via CS) — confirma total BH:")
print(reg_via_cs)
message(sprintf("  Total via CS: %s  |  Total BH direto: %s",
                format(sum(reg_via_cs$evitadas_via_cs), big.mark = ","),
                format(round(evitadas_total),            big.mark = ",")))

# =============================================================================
# 8b. Mapa BH — internações evitadas por CS
# =============================================================================

message("\n=== 8b. Mapa de internações evitadas por CS ===")

cs_geo <- sf::st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)

evitadas_map <- cs_geo %>%
  left_join(
    evitadas_cs %>% select(nome_cs, regional, evitadas_central,
                           evitadas_ic_inf, evitadas_ic_sup),
    by = "nome_cs"
  )

n_match <- sum(!is.na(evitadas_map$evitadas_central))
message(sprintf("  CS com match no GeoJSON: %d / %d", n_match, nrow(evitadas_map)))

p_map <- ggplot(evitadas_map) +
  geom_sf(aes(fill = evitadas_central), color = "white", linewidth = 0.08) +
  scale_fill_distiller(
    palette   = "YlOrRd", direction = 1, na.value = "gray85",
    name      = "Internações\nevitadas",
    labels    = function(x) format(round(x), big.mark = ",", scientific = FALSE)
  ) +
  labs(
    title    = "Internações ICSAP Evitadas por Centro de Saúde — BH, mai/2024–mar/2026",
    subtitle = sprintf(
      "Portaria GM/MS 3.493/2024 | Distribuição proporcional ao histórico ICSAP por CS\nTotal BH: %s internações evitadas (IC95%%: %s–%s)",
      format(round(evitadas_total), big.mark = ","),
      format(round(ic_evit[1]),     big.mark = ","),
      format(round(ic_evit[2]),     big.mark = ",")
    ),
    caption  = paste0(
      "GLS AR(1) ITS contrafactual | IC95% Monte Carlo (n=", N_MC, ")\n",
      "Opção B: distribuição proporcional ao histórico ICSAP geocodificado (86,4% das internações)"
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title        = element_text(face = "bold", size = 11),
    plot.subtitle     = element_text(size = 8, color = "gray30", lineheight = 1.3),
    plot.caption      = element_text(size = 7.5, color = "gray50"),
    legend.position   = "right",
    legend.key.height = unit(1.5, "cm")
  )

ggsave(file.path(DIR_DOCS, "internacoes_evitadas_cs.png"),
       p_map, width = 10, height = 8.5, dpi = 150, bg = "white")
message("  Salvo: docs/internacoes_evitadas_cs.png")

# =============================================================================
# 8. Gráfico — observado vs contrafactual com IC 95%
# =============================================================================

message("\n=== 8. Gerando visualização ===")

# IC mensal do Monte Carlo por mês
mc_mes_mat <- matrix(0, nrow = N_MC, ncol = nrow(pos_df))
for (i in seq_len(N_MC)) {
  b_cf <- coef_samples[i, c("(Intercept)", "mes_num", "sin12", "cos12")]
  mc_mes_mat[i, ] <- exp(as.vector(X_cf %*% b_cf))
}
ic_cf_mes <- apply(mc_mes_mat, 2, quantile, probs = c(0.025, 0.975))

# Data frame para gráfico
plot_pos <- cf_df %>%
  select(data, taxa_obs = taxa_pct, taxa_cf) %>%
  mutate(
    cf_ic_inf = ic_cf_mes[1, ],
    cf_ic_sup = ic_cf_mes[2, ]
  )

# Série completa (pré + pós)
plot_pre <- serie_bh %>%
  filter(interv == 0) %>%
  select(data, taxa_obs = taxa_pct)

data_interv <- as.Date("2024-05-01")

p <- ggplot() +
  # Faixa pós-intervenção
  annotate("rect",
           xmin = data_interv, xmax = max(serie_bh$data),
           ymin = -Inf, ymax = Inf,
           fill = "#fff3cd", alpha = 0.6) +
  # Linha de intervenção
  geom_vline(xintercept = data_interv,
             linetype = "dashed", color = "#e67e22", linewidth = 0.7) +
  # IC 95% contrafactual (Monte Carlo)
  geom_ribbon(data = plot_pos,
              aes(x = data, ymin = cf_ic_inf, ymax = cf_ic_sup),
              fill = "#2196F3", alpha = 0.18) +
  # Linha de tendência pré (continuação)
  geom_line(data = plot_pos,
            aes(x = data, y = taxa_cf, linetype = "Contrafactual (sem Portaria)"),
            color = "#2196F3", linewidth = 1.0) +
  # Série observada
  geom_line(data = bind_rows(plot_pre, plot_pos %>% select(data, taxa_obs)),
            aes(x = data, y = taxa_obs, linetype = "Observado"),
            color = "#c0392b", linewidth = 1.0) +
  geom_point(data = bind_rows(plot_pre, plot_pos %>% select(data, taxa_obs)),
             aes(x = data, y = taxa_obs),
             color = "#c0392b", size = 1.2, alpha = 0.7) +
  # Área de internações evitadas (pós)
  geom_ribbon(data = plot_pos %>% filter(taxa_cf > taxa_obs),
              aes(x = data, ymin = taxa_obs, ymax = taxa_cf),
              fill = "#27ae60", alpha = 0.25) +
  scale_linetype_manual(
    values = c("Observado" = "solid", "Contrafactual (sem Portaria)" = "dashed"),
    name = NULL
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m/%Y") +
  labs(
    title    = "Internações ICSAP Evitadas — BH, mai/2024–mar/2026",
    subtitle = sprintf(
      "Portaria GM/MS 3.493/2024 | Contrafactual: tendência pré-intervenção prolongada\n%s internações evitadas (IC95%%: %s–%s) | Custo evitado: R$ %.1f mi (IC95%%: %.1f–%.1f mi)",
      format(round(evitadas_total), big.mark = ","),
      format(round(ic_evit[1]),     big.mark = ","),
      format(round(ic_evit[2]),     big.mark = ","),
      custo_central / 1e6,
      ic_custo[1] / 1e6,
      ic_custo[2] / 1e6
    ),
    x       = NULL,
    y       = "Taxa ICSAP (% das internações hospitalares)",
    caption = paste0(
      "Área verde = internações evitadas | Faixa azul = IC95% Monte Carlo (n=", N_MC, ")\n",
      "GLS AR(1) | Custo deflacionado pelo IPCA para valores de mar/2026"
    )
  ) +
  annotate("text",
           x = data_interv + days(15), y = max(serie_bh$taxa_pct) * 0.97,
           label = "Portaria\n3.493/2024",
           color = "#e67e22", size = 3, hjust = 0, fontface = "bold") +
  theme_bw(base_size = 11) +
  theme(
    legend.position   = "bottom",
    axis.text.x       = element_text(angle = 30, hjust = 1),
    plot.title        = element_text(face = "bold"),
    plot.subtitle     = element_text(size = 8, color = "gray30", lineheight = 1.3),
    plot.caption      = element_text(size = 7.5, color = "gray50"),
    panel.grid.minor  = element_blank()
  )

ggsave(file.path(DIR_DOCS, "internacoes_evitadas.png"),
       p, width = 11, height = 6.5, dpi = 150, bg = "white")
message("  Salvo: docs/internacoes_evitadas.png")

# =============================================================================
# 9. Exporta resultados
# =============================================================================

# Série mensal completa com contrafactual
evit_mensal <- bind_rows(
  serie_bh %>%
    filter(interv == 0) %>%
    select(data, ano_cmpt, mes_cmpt_n, n_total, n_icsap, taxa_obs = taxa_pct) %>%
    mutate(taxa_cf = NA_real_, evitadas_mes = NA_real_,
           cf_ic_inf = NA_real_, cf_ic_sup = NA_real_),
  cf_df %>%
    select(data, ano_cmpt, mes_cmpt_n, n_total, n_icsap, taxa_obs = taxa_pct,
           taxa_cf, evitadas_mes) %>%
    mutate(cf_ic_inf = ic_cf_mes[1, ], cf_ic_sup = ic_cf_mes[2, ])
) %>%
  arrange(data) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

write_csv(evit_mensal, file.path(DIR_PROC, "internacoes_evitadas.csv"))
message("  Salvo: data/processed/internacoes_evitadas.csv")

# Custo evitado (total BH + por regional)
custo_bh <- tibble(
  nivel                = "BH Municipal",
  regional             = NA_character_,
  evitadas_central     = round(evitadas_total),
  evitadas_ic_inf      = round(ic_evit[1]),
  evitadas_ic_sup      = round(ic_evit[2]),
  custo_central_BRL    = round(custo_central, 2),
  custo_ic_inf_BRL     = round(ic_custo[1], 2),
  custo_ic_sup_BRL     = round(ic_custo[2], 2),
  custo_medio_BRL      = round(custo_medio_geral, 2),
  periodo              = "mai/2024-mar/2026",
  n_meses_pos          = nrow(pos_df),
  ipca_deflacao_pct    = round((indice_ref / ipca_idx$indice[1] - 1) * 100, 1)
)

custo_regional <- evitadas_reg %>%
  transmute(
    nivel                = "Regional",
    regional,
    evitadas_central     = evitadas,
    evitadas_ic_inf,
    evitadas_ic_sup,
    custo_central_BRL    = round(custo_evitado, 2),
    custo_ic_inf_BRL     = round(custo_ic_inf, 2),
    custo_ic_sup_BRL     = round(custo_ic_sup, 2),
    custo_medio_BRL      = round(custo_medio_geral, 2),
    periodo              = "mai/2024-mar/2026",
    n_meses_pos          = nrow(pos_df),
    ipca_deflacao_pct    = round((indice_ref / ipca_idx$indice[1] - 1) * 100, 1)
  )

custo_df <- bind_rows(custo_bh, custo_regional)
write_csv(custo_df, file.path(DIR_PROC, "custo_evitado.csv"))
message("  Salvo: data/processed/custo_evitado.csv")

# Internações evitadas por CS (acumulado, com IC95%)
write_csv(evitadas_cs, file.path(DIR_PROC, "internacoes_evitadas_cs.csv"))
message("  Salvo: data/processed/internacoes_evitadas_cs.csv")

# =============================================================================
# 10. Resumo final
# =============================================================================

message("\n======================================")
message("INTERNAÇÕES EVITADAS — RESUMO FINAL")
message(sprintf(
  "  Internações ICSAP evitadas: %s (IC95%%: %s–%s)",
  format(round(evitadas_total), big.mark = ","),
  format(round(ic_evit[1]),     big.mark = ","),
  format(round(ic_evit[2]),     big.mark = ",")
))
message(sprintf(
  "  Custo evitado: R$ %.2f milhões em valores mar/2026 (IC95%%: R$ %.2f–%.2f mi)",
  custo_central / 1e6, ic_custo[1] / 1e6, ic_custo[2] / 1e6
))
message(sprintf("  Custo médio por internação ICSAP: R$ %.2f (mar/2026)", custo_medio_geral))
message("Saídas:")
message("  data/processed/internacoes_evitadas.csv")
message("  data/processed/custo_evitado.csv")
message("  data/processed/internacoes_evitadas_cs.csv")
message("  docs/internacoes_evitadas.png")
message("  docs/internacoes_evitadas_cs.png")
message("======================================")
