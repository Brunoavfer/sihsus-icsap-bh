# =============================================================================
# 24_sensibilidade_saneamento.R
#
# P8/P16/P19 — Análise de sensibilidade: saneamento estratificado por
# densidade populacional REAL (hab/km²) e modelo com interação contínua
#
# Objetivo: verificar se o coeficiente contraintuitivo de pct_sem_saneamento
# (IRR < 1, modelo M2 do script 21) reflete confundimento por densidade.
# CS em áreas menos densas têm pior saneamento E menor registro de internações
# (menor acesso hospitalar) — denominador é o mesmo para todos.
#
# P16 (corr. jun/2026): densidade real = pop_total_censo / area_km2 (hab/km²),
#   calculada a partir dos polígonos oficiais PBH (data/ref/areas_abrangencia_cs.geojson)
#   Estratificação anterior usava pop_total_censo como proxy de porte (não densidade).
#
# P19 (jun/2026): modelo com interação pct_sem_saneamento × den_std (contínua)
#   para complementar a estratificação por tercis.
#
# Saídas:
#   data/processed/sensibilidade_saneamento.csv
#   docs/tabela_s5_saneamento_estratificado.html
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(fixest)
  library(gt)
  library(stringr)
})

select <- dplyr::select
filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

# =============================================================================
# 1. Carrega dados
# =============================================================================

message("=== 1. Carregando dados ===")

icsap_reg  <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                       show_col_types = FALSE)
variaveis  <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                       show_col_types = FALSE)
pop_faixas <- read_csv(file.path(DIR_REF, "pop_cs_faixas.csv"),
                       show_col_types = FALSE)

# P16: calcula área real dos CS a partir dos polígonos PBH
message("  Calculando área dos CS a partir dos polígonos PBH...")
cs_sf <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)
cs_areas <- cs_sf |>
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry() |>
  select(nome_cs, area_km2)

message(sprintf("  Polígonos: %d CS | range area: %.2f–%.2f km²",
                nrow(cs_areas), min(cs_areas$area_km2), max(cs_areas$area_km2)))

# =============================================================================
# 2. Constrói painel CS × mês
# =============================================================================

message("=== 2. Construindo painel ===")

static_cs <- variaveis |>
  group_by(nome_cs) |>
  slice(1) |>
  ungroup() |>
  select(nome_cs, cod_smsa, regional,
         pop_total_censo, pct_sem_saneamento, renda_media, pct_area_favela,
         ivs_score, ivs_predominante) |>
  left_join(cs_areas, by = "nome_cs") |>
  mutate(
    # P16: densidade real (hab/km²)
    densidade = pop_total_censo / area_km2,
    den_std   = as.numeric(scale(densidade)),
    san_std   = as.numeric(scale(pct_sem_saneamento))
  )

pop_cs_total <- pop_faixas |>
  group_by(nome_cs) |>
  summarise(pop_censo = sum(pop_faixa, na.rm = TRUE), .groups = "drop")

cs_ref <- static_cs |> select(nome_cs, cod_smsa, regional)

meses_ref <- tibble(
  ano_cmpt = c(rep(2022L, 12), rep(2023L, 12), rep(2024L, 12),
               rep(2025L, 12), rep(2026L,  3)),
  mes_cmpt = c(1:12, 1:12, 1:12, 1:12, 1:3)
) |> mutate(mes_num = row_number())

skeleton <- tidyr::crossing(cs_ref, meses_ref)

n_icsap_cs_mes <- icsap_reg |>
  filter(!is.na(nome_cs)) |>
  mutate(mes_cmpt = as.integer(mes_cmpt)) |>
  group_by(nome_cs, ano_cmpt, mes_cmpt) |>
  summarise(n_icsap = n(), .groups = "drop")

painel <- skeleton |>
  left_join(n_icsap_cs_mes, by = c("nome_cs", "ano_cmpt", "mes_cmpt")) |>
  mutate(n_icsap = replace_na(n_icsap, 0L)) |>
  left_join(static_cs |> select(-regional, -cod_smsa), by = "nome_cs") |>
  left_join(pop_cs_total, by = "nome_cs") |>
  mutate(
    sin12   = sin(2 * pi * mes_num / 12),
    cos12   = cos(2 * pi * mes_num / 12),
    ano_fac = factor(ano_cmpt),
    pop_cs  = if_else(!is.na(pop_censo) & pop_censo > 0,
                      as.double(pop_censo),
                      median(pop_censo, na.rm = TRUE))
  )

message(sprintf("  Painel: %s obs | %d CS | %d meses",
                format(nrow(painel), big.mark = ","),
                n_distinct(painel$nome_cs),
                n_distinct(painel$mes_num)))

# =============================================================================
# 3. Estratificação por tercil de densidade REAL (hab/km²)
# =============================================================================

message("=== 3. Criando tercis de densidade real (hab/km²) ===")

breaks_den <- quantile(static_cs$densidade, c(0, 1/3, 2/3, 1), na.rm = TRUE)
message(sprintf("  Limites: T1 <= %.0f | T2 = %.0f–%.0f | T3 >= %.0f hab/km²",
                breaks_den[2], breaks_den[2], breaks_den[3], breaks_den[3]))

tercis_cs <- static_cs |>
  select(nome_cs, densidade) |>
  mutate(
    tercil_den = ntile(densidade, 3),
    tercil_label = case_when(
      tercil_den == 1 ~ sprintf("T1 — Baixa densidade (<= %.0f hab/km²)", breaks_den[2]),
      tercil_den == 2 ~ sprintf("T2 — Densidade intermediária (%.0f–%.0f hab/km²)",
                                 breaks_den[2], breaks_den[3]),
      tercil_den == 3 ~ sprintf("T3 — Alta densidade (>= %.0f hab/km²)", breaks_den[3])
    )
  )

message(sprintf("  N CS por tercil: %s",
                paste(table(tercis_cs$tercil_den), collapse = " / ")))

painel <- painel |>
  left_join(tercis_cs |> select(nome_cs, tercil_den, tercil_label), by = "nome_cs")

setFixest_notes(FALSE)

# =============================================================================
# 4. Modelo geral (M2 do script 21 com densidade real)
# =============================================================================

message("=== 4. Modelo geral ===")

m_geral <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 +
    ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
    regional + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel,
  vcov   = ~nome_cs
)
message(sprintf("  Geral: N = %s | IRR san = %.3f | p = %.4f",
                format(nobs(m_geral), big.mark = ","),
                exp(coef(m_geral)["pct_sem_saneamento"]),
                pvalue(m_geral)["pct_sem_saneamento"]))

# =============================================================================
# 5. P19 — Modelo com interação saneamento × densidade (contínua)
# =============================================================================

message("=== 5. P19 — Interação saneamento × densidade contínua ===")

m_inter <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 +
    pct_sem_saneamento + den_std + pct_sem_saneamento:den_std + ivs_score |
    regional + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel,
  vcov   = ~nome_cs
)
p_inter  <- pvalue(m_inter)["pct_sem_saneamento:den_std"]
irr_inter <- exp(coef(m_inter)["pct_sem_saneamento:den_std"])
message(sprintf("  Interação san×den: IRR = %.4f | p = %.4f (%s)",
                irr_inter, p_inter,
                if (p_inter < 0.05) "SIGNIFICATIVA" else "NS"))

# =============================================================================
# 6. Modelos estratificados por tercil
# =============================================================================

message("=== 6. Modelos estratificados por tercil de densidade ===")

resultados <- list()

ci_g <- confint(m_geral)
resultados[["Geral"]] <- tibble(
  estrato  = "Modelo geral (todos os CS)",
  n_cs     = n_distinct(painel$nome_cs),
  n_obs    = nobs(m_geral),
  irr_san  = exp(coef(m_geral)["pct_sem_saneamento"]),
  irr_inf  = exp(ci_g["pct_sem_saneamento", 1]),
  irr_sup  = exp(ci_g["pct_sem_saneamento", 2]),
  p_san    = pvalue(m_geral)["pct_sem_saneamento"],
  irr_ivs  = exp(coef(m_geral)["ivs_score"]),
  p_ivs    = pvalue(m_geral)["ivs_score"],
  den_med  = NA_real_
)

for (t in 1:3) {
  lbl <- unique(tercis_cs$tercil_label[tercis_cs$tercil_den == t])
  d_t <- painel |> filter(tercil_den == t)
  den_med <- round(median(static_cs$densidade[tercis_cs$tercil_den[match(static_cs$nome_cs, tercis_cs$nome_cs)] == t], na.rm = TRUE))

  message(sprintf("  Tercil %d: %d CS | mediana densidade: %.0f hab/km²",
                  t, n_distinct(d_t$nome_cs),
                  median(d_t$densidade, na.rm = TRUE)))

  m_t <- tryCatch(
    feglm(
      n_icsap ~ mes_num + sin12 + cos12 +
        ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
        regional + ano_fac,
      offset = ~log(pop_cs), family = "poisson", data = d_t, vcov = ~nome_cs
    ),
    error = function(e) {
      message(sprintf("    regional FE falhou; usando nome_cs FE"))
      feglm(
        n_icsap ~ mes_num + sin12 + cos12 +
          ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
          nome_cs + ano_fac,
        offset = ~log(pop_cs), family = "poisson", data = d_t, vcov = ~nome_cs
      )
    }
  )

  ci_t <- tryCatch(confint(m_t), error = function(e) NULL)
  irr_inf <- if (!is.null(ci_t)) exp(ci_t["pct_sem_saneamento", 1]) else NA_real_
  irr_sup <- if (!is.null(ci_t)) exp(ci_t["pct_sem_saneamento", 2]) else NA_real_

  message(sprintf("    IRR san = %.3f (%.3f–%.3f) p = %.4f",
                  exp(coef(m_t)["pct_sem_saneamento"]),
                  irr_inf, irr_sup, pvalue(m_t)["pct_sem_saneamento"]))

  resultados[[lbl]] <- tibble(
    estrato  = lbl,
    n_cs     = n_distinct(d_t$nome_cs),
    n_obs    = nobs(m_t),
    irr_san  = exp(coef(m_t)["pct_sem_saneamento"]),
    irr_inf  = irr_inf,
    irr_sup  = irr_sup,
    p_san    = pvalue(m_t)["pct_sem_saneamento"],
    irr_ivs  = tryCatch(exp(coef(m_t)["ivs_score"]), error = function(e) NA_real_),
    p_ivs    = tryCatch(pvalue(m_t)["ivs_score"], error = function(e) NA_real_),
    den_med  = median(d_t$densidade, na.rm = TRUE)
  )
}

res_df <- bind_rows(resultados)

# Adiciona resultado da interação como atributo
attr(res_df, "p_inter")   <- p_inter
attr(res_df, "irr_inter") <- irr_inter

write_csv(res_df, file.path(DIR_PROC, "sensibilidade_saneamento.csv"))
message(sprintf("\nSalvo: %s", file.path(DIR_PROC, "sensibilidade_saneamento.csv")))

# =============================================================================
# 7. Resumo
# =============================================================================

message("\n=== RESUMO SENSIBILIDADE SANEAMENTO ===")
for (i in seq_len(nrow(res_df))) {
  r <- res_df[i, ]
  message(sprintf("  %-55s IRR=%.3f (%.3f–%.3f) p=%.4f",
                  r$estrato, r$irr_san, r$irr_inf, r$irr_sup, r$p_san))
}
message(sprintf("\n  INTERAÇÃO saneamento×densidade (contínua): IRR=%.4f p=%.4f (%s)",
                irr_inter, p_inter,
                if (p_inter < 0.05) "sig" else "NS"))

if (all(res_df$p_san[-1] > 0.05, na.rm = TRUE) && p_inter > 0.05) {
  message("\n  CONCLUSÃO: IRR san NS em todos os tercis e interação NS —")
  message("  padrão consistente com confundimento ecológico.")
  message("  Evidência de que o efeito no modelo geral não reflete")
  message("  associação direta saneamento → ICSAP.")
}

message("\nScript 24 concluído.")
