# =============================================================================
# 29_diagnosticos_p8_p9_p2_p10.R
# Diagnósticos pré-correção: P9 (saneamento+densidade), P8 (quartis ESF),
# P2 (missing vs IVS), P10 (método evitadas CS), Pico abr/2024
# Arquivos reais: icsap_bh.csv, icsap_bh_regional.csv, variaveis_cs.csv,
#                 ivs_por_cs.csv, internacoes_evitadas_cs.csv
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
})
select <- dplyr::select; filter <- dplyr::filter

sf::sf_use_s2(FALSE)
options(scipen=999)

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# ─── helpers ────────────────────────────────────────────────────────────────
extract_irr <- function(mod, var) {
  s   <- summary(mod)$coefficients
  b   <- s[var, "Estimate"]
  se  <- s[var, "Std. Error"]
  p   <- s[var, "Pr(>|z|)"]
  cat(sprintf("  %-26s IRR=%5.3f  IC95%%(%5.3f; %5.3f)  p=%6.4f\n",
              var, exp(b), exp(b-1.96*se), exp(b+1.96*se), p))
  invisible(list(irr=exp(b), p=p))
}

cat(strrep("=",60), "\n")
cat("P9 — SANEAMENTO AJUSTADO POR DENSIDADE\n")
cat(strrep("=",60), "\n\n")

# ---------- 1. Área real dos CS (polígonos PBH) ----------
cs_sf    <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet=TRUE)
cs_areas <- cs_sf |>
  mutate(area_km2 = as.numeric(st_area(st_transform(geometry, 31983))) / 1e6) |>
  st_drop_geometry() |>
  select(nome_cs, area_km2)
cat(sprintf("Polígonos: %d CS | área: %.2f–%.2f km²\n",
            nrow(cs_areas), min(cs_areas$area_km2), max(cs_areas$area_km2)))

# ---------- 2. Covariáveis estáticas ----------
variaveis <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"), show_col_types=FALSE)
pop_faixas <- read_csv(file.path(DIR_REF, "pop_cs_faixas.csv"), show_col_types=FALSE)

static_cs <- variaveis |>
  group_by(nome_cs) |> slice(1) |> ungroup() |>
  select(nome_cs, cod_smsa, regional, pop_total_censo,
         pct_sem_saneamento, ivs_score) |>
  left_join(cs_areas, by="nome_cs") |>
  mutate(
    densidade = pop_total_censo / area_km2,
    log_den   = log(densidade)
  ) |>
  filter(!is.na(area_km2), !is.na(ivs_score), !is.na(pct_sem_saneamento))

cat(sprintf("CS após join área+covars: %d\n\n", nrow(static_cs)))
cat(sprintf("Densidade hab/km²: mediana=%.0f | p25=%.0f | p75=%.0f | min=%.0f | max=%.0f\n",
            median(static_cs$densidade), quantile(static_cs$densidade, .25),
            quantile(static_cs$densidade, .75),
            min(static_cs$densidade), max(static_cs$densidade)))

# ---------- 3. Painel CS × mês ----------
icsap_reg <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                      col_types=cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                      show_col_types=FALSE)

pop_cs <- pop_faixas |> group_by(nome_cs) |>
  summarise(pop_censo=sum(pop_faixa, na.rm=TRUE), .groups="drop")

static_cs <- static_cs |>
  left_join(pop_cs, by="nome_cs") |>
  mutate(pop_cs = coalesce(pop_censo, pop_total_censo))

meses_ref <- tibble(
  ano_cmpt = c(rep(2022L,12),rep(2023L,12),rep(2024L,12),rep(2025L,12),rep(2026L,3)),
  mes_cmpt = c(1:12,1:12,1:12,1:12,1:3)
) |> mutate(mes_num=row_number())

cs_ref <- static_cs |> select(nome_cs, cod_smsa, regional)
skeleton <- tidyr::crossing(cs_ref, meses_ref)

n_cs_mes <- icsap_reg |>
  filter(!is.na(nome_cs)) |>
  group_by(nome_cs, ano_cmpt, mes_cmpt) |>
  summarise(n_icsap=n(), .groups="drop")

painel <- skeleton |>
  left_join(n_cs_mes, by=c("nome_cs","ano_cmpt","mes_cmpt")) |>
  mutate(n_icsap=replace_na(n_icsap, 0L)) |>
  left_join(static_cs |> select(-cod_smsa,-regional), by="nome_cs") |>
  filter(!is.na(ivs_score), !is.na(pct_sem_saneamento),
         !is.na(area_km2), !is.na(pop_cs), pop_cs>0) |>
  mutate(ano_fac=factor(ano_cmpt), reg_fac=factor(regional), log_pop=log(pop_cs))

cat(sprintf("\nPainel P9: %s obs | %d CS | %d meses\n",
            format(nrow(painel), big.mark=","),
            length(unique(painel$nome_cs)),
            length(unique(painel$mes_num))))

# ---------- 4. Modelos ----------
cat("\n--- M_REF: Regional+Ano (sem densidade) ---\n")
m_ref <- glm(n_icsap ~ pct_sem_saneamento + ivs_score + reg_fac + ano_fac,
             family=poisson(), offset=log_pop, data=painel)
extract_irr(m_ref, "pct_sem_saneamento")
extract_irr(m_ref, "ivs_score")
cat(sprintf("  Dispersão Pearson: %.2f\n",
            sum(residuals(m_ref,"pearson")^2)/m_ref$df.residual))

cat("\n--- M_ADJ: + densidade contínua (log) ---\n")
m_adj <- glm(n_icsap ~ pct_sem_saneamento + ivs_score + log_den + reg_fac + ano_fac,
             family=poisson(), offset=log_pop, data=painel)
extract_irr(m_adj, "pct_sem_saneamento")
extract_irr(m_adj, "ivs_score")
extract_irr(m_adj, "log_den")
cat(sprintf("  Dispersão Pearson: %.2f\n",
            sum(residuals(m_adj,"pearson")^2)/m_adj$df.residual))

cat("\n--- M_DEN_ONLY: substituindo saneamento por densidade ---\n")
m_den <- glm(n_icsap ~ ivs_score + log_den + reg_fac + ano_fac,
             family=poisson(), offset=log_pop, data=painel)
extract_irr(m_den, "ivs_score")
extract_irr(m_den, "log_den")

# Correlação saneamento × log_densidade
r_san_den <- cor(painel$pct_sem_saneamento, painel$log_den,
                 use="complete.obs", method="spearman")
cat(sprintf("\nSpearman saneamento × log_den: rho=%.3f\n", r_san_den))

cat("\n>>> VEREDICTO P9: ")
p_san_adj <- summary(m_adj)$coefficients["pct_sem_saneamento","Pr(>|z|)"]
if (p_san_adj > 0.05) {
  cat(sprintf("OPÇÃO A — saneamento NS após ajuste por densidade (p=%.4f)\n", p_san_adj))
} else {
  cat(sprintf("OPÇÃO B — saneamento ainda sig após ajuste por densidade (p=%.4f)\n", p_san_adj))
}

# ============================================================================
cat("\n", strrep("=",60), "\n", sep="")
cat("P8 — DISTRIBUIÇÃO QUARTIS ESF (n CS reais)\n")
cat(strrep("=",60), "\n\n")

# Usar variaveis_cs.csv: n_esf por CS × mês (2023-2025 — mesmo período script 21)
nesf_cs <- variaveis |>
  filter(ano_cmpt >= 2023, ano_cmpt <= 2025, !is.na(n_esf), n_esf >= 1) |>
  mutate(quartil = case_when(
    n_esf <= 4 ~ "Q1 (1-4)",
    n_esf <= 6 ~ "Q2 (5-6)",
    n_esf <= 8 ~ "Q3 (7-8)",
    TRUE       ~ "Q4 (9+)"
  ))

cat("--- Distribuição de observações (CS × mês, 2023-2025) ---\n")
nesf_cs |>
  count(quartil) |>
  mutate(pct=round(100*n/sum(n),1)) |>
  print()

cat("\n--- N CS distintos por quartil modal ---\n")
modal_q <- nesf_cs |>
  group_by(nome_cs) |>
  summarise(
    q_modal  = names(sort(table(quartil), decreasing=TRUE))[1],
    n_esf_med = median(n_esf),
    .groups="drop"
  )
modal_q |> count(q_modal, name="n_cs") |>
  mutate(pct=round(100*n_cs/sum(n_cs),1)) |>
  print()

cat("\n--- Breakdown Q3+Q4 separado ---\n")
modal_q |>
  filter(q_modal %in% c("Q3 (7-8)","Q4 (9+)")) |>
  count(q_modal, name="n_cs") |>
  print()

n_q3q4_cs <- sum(modal_q$q_modal %in% c("Q3 (7-8)","Q4 (9+)"))
cat(sprintf("\nN CS com n_esf modal ≥ 7 (Q3+Q4): %d\n", n_q3q4_cs))

# ============================================================================
cat("\n", strrep("=",60), "\n", sep="")
cat("P2 — CORRELAÇÃO % MISSING vs IVS (por CS)\n")
cat(strrep("=",60), "\n\n")

reg_df <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                   col_types=cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                   show_col_types=FALSE)
ivs_df <- read_csv(file.path(DIR_REF, "ivs_por_cs.csv"), show_col_types=FALSE)

# % missing por CS de RESIDÊNCIA (melhor proxy do endereço do paciente)
# Nome CS está disponível apenas para geocodificados; para não-geocodificados usamos
# outro campo. Aqui vamos usar a série completa: missing = is.na(nome_cs)
missing_cs <- reg_df |>
  mutate(geocod = !is.na(nome_cs)) |>
  group_by(nome_cs) |>
  summarise(n_total=n(), n_miss=sum(!geocod),
            pct_miss=mean(!geocod)*100, .groups="drop") |>
  filter(!is.na(nome_cs))

# Nota: para CS com alta geocodificação (nome_cs preenchido), % missing
# é a fração de registros desse CS sem coordenada. Faz sentido como variável CS-nível.

comp_ivs <- missing_cs |>
  left_join(ivs_df |> select(nome_cs, ivs_score), by="nome_cs") |>
  filter(!is.na(ivs_score))

cat(sprintf("CS com dados completos: %d (de %d com nome_cs)\n",
            nrow(comp_ivs), nrow(missing_cs)))

rho_test <- cor.test(comp_ivs$pct_miss, comp_ivs$ivs_score,
                     method="spearman", exact=FALSE)
cat(sprintf("Spearman rho = %.3f | p = %.4f\n\n",
            rho_test$estimate, rho_test$p.value))

cat("% missing por quartil IVS:\n")
comp_ivs |>
  mutate(q_ivs = ntile(ivs_score, 4)) |>
  group_by(q_ivs) |>
  summarise(
    n_cs     = n(),
    ivs_med  = round(median(ivs_score),2),
    miss_med = round(median(pct_miss),1),
    miss_iqr = round(IQR(pct_miss),1),
    .groups="drop"
  ) |>
  print()

cat("\n>>> VEREDICTO P2: ")
if (rho_test$p.value < 0.05 & rho_test$estimate > 0) {
  cat(sprintf("Correlação positiva significativa (rho=%.3f; p=%.4f) → subestimação do efeito IVS\n",
              rho_test$estimate, rho_test$p.value))
} else if (rho_test$p.value < 0.05 & rho_test$estimate < 0) {
  cat(sprintf("Correlação negativa significativa (rho=%.3f; p=%.4f) → possível superestimação do efeito IVS\n",
              rho_test$estimate, rho_test$p.value))
} else {
  cat(sprintf("Correlação não significativa (rho=%.3f; p=%.4f) → sem viés sistemático detectável\n",
              rho_test$estimate, rho_test$p.value))
}

# ============================================================================
cat("\n", strrep("=",60), "\n", sep="")
cat("P10 — MÉTODO DE CÁLCULO DAS EVITADAS POR CS\n")
cat(strrep("=",60), "\n\n")

ev_cs <- tryCatch(
  read_csv(file.path(DIR_PROC, "internacoes_evitadas_cs.csv"), show_col_types=FALSE),
  error=function(e) { cat("ERRO:", e$message, "\n"); NULL }
)
if (!is.null(ev_cs)) {
  cat(sprintf("Colunas: %s\n", paste(names(ev_cs), collapse=", ")))
  cat(sprintf("N CS: %d\n\n", nrow(ev_cs)))
  print(head(ev_cs, 5))

  # Verificar se é proporcional ao n_icsap histórico
  cat("\nVerificação: prop_cs × evitadas_total ≈ evitadas_central?\n")
  total_ev <- read_csv(file.path(DIR_PROC, "custo_evitado.csv"), show_col_types=FALSE) |>
    filter(grepl("BH Municipal", nivel, ignore.case=TRUE)) |>
    pull(evitadas_central)
  if (length(total_ev) == 1) {
    cat(sprintf("Total BH evitadas: %.0f\n", total_ev))
    cat(sprintf("Soma CS: %.0f\n", sum(ev_cs$evitadas_central, na.rm=TRUE)))
    cat(sprintf("Diferença: %.1f%%\n",
                100*(sum(ev_cs$evitadas_central, na.rm=TRUE) - total_ev)/total_ev))
  }
}

# Verificar método no script 20
cat("\nTrecho do script 20 que gera internacoes_evitadas_cs.csv:\n")
lines20 <- readLines("R/20_internacoes_evitadas.R")
idx <- grep("evitadas_cs|prop_cs|internacoes_evitadas_cs", lines20)
cat(paste(sprintf("[%d] %s", idx, lines20[idx]), collapse="\n"))

# ============================================================================
cat("\n\n", strrep("=",60), "\n", sep="")
cat("PICO ABR/2024 — COMPOSIÇÃO POR GRUPO CLÍNICO\n")
cat(strrep("=",60), "\n\n")

icsap <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                  col_types=cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                  show_col_types=FALSE)

pico_comp <- icsap |>
  mutate(
    periodo = case_when(
      ano_cmpt == 2023 ~ "2023 (anual)",
      ano_cmpt == 2024 & mes_cmpt == 4 ~ "abr/2024",
      ano_cmpt == 2024 & mes_cmpt == 3 ~ "mar/2024",
      TRUE ~ "outro"
    )
  ) |>
  filter(periodo != "outro") |>
  group_by(periodo, grupo) |>
  summarise(n=n(), .groups="drop") |>
  group_by(periodo) |>
  mutate(
    n_mes = if_else(periodo=="2023 (anual)", n/12, n),  # média mensal para 2023
    pct = round(n_mes / sum(n_mes) * 100, 1)
  ) |>
  arrange(periodo, desc(n_mes))

cat("Top 5 grupos por período (n médio/mês e %):\n")
pico_comp |>
  group_by(periodo) |>
  slice_head(n=5) |>
  print(n=30)

# Variação relativa abr/2024 vs média mensal 2023
cat("\nVariação abr/2024 vs média mensal 2023 por grupo:\n")
comp_wide <- pico_comp |>
  select(periodo, grupo, n_mes) |>
  filter(periodo %in% c("2023 (anual)", "abr/2024")) |>
  pivot_wider(names_from=periodo, values_from=n_mes) |>
  rename(n_2023 = `2023 (anual)`, n_abr24 = `abr/2024`) |>
  mutate(delta_pct = round((n_abr24/n_2023 - 1)*100, 1)) |>
  arrange(desc(delta_pct))
print(as.data.frame(comp_wide), row.names=FALSE)

cat("\n=== Script 29 concluído ===\n")
