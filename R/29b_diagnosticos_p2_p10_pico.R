# =============================================================================
# 29b_diagnosticos_p2_p10_pico.R  — continuação do 29 (P2, P10, Pico)
# P2: correlação missing vs IVS via proxy CEP → CS modal
# P10: verificar método evitadas_cs (script 20)
# Pico: composição por grupo abr/2024 vs 2023
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})
select <- dplyr::select; filter <- dplyr::filter

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# ============================================================================
cat(strrep("=",60), "\n")
cat("P2 — CORRELAÇÃO % MISSING vs IVS (proxy via CEP)\n")
cat(strrep("=",60), "\n\n")

# Fonte: icsap_bh_regional.csv tem TODOS os registros ICSAP.
# Registros geocodificados têm nome_cs; não-geocodificados têm nome_cs=NA.
# Para computar % missing por CS-área usamos o CEP como proxy:
#   Para cada CEP, calculamos % sem geocodificação.
#   Ligamos CEPs a CS via o CS modal dos registros geocodificados.

reg_df <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                   col_types=cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                   show_col_types=FALSE)
ivs_df <- read_csv(file.path(DIR_REF, "ivs_por_cs.csv"), show_col_types=FALSE)

# 1. % missing por CEP
cep_stats <- reg_df |>
  mutate(cep=as.character(cep), geocod=!is.na(nome_cs)) |>
  group_by(cep) |>
  summarise(n_tot=n(), n_geocod=sum(geocod),
            pct_miss=100*(1-mean(geocod)), .groups="drop") |>
  filter(n_tot >= 5)  # só CEPs com pelo menos 5 registros

cat(sprintf("CEPs com ≥5 registros: %d\n", nrow(cep_stats)))

# 2. CS modal para cada CEP (usando registros geocodificados)
cep_cs_modal <- reg_df |>
  filter(!is.na(nome_cs), !is.na(cep)) |>
  mutate(cep=as.character(cep)) |>
  group_by(cep, nome_cs) |>
  summarise(n=n(), .groups="drop") |>
  group_by(cep) |>
  slice_max(n, n=1, with_ties=FALSE) |>
  ungroup() |>
  select(cep, nome_cs_modal=nome_cs)

cat(sprintf("CEPs com CS modal: %d\n", nrow(cep_cs_modal)))

# 3. Join CEP → CS → IVS
cep_join <- cep_stats |>
  left_join(cep_cs_modal, by="cep") |>
  left_join(ivs_df |> select(nome_cs, ivs_score),
            by=c("nome_cs_modal"="nome_cs")) |>
  filter(!is.na(ivs_score))

cat(sprintf("CEPs com IVS: %d (de %d)\n\n", nrow(cep_join), nrow(cep_stats)))

# 4. Correlação Spearman % missing × IVS (ponderada por n_tot)
rho_test <- cor.test(cep_join$pct_miss, cep_join$ivs_score,
                     method="spearman", exact=FALSE)
cat(sprintf("Spearman rho (CEP-nível, n=%d): %.3f | p=%.4f\n\n",
            nrow(cep_join), rho_test$estimate, rho_test$p.value))

# 5. % missing por quartil IVS do CS modal
cat("% missing por quartil IVS do CS modal:\n")
cep_join |>
  mutate(q_ivs=ntile(ivs_score, 4)) |>
  group_by(q_ivs) |>
  summarise(
    n_cep   = n(),
    ivs_med = round(median(ivs_score),2),
    miss_med= round(median(pct_miss),1),
    miss_p75= round(quantile(pct_miss,.75),1),
    .groups ="drop"
  ) |>
  print()

# 6. Alternativa: % missing agregado por CS (via todos os CEPs daquele CS)
cat("\n--- CS-level: % missing médio (ponderado por n registros) ---\n")
cs_miss <- cep_join |>
  group_by(nome_cs_modal) |>
  summarise(
    n_tot_cs    = sum(n_tot),
    pct_miss_cs = sum(n_tot * pct_miss) / sum(n_tot),
    .groups     = "drop"
  ) |>
  left_join(ivs_df |> select(nome_cs, ivs_score),
            by=c("nome_cs_modal"="nome_cs"))

cat(sprintf("N CS: %d\n", nrow(cs_miss)))
rho_cs <- cor.test(cs_miss$pct_miss_cs, cs_miss$ivs_score,
                   method="spearman", exact=FALSE)
cat(sprintf("Spearman rho (CS-nível): %.3f | p=%.4f\n", rho_cs$estimate, rho_cs$p.value))

cs_miss |>
  mutate(q_ivs=ntile(ivs_score,4)) |>
  group_by(q_ivs) |>
  summarise(
    n_cs    = n(),
    ivs_med = round(median(ivs_score),2),
    miss_med= round(median(pct_miss_cs),1),
    .groups ="drop"
  ) |>
  print()

cat("\n>>> VEREDICTO P2 (CS-nível): ")
if (!is.na(rho_cs$p.value) && rho_cs$p.value < 0.05) {
  cat(sprintf("Correlação SIG (rho=%.3f; p=%.4f)\n", rho_cs$estimate, rho_cs$p.value))
} else {
  cat(sprintf("Correlação NS (rho=%.3f; p=%.4f)\n",
              rho_cs$estimate, if(is.na(rho_cs$p.value)) Inf else rho_cs$p.value))
}

# ============================================================================
cat("\n", strrep("=",60), "\n", sep="")
cat("P10 — MÉTODO EVITADAS POR CS (script 20)\n")
cat(strrep("=",60), "\n\n")

ev_cs <- read_csv(file.path(DIR_PROC, "internacoes_evitadas_cs.csv"),
                  show_col_types=FALSE)
cat(sprintf("Colunas: %s\n", paste(names(ev_cs), collapse=", ")))
cat(sprintf("N CS: %d\n\n", nrow(ev_cs)))
print(head(ev_cs, 5))

cat("\nVerificando se a distribuição é proporcional ao n_icsap histórico:\n")
cat("Variância do prop_cs (se proporcional, deve ser não-uniforme):\n")
if ("prop_cs" %in% names(ev_cs)) {
  cat(sprintf("  min prop_cs=%.4f | max=%.4f | CV=%.1f%%\n",
              min(ev_cs$prop_cs), max(ev_cs$prop_cs),
              100*sd(ev_cs$prop_cs)/mean(ev_cs$prop_cs)))
}

# Verificar método no script 20
lines20 <- readLines("R/20_internacoes_evitadas.R")
idx_ev  <- grep("prop_cs|evitadas_cs|evit_cs|intern.*evit.*cs|cs.*evit", lines20, ignore.case=TRUE)
cat(sprintf("\nLinhas relevantes do script 20 (%d encontradas):\n", length(idx_ev)))
if (length(idx_ev) > 0) {
  cat(paste(sprintf("[%d] %s", idx_ev, lines20[idx_ev]), collapse="\n"), "\n")
}

# ============================================================================
cat("\n", strrep("=",60), "\n", sep="")
cat("PICO ABR/2024 — COMPOSIÇÃO POR GRUPO CLÍNICO\n")
cat(strrep("=",60), "\n\n")

icsap <- read_csv(file.path(DIR_PROC, "icsap_bh.csv"),
                  col_types=cols(ano_cmpt=col_integer(), mes_cmpt=col_integer()),
                  show_col_types=FALSE)

# Média mensal por grupo em 2023 vs abr/2024
n_2023 <- icsap |> filter(ano_cmpt==2023) |>
  group_by(grupo) |> summarise(n_2023=n()/12, .groups="drop")  # média mensal

n_abr24 <- icsap |> filter(ano_cmpt==2024, mes_cmpt==4) |>
  group_by(grupo) |> summarise(n_abr24=n(), .groups="drop")

n_mar24 <- icsap |> filter(ano_cmpt==2024, mes_cmpt==3) |>
  group_by(grupo) |> summarise(n_mar24=n(), .groups="drop")

comp <- n_2023 |>
  full_join(n_abr24, by="grupo") |>
  full_join(n_mar24, by="grupo") |>
  mutate(
    delta_abr = round((n_abr24/n_2023 - 1)*100, 1),
    delta_mar = round((n_mar24/n_2023 - 1)*100, 1)
  ) |>
  arrange(desc(delta_abr))

cat("Variação vs média mensal 2023 (top 10 por crescimento abr/2024):\n")
print(as.data.frame(mutate(comp, n_2023=round(n_2023,1))[1:10,]), row.names=FALSE)

cat("\nTotal geral:\n")
icsap |>
  filter(ano_cmpt==2024 | ano_cmpt==2023) |>
  group_by(periodo=if_else(ano_cmpt==2023,"2023 (media/mes)", paste0(ano_cmpt,"/",sprintf("%02d",mes_cmpt)))) |>
  summarise(n=n(), .groups="drop") |>
  mutate(n=if_else(grepl("2023",periodo), n/12, as.double(n))) |>
  filter(grepl("2023|2024-03|2024-04", periodo)) |>
  arrange(periodo) |>
  print()

cat("\n=== Script 29b concluído ===\n")
