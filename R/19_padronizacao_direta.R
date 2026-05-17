# =============================================================================
# 19_padronizacao_direta.R
#
# Padronização direta das taxas ICSAP por faixa etária, por CS × ano
#
# Método: padronização direta (Kitagawa 1964)
# População padrão: BH Censo 2022 — distribuição etária observada na cidade
# Referência: Ahmad et al. 2001 (OMS Technical Report)
#
# Limitação: distribuição etária de 2022 aplicada estaticamente a 2022–2025;
#   pode subestimar variação real caso haja envelhecimento diferencial entre CS.
#
# Saídas:
#   data/ref/pop_cs_faixas.csv              — pop por CS × faixa etária (Censo 2022)
#   data/processed/taxas_padronizadas_v2.csv — taxa bruta e padronizada por CS × ano
#   docs/padronizacao_comparacao.png        — scatter taxa bruta × padronizada por ano
#   docs/mapa_diferenca_padronizacao.png    — diferença bruta−padronizada por CS
# =============================================================================

suppressPackageStartupMessages({
  library(censobr)
  library(geobr)
  library(arrow)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(sf)
  library(ggplot2)
})

sf_use_s2(FALSE)

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

FAIXAS_LABELS <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                   "30-39", "40-49", "50-59", "60-69", "70+")

# Colunas censobr — prefixo "demografia_", ambos sexos V01031–V01041
COLS_CENSO <- paste0("demografia_V01", str_pad(31:41, 3, pad = "0"))

# =============================================================================
# 1. Carrega faixas etárias por setor censitário (BH) via censobr
# =============================================================================

message("=== 1. Carregando censobr Pessoas 2022 (BH) ===")
message("  Usando cache em AppData/Local/R/cache/R/censobr/")

tracts_raw <- read_tracts(year = 2022, dataset = "Pessoas", cache = TRUE)

# Filtra BH: code_muni é double no schema Arrow → comparação numérica
tracts_bh <- tracts_raw %>%
  filter(code_muni == 3106200) %>%
  select(code_tract, all_of(COLS_CENSO)) %>%
  collect() %>%
  as.data.frame() %>%          # censobr retorna data.table; força data.frame
  mutate(
    code_tract = as.character(code_tract),
    across(all_of(COLS_CENSO), ~ replace_na(as.numeric(.), 0))
  )

message("  Setores BH: ", nrow(tracts_bh))
message("  Colunas de faixa etária: ", paste(COLS_CENSO, collapse = ", "))
message("  NAs substituídos por 0 (setores desocupados)")

# Renomeia colunas de censo para os labels de faixa etária
names_map <- setNames(COLS_CENSO, FAIXAS_LABELS)
tracts_bh <- tracts_bh %>%
  rename(!!!setNames(COLS_CENSO, FAIXAS_LABELS))

message("  Pop. total BH (censobr): ",
        format(sum(rowSums(tracts_bh[, FAIXAS_LABELS], na.rm = TRUE)), big.mark = "."))

# =============================================================================
# 2. Geometrias dos setores (geobr) e join com polígonos CS
# =============================================================================

message("\n=== 2. Geometrias dos setores censitários (geobr) ===")

tracts_geo <- tryCatch(
  read_census_tract(code_tract = 3106200, year = 2022,
                    simplified = FALSE, showProgress = FALSE),
  error = function(e) {
    message("  2022 indisponível (", conditionMessage(e), "); tentando 2010...")
    read_census_tract(code_tract = 3106200, year = 2010,
                      simplified = FALSE, showProgress = FALSE)
  }
)
tracts_geo <- tracts_geo %>%
  mutate(code_tract = as.character(code_tract))

message("  Setores geobr: ", nrow(tracts_geo), " | CRS: ", st_crs(tracts_geo)$input)

# Polígonos dos 153 CS
cs_poly <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)
cs_poly <- st_transform(cs_poly, st_crs(tracts_geo))

message("  CS carregados: ", nrow(cs_poly))

# =============================================================================
# 3. Associa cada setor a um CS (centroide → polígono)
# =============================================================================

message("\n=== 3. Associa setores aos CS via centroide ===")

# Junta dados de população às geometrias
pop_geo <- tracts_geo %>%
  left_join(tracts_bh, by = "code_tract") %>%
  mutate(across(all_of(FAIXAS_LABELS), ~ replace_na(., 0)))

# Centroide de cada setor
centroids <- st_centroid(pop_geo)

# Spatial join: centroide dentro de qual CS?
join_cs <- st_join(
  centroids %>% select(code_tract, all_of(FAIXAS_LABELS)),
  cs_poly   %>% select(nome_cs),
  join = st_within
)

n_atrib   <- sum(!is.na(join_cs$nome_cs))
n_sem_cs  <- sum(is.na(join_cs$nome_cs))
message("  Setores atribuídos a um CS: ", n_atrib, " / ", nrow(join_cs),
        " (", n_sem_cs, " fora dos polígonos)")

# =============================================================================
# 4. Agrega população por CS × faixa etária
# =============================================================================

message("\n=== 4. Agrega população por CS × faixa etária ===")

pop_cs <- join_cs %>%
  st_drop_geometry() %>%
  filter(!is.na(nome_cs)) %>%
  group_by(nome_cs) %>%
  summarise(across(all_of(FAIXAS_LABELS), ~ sum(., na.rm = TRUE)), .groups = "drop")

message("  CS com população agregada: ", nrow(pop_cs))
message("  Pop. total nos CS: ",
        format(sum(rowSums(pop_cs[, FAIXAS_LABELS], na.rm = TRUE)), big.mark = "."))

# Checa CS sem dados de população
cs_sem_pop <- setdiff(cs_poly$nome_cs, pop_cs$nome_cs)
if (length(cs_sem_pop) > 0) {
  message("  AVISO: CS sem setores mapeados (", length(cs_sem_pop), "):")
  message("    ", paste(cs_sem_pop, collapse = "\n    "))
}

# Formato longo para exportação
pop_cs_long <- pop_cs %>%
  pivot_longer(all_of(FAIXAS_LABELS),
               names_to  = "faixa_etaria",
               values_to = "pop_faixa") %>%
  mutate(faixa_etaria = factor(faixa_etaria, levels = FAIXAS_LABELS))

write_csv(pop_cs_long, file.path(DIR_REF, "pop_cs_faixas.csv"))
message("  Salvo: data/ref/pop_cs_faixas.csv (",
        nrow(pop_cs), " CS × ", length(FAIXAS_LABELS), " faixas)")

# =============================================================================
# 5. Define população padrão de BH (distribuição etária da cidade)
# =============================================================================

message("\n=== 5. Definindo população padrão BH ===")

pop_bh_faixa <- colSums(pop_cs[, FAIXAS_LABELS], na.rm = TRUE)
pop_bh_total <- sum(pop_bh_faixa)

prop_padrao <- tibble(
  faixa_etaria = factor(FAIXAS_LABELS, levels = FAIXAS_LABELS),
  pop_bh       = pop_bh_faixa,
  prop_bh      = pop_bh_faixa / pop_bh_total
)

message("  Pop. total padrão BH: ", format(pop_bh_total, big.mark = "."))
message("\n  Distribuição etária padrão:")
print(prop_padrao %>% mutate(prop_pct = round(prop_bh * 100, 1)))

# =============================================================================
# 6. Carrega ICSAP e mapeia faixas etárias
# =============================================================================

message("\n=== 6. Carregando ICSAP e mapeando faixas etárias ===")

icsap <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                  show_col_types = FALSE)

message("  Total ICSAP: ", nrow(icsap))
message("  Coluna idade: 'idade' (anos, min=", min(icsap$idade, na.rm=TRUE),
        ", max=", max(icsap$idade, na.rm=TRUE), ", NAs=", sum(is.na(icsap$idade)), ")")

icsap <- icsap %>%
  mutate(
    faixa_etaria = factor(case_when(
      idade <  5  ~ "0-4",
      idade < 10  ~ "5-9",
      idade < 15  ~ "10-14",
      idade < 20  ~ "15-19",
      idade < 25  ~ "20-24",
      idade < 30  ~ "25-29",
      idade < 40  ~ "30-39",
      idade < 50  ~ "40-49",
      idade < 60  ~ "50-59",
      idade < 70  ~ "60-69",
      TRUE        ~ "70+"
    ), levels = FAIXAS_LABELS)
  )

message("\n  Distribuição por faixa etária (ICSAP total):")
print(icsap %>%
        count(faixa_etaria) %>%
        mutate(pct = round(n / sum(n) * 100, 1)))

# =============================================================================
# 7. Padronização direta por CS × ano
# =============================================================================

message("\n=== 7. Padronização direta por CS × ano ===")

anos <- sort(unique(icsap$ano_cmpt))
message("  Anos na série: ", paste(anos, collapse = ", "))

calcular_padronizacao <- function(ano) {
  message("  Processando ", ano, "...")

  icsap_ano <- icsap %>%
    filter(ano_cmpt == ano, !is.na(nome_cs), !is.na(faixa_etaria))

  # n_icsap por CS × faixa
  n_faixa <- icsap_ano %>%
    count(nome_cs, faixa_etaria, name = "n_icsap", .drop = FALSE)

  # n_icsap total por CS (numerador taxa bruta)
  n_total_cs <- icsap_ano %>%
    count(nome_cs, name = "n_icsap_total")

  # Grade completa CS × faixa para garantir zeros explícitos
  grade <- expand.grid(
    nome_cs      = pop_cs$nome_cs,
    faixa_etaria = factor(FAIXAS_LABELS, levels = FAIXAS_LABELS),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble()

  resultado <- grade %>%
    left_join(pop_cs_long %>% mutate(faixa_etaria = as.character(faixa_etaria)),
              by = c("nome_cs", "faixa_etaria")) %>%
    left_join(n_faixa %>% mutate(faixa_etaria = as.character(faixa_etaria)),
              by = c("nome_cs", "faixa_etaria")) %>%
    left_join(prop_padrao %>% mutate(faixa_etaria = as.character(faixa_etaria)),
              by = "faixa_etaria") %>%
    mutate(
      pop_faixa = replace_na(pop_faixa, 0),
      n_icsap   = replace_na(n_icsap, 0L),
      # Taxa específica por faixa (por 10.000 hab/faixa)
      taxa_esp  = if_else(pop_faixa > 0, n_icsap / pop_faixa * 10000, 0),
      # Contribuição ponderada pela proporção padrão BH
      contrib   = taxa_esp * prop_bh
    )

  # Taxa padronizada por CS (soma das contribuições ponderadas)
  taxa_pad_cs <- resultado %>%
    group_by(nome_cs) %>%
    summarise(
      taxa_padronizada = sum(contrib, na.rm = TRUE),
      pop_cs_total     = sum(pop_faixa, na.rm = TRUE),
      .groups = "drop"
    )

  # Taxa bruta: n_icsap_total / pop_cs_total × 10.000
  taxa_pad_cs %>%
    left_join(n_total_cs, by = "nome_cs") %>%
    mutate(
      ano           = ano,
      n_icsap_total = replace_na(n_icsap_total, 0L),
      taxa_bruta    = if_else(pop_cs_total > 0,
                              n_icsap_total / pop_cs_total * 10000, 0)
    ) %>%
    select(ano, nome_cs, pop_cs_total, n_icsap_total, taxa_bruta, taxa_padronizada)
}

taxas_df <- bind_rows(lapply(anos, calcular_padronizacao))

message("\n  Resultado: ", nrow(taxas_df), " obs | ",
        n_distinct(taxas_df$nome_cs), " CS | ", length(anos), " anos")

# =============================================================================
# 8. Comparação bruta × padronizada
# =============================================================================

message("\n=== 8. Comparação taxa bruta × padronizada ===")

# Filtra CS com população > 0
taxas_validas <- taxas_df %>% filter(pop_cs_total > 0)

# Correlação de Spearman
cor_sp <- cor.test(taxas_validas$taxa_bruta, taxas_validas$taxa_padronizada,
                   method = "spearman", use = "complete.obs")
message(sprintf("  Spearman rho = %.4f  (p = %.2e)", cor_sp$estimate, cor_sp$p.value))

# Ranking por ano (para identificar mudanças)
ranks_df <- taxas_validas %>%
  group_by(ano) %>%
  mutate(
    rank_bruta = rank(-taxa_bruta,        ties.method = "min"),
    rank_pad   = rank(-taxa_padronizada,  ties.method = "min"),
    delta_rank = abs(rank_bruta - rank_pad)
  ) %>%
  ungroup()

pct_muda_10 <- mean(ranks_df$delta_rank > 10, na.rm = TRUE) * 100
message(sprintf("  CS com Δranking > 10 posições: %.1f%%", pct_muda_10))

# Top 10 CS com maior mudança média de ranking
top_mudancas <- ranks_df %>%
  group_by(nome_cs) %>%
  summarise(delta_medio = mean(delta_rank, na.rm = TRUE),
            taxa_bruta_media  = mean(taxa_bruta,  na.rm = TRUE),
            taxa_pad_media    = mean(taxa_padronizada, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(delta_medio)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

message("\n  Top 10 CS com maior mudança de ranking (média 2022–2025):")
print(top_mudancas %>% head(10))

# Estatísticas por ano
message("\n  Resumo por ano (correlação Spearman, média bruta vs pad):")
ranks_df %>%
  group_by(ano) %>%
  summarise(
    rho       = round(cor(taxa_bruta, taxa_padronizada, method = "spearman",
                          use = "complete.obs"), 4),
    media_bruta = round(mean(taxa_bruta,       na.rm = TRUE), 2),
    media_pad   = round(mean(taxa_padronizada, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

# =============================================================================
# 9. Visualizações
# =============================================================================

message("\n=== 9. Gerando visualizações ===")

# 9a. Scatter taxa bruta × padronizada
p_scatter <- ggplot(taxas_validas %>% filter(taxa_bruta > 0),
                    aes(x = taxa_bruta, y = taxa_padronizada)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "grey40", linewidth = 0.6) +
  geom_point(aes(color = factor(ano)), alpha = 0.55, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = "#c0392b", linewidth = 0.7) +
  scale_color_brewer(palette = "Set2", name = "Ano") +
  facet_wrap(~ ano, ncol = 2) +
  labs(
    title    = "Taxa ICSAP Bruta × Padronizada por Idade — por CS (Belo Horizonte)",
    subtitle = sprintf(
      "Padronização direta — população padrão: BH Censo 2022 | Spearman rho = %.3f",
      cor_sp$estimate
    ),
    x       = "Taxa bruta (por 10.000 hab)",
    y       = "Taxa padronizada (por 10.000 hab)",
    caption = "Linha tracejada = identidade (bruta = padronizada) | Linha sólida = ajuste linear"
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey92"),
    legend.position  = "none",
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 8.5, color = "gray30"),
    plot.caption  = element_text(size = 7.5, color = "gray50")
  )

ggsave(file.path(DIR_DOCS, "padronizacao_comparacao.png"),
       p_scatter, width = 10, height = 8, dpi = 150, bg = "white")
message("  Salvo: docs/padronizacao_comparacao.png")

# 9b. Mapa diferença bruta − padronizada (média 2022–2025)
diff_media_cs <- taxas_validas %>%
  group_by(nome_cs) %>%
  summarise(
    diff_media   = mean(taxa_bruta - taxa_padronizada, na.rm = TRUE),
    taxa_bruta_m = mean(taxa_bruta, na.rm = TRUE),
    .groups = "drop"
  )

cs_mapa <- cs_poly %>%
  left_join(diff_media_cs, by = "nome_cs")

lim_abs <- max(abs(cs_mapa$diff_media), na.rm = TRUE)

p_mapa <- ggplot(cs_mapa) +
  geom_sf(aes(fill = diff_media), color = "white", linewidth = 0.1) +
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#d73027",
    midpoint = 0,
    limits   = c(-lim_abs, lim_abs),
    name     = "Taxa bruta −\npadronizada\n(por 10.000)",
    na.value = "grey80"
  ) +
  labs(
    title    = "Diferença entre Taxa ICSAP Bruta e Padronizada por Idade",
    subtitle = paste0(
      "BH 2022–2025 | Média anual por CS\n",
      "Vermelho: CS com pop. mais jovem que o padrão BH (bruta subestima)\n",
      "Azul: CS com pop. mais idosa que o padrão BH (bruta superestima)"
    ),
    caption  = "Fonte: SIHSUS/DATASUS + Censo IBGE 2022 | Padronização direta (Kitagawa 1964)"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 11),
    plot.subtitle = element_text(size = 7.5, hjust = 0.5, color = "gray35", lineheight = 1.3),
    plot.caption  = element_text(size = 7, color = "gray50"),
    legend.position = "right"
  )

ggsave(file.path(DIR_DOCS, "mapa_diferenca_padronizacao.png"),
       p_mapa, width = 10, height = 9, dpi = 150, bg = "white")
message("  Salvo: docs/mapa_diferenca_padronizacao.png")

# =============================================================================
# 10. Exporta resultados
# =============================================================================

write_csv(
  taxas_df %>%
    arrange(ano, nome_cs) %>%
    select(ano, nome_cs, pop_cs_total, n_icsap_total, taxa_bruta, taxa_padronizada),
  file.path(DIR_PROC, "taxas_padronizadas_v2.csv")
)

message("\n======================================")
message("PADRONIZAÇÃO DIRETA CONCLUÍDA")
message(sprintf("  CS com dados: %d | Anos: %s",
                n_distinct(taxas_df$nome_cs), paste(anos, collapse = "/")))
message(sprintf("  Spearman rho bruta×pad: %.4f (p = %.2e)",
                cor_sp$estimate, cor_sp$p.value))
message(sprintf("  CS com Δranking > 10 posições: %.1f%%", pct_muda_10))
message("Saídas:")
message("  data/ref/pop_cs_faixas.csv")
message("  data/processed/taxas_padronizadas_v2.csv")
message("  docs/padronizacao_comparacao.png")
message("  docs/mapa_diferenca_padronizacao.png")
message("======================================")
