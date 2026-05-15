# =============================================================================
# 13_incorpora_ivs.R
#
# Incorpora o Índice de Vulnerabilidade em Saúde (IVS-BH) ao nível de
# Centro de Saúde (CS), cruzando setores censitários IVS com polígonos CS.
#
# Entrada:
#   data/ref/ivs_bh.csv              — IVS por setor censitário (UTM EPSG:31983)
#   data/ref/areas_abrangencia_cs.geojson — polígonos CS (WGS 84 / EPSG:4326)
#
# Saída:
#   data/ref/ivs_por_cs.csv          — IVS agregado por CS com cod_smsa como chave
#
# Método:
#   st_join com largest=TRUE — cada setor IVS é alocado ao CS com maior sobreposição.
#   Agregação por CS ponderada por POPULACAO_TOTAL do setor.
#
# Categorias IVS_2012: Baixo | Médio | Elevado | Muito Elevado | Não Avaliado
#   ivs_score: Baixo=1, Médio=2, Elevado=3, Muito Elevado=4 (média ponderada por pop)
#   ivs_predominante: categoria modal ponderada por pop
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(tidyr)
})

DIR_REF  <- "data/ref"

# =============================================================================
# 1. Lê IVS como sf (UTM SIRGAS 2000, EPSG:31983)
# =============================================================================

message("Lendo ivs_bh.csv...")
ivs_raw <- read_csv(
  file.path(DIR_REF, "ivs_bh.csv"),
  show_col_types = FALSE
)

message("Total de setores IVS: ", nrow(ivs_raw))
message("Categorias IVS_2012: ", paste(sort(unique(ivs_raw$IVS_2012)), collapse = ", "))
message("Pop. total IVS: ", format(sum(ivs_raw$POPULACAO_TOTAL, na.rm = TRUE), big.mark = "."))

ivs_sf <- ivs_raw %>%
  filter(IVS_2012 != "Não Avaliado") %>%
  mutate(
    ivs_num = case_when(
      IVS_2012 == "Baixo"         ~ 1L,
      IVS_2012 == "Médio"         ~ 2L,
      IVS_2012 == "Elevado"       ~ 3L,
      IVS_2012 == "Muito Elevado" ~ 4L,
      TRUE                        ~ NA_integer_
    )
  ) %>%
  st_as_sf(wkt = "GEOMETRIA", crs = 31983) %>%
  st_transform(crs = 4326)

message("Setores após remover 'Não Avaliado': ", nrow(ivs_sf))

# Desabilita S2 para tolerar geometrias degeneradas (vértices duplicados no IVS)
sf_use_s2(FALSE)

# =============================================================================
# 2. Lê polígonos CS (já em EPSG:4326)
# =============================================================================

message("Lendo áreas de abrangência CS...")
cs_sf <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE)
message("CS carregados: ", nrow(cs_sf))

# =============================================================================
# 3. Cruzamento espacial — centroide do setor IVS → CS
# =============================================================================

message("Calculando centroides dos setores IVS...")
# Centroide de cada setor IVS (evita necessidade de lwgeom para st_area)
ivs_centroides <- st_centroid(ivs_sf)

message("Realizando st_join (centroide dentro de CS)...")
ivs_cs <- st_join(ivs_centroides, cs_sf["cod_smsa"], join = st_within)

n_sem_cs <- sum(is.na(ivs_cs$cod_smsa))
message("Setores sem CS alocado: ", n_sem_cs, " de ", nrow(ivs_cs))

# =============================================================================
# 4. Agrega por CS — % população por categoria e score IVS
# =============================================================================

message("Agregando por CS...")

ivs_por_cs <- ivs_cs %>%
  st_drop_geometry() %>%
  filter(!is.na(cod_smsa)) %>%
  group_by(cod_smsa) %>%
  summarise(
    n_setores         = n(),
    pop_total_ivs     = sum(POPULACAO_TOTAL, na.rm = TRUE),
    pop_baixo         = sum(POPULACAO_TOTAL[IVS_2012 == "Baixo"],         na.rm = TRUE),
    pop_medio         = sum(POPULACAO_TOTAL[IVS_2012 == "Médio"],         na.rm = TRUE),
    pop_elevado       = sum(POPULACAO_TOTAL[IVS_2012 == "Elevado"],       na.rm = TRUE),
    pop_muito_elevado = sum(POPULACAO_TOTAL[IVS_2012 == "Muito Elevado"], na.rm = TRUE),
    ivs_score         = if (sum(POPULACAO_TOTAL, na.rm = TRUE) > 0)
                          weighted.mean(ivs_num, w = POPULACAO_TOTAL, na.rm = TRUE)
                        else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    pct_baixo         = round(pop_baixo         / pop_total_ivs * 100, 2),
    pct_medio         = round(pop_medio         / pop_total_ivs * 100, 2),
    pct_elevado       = round(pop_elevado       / pop_total_ivs * 100, 2),
    pct_muito_elevado = round(pop_muito_elevado / pop_total_ivs * 100, 2),
    ivs_score         = round(ivs_score, 3),
    # Modal category by population share
    ivs_predominante  = case_when(
      pct_baixo >= pct_medio & pct_baixo >= pct_elevado & pct_baixo >= pct_muito_elevado ~ "Baixo",
      pct_medio >= pct_elevado & pct_medio >= pct_muito_elevado                          ~ "Médio",
      pct_elevado >= pct_muito_elevado                                                   ~ "Elevado",
      TRUE                                                                               ~ "Muito Elevado"
    )
  )

# =============================================================================
# 5. Junta nome_cs e regional para facilitar leitura
# =============================================================================

cs_meta <- cs_sf %>%
  st_drop_geometry() %>%
  select(cod_smsa, nome_cs, regional)

ivs_por_cs <- ivs_por_cs %>%
  left_join(cs_meta, by = "cod_smsa") %>%
  select(cod_smsa, nome_cs, regional, n_setores, pop_total_ivs,
         pct_baixo, pct_medio, pct_elevado, pct_muito_elevado,
         ivs_predominante, ivs_score)

# =============================================================================
# 6. Diagnósticos
# =============================================================================

message("\n=== RESULTADO ===")
message("CS com IVS calculado: ", nrow(ivs_por_cs), " de 153")
message("CS sem IVS: ", 153 - nrow(ivs_por_cs))

message("\nDistribuição ivs_predominante:")
print(table(ivs_por_cs$ivs_predominante))

message("\nivs_score (summary):")
print(summary(ivs_por_cs$ivs_score))

message("\nTop 5 CS mais vulneráveis (maior ivs_score):")
ivs_por_cs %>%
  arrange(desc(ivs_score)) %>%
  select(nome_cs, regional, ivs_predominante, ivs_score) %>%
  head(5) %>%
  print()

message("\nTop 5 CS menos vulneráveis:")
ivs_por_cs %>%
  arrange(ivs_score) %>%
  select(nome_cs, regional, ivs_predominante, ivs_score) %>%
  head(5) %>%
  print()

# =============================================================================
# 7. Exporta
# =============================================================================

write_csv(ivs_por_cs, file.path(DIR_REF, "ivs_por_cs.csv"))

message("\n======================================")
message("IVS POR CS CONCLUÍDO")
message("Saída: ", file.path(DIR_REF, "ivs_por_cs.csv"))
message("======================================")
