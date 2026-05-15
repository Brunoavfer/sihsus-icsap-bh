# =============================================================================
# 14_alocacao_proporcional.R
#
# Alocação proporcional de internações de CEPs limítrofes entre CS
#
# Motivação (gap metodológico vs BMJ Open doi:10.1136/bmjopen-2024-086694):
#   O pipeline atual (script 03) usa st_within ponto-no-polígono: um CEP
#   geocodificado como ponto pertence a exatamente um CS. CEPs próximos de
#   divisas recebem alocação "tudo-ou-nada" ao invés de proporcional.
#
# Método:
#   1. Buffer de BUFFER_M metros ao redor do centroide de cada CEP (UTM 31983)
#      — proxy da incerteza espacial do centroide dentro da área postal
#   2. st_intersection buffer × polígonos CS → área de sobreposição por CS
#   3. CEP "interno": buffer em 1 CS → peso = 1,0 (igual ao original)
#   4. CEP "limítrofe": buffer em 2+ CS → peso_cs = área_cs / área_total_buffer
#   5. Para cada ICSAP geocodificada: a internação contribui fracionalmente
#      a cada CS da tabela de pesos (sum(peso) = 1 por CEP)
#
# Buffer padrão: 100 m (conservador; com 200 m são 59,6% limítrofes em BH)
#
# Saídas:
#   data/processed/cep_pesos_cs.csv          — tabela CEP × CS × peso
#   data/processed/n_icsap_cs_mes_prop.csv   — n_icsap por CS × mês (ponderado)
#   data/processed/alocacao_impacto.txt      — relatório de impacto
# =============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

sf_use_s2(FALSE)

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

BUFFER_M <- 100   # raio em metros (ajustável)

# =============================================================================
# 1. Carrega CEPs geocodificados
# =============================================================================

message("=== 1. Carregando cache_cep.csv ===")
cache <- read_csv(file.path(DIR_REF, "cache_cep.csv"), show_col_types = FALSE) %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  mutate(cep_str = str_pad(as.character(as.integer(cep)), 8, pad = "0"))

message("  CEPs com coordenadas: ", nrow(cache))

ceps_sf <- cache %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 31983) %>%
  select(cep_str)

# =============================================================================
# 2. Carrega polígonos CS em UTM
# =============================================================================

message("\n=== 2. Carregando polígonos CS ===")
cs_sf <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE) %>%
  st_transform(crs = 31983) %>%
  select(nome_cs)

# =============================================================================
# 3. Buffer + st_intersection → pesos por CS
# =============================================================================

message("\n=== 3. Buffer ", BUFFER_M, "m + interseção ===")

ceps_buf <- st_buffer(ceps_sf, dist = BUFFER_M)

message("  Executando st_intersection (pode demorar 1-2 min)...")
suppressWarnings({
  intersec <- st_intersection(ceps_buf, cs_sf) %>%
    mutate(area_m2 = as.numeric(st_area(geometry))) %>%
    st_drop_geometry() %>%
    filter(area_m2 > 0)
})

message("  Registros CEP × CS: ", nrow(intersec))

# Pesos proporcionais: area_cs / area_total_buffer por CEP
pesos <- intersec %>%
  group_by(cep_str) %>%
  mutate(
    n_cs_overlap = n(),
    peso = area_m2 / sum(area_m2)
  ) %>%
  ungroup()

n_internos    <- sum(pesos %>% distinct(cep_str, n_cs_overlap) %>% pull(n_cs_overlap) == 1)
n_limitrofes  <- sum(pesos %>% distinct(cep_str, n_cs_overlap) %>% pull(n_cs_overlap) > 1)

message("\n=== 4. Diagnóstico de limítrofes ===")
message("  Internos (1 CS):       ", n_internos,   " CEPs (",
        round(n_internos   / (n_internos + n_limitrofes) * 100, 1), "%)")
message("  Limítrofes (2+ CS):    ", n_limitrofes, " CEPs (",
        round(n_limitrofes / (n_internos + n_limitrofes) * 100, 1), "%)")
message("  Distribuição n_cs:")
print(table(pesos %>% distinct(cep_str, n_cs_overlap) %>% pull(n_cs_overlap)))

# =============================================================================
# 4. Carrega ICSAP e aplica pesos
# =============================================================================

message("\n=== 5. Carregando ICSAP e aplicando pesos ===")
icsap <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"), show_col_types = FALSE) %>%
  filter(ano_cmpt %in% 2023:2025) %>%
  mutate(
    cep_str     = str_pad(as.character(as.integer(cep)), 8, pad = "0"),
    competencia = paste0(str_sub(as.character(ano_cmpt), 3, 4),
                         str_pad(as.integer(mes_cmpt), 2, pad = "0"))
  )

icsap_geo <- icsap %>% filter(!is.na(nome_cs))
message("  ICSAP geocodificadas 2023-2025: ", nrow(icsap_geo))

# Junta cada internação com TODOS os pares (CEP → CS → peso) correspondentes
# Internações em CEPs limítrofes resultarão em múltiplas linhas (uma por CS)
icsap_prop <- icsap_geo %>%
  select(cep_str, competencia) %>%
  left_join(
    pesos %>% select(cep_str, nome_cs_destino = nome_cs, peso),
    by = "cep_str",
    relationship = "many-to-many"
  )

# CEPs sem pesos (raros: geocodificados mas fora dos buffers) → peso = 1 no CS original
sem_peso <- icsap_geo %>%
  filter(!cep_str %in% pesos$cep_str) %>%
  select(nome_cs_destino = nome_cs, competencia) %>%
  mutate(peso = 1.0)

message("  Internações em CEPs sem buffer (fora dos CS): ", nrow(sem_peso))

# Combina e agrega
icsap_expandido <- bind_rows(
  icsap_prop %>%
    filter(!is.na(nome_cs_destino)) %>%
    select(nome_cs_destino, competencia, peso),
  sem_peso
)

n_icsap_prop <- icsap_expandido %>%
  group_by(nome_cs = nome_cs_destino, competencia) %>%
  summarise(n_icsap_prop = sum(peso, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ano_cmpt = as.integer(paste0("20", str_sub(competencia, 1, 2))),
    mes_cmpt = as.integer(str_sub(competencia, 3, 4))
  )

# =============================================================================
# 5. Compara original vs proporcional
# =============================================================================

message("\n=== 6. Comparando original vs proporcional ===")

n_icsap_orig <- icsap_geo %>%
  group_by(nome_cs, competencia) %>%
  summarise(n_icsap_orig = n(), .groups = "drop")

comparacao <- n_icsap_orig %>%
  full_join(n_icsap_prop, by = c("nome_cs", "competencia")) %>%
  replace_na(list(n_icsap_orig = 0, n_icsap_prop = 0)) %>%
  mutate(
    diferenca      = round(n_icsap_prop - n_icsap_orig, 3),
    pct_diferenca  = round((n_icsap_prop - n_icsap_orig) / pmax(n_icsap_orig, 1) * 100, 2)
  )

total_orig <- sum(n_icsap_orig$n_icsap_orig)
total_prop <- sum(n_icsap_prop$n_icsap_prop)
cs_mes_alterados <- sum(abs(comparacao$diferenca) > 0.01, na.rm = TRUE)
total_redistrib  <- sum(abs(comparacao$diferenca), na.rm = TRUE)

message("  Total internações orig:  ", round(total_orig, 1))
message("  Total internações prop:  ", round(total_prop, 1))
message("  CS × mês alterados:      ", cs_mes_alterados, " de ", nrow(comparacao),
        " (", round(cs_mes_alterados / nrow(comparacao) * 100, 1), "%)")
message("  Redistribuição absoluta: ", round(total_redistrib / 2, 1),
        " internações (", round(total_redistrib / 2 / total_orig * 100, 2), "%)")
message("  Diferença máxima (CS×mês): ", round(max(abs(comparacao$diferenca), na.rm=TRUE), 1))

message("\n  CS mais afetados (variação total acumulada):")
comparacao %>%
  group_by(nome_cs) %>%
  summarise(variacao = round(sum(abs(diferenca), na.rm = TRUE), 1), .groups = "drop") %>%
  arrange(desc(variacao)) %>%
  head(6) %>%
  print()

# =============================================================================
# 6. Exporta
# =============================================================================

write_csv(
  pesos %>% select(cep_str, nome_cs, peso, n_cs_overlap),
  file.path(DIR_PROC, "cep_pesos_cs.csv")
)
write_csv(n_icsap_prop, file.path(DIR_PROC, "n_icsap_cs_mes_prop.csv"))

relatorio <- c(
  "=== RELATÓRIO — ALOCAÇÃO PROPORCIONAL DE CEPs LIMÍTROFES ===",
  paste0("Data: ", Sys.Date()),
  paste0("Buffer: ", BUFFER_M, " m"),
  "",
  paste0("CEPs com coordenadas: ", nrow(cache)),
  paste0("CEPs internos (1 CS):   ", n_internos,
         " (", round(n_internos/(n_internos+n_limitrofes)*100,1), "%)"),
  paste0("CEPs limítrofes (2+ CS): ", n_limitrofes,
         " (", round(n_limitrofes/(n_internos+n_limitrofes)*100,1), "%)"),
  "",
  paste0("ICSAP geocodificadas 2023-2025: ", nrow(icsap_geo)),
  paste0("CS × mês com alocação alterada: ", cs_mes_alterados, " de ", nrow(comparacao)),
  paste0("Internações redistribuídas:     ", round(total_redistrib/2,1),
         " (", round(total_redistrib/2/total_orig*100,2), "% do total geocodificado)"),
  "",
  paste0("Conclusão: com buffer de ", BUFFER_M, "m, ",
         round(n_limitrofes/(n_internos+n_limitrofes)*100,1),
         "% dos CEPs são limítrofes."),
  paste0("Redistribuição de ", round(total_redistrib/2/total_orig*100,2),
         "% das internações geocodificadas."),
  "Avalie se o impacto justifica uso de n_icsap_cs_mes_prop.csv como denominador."
)
writeLines(relatorio, file.path(DIR_PROC, "alocacao_impacto.txt"))

message("\n======================================")
message("ALOCAÇÃO PROPORCIONAL CONCLUÍDA")
message("  ", file.path(DIR_PROC, "cep_pesos_cs.csv"))
message("  ", file.path(DIR_PROC, "n_icsap_cs_mes_prop.csv"))
message("  ", file.path(DIR_PROC, "alocacao_impacto.txt"))
message("======================================")
