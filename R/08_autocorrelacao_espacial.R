# =============================================================================
# 08_autocorrelacao_espacial.R
#
# Análise de autocorrelação espacial das taxas ICSAP por Centro de Saúde
#
# Métricas:
#   - Moran's I global: detecta padrão espacial global (clustering ou dispersão)
#   - Moran's I local (LISA): classifica cada CS em High-High, Low-Low,
#     High-Low, Low-High ou Not Significant
#
# Justificativa:
#   Em estudos ecológicos com unidades geográficas adjacentes, a autocorrelação
#   espacial viola o pressuposto de independência dos resíduos do GLM.
#   Moran's I > 0 com p < 0,05 indica clustering espacial e justifica o uso
#   de GEE com estrutura espacial ou de modelo espacial autorregressivo (SAR/CAR)
#   em vez do GLM-Gama padrão.
#
# Referências:
#   Anselin L. Local indicators of spatial association — LISA.
#   Geographical Analysis. 1995;27(2):93-115. doi:10.1111/j.1538-4632.1995.tb00338.x
#
#   Moran PAP. Notes on continuous stochastic phenomena.
#   Biometrika. 1950;37(1/2):17-23. doi:10.2307/2332142
#
# Saídas:
#   data/processed/moran_resultados.csv — classificação LISA por CS
#   docs/mapa_lisa.png                  — mapa LISA com ggplot2
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(sf)
  library(ggplot2)
})

if (!requireNamespace("spdep", quietly = TRUE)) {
  install.packages("spdep", repos = "https://cloud.r-project.org")
}
library(spdep)

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"
DIR_DOCS <- "docs"

SEMENTE <- 42  # semente para reprodutibilidade do teste de permutação
set.seed(SEMENTE)

# =============================================================================
# 1. Carrega polígonos e taxas padronizadas
# =============================================================================

geo_path   <- file.path(DIR_REF, "areas_abrangencia_cs.geojson")
taxas_path <- file.path(DIR_PROC, "taxas_padronizadas.csv")

if (!file.exists(geo_path)) {
  stop("GeoJSON não encontrado: ", geo_path)
}
if (!file.exists(taxas_path)) {
  stop("taxas_padronizadas.csv não encontrado: ", taxas_path,
       "\nExecute primeiro R/07_padronizacao_taxa.R")
}

poligonos <- st_read(geo_path, quiet = TRUE)
taxas     <- read_csv(taxas_path, show_col_types = FALSE)

message("Polígonos carregados: ", nrow(poligonos), " CS")
message("Taxas disponíveis: ", nrow(taxas), " registros (CS × ano)")

# =============================================================================
# 2. Taxa média por CS (média dos anos disponíveis)
# =============================================================================

taxa_media <- taxas %>%
  group_by(nome_cs) %>%
  summarise(
    taxa_padronizada_media = mean(taxa_padronizada, na.rm = TRUE),
    taxa_bruta_media       = mean(taxa_bruta,       na.rm = TRUE),
    n_anos                 = n(),
    regional               = first(regional),
    .groups = "drop"
  ) %>%
  filter(!is.na(taxa_padronizada_media))

message("CS com taxa média disponível: ", nrow(taxa_media))

# =============================================================================
# 3. Junta polígonos + taxas
# =============================================================================

sf_dados <- poligonos %>%
  left_join(taxa_media %>% select(-any_of("regional")), by = "nome_cs") %>%
  filter(!is.na(taxa_padronizada_media))

n_cs <- nrow(sf_dados)
message("CS para análise espacial: ", n_cs)

if (n_cs < 10) {
  stop("Poucos CS para análise de autocorrelação (mínimo recomendado: 10). ",
       "Verifique se as taxas foram calculadas corretamente.")
}

# =============================================================================
# 4. Matriz de vizinhança — Queen Contiguity
#    (CS vizinhas = compartilham pelo menos um ponto de fronteira)
# =============================================================================

message("\nConstruindo matriz de vizinhança (queen contiguity)...")

nb  <- poly2nb(sf_dados, queen = TRUE)
lw  <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Diagnóstico da conectividade
componentes    <- n.comp.nb(nb)
n_sem_vizinhos <- sum(card(nb) == 0)

message("  Componentes conectados: ", componentes$nc)
message("  CS sem vizinhos (ilhas): ", n_sem_vizinhos)

if (n_sem_vizinhos > 0) {
  cs_ilhas <- sf_dados$nome_cs[card(nb) == 0]
  message("  CS ilhas: ", paste(cs_ilhas, collapse = "; "))
  message("  (Essas unidades terão LISA = NA — normal para polígonos não contíguos)")
}

# =============================================================================
# 5. Moran's I Global
# =============================================================================

message("\n=== MORAN'S I GLOBAL ===")

taxa_vec <- sf_dados$taxa_padronizada_media

moran_global <- moran.test(
  taxa_vec,
  lw,
  randomisation = TRUE,
  zero.policy   = TRUE
)

I_global <- moran_global$estimate["Moran I statistic"]
p_global <- moran_global$p.value
z_global <- moran_global$statistic

message("Moran's I = ", round(I_global, 4))
message("p-valor   = ", round(p_global, 4))
message("Z-score   = ", round(z_global, 4))

# Interpretação
if (p_global < 0.05 && I_global > 0) {
  interpretacao_global <- paste0(
    "Autocorrelação espacial POSITIVA e significativa (I=", round(I_global, 3),
    ", p=", round(p_global, 4), "). ",
    "CS com alta taxa ICSAP tendem a ser vizinhos de outros CS com alta taxa. ",
    "Isso viola o pressuposto de independência do GLM padrão. ",
    "Recomenda-se verificar modelo espacial autorregressivo (SAR) ou GEE com ",
    "estrutura de correlação espacial."
  )
} else if (p_global < 0.05 && I_global < 0) {
  interpretacao_global <- paste0(
    "Autocorrelação espacial NEGATIVA e significativa (I=", round(I_global, 3),
    ", p=", round(p_global, 4), "). ",
    "CS com alta taxa ICSAP tendem a ser vizinhos de CS com baixa taxa. ",
    "Padrão incomum — verificar qualidade dos dados e da vizinhança."
  )
} else {
  interpretacao_global <- paste0(
    "Moran's I não significativo (I=", round(I_global, 3),
    ", p=", round(p_global, 4), "). ",
    "Não há evidência de autocorrelação espacial global. ",
    "O GLM padrão sem estrutura espacial pode ser adequado. ",
    "Verificar LISA local para padrões em subáreas específicas."
  )
}

message("\nInterpretação: ", interpretacao_global)

# =============================================================================
# 6. Moran's I Local (LISA)
# =============================================================================

message("\n=== MORAN'S I LOCAL (LISA) ===")

moran_local <- localmoran(
  taxa_vec,
  lw,
  zero.policy = TRUE,
  alternative = "two.sided"
)

# Extrai estatísticas
sf_dados <- sf_dados %>%
  mutate(
    lisa_I    = moran_local[, "Ii"],
    lisa_z    = moran_local[, "Z.Ii"],
    lisa_p    = moran_local[, "Pr(z != E(Ii))"],
    taxa_z    = scale(taxa_padronizada_media)[, 1],  # valor padronizado do CS
    lag_taxa  = lag.listw(lw, taxa_padronizada_media, zero.policy = TRUE),
    lag_taxa_z = scale(lag_taxa)[, 1]  # valor padronizado da média das vizinhas
  )

# Classifica cada CS (limiar de significância: p < 0,05)
sf_dados <- sf_dados %>%
  mutate(
    lisa_cluster = case_when(
      lisa_p < 0.05 & taxa_z > 0 & lag_taxa_z > 0 ~ "High-High",
      lisa_p < 0.05 & taxa_z < 0 & lag_taxa_z < 0 ~ "Low-Low",
      lisa_p < 0.05 & taxa_z > 0 & lag_taxa_z < 0 ~ "High-Low",
      lisa_p < 0.05 & taxa_z < 0 & lag_taxa_z > 0 ~ "Low-High",
      TRUE                                          ~ "Not Significant"
    ),
    lisa_cluster = factor(
      lisa_cluster,
      levels = c("High-High", "Low-Low", "High-Low", "Low-High", "Not Significant")
    )
  )

# Resumo LISA
tab_lisa <- sf_dados %>%
  st_drop_geometry() %>%
  count(lisa_cluster) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

message("Distribuição dos clusters LISA:")
print(tab_lisa)

# =============================================================================
# 7. Mapa LISA com ggplot2
# =============================================================================

message("\nGerando mapa LISA...")

cores_lisa <- c(
  "High-High"       = "#d7191c",   # vermelho — alto rodeado de alto
  "Low-Low"         = "#2c7bb6",   # azul     — baixo rodeado de baixo
  "High-Low"        = "#fdae61",   # laranja  — alto rodeado de baixo (outlier)
  "Low-High"        = "#abd9e9",   # azul claro — baixo rodeado de alto
  "Not Significant" = "#f0f0f0"    # cinza claro
)

mapa_lisa <- ggplot(sf_dados) +
  geom_sf(aes(fill = lisa_cluster), color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = cores_lisa,
    name   = "Cluster LISA",
    guide  = guide_legend(
      title.position = "top",
      nrow = 5
    )
  ) +
  labs(
    title    = "Autocorrelação Espacial Local (LISA) — Taxas ICSAP",
    subtitle = paste0(
      "Belo Horizonte · Centros de Saúde · Média ",
      min(taxas$ano, na.rm = TRUE), "–", max(taxas$ano, na.rm = TRUE),
      "\nMoran's I global = ", round(I_global, 3),
      " (p = ", round(p_global, 4), ")"
    ),
    caption  = paste0(
      "Vizinhança: queen contiguity · Limiar de significância: p < 0,05 · ",
      "Taxa padronizada por idade e sexo (por 10.000 hab.)\n",
      "Fonte: SIHSUS/DATASUS · Polígonos: SMSA/PBH · Análise: ICSAP-BH"
    )
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(size = 7, color = "gray50", hjust = 0.5),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 9),
    legend.text     = element_text(size = 8),
    plot.margin     = margin(10, 10, 10, 10)
  )

mapa_path <- file.path(DIR_DOCS, "mapa_lisa.png")
ggsave(mapa_path, mapa_lisa, width = 10, height = 9, dpi = 300, bg = "white")
message("  Mapa salvo: ", mapa_path)

# =============================================================================
# 8. Salva resultados LISA
# =============================================================================

moran_resultados <- sf_dados %>%
  st_drop_geometry() %>%
  select(
    nome_cs,
    regional,
    taxa_padronizada_media,
    taxa_bruta_media,
    n_anos,
    lisa_I,
    lisa_z,
    lisa_p,
    lisa_cluster
  ) %>%
  mutate(
    across(c(taxa_padronizada_media, taxa_bruta_media, lisa_I, lisa_z),
           ~round(.x, 4)),
    lisa_p         = round(lisa_p, 4),
    moran_I_global = round(I_global, 4),
    moran_p_global = round(p_global, 4),
    semente        = SEMENTE,
    data_analise   = format(Sys.Date())
  )

saida_path <- file.path(DIR_PROC, "moran_resultados.csv")
write_csv(moran_resultados, saida_path)

# =============================================================================
# 9. Resumo final
# =============================================================================

message("\n======================================")
message("AUTOCORRELAÇÃO ESPACIAL CONCLUÍDA")
message("")
message("Moran's I global: ", round(I_global, 4),
        " (p=", round(p_global, 4), ")")
message("")
message("Interpretação:")
message(strwrap(interpretacao_global, width = 70, prefix = "  "))
message("")
message("Clusters LISA:")
for (i in seq_len(nrow(tab_lisa))) {
  message("  ", str_pad(as.character(tab_lisa$lisa_cluster[i]), 18),
          " — ", tab_lisa$n[i], " CS (", tab_lisa$pct[i], "%)")
}
message("")
message("Saídas:")
message("  ", saida_path)
message("  ", mapa_path)
message("======================================")
