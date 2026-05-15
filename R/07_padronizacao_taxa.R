# =============================================================================
# 07_padronizacao_taxa.R
#
# Padronização direta da taxa ICSAP por idade e sexo
# Unidade de análise: Centro de Saúde (CS) × ano
#
# Método: Padronização direta (Ahmad et al., 2001)
#   taxa_pad = Σ_j [ (n_ij / pop_ij) × pop_padrao_j ] / Σ_j pop_padrao_j × 10.000
#   onde j = faixa etária × sexo
#
# População-padrão: distribuição etária e de sexo de Belo Horizonte
#   conforme Censo IBGE 2022 (valores aproximados, ajustar com dados definitivos)
#
# Saída: data/processed/taxas_padronizadas.csv
#   Colunas: nome_cs, regional, ano, n_icsap, populacao, taxa_bruta, taxa_padronizada
#
# Referência metodológica:
#   Ahmad OB, Boschi-Pinto C, Lopez AD, Murray CJL, Lozano R, Inoue M.
#   Age standardization of rates: a new WHO standard.
#   GPE Discussion Paper Series No. 31. Geneva: WHO; 2001.
#   Disponível em: https://www.who.int/healthinfo/paper31.pdf
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# =============================================================================
# 1. Dados ICSAP com regional
# =============================================================================

dados <- read_csv(
  file.path(DIR_PROC, "icsap_bh_regional.csv"),
  show_col_types = FALSE
)

dados <- dados %>%
  filter(!is.na(regional), !is.na(nome_cs)) %>%
  mutate(
    # Faixas etárias (RIPSA / Portaria 221/2008)
    faixa_etaria = cut(
      idade,
      breaks = c(-Inf, 4, 14, 29, 44, 59, 74, Inf),
      labels = c("<5", "5-14", "15-29", "30-44", "45-59", "60-74", "75+"),
      right  = TRUE
    ),
    sexo_cat = case_when(
      sexo == 1 ~ "M",
      sexo == 3 ~ "F",
      TRUE      ~ NA_character_
    )
  ) %>%
  filter(!is.na(faixa_etaria), !is.na(sexo_cat))

# =============================================================================
# 2. População-padrão: BH — Censo IBGE 2022
#
#    Distribuição etária e de sexo da população de BH.
#    Fonte: IBGE Censo 2022 — Resultados Preliminares por Município.
#    Valores: https://www.ibge.gov.br/cidades-e-estados/mg/belo-horizonte.html
#
#    NOTA: Os valores abaixo são estimativas baseadas nos resultados
#    preliminares do Censo 2022. Substitua pelos dados definitivos quando
#    publicados. A população total de BH no Censo 2022 é de 2.315.560 hab.
# =============================================================================

pop_padrao_bh <- tribble(
  ~faixa_etaria, ~sexo_cat, ~pop_padrao,
  # Masculino
  "<5",     "M",   72000,
  "5-14",   "M",  155000,
  "15-29",  "M",  260000,
  "30-44",  "M",  270000,
  "45-59",  "M",  220000,
  "60-74",  "M",  140000,
  "75+",    "M",   55000,
  # Feminino
  "<5",     "F",   69000,
  "5-14",   "F",  149000,
  "15-29",  "F",  260000,
  "30-44",  "F",  285000,
  "45-59",  "F",  250000,
  "60-74",  "F",  175000,
  "75+",    "F",   85000
)

pop_padrao_total <- sum(pop_padrao_bh$pop_padrao)

message("População-padrão BH (Censo 2022): ",
        format(pop_padrao_total, big.mark = "."), " habitantes")
message("ATENÇÃO: Use os dados definitivos do Censo 2022 quando disponíveis.\n")

# =============================================================================
# 3. Denominador: população por CS por faixa etária e sexo
#
#    Idealmente: dados censitários por CS (de 07_padronizacao_taxa.R,
#    após cruzamento setor × polígono CS). Se não disponível, usa distribuição
#    proporcional de BH aplicada à população total do CS (fonte: e-Gestor AB).
# =============================================================================

variaveis_path <- file.path(DIR_REF, "variaveis_cs.csv")

if (file.exists(variaveis_path)) {
  variaveis <- read_csv(variaveis_path, show_col_types = FALSE) %>%
    filter(mes_cmpt == 1) %>%   # usa janeiro como referência anual
    select(nome_cs, ano_cmpt, populacao_referencia, pop_total_censo, pct_idosos)

  # Prioriza população do e-Gestor (mais próxima da população coberta)
  # Usa Censo como fallback
  pop_cs_ano <- variaveis %>%
    mutate(
      pop_cs = coalesce(
        as.numeric(populacao_referencia),
        as.numeric(pop_total_censo)
      )
    ) %>%
    select(nome_cs, ano = ano_cmpt, pop_cs) %>%
    filter(!is.na(pop_cs))
} else {
  message("variaveis_cs.csv não encontrado.")
  message("Usando distribuição proporcional BH para TODAS as faixas.")
  pop_cs_ano <- NULL
}

# =============================================================================
# 4. Contagem de ICSAP por CS × ano × faixa etária × sexo (numerador)
# =============================================================================

icsap_estratificado <- dados %>%
  group_by(nome_cs, regional, ano_cmpt, faixa_etaria, sexo_cat) %>%
  summarise(n_icsap = n(), .groups = "drop")

# =============================================================================
# 5. Distribui a população do CS pelas faixas etárias e sexos
#    usando a distribuição proporcional de BH como padrão
# =============================================================================

# Proporção de cada faixa × sexo na população de BH
prop_padrao <- pop_padrao_bh %>%
  mutate(prop = pop_padrao / pop_padrao_total)

# Grade completa: todos CS × anos × faixas × sexos
anos_disponiveis <- sort(unique(dados$ano_cmpt))
cs_disponiveis   <- sort(unique(dados$nome_cs))

grade <- expand.grid(
  nome_cs      = cs_disponiveis,
  ano_cmpt     = anos_disponiveis,
  faixa_etaria = unique(pop_padrao_bh$faixa_etaria),
  sexo_cat     = c("M", "F"),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  left_join(
    dados %>%
      distinct(nome_cs, regional),
    by = "nome_cs"
  )

# Junta população total do CS (de e-Gestor ou Censo)
if (!is.null(pop_cs_ano)) {
  grade <- grade %>%
    left_join(pop_cs_ano, by = c("nome_cs", "ano_cmpt" = "ano"))
} else {
  # Fallback: população de BH proporcional (cada CS = 1/153 de BH)
  grade <- grade %>%
    mutate(pop_cs = pop_padrao_total / length(cs_disponiveis))
}

# Aplica proporção de BH para distribuir a população do CS por faixa × sexo
grade <- grade %>%
  left_join(prop_padrao, by = c("faixa_etaria", "sexo_cat")) %>%
  mutate(
    pop_estrato = replace_na(pop_cs, pop_padrao_total / length(cs_disponiveis)) * prop
  )

# =============================================================================
# 6. Taxa específica por estrato e taxa padronizada
# =============================================================================

taxa_estratos <- grade %>%
  left_join(icsap_estratificado,
            by = c("nome_cs", "regional", "ano_cmpt", "faixa_etaria", "sexo_cat")) %>%
  mutate(
    n_icsap           = replace_na(n_icsap, 0),
    taxa_estrato_raw  = ifelse(pop_estrato > 0, n_icsap / pop_estrato, 0),
    # Contribuição para a taxa padronizada:
    # taxa_estrato × pop_padrao / pop_padrao_total × 10.000
    contrib_padronizado = taxa_estrato_raw * pop_padrao * 10000
  )

# Agrega por CS × ano
taxas_cs <- taxa_estratos %>%
  group_by(nome_cs, regional, ano_cmpt) %>%
  summarise(
    n_icsap           = sum(n_icsap),
    pop_cs            = first(replace_na(pop_cs, 0)),
    taxa_bruta        = ifelse(first(pop_cs) > 0,
                               sum(n_icsap) / first(pop_cs) * 10000,
                               NA_real_),
    taxa_padronizada  = sum(contrib_padronizado) / pop_padrao_total,
    .groups           = "drop"
  ) %>%
  mutate(
    taxa_bruta       = round(taxa_bruta, 2),
    taxa_padronizada = round(taxa_padronizada, 2)
  ) %>%
  rename(ano = ano_cmpt, populacao = pop_cs)

# =============================================================================
# 7. Identifica CS onde padronização inverte o ranking
# =============================================================================

message("=== Análise: padronização inverte ranking? ===")

for (ano_sel in anos_disponiveis) {
  df_ano <- taxas_cs %>%
    filter(ano == ano_sel, !is.na(taxa_bruta), !is.na(taxa_padronizada)) %>%
    mutate(
      rank_bruta   = rank(-taxa_bruta,       ties.method = "first"),
      rank_padron  = rank(-taxa_padronizada,  ties.method = "first"),
      delta_rank   = abs(rank_bruta - rank_padron)
    ) %>%
    arrange(desc(delta_rank))

  n_invertidos <- sum(df_ano$delta_rank > nrow(df_ano) * 0.20, na.rm = TRUE)
  message("  Ano ", ano_sel, ": ", n_invertidos, " CS com inversão de ranking > 20 posições")

  if (n_invertidos > 0) {
    message("  Exemplos:")
    df_ano %>%
      filter(delta_rank > nrow(.) * 0.20) %>%
      select(nome_cs, regional, taxa_bruta, taxa_padronizada, rank_bruta, rank_padron, delta_rank) %>%
      head(5) %>%
      print()
  }
}

# =============================================================================
# 8. Exporta
# =============================================================================

saida_path <- file.path(DIR_PROC, "taxas_padronizadas.csv")
write_csv(taxas_cs, saida_path)

message("\n======================================")
message("PADRONIZAÇÃO CONCLUÍDA")
message("Arquivo: ", saida_path)
message("Dimensões: ", nrow(taxas_cs), " linhas × ", ncol(taxas_cs), " colunas")
message("")
message("Resumo por ano:")
taxas_cs %>%
  group_by(ano) %>%
  summarise(
    n_cs              = n(),
    taxa_bruta_media   = round(mean(taxa_bruta, na.rm = TRUE), 2),
    taxa_padron_media  = round(mean(taxa_padronizada, na.rm = TRUE), 2),
    correlacao_metodos = round(cor(taxa_bruta, taxa_padronizada,
                                   use = "complete.obs"), 3),
    .groups = "drop"
  ) %>%
  print()
message("======================================")
