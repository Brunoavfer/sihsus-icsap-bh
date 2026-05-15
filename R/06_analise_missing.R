# =============================================================================
# 06_analise_missing.R
#
# Análise de sensibilidade dos registros ICSAP sem geocodificação (~14%)
#
# Objetivo: Verificar se os CEPs não identificados constituem ausência
# aleatória (MCAR/MAR) ou sistemática (MNAR), e quantificar o risco de
# viés introduzido pela exclusão desses registros nas análises por CS.
#
# Saídas:
#   data/processed/tabela_missing.csv   — comparação COM vs SEM regional
#   data/processed/conclusao_missing.txt — interpretação automática MAR/MNAR
#
# Referência metodológica:
#   Sterne JAC et al. (2009). Multiple imputation for missing data in
#   epidemiological and clinical research. BMJ, 338:b2393.
#   doi:10.1136/bmj.b2393
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

DIR_PROC <- "data/processed"

# -----------------------------------------------------------------------------
# Lê os dados
# -----------------------------------------------------------------------------

dados_path <- file.path(DIR_PROC, "icsap_bh_regional.csv")
if (!file.exists(dados_path)) {
  stop("Arquivo não encontrado: ", dados_path,
       "\nExecute primeiro R/03_cep_regional.R")
}

dados <- read_csv(dados_path, show_col_types = FALSE)

message("Total de registros ICSAP: ", nrow(dados))
message("Com regional (geocodificado): ",
        sum(!is.na(dados$regional)), " (",
        round(mean(!is.na(dados$regional)) * 100, 1), "%)")
message("Sem regional (missing):      ",
        sum(is.na(dados$regional)), " (",
        round(mean(is.na(dados$regional)) * 100, 1), "%)\n")

# -----------------------------------------------------------------------------
# Variável indicadora
# -----------------------------------------------------------------------------

dados <- dados %>%
  mutate(
    tem_regional = !is.na(regional),
    grupo_miss   = ifelse(tem_regional, "Com regional", "Sem regional")
  )

# -----------------------------------------------------------------------------
# 1. Distribuição temporal dos missing (ano × tem_regional)
# -----------------------------------------------------------------------------

tab_ano <- dados %>%
  count(ano_cmpt, tem_regional) %>%
  mutate(tem_regional = ifelse(tem_regional, "com_regional", "sem_regional")) %>%
  tidyr::pivot_wider(names_from = tem_regional, values_from = n, values_fill = 0) %>%
  mutate(
    total       = com_regional + sem_regional,
    pct_missing = round(sem_regional / total * 100, 1)
  )

message("=== Distribuição temporal dos missing ===")
print(tab_ano)

# Qui-quadrado: distribuição dos missing por ano
tab_chi_ano <- table(dados$ano_cmpt, dados$tem_regional)
chi_ano     <- chisq.test(tab_chi_ano)
message("\nQui-quadrado (missing × ano): χ²=",
        round(chi_ano$statistic, 2), " p=", round(chi_ano$p.value, 4))

# -----------------------------------------------------------------------------
# 2. Idade — Mann-Whitney
# -----------------------------------------------------------------------------

idade_com <- dados$idade[dados$tem_regional == TRUE]
idade_sem <- dados$idade[dados$tem_regional == FALSE]

mw_idade <- wilcox.test(idade_com, idade_sem, exact = FALSE)

tab_idade <- dados %>%
  group_by(grupo_miss) %>%
  summarise(
    n          = n(),
    mediana    = median(idade, na.rm = TRUE),
    q25        = quantile(idade, 0.25, na.rm = TRUE),
    q75        = quantile(idade, 0.75, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(mediana_iqr = paste0(mediana, " [", q25, "–", q75, "]"))

message("\n=== Idade ===")
print(select(tab_idade, grupo_miss, n, mediana_iqr))
message("Mann-Whitney: W=", round(mw_idade$statistic, 0),
        " p=", round(mw_idade$p.value, 4))

# -----------------------------------------------------------------------------
# 3. Sexo — qui-quadrado
# -----------------------------------------------------------------------------

tab_sexo  <- table(dados$sexo, dados$tem_regional)
chi_sexo  <- tryCatch(chisq.test(tab_sexo), error = function(e) NULL)

tab_sexo_prop <- dados %>%
  count(grupo_miss, sexo) %>%
  group_by(grupo_miss) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

message("\n=== Sexo (1=M, 3=F) ===")
print(tab_sexo_prop)
if (!is.null(chi_sexo)) {
  message("Qui-quadrado: χ²=", round(chi_sexo$statistic, 2),
          " p=", round(chi_sexo$p.value, 4))
}

# -----------------------------------------------------------------------------
# 4. Grupo ICSAP (descricao) — qui-quadrado + top 5 por grupo
# -----------------------------------------------------------------------------

# Top 5 condições em cada grupo
top5 <- dados %>%
  filter(!is.na(descricao)) %>%
  count(grupo_miss, descricao) %>%
  group_by(grupo_miss) %>%
  slice_max(n, n = 5) %>%
  mutate(rank = row_number()) %>%
  ungroup()

message("\n=== Top 5 condições ICSAP por grupo ===")
print(top5)

# Qui-quadrado: distribuição das condições
tab_cond  <- table(dados$descricao, dados$tem_regional)
chi_cond  <- tryCatch(chisq.test(tab_cond), error = function(e) NULL)
if (!is.null(chi_cond)) {
  message("\nQui-quadrado (condição × tem_regional): χ²=",
          round(chi_cond$statistic, 2), " p=", round(chi_cond$p.value, 4))
}

# -----------------------------------------------------------------------------
# 5. Valor da internação (val_tot) — Mann-Whitney
# -----------------------------------------------------------------------------

val_com  <- dados$val_tot[dados$tem_regional == TRUE]
val_sem  <- dados$val_tot[dados$tem_regional == FALSE]
mw_val   <- wilcox.test(val_com, val_sem, exact = FALSE)

tab_val <- dados %>%
  group_by(grupo_miss) %>%
  summarise(
    mediana_val = round(median(val_tot, na.rm = TRUE), 2),
    q25_val     = round(quantile(val_tot, 0.25, na.rm = TRUE), 2),
    q75_val     = round(quantile(val_tot, 0.75, na.rm = TRUE), 2),
    .groups     = "drop"
  )

message("\n=== Valor da internação (R$) ===")
print(tab_val)
message("Mann-Whitney: W=", round(mw_val$statistic, 0),
        " p=", round(mw_val$p.value, 4))

# -----------------------------------------------------------------------------
# 6. Dias de permanência (dias_perm) — Mann-Whitney
# -----------------------------------------------------------------------------

dias_com <- dados$dias_perm[dados$tem_regional == TRUE]
dias_sem <- dados$dias_perm[dados$tem_regional == FALSE]
mw_dias  <- wilcox.test(dias_com, dias_sem, exact = FALSE)

tab_dias <- dados %>%
  group_by(grupo_miss) %>%
  summarise(
    mediana_dias = median(dias_perm, na.rm = TRUE),
    q25_dias     = quantile(dias_perm, 0.25, na.rm = TRUE),
    q75_dias     = quantile(dias_perm, 0.75, na.rm = TRUE),
    .groups      = "drop"
  )

message("\n=== Dias de permanência ===")
print(tab_dias)
message("Mann-Whitney: W=", round(mw_dias$statistic, 0),
        " p=", round(mw_dias$p.value, 4))

# -----------------------------------------------------------------------------
# Tabela comparativa consolidada
# -----------------------------------------------------------------------------

tabela_miss <- bind_rows(
  tibble(
    variavel    = "n",
    stat        = "n",
    com_regional = as.character(sum(dados$tem_regional)),
    sem_regional = as.character(sum(!dados$tem_regional)),
    p_valor     = NA_character_,
    teste       = NA_character_
  ),
  tibble(
    variavel    = "pct_do_total",
    stat        = "%",
    com_regional = paste0(round(mean(dados$tem_regional) * 100, 1), "%"),
    sem_regional = paste0(round(mean(!dados$tem_regional) * 100, 1), "%"),
    p_valor     = NA_character_,
    teste       = NA_character_
  ),
  tibble(
    variavel    = "idade_mediana_iqr",
    stat        = "mediana [IQR]",
    com_regional = with(tab_idade[tab_idade$grupo_miss == "Com regional",], mediana_iqr),
    sem_regional = with(tab_idade[tab_idade$grupo_miss == "Sem regional",], mediana_iqr),
    p_valor     = format(round(mw_idade$p.value, 4), scientific = FALSE),
    teste       = "Mann-Whitney"
  ),
  tibble(
    variavel    = "sexo_masculino_pct",
    stat        = "%",
    com_regional = with(filter(tab_sexo_prop, grupo_miss=="Com regional", sexo==1),
                        paste0(pct, "%")),
    sem_regional = with(filter(tab_sexo_prop, grupo_miss=="Sem regional", sexo==1),
                        paste0(pct, "%")),
    p_valor     = if (!is.null(chi_sexo)) format(round(chi_sexo$p.value, 4)) else NA_character_,
    teste       = "qui-quadrado"
  ),
  tibble(
    variavel    = "condicao_icsap",
    stat        = "distribuição",
    com_regional = "ver top5",
    sem_regional = "ver top5",
    p_valor     = if (!is.null(chi_cond)) format(round(chi_cond$p.value, 4)) else NA_character_,
    teste       = "qui-quadrado"
  ),
  tibble(
    variavel    = "val_tot_mediana",
    stat        = "mediana R$",
    com_regional = as.character(tab_val$mediana_val[tab_val$grupo_miss == "Com regional"]),
    sem_regional = as.character(tab_val$mediana_val[tab_val$grupo_miss == "Sem regional"]),
    p_valor     = format(round(mw_val$p.value, 4), scientific = FALSE),
    teste       = "Mann-Whitney"
  ),
  tibble(
    variavel    = "dias_perm_mediana",
    stat        = "mediana dias",
    com_regional = as.character(tab_dias$mediana_dias[tab_dias$grupo_miss == "Com regional"]),
    sem_regional = as.character(tab_dias$mediana_dias[tab_dias$grupo_miss == "Sem regional"]),
    p_valor     = format(round(mw_dias$p.value, 4), scientific = FALSE),
    teste       = "Mann-Whitney"
  ),
  tibble(
    variavel    = "missing_por_ano",
    stat        = "% missing",
    com_regional = NA_character_,
    sem_regional = paste(tab_ano$ano_cmpt, tab_ano$pct_missing, sep="=", collapse="; "),
    p_valor     = format(round(chi_ano$p.value, 4), scientific = FALSE),
    teste       = "qui-quadrado"
  )
)

# -----------------------------------------------------------------------------
# Interpretação automática (MAR vs MNAR)
# -----------------------------------------------------------------------------

p_values_numericos <- c(
  idade   = mw_idade$p.value,
  sexo    = if (!is.null(chi_sexo))  chi_sexo$p.value  else 1,
  condicao = if (!is.null(chi_cond)) chi_cond$p.value  else 1,
  val_tot = mw_val$p.value,
  dias    = mw_dias$p.value,
  ano     = chi_ano$p.value
)

n_significativos  <- sum(p_values_numericos < 0.05)
pct_missing_total <- round(mean(!dados$tem_regional) * 100, 1)

if (n_significativos == 0) {
  interpretacao <- "MCAR (Missing Completely at Random)"
  conclusao     <- "MCAR: os registros sem geocodificação não diferem sistematicamente dos registros com geocodificação em nenhuma variável testada. A exclusão desses registros provavelmente não introduz viés substancial nas análises por CS."
  risco_vies    <- "Baixo"
} else if (n_significativos <= 2) {
  interpretacao <- "MAR (Missing at Random) — viés improvável"
  conclusao     <- paste0(
    "MAR: diferenças estatisticamente significativas foram encontradas em ",
    n_significativos, " de ", length(p_values_numericos),
    " variáveis testadas. O padrão de missingness não é completamente aleatório, ",
    "mas é improvável que introduza viés substancial nas estimativas de taxa ICSAP ",
    "por CS, uma vez que a exclusão afeta igualmente todos os grupos de condições ",
    "e regionais. Recomenda-se reportar esta análise como limitação no artigo."
  )
  risco_vies    <- "Moderado — reportar como limitação"
} else {
  interpretacao <- "MNAR (Missing Not at Random) — limitação importante"
  conclusao     <- paste0(
    "MNAR: diferenças estatisticamente significativas foram encontradas em ",
    n_significativos, " de ", length(p_values_numericos),
    " variáveis testadas. O padrão de missingness é sistematicamente diferente ",
    "entre os grupos COM e SEM geocodificação. Isso representa uma limitação ",
    "importante que deve ser detalhada na seção de Limitações do artigo. ",
    "Considere análise de sensibilidade com imputação ou ponderação inversa ",
    "de probabilidade (IPW) para avaliar o impacto nas estimativas principais."
  )
  risco_vies    <- "Alto — requer análise adicional"
}

# -----------------------------------------------------------------------------
# Salva saídas
# -----------------------------------------------------------------------------

write_csv(tabela_miss, file.path(DIR_PROC, "tabela_missing.csv"))

writeLines(
  c(
    paste0("ANÁLISE DE MISSING — ICSAP-BH"),
    paste0("Data: ", Sys.Date()),
    paste0(""),
    paste0("Total de registros: ", nrow(dados)),
    paste0("Com geocodificação: ",
           sum(dados$tem_regional), " (", round(mean(dados$tem_regional)*100,1), "%)"),
    paste0("Sem geocodificação: ",
           sum(!dados$tem_regional), " (", pct_missing_total, "%)"),
    paste0(""),
    paste0("Testes realizados: ", length(p_values_numericos)),
    paste0("Testes significativos (p < 0,05): ", n_significativos),
    paste0(""),
    paste0("CLASSIFICAÇÃO: ", interpretacao),
    paste0("Risco de viés: ", risco_vies),
    paste0(""),
    paste0("INTERPRETAÇÃO:"),
    paste0(conclusao),
    paste0(""),
    paste0("p-valores individuais:"),
    paste(names(p_values_numericos),
          round(p_values_numericos, 4),
          sep = " = ", collapse = "\n")
  ),
  file.path(DIR_PROC, "conclusao_missing.txt")
)

message("\n======================================")
message("ANÁLISE DE MISSING CONCLUÍDA")
message("Classificação: ", interpretacao)
message("Risco de viés: ", risco_vies)
message("")
message("Saídas:")
message("  ", file.path(DIR_PROC, "tabela_missing.csv"))
message("  ", file.path(DIR_PROC, "conclusao_missing.txt"))
message("======================================")
