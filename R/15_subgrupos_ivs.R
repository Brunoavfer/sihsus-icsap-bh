# =============================================================================
# 15_subgrupos_ivs.R
#
# GEE AR-1 estratificado por nível de vulnerabilidade (IVS-BH)
#
# Objetivo: testar se o efeito da tendência temporal e da Portaria GM/MS
#   3.493/2024 (ITS: intervenção em maio/2024) difere entre CS com diferentes
#   níveis de vulnerabilidade — i.e., a portaria reduziu ou ampliou
#   desigualdades entre CS?
#
# Desfecho: taxa_cs = n_icsap / pop_ref × 10.000 (mensal, por CS)
# Estratos: ivs_predominante ∈ {Baixo, Médio, Elevado, Muito Elevado}
#
# Modelo por estrato (GEE AR-1, Gama log):
#   taxa_cs ~ mes_num + interv + tempo_pos + sin12 + cos12 + pct_sem_saneamento
#
#   mes_num   — tendência pré-intervenção (slope, meses 1-36)
#   interv    — mudança de nível em maio/2024 (0→1 em mes_num=17)
#   tempo_pos — mudança de slope pós-intervenção (ramp: 0,0,...,1,2,...)
#   sin12/cos12 — sazonalidade Fourier
#   pct_sem_saneamento — controle socioeconômico dentro do estrato
#
# Saídas:
#   data/processed/gee_subgrupos_ivs.csv  — coeficientes por estrato
#   docs/subgrupos_ivs.png                — gráfico de forest plot
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(geepack)
})

DIR_PROC <- "data/processed"
DIR_REF  <- "data/ref"

# =============================================================================
# 1. Reconstrói painel mensal (mesma lógica do script 09)
# =============================================================================

message("=== 1. Construindo painel CS × mês ===")

icsap_raw <- read_csv(file.path(DIR_PROC, "icsap_bh_regional.csv"),
                      show_col_types = FALSE)

icsap_mes <- icsap_raw %>%
  filter(ano_cmpt %in% 2023:2025, !is.na(nome_cs)) %>%
  mutate(mes_pad = str_pad(as.integer(mes_cmpt), 2, pad = "0")) %>%
  group_by(nome_cs, ano_cmpt, mes_pad) %>%
  summarise(n_icsap = n(), .groups = "drop") %>%
  rename(mes_cmpt_n = mes_pad) %>%
  mutate(competencia = paste0(str_sub(as.character(ano_cmpt), 3, 4), mes_cmpt_n))

vars <- read_csv(file.path(DIR_REF, "variaveis_cs.csv"),
                 show_col_types = FALSE) %>%
  filter(ano_cmpt %in% 2023:2025) %>%
  mutate(
    competencia = str_pad(as.character(as.integer(competencia)), 4, pad = "0"),
    mes_cmpt_n  = str_pad(as.integer(mes_cmpt), 2, pad = "0")
  )

grade <- expand.grid(
  nome_cs    = unique(vars$nome_cs),
  ano_cmpt   = 2023:2025,
  mes_cmpt_n = str_pad(1:12, 2, pad = "0"),
  stringsAsFactors = FALSE
) %>%
  as_tibble() %>%
  mutate(
    ano_cmpt    = as.integer(ano_cmpt),
    competencia = paste0(str_sub(as.character(ano_cmpt), 3, 4), mes_cmpt_n)
  ) %>%
  left_join(icsap_mes %>% select(nome_cs, competencia, n_icsap),
            by = c("nome_cs", "competencia")) %>%
  mutate(n_icsap = replace_na(n_icsap, 0L)) %>%
  left_join(
    vars %>% select(nome_cs, competencia, populacao_referencia,
                    pct_sem_saneamento, ivs_score, ivs_predominante),
    by = c("nome_cs", "competencia")
  )

dados_painel <- grade %>%
  filter(!is.na(populacao_referencia), populacao_referencia > 0,
         !is.na(pct_sem_saneamento), !is.na(ivs_predominante)) %>%
  mutate(
    taxa_cs   = n_icsap / populacao_referencia * 10000,
    mes_num   = (as.integer(ano_cmpt) - 2023L) * 12L + as.integer(mes_cmpt_n),
    # ITS: Portaria 3.493/2024 vigência maio/2024 = mes_num 17
    interv    = as.integer(mes_num >= 17L),
    tempo_pos = as.integer(pmax(0L, mes_num - 16L)),
    sin12     = sin(2 * pi * mes_num / 12),
    cos12     = cos(2 * pi * mes_num / 12),
    cs_id     = as.integer(factor(nome_cs))
  ) %>%
  filter(taxa_cs > 0) %>%
  arrange(cs_id, mes_num)

message("  Painel total: ", nrow(dados_painel), " obs | ", n_distinct(dados_painel$nome_cs), " CS")

# Distribuição por IVS
message("\nDistribuição dos CS por IVS:")
dados_painel %>%
  distinct(nome_cs, ivs_predominante) %>%
  count(ivs_predominante) %>%
  print()

# =============================================================================
# 2. GEE por estrato IVS
# =============================================================================

message("\n=== 2. GEE AR-1 por estrato IVS ===")
message("Fórmula: taxa_cs ~ mes_num + interv + tempo_pos + sin12 + cos12 + pct_sem_saneamento\n")

niveis_ivs <- c("Baixo", "Médio", "Elevado", "Muito Elevado")
resultados_ivs <- list()

for (nivel in niveis_ivs) {
  sub <- dados_painel %>%
    filter(ivs_predominante == nivel) %>%
    mutate(cs_id_sub = as.integer(factor(nome_cs))) %>%
    arrange(cs_id_sub, mes_num)

  n_cs  <- n_distinct(sub$nome_cs)
  n_obs <- nrow(sub)
  message("--- IVS: ", nivel, " (", n_cs, " CS, ", n_obs, " obs) ---")

  if (n_cs < 5) {
    message("  AVISO: n_cs < 5 — GEE instável, pulando")
    next
  }

  # Para grupos pequenos (Muito Elevado < 15 CS): usa exchangeable
  corr_str <- if (n_cs < 15) "exchangeable" else "ar1"
  if (corr_str == "exchangeable") {
    message("  Usando corstr='exchangeable' (n_cs=", n_cs, " < 15)")
  }

  mod <- tryCatch(
    geeglm(
      taxa_cs ~ mes_num + interv + tempo_pos + sin12 + cos12 + pct_sem_saneamento,
      family  = Gamma(link = "log"),
      data    = sub,
      id      = cs_id_sub,
      corstr  = corr_str,
      waves   = mes_num
    ),
    error = function(e) { message("  Falhou: ", e$message); NULL }
  )

  if (is.null(mod)) {
    message("  Tentando com exchangeable...")
    mod <- tryCatch(
      geeglm(
        taxa_cs ~ mes_num + interv + tempo_pos + sin12 + cos12 + pct_sem_saneamento,
        family  = Gamma(link = "log"),
        data    = sub,
        id      = cs_id_sub,
        corstr  = "exchangeable"
      ),
      error = function(e) { message("  Falhou novamente: ", e$message); NULL }
    )
  }

  if (is.null(mod)) next
  print(summary(mod))

  cf <- coef(summary(mod))
  se_col <- if ("Std.err" %in% colnames(cf)) "Std.err" else "Std. Error"
  p_col  <- if ("Pr(>|W|)" %in% colnames(cf)) "Pr(>|W|)" else "Pr(>|z|)"

  res <- as_tibble(cf, rownames = "variavel") %>%
    rename(beta = Estimate, se = !!se_col, p_valor = !!p_col) %>%
    mutate(
      ivs_nivel  = nivel,
      n_cs       = n_cs,
      n_obs      = n_obs,
      corr_str   = corr_str,
      RR         = round(exp(beta), 4),
      RR_ic_inf  = round(exp(beta - 1.96 * se), 4),
      RR_ic_sup  = round(exp(beta + 1.96 * se), 4),
      p_valor    = round(p_valor, 4),
      beta       = round(beta, 4),
      se         = round(se, 4),
      sig        = case_when(p_valor < 0.001 ~ "***",
                             p_valor < 0.01  ~ "**",
                             p_valor < 0.05  ~ "*",
                             TRUE            ~ "ns")
    ) %>%
    select(ivs_nivel, n_cs, n_obs, corr_str, variavel, beta, se, p_valor, RR, RR_ic_inf, RR_ic_sup, sig)

  resultados_ivs[[nivel]] <- res
  message("")
}

resultados_df <- bind_rows(resultados_ivs)

# =============================================================================
# 3. Tabela comparativa de efeitos-chave
# =============================================================================

message("\n=== 3. EFEITOS-CHAVE POR ESTRATO IVS ===\n")

vars_interesse <- c("mes_num", "interv", "tempo_pos")

message("Variável     IVS           RR       IC 95%            p      sig  APC/tendência")
message(strrep("-", 85))

for (v in vars_interesse) {
  for (nivel in niveis_ivs) {
    row <- resultados_df %>% filter(variavel == v, ivs_nivel == nivel)
    if (nrow(row) == 0) next

    # Para mes_num: calcula APC/ano (mensal → anual)
    anot <- if (v == "mes_num") {
      paste0("  APC/ano=", round((exp(12 * row$beta) - 1) * 100, 1), "%")
    } else if (v == "interv") {
      paste0("  nível=", round((row$RR - 1) * 100, 1), "%")
    } else {
      paste0("  slope pós=", round((exp(12 * (row$beta)) - 1) * 100, 1), "%/ano")
    }

    message(
      str_pad(v,        12), "  ",
      str_pad(nivel,    14), "  ",
      str_pad(row$RR,    6), "  ",
      "(", row$RR_ic_inf, "–", row$RR_ic_sup, ")  ",
      str_pad(row$p_valor, 6), "  ", row$sig, anot
    )
  }
  message("")
}

# =============================================================================
# 4. Interpretação: a Portaria 3.493 reduziu ou ampliou desigualdades?
# =============================================================================

message("=== 4. INTERPRETAÇÃO — EFEITO DA PORTARIA 3.493 ===\n")

efeito_portaria <- resultados_df %>%
  filter(variavel %in% c("interv", "tempo_pos")) %>%
  select(ivs_nivel, variavel, RR, p_valor, sig)

if (nrow(efeito_portaria) > 0) {
  # Compara nível_change e slope_change entre grupos IVS
  nivel_change <- efeito_portaria %>% filter(variavel == "interv") %>%
    arrange(match(ivs_nivel, niveis_ivs))
  slope_change <- efeito_portaria %>% filter(variavel == "tempo_pos") %>%
    arrange(match(ivs_nivel, niveis_ivs))

  message("Mudança de nível (interv):")
  print(nivel_change)

  message("\nMudança de slope pós-interv (tempo_pos):")
  print(slope_change)

  rr_baixo     <- slope_change %>% filter(ivs_nivel == "Baixo")     %>% pull(RR)
  rr_m_elevado <- slope_change %>% filter(ivs_nivel == "Muito Elevado") %>% pull(RR)

  if (length(rr_baixo) > 0 && length(rr_m_elevado) > 0) {
    message("\n  RR slope pós: Baixo=", rr_baixo, " vs Muito Elevado=", rr_m_elevado)
    if (rr_m_elevado < rr_baixo) {
      message("  → A Portaria 3.493 parece ter REDUZIDO desigualdades: slope de queda")
      message("    mais acentuado em CS de maior vulnerabilidade (Muito Elevado).")
    } else {
      message("  → Evidência de AMPLIAÇÃO de desigualdades ou ausência de diferencial")
      message("    entre CS de alto e baixo IVS.")
    }
  }
}

# =============================================================================
# 5. Gráfico forest plot
# =============================================================================

message("\n=== 5. Gerando forest plot ===")

plot_df <- resultados_df %>%
  filter(variavel %in% c("mes_num", "interv", "tempo_pos")) %>%
  mutate(
    variavel_label = case_when(
      variavel == "mes_num"   ~ "Tendência pré (APC/mês)",
      variavel == "interv"    ~ "Mudança de nível (Portaria)",
      variavel == "tempo_pos" ~ "Mudança de slope pós"
    ),
    ivs_nivel = factor(ivs_nivel, levels = rev(niveis_ivs))
  )

p <- ggplot(plot_df, aes(x = RR, xmin = RR_ic_inf, xmax = RR_ic_sup, y = ivs_nivel,
                          color = sig != "ns")) +
  geom_pointrange(size = 0.7) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c("TRUE" = "#c0392b", "FALSE" = "#7f8c8d"),
                     labels = c("ns", "p < 0.05"), name = NULL) +
  facet_wrap(~variavel_label, scales = "free_x", ncol = 3) +
  labs(
    title    = "GEE Gama AR-1 por Nível de Vulnerabilidade (IVS-BH)",
    subtitle = "Portaria GM/MS 3.493/2024 — efeito por categoria de IVS | jan/2023–dez/2025",
    x = "Razão de Taxas (IC 95%)",
    y = "IVS Predominante do CS",
    caption = paste0("n CS: Baixo=", sum(dados_painel %>% distinct(nome_cs, ivs_predominante) %>% pull(ivs_predominante) == "Baixo"),
                     ", Médio=", sum(dados_painel %>% distinct(nome_cs, ivs_predominante) %>% pull(ivs_predominante) == "Médio"),
                     ", Elevado=", sum(dados_painel %>% distinct(nome_cs, ivs_predominante) %>% pull(ivs_predominante) == "Elevado"),
                     ", Muito Elevado=", sum(dados_painel %>% distinct(nome_cs, ivs_predominante) %>% pull(ivs_predominante) == "Muito Elevado"))
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "grey90"))

ggsave("docs/subgrupos_ivs.png", p, width = 12, height = 5, dpi = 150)
message("  Plot salvo: docs/subgrupos_ivs.png")

# =============================================================================
# 6. Exporta
# =============================================================================

write_csv(resultados_df, file.path(DIR_PROC, "gee_subgrupos_ivs.csv"))

message("\n======================================")
message("SUBGRUPOS IVS CONCLUÍDO")
message("  ", file.path(DIR_PROC, "gee_subgrupos_ivs.csv"))
message("  docs/subgrupos_ivs.png")
message("======================================")
