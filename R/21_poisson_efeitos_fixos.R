# =============================================================================
# 21_poisson_efeitos_fixos.R
#
# Análise principal: Poisson com efeitos fixos two-way (CS + ano)
# Padrão Lancet Public Health — análise de sensibilidade ao GEE AR-1
#
# Nota metodológica:
#   M1: FE por CS + ano  → estima variação WITHIN-CS ao longo do tempo
#   M2: FE por regional + ano → permite preditores estáticos entre CS
#       (IVS, renda, saneamento são time-invariant → absorvidos por FE de CS)
#   M3: FE por CS + ano + n_esf → identifica efeito WITHIN-CS do n_esf
#
# Referência: Bergé L (2023). fixest: Fast Fixed Effects Estimations.
#   R package v0.11.x. DOI:10.1177/1536867X19874235
#
# Saídas:
#   data/processed/poisson_resultados.csv
#   docs/poisson_forest_plot.png
# =============================================================================

# Instala fixest se necessário
if (!requireNamespace("fixest", quietly = TRUE)) {
  message("Instalando fixest...")
  install.packages("fixest", type = "binary",
                   repos = "https://cloud.r-project.org", quiet = TRUE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(lubridate)
  library(stringr)
  library(fixest)
  library(ggplot2)
})

# nlme/MASS/fixest podem mascarar dplyr::select e filter
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

message(sprintf("  icsap_bh_regional: %s linhas | variaveis_cs: %s linhas",
                format(nrow(icsap_reg), big.mark=","),
                format(nrow(variaveis),  big.mark=",")))

# =============================================================================
# 2. Prepara componentes do painel
# =============================================================================

message("\n=== 2. Construindo painel CS × mês (153 × 51 = 7.803 obs) ===")

# 2a. Covariáveis ESTÁTICAS por CS (Censo 2022 + IVS — único valor por CS)
static_cs <- variaveis %>%
  group_by(nome_cs) %>%
  slice(1) %>%
  ungroup() %>%
  select(nome_cs, cod_smsa, regional,
         pop_total_censo, pct_sem_saneamento, renda_media, pct_area_favela,
         ivs_score, ivs_predominante)

# 2b. Covariáveis DINÂMICAS por CS × competência (CNES, 2023-2025)
timevar_cs <- variaveis %>%
  select(nome_cs, ano_cmpt, mes_cmpt, n_esf, n_emulti, n_esb, n_acs, ch_medica_total)

# 2c. Populaçao por CS — Censo 2022 (offset estático)
pop_cs_total <- pop_faixas %>%
  group_by(nome_cs) %>%
  summarise(pop_censo = sum(pop_faixa, na.rm = TRUE), .groups = "drop")

# 2d. Skeleton: 153 CS × 51 meses
cs_ref <- static_cs %>% select(nome_cs, cod_smsa, regional)

meses_ref <- tibble(
  ano_cmpt = c(rep(2022L, 12), rep(2023L, 12), rep(2024L, 12),
               rep(2025L, 12), rep(2026L,  3)),
  mes_cmpt = c(1:12, 1:12, 1:12, 1:12, 1:3)
) %>%
  mutate(mes_num = row_number())   # 1 = jan/2022, 51 = mar/2026

skeleton <- tidyr::crossing(cs_ref, meses_ref)   # 153 × 51 = 7.803
message(sprintf("  Skeleton: %s obs", format(nrow(skeleton), big.mark=",")))

# 2e. Conta n_icsap por CS × mês (apenas geocodificados, ~86,4%)
n_icsap_cs_mes <- icsap_reg %>%
  filter(!is.na(nome_cs)) %>%
  mutate(mes_cmpt = as.integer(mes_cmpt)) %>%
  group_by(nome_cs, ano_cmpt, mes_cmpt) %>%
  summarise(n_icsap = n(), .groups = "drop")

# 2f. Monta painel completo
painel <- skeleton %>%
  left_join(n_icsap_cs_mes, by = c("nome_cs", "ano_cmpt", "mes_cmpt")) %>%
  mutate(n_icsap = replace_na(n_icsap, 0L)) %>%
  left_join(static_cs %>% select(-regional, -cod_smsa), by = "nome_cs") %>%
  left_join(timevar_cs, by = c("nome_cs", "ano_cmpt", "mes_cmpt")) %>%
  left_join(pop_cs_total, by = "nome_cs") %>%
  mutate(
    sin12   = sin(2 * pi * mes_num / 12),
    cos12   = cos(2 * pi * mes_num / 12),
    ano_fac = factor(ano_cmpt),
    # offset: pop_censo (fallback mediana se NA/0)
    pop_cs  = if_else(!is.na(pop_censo) & pop_censo > 0,
                      as.double(pop_censo),
                      median(pop_censo, na.rm = TRUE)),
    # z-scores para interpretação padronizada
    ivs_z    = as.numeric(scale(ivs_score)),
    renda_z  = as.numeric(scale(renda_media)),
    san_z    = as.numeric(scale(pct_sem_saneamento)),
    favela_z = as.numeric(scale(pct_area_favela))
  )

message(sprintf("  Painel: %s obs | n_icsap total: %s",
                format(nrow(painel), big.mark=","),
                format(sum(painel$n_icsap), big.mark=",")))
message(sprintf("  n_icsap = 0: %.1f%% das obs-CS-mês",
                100 * mean(painel$n_icsap == 0)))
message(sprintf("  n_esf disponível: %.1f%% das obs (2023-2025)",
                100 * mean(!is.na(painel$n_esf))))
message(sprintf("  offset pop_cs: mediana = %s | min = %s | max = %s",
                format(round(median(painel$pop_cs)), big.mark=","),
                format(round(min(painel$pop_cs)),    big.mark=","),
                format(round(max(painel$pop_cs)),    big.mark=",")))

# =============================================================================
# 3. Modelos Poisson com efeitos fixos (fixest::feglm)
# =============================================================================

message("\n=== 3. Modelos Poisson FE (fixest::feglm) ===")
message(sprintf("  fixest versão: %s", as.character(packageVersion("fixest"))))

# Suprime verbose do fixest durante o ajuste
setFixest_notes(FALSE)

# ── M1: baseline (CS + ano FE, todos os 51 meses) ──
message("  Ajustando M1_base ...")
M1 <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 | nome_cs + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel,
  vcov   = ~nome_cs
)
message(sprintf("    N = %s | CS = %d | anos = %d",
                format(nobs(M1), big.mark=","),
                n_distinct(painel$nome_cs),
                n_distinct(painel$ano_fac)))

# ── M2: contextual — regional FE para preditores estáticos entre CS ──
# IVS/socioeconomic são time-invariant → NÃO identificáveis com CS FE
# Usa regional FE (9 categorias): estima variação ENTRE CS dentro de cada regional
message("  Ajustando M2_contextual (regional FE — permite preditores estáticos) ...")
M2 <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 +
    ivs_score + pct_area_favela + renda_media + pct_sem_saneamento |
    regional + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel,
  vcov   = ~nome_cs
)
message(sprintf("    N = %s", format(nobs(M2), big.mark=",")))

# ── M3: estrutural — CS FE + n_esf time-varying (2023-2025) ──
# Identifica efeito WITHIN-CS: quando CS X ganhou/perdeu equipes ESF,
# como mudou o n_icsap? (estratégia mais robusta para causalidade)
painel_cnes <- painel %>% filter(!is.na(n_esf))
message(sprintf("  Ajustando M3_estrutural (CS FE + n_esf, N_cnes = %s) ...",
                format(nrow(painel_cnes), big.mark=",")))
M3 <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 + n_esf + n_emulti | nome_cs + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel_cnes,
  vcov   = ~nome_cs
)
message(sprintf("    N = %s | CS = %d",
                format(nobs(M3), big.mark=","),
                n_distinct(painel_cnes$nome_cs)))

# =============================================================================
# 4. Extrai IRR, IC 95%, p-valor
# =============================================================================

message("\n=== 4. IRR e IC 95% ===")

extrai_irr <- function(mod, nome_modelo) {
  cf  <- coef(mod)
  ci  <- confint(mod, level = 0.95)
  pv  <- pvalue(mod)
  tibble(
    modelo   = nome_modelo,
    variavel = names(cf),
    beta     = cf,
    irr      = exp(cf),
    ic_inf   = exp(ci[, 1]),
    ic_sup   = exp(ci[, 2]),
    p_valor  = as.numeric(pv[names(cf)])
  )
}

res_M1 <- extrai_irr(M1, "M1_base")
res_M2 <- extrai_irr(M2, "M2_contextual")
res_M3 <- extrai_irr(M3, "M3_estrutural")

resultados <- bind_rows(res_M1, res_M2, res_M3)

imprime_mod <- function(res, nome) {
  cat(sprintf("\n--- %s ---\n", nome))
  print(
    res %>%
      mutate(
        IRR  = sprintf("%.3f", irr),
        IC95 = sprintf("%.3f–%.3f", ic_inf, ic_sup),
        p    = sprintf("%.4f", p_valor),
        sig  = case_when(
          p_valor < 0.001 ~ "***", p_valor < 0.01 ~ "**",
          p_valor < 0.05  ~ "*",   p_valor < 0.10 ~ ".",
          TRUE            ~ ""
        )
      ) %>%
      select(variavel, IRR, IC95, p, sig),
    n = 20
  )
}

imprime_mod(res_M1, "M1_base")
imprime_mod(res_M2, "M2_contextual")
imprime_mod(res_M3, "M3_estrutural")

# =============================================================================
# 5. Pseudo R² de McFadden e superdispersão
# =============================================================================

message("\n=== 5. Qualidade do ajuste e superdispersão ===")

disp_pearson <- function(mod, dados) {
  mu  <- fitted(mod)
  # feglm não implementa model.frame padrão — usa dados diretamente
  y   <- tryCatch(
    as.numeric(model.response(model.frame(mod))),
    error = function(e) as.numeric(dados$n_icsap)[seq_along(mu)]
  )
  if (length(y) != length(mu)) y <- as.numeric(dados$n_icsap)[seq_along(mu)]
  p_chi <- sum((y - mu)^2 / pmax(mu, 0.001), na.rm = TRUE)
  df_r  <- max(length(mu) - length(coef(mod)), 1L)
  list(ratio = p_chi / df_r, p_chi = p_chi, df = df_r)
}

d1 <- disp_pearson(M1, painel)
d2 <- disp_pearson(M2, painel)
d3 <- disp_pearson(M3, painel_cnes)

for (nm in c("M1_base", "M2_contextual", "M3_estrutural")) {
  d <- switch(nm, M1_base = d1, M2_contextual = d2, M3_estrutural = d3)
  verdict <- if (d$ratio > 2)  "SUPERDISPERSÃO severa → NB necessário"
             else if (d$ratio > 1.5) "superdispersão moderada → NB recomendado"
             else "Poisson adequado"
  message(sprintf("  %-20s: dispersão Pearson/df = %.2f  [%s]", nm, d$ratio, verdict))
}

disp_M1 <- d1$ratio
disp_M2 <- d2$ratio
disp_M3 <- d3$ratio

# =============================================================================
# 6. Sensibilidade: Negative Binomial (se dispM1 > 1.5)
# =============================================================================

message("\n=== 6. Sensibilidade: Negative Binomial ===")

res_nb <- NULL
M1_nb  <- NULL

if (disp_M1 > 1.5) {
  message("  Ajustando M1_NB (femlm, family='negbin')...")
  tryCatch({
    M1_nb <- femlm(
      n_icsap ~ mes_num + sin12 + cos12 + offset(log(pop_cs)) |
        nome_cs + ano_fac,
      family = "negbin",
      data   = painel,
      vcov   = ~nome_cs
    )
    res_nb <- extrai_irr(M1_nb, "M1_NB")
    imprime_mod(res_nb, "M1_NB")
  }, error = function(e) {
    message("  AVISO: femlm NB falhou (", conditionMessage(e), ")")
    message("  Usando MASS::glm.nb como fallback...")
    tryCatch({
      library(MASS)
      M1_nb_mass <- MASS::glm.nb(
        n_icsap ~ mes_num + sin12 + cos12 + offset(log(pop_cs)) + nome_cs + ano_fac,
        data = painel
      )
      message("  MASS::glm.nb ajustado (sem FE clustered — somente para dispersão)")
      message(sprintf("  theta (dispersion): %.3f", M1_nb_mass$theta))
    }, error = function(e2) {
      message("  AVISO: glm.nb também falhou — superdispersão documentada mas NB não ajustado")
    })
  })
} else {
  message(sprintf("  Dispersão M1 = %.2f ≤ 1.5 → Poisson adequado, NB desnecessário", disp_M1))
}

# =============================================================================
# 7. Dose-resposta n_esf (quartis pré-especificados)
# =============================================================================

message("\n=== 7. Dose-resposta: n_esf por quartis (Q1=1-4, Q2=5-6, Q3=7-8, Q4=9+) ===")

painel_esf <- painel_cnes %>%
  filter(!is.na(n_esf), n_esf >= 1) %>%
  mutate(
    n_esf_q = case_when(
      n_esf <= 4 ~ "Q1 (1-4)",
      n_esf <= 6 ~ "Q2 (5-6)",
      n_esf <= 8 ~ "Q3 (7-8)",
      TRUE       ~ "Q4 (9+)"
    ),
    n_esf_q = relevel(
      factor(n_esf_q, levels = c("Q1 (1-4)", "Q2 (5-6)", "Q3 (7-8)", "Q4 (9+)")),
      ref = "Q1 (1-4)"
    )
  )

message("  Distribuição n_esf por quartil:")
print(
  painel_esf %>%
    count(n_esf_q, name = "n_obs") %>%
    mutate(pct = round(100 * n_obs / sum(n_obs), 1),
           n_esf_range = case_when(
             n_esf_q == "Q1 (1-4)" ~ paste0("[1,", quantile(painel_esf$n_esf[painel_esf$n_esf<=4], .75), "]"),
             TRUE ~ ""
           ))
)

M_dr <- feglm(
  n_icsap ~ n_esf_q + mes_num + sin12 + cos12 | nome_cs + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel_esf,
  vcov   = ~nome_cs
)

res_dr <- extrai_irr(M_dr, "M_dose_resposta")

message("\n  IRR por quartil de n_esf (ref = Q1):")
print(
  res_dr %>%
    filter(str_detect(variavel, "n_esf_q")) %>%
    mutate(
      IRR  = sprintf("%.3f", irr),
      IC95 = sprintf("%.3f–%.3f", ic_inf, ic_sup),
      p    = sprintf("%.4f", p_valor),
      sig  = case_when(
        p_valor < 0.001 ~ "***", p_valor < 0.01 ~ "**",
        p_valor < 0.05  ~ "*",   TRUE ~ ""
      )
    ) %>%
    select(variavel, IRR, IC95, p, sig)
)

# Teste de tendência (n_esf como numérico — já em M3)
message(sprintf("\n  Teste tendência linear (M3): n_esf IRR = %.3f (p = %.4f)",
                exp(coef(M3)["n_esf"]), pvalue(M3)["n_esf"]))

# =============================================================================
# 8. Sensibilidades adicionais
# =============================================================================

message("\n=== 8. Sensibilidades adicionais ===")

# 8a. Apenas CS com dados CNES em ≥ 90% dos meses 2023-2025
cs_cnes_ok <- painel %>%
  filter(ano_cmpt %in% 2023:2025) %>%
  group_by(nome_cs) %>%
  summarise(pct_cnes = mean(!is.na(n_esf)), .groups = "drop") %>%
  filter(pct_cnes >= 0.9) %>%
  pull(nome_cs)

message(sprintf("  8a. CS com CNES ≥ 90%% dos meses: %d / %d",
                length(cs_cnes_ok), n_distinct(painel$nome_cs)))

M3_cnes_ok <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 + n_esf + n_emulti | nome_cs + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel_cnes %>% filter(nome_cs %in% cs_cnes_ok),
  vcov   = ~nome_cs
)
res_cnes_ok <- extrai_irr(M3_cnes_ok, "M3_cnes_completo")
message(sprintf("    n_esf IRR = %.3f (p = %.4f) — N = %s",
                exp(coef(M3_cnes_ok)["n_esf"]),
                pvalue(M3_cnes_ok)["n_esf"],
                format(nobs(M3_cnes_ok), big.mark=",")))

# 8b. Excluindo CS com < 70% dos meses com n_icsap > 0 (proxy de cobertura geográfica)
cs_geo_ok <- painel %>%
  group_by(nome_cs) %>%
  summarise(pct_pos = mean(n_icsap > 0), .groups = "drop") %>%
  filter(pct_pos >= 0.70) %>%
  pull(nome_cs)

message(sprintf("  8b. CS com n_icsap > 0 em ≥ 70%% dos meses: %d / %d",
                length(cs_geo_ok), n_distinct(painel$nome_cs)))

M1_geo_ok <- feglm(
  n_icsap ~ mes_num + sin12 + cos12 | nome_cs + ano_fac,
  offset = ~log(pop_cs),
  family = "poisson",
  data   = painel %>% filter(nome_cs %in% cs_geo_ok),
  vcov   = ~nome_cs
)
res_geo_ok <- extrai_irr(M1_geo_ok, "M1_geo_70pct")
message(sprintf("    mes_num IRR = %.4f (p = %.4f) — N = %s",
                exp(coef(M1_geo_ok)["mes_num"]),
                pvalue(M1_geo_ok)["mes_num"],
                format(nobs(M1_geo_ok), big.mark=",")))

# =============================================================================
# 9. Tabela comparativa: GEE AR-1 vs Poisson FE
# =============================================================================

message("\n=== 9. Comparação GEE AR-1 vs Poisson FE ===")

tryCatch({
  glm_res <- read_csv(file.path(DIR_PROC, "glm_resultados.csv"),
                      show_col_types = FALSE)

  vars_compar <- c("pct_sem_saneamento", "ivs_score", "n_esf",
                   "renda_media", "pct_area_favela", "mes_num")

  gee_comp <- glm_res %>%
    filter(variavel %in% vars_compar) %>%
    transmute(
      metodo   = "GEE AR-1",
      modelo,
      variavel,
      irr      = RR,
      ic_inf   = RR_ic_inf,
      ic_sup   = RR_ic_sup,
      p_valor
    )

  pfe_comp <- resultados %>%
    filter(variavel %in% vars_compar) %>%
    mutate(metodo = "Poisson FE") %>%
    select(metodo, modelo, variavel, irr, ic_inf, ic_sup, p_valor)

  tabela_comp <- bind_rows(gee_comp, pfe_comp) %>%
    mutate(
      IRR  = sprintf("%.3f", irr),
      IC95 = sprintf("%.3f–%.3f", ic_inf, ic_sup),
      p    = sprintf("%.4f", p_valor),
      sig  = case_when(
        p_valor < 0.001 ~ "***", p_valor < 0.01 ~ "**",
        p_valor < 0.05  ~ "*",   p_valor < 0.10 ~ ".",
        TRUE ~ ""
      )
    ) %>%
    select(metodo, modelo, variavel, IRR, IC95, p, sig) %>%
    arrange(variavel, metodo)

  message("\n  GEE AR-1 vs Poisson FE — preditores comparáveis:")
  print(tabela_comp, n = 40)

  write_csv(tabela_comp, file.path(DIR_PROC, "comparacao_gee_poisson.csv"))
  message("  Salvo: data/processed/comparacao_gee_poisson.csv")

}, error = function(e) {
  message("  AVISO: comparação GEE falhou — ", conditionMessage(e))
})

# =============================================================================
# 10. Forest plot — IRR com IC 95%
# =============================================================================

message("\n=== 10. Forest plot ===")

# Variáveis de interesse (excluindo sazonalidade e intercepto)
vars_plot <- c("mes_num", "ivs_score", "pct_area_favela",
               "renda_media", "pct_sem_saneamento", "n_esf", "n_emulti")

labels_map <- c(
  mes_num            = "Tendência mensal",
  ivs_score          = "IVS score",
  pct_area_favela    = "% área de favela",
  renda_media        = "Renda média (SM)",
  pct_sem_saneamento = "% sem saneamento",
  n_esf              = "N° equipes ESF",
  n_emulti           = "N° equipes eMulti"
)

plot_data <- bind_rows(
  res_M1, res_M2, res_M3,
  if (!is.null(res_nb)) res_nb
) %>%
  filter(variavel %in% vars_plot) %>%
  mutate(
    label  = recode(variavel, !!!labels_map),
    modelo = factor(modelo,
                    levels = c("M1_base", "M2_contextual",
                               "M3_estrutural", "M1_NB"),
                    labels = c("M1: baseline\n(CS + ano FE)",
                               "M2: contextual\n(regional + ano FE)",
                               "M3: estrutural\n(CS FE + n_ESF, 23-25)",
                               "M1: NB (sensibilidade)"))
  ) %>%
  # Remove IRR fora de [0.5, 2.5] para escala legível
  filter(ic_inf > 0.3, ic_sup < 5)

p_forest <- ggplot(plot_data,
                   aes(x = irr, y = label, color = modelo, shape = modelo)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50",
             linewidth = 0.6) +
  geom_pointrange(
    aes(xmin = ic_inf, xmax = ic_sup),
    position = position_dodge(width = 0.65),
    linewidth = 0.5, size = 0.4
  ) +
  scale_x_log10(
    breaks = c(0.90, 0.95, 1.00, 1.05, 1.10, 1.20),
    labels = function(x) sprintf("%.2f", x)
  ) +
  scale_color_brewer(palette = "Set1", name = NULL) +
  scale_shape_manual(values = c(16, 17, 15, 4), name = NULL) +
  labs(
    title    = "Poisson com Efeitos Fixos — Incidence Rate Ratios (IC 95%)",
    subtitle = paste0(
      "Desfecho: n_icsap/CS/mês | Offset: log(pop Censo 2022)\n",
      "Erros padrão clusterizados por CS | fixest ",
      packageVersion("fixest")
    ),
    x       = "IRR (escala log)",
    y       = NULL,
    caption = paste0(
      "M1: CS + ano FE (7.803 obs) | M2: regional + ano FE + preditores estáticos | ",
      "M3: CS + ano FE + n_ESF (2023-2025)\n",
      "Variáveis time-invariant (IVS, renda, saneamento) não identificáveis com CS FE → apenas em M2"
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 11),
    plot.subtitle   = element_text(size = 8, color = "gray30", lineheight = 1.3),
    plot.caption    = element_text(size = 7.5, color = "gray50"),
    legend.position = "bottom",
    legend.text     = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(DIR_DOCS, "poisson_forest_plot.png"),
       p_forest, width = 11, height = 6, dpi = 150, bg = "white")
message("  Salvo: docs/poisson_forest_plot.png")

# =============================================================================
# 11. Exporta resultados completos
# =============================================================================

message("\n=== 11. Exporta resultados ===")

resultados_all <- bind_rows(
  resultados,
  res_dr,
  res_cnes_ok,
  res_geo_ok,
  if (!is.null(res_nb)) res_nb
) %>%
  mutate(
    n_obs = case_when(
      modelo == "M1_base"          ~ nobs(M1),
      modelo == "M2_contextual"    ~ nobs(M2),
      modelo == "M3_estrutural"    ~ nobs(M3),
      modelo == "M_dose_resposta"  ~ nobs(M_dr),
      modelo == "M3_cnes_completo" ~ nobs(M3_cnes_ok),
      modelo == "M1_geo_70pct"     ~ nobs(M1_geo_ok),
      TRUE                         ~ NA_integer_
    ),
    dispersao_pearson = case_when(
      str_detect(modelo, "M1") ~ disp_M1,
      str_detect(modelo, "M2") ~ disp_M2,
      str_detect(modelo, "M3") ~ disp_M3,
      TRUE                     ~ NA_real_
    ),
    fe_cs  = str_detect(modelo, "M1|M3"),
    fe_reg = str_detect(modelo, "M2")
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 5)))

write_csv(resultados_all, file.path(DIR_PROC, "poisson_resultados.csv"))
message(sprintf("  Salvo: data/processed/poisson_resultados.csv (%d linhas)",
                nrow(resultados_all)))

# =============================================================================
# 12. Resumo final
# =============================================================================

message("\n======================================")
message("POISSON EFEITOS FIXOS — RESUMO FINAL")
message(sprintf("  Painel: %s obs (%d CS × %d meses)",
                format(nrow(painel), big.mark=","),
                n_distinct(painel$nome_cs),
                nrow(meses_ref)))
message(sprintf("  M1_base: tendência IRR = %.4f/mês (p = %.4f)",
                exp(coef(M1)["mes_num"]), pvalue(M1)["mes_num"]))
message(sprintf("  M2_contextual: IVS IRR = %.3f (p = %.4f)",
                exp(coef(M2)["ivs_score"]), pvalue(M2)["ivs_score"]))
message(sprintf("  M3_estrutural: n_esf IRR = %.3f (p = %.4f)",
                exp(coef(M3)["n_esf"]), pvalue(M3)["n_esf"]))
message(sprintf("  Superdispersão: M1=%.2f | M2=%.2f | M3=%.2f",
                disp_M1, disp_M2, disp_M3))
message(sprintf("  Modelo NB: %s",
                if (!is.null(res_nb)) "ajustado" else "não necessário"))
message("Saídas:")
message("  data/processed/poisson_resultados.csv")
message("  docs/poisson_forest_plot.png")
message("======================================")
