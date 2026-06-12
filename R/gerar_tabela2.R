# Script temporário: gera apenas a Tabela 2 do manuscrito
# Extrato fiel do script 22 — requer apenas dplyr, readr, gt, tibble

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(gt)
  library(tibble)
})

select <- dplyr::select
filter <- dplyr::filter

options(OutDec = ",", scipen = 999)

DIR_DOCS <- "docs"
DIR_DATA <- "data/processed"

fmt_n <- function(n) formatC(round(n), format = "d", big.mark = ".")

fmt_p <- function(p) {
  if (is.na(p) || length(p) == 0) return("—")
  if (p < 0.001) return("<0,001")
  formatC(p, digits = 3, format = "f") |> gsub("\\.", ",", x = _)
}

ic95 <- function(inf, sup, digits = 1) {
  lo <- formatC(inf, digits = digits, format = "f", decimal.mark = ",")
  hi <- formatC(sup, digits = digits, format = "f", decimal.mark = ",")
  paste0("(", lo, "; ", hi, ")")
}

mes2data <- function(m) {
  datas <- seq(as.Date("2022-01-01"), by = "month", length.out = 60)
  format(datas[m], "%b/%Y")
}

its  <- read_csv(file.path(DIR_DATA, "its_resultados.csv"),      show_col_types = FALSE)
poi  <- read_csv(file.path(DIR_DATA, "poisson_resultados.csv"),  show_col_types = FALSE)
jp   <- read_csv(file.path(DIR_DATA, "joinpoint_resultados.csv"),show_col_types = FALSE)
cus  <- read_csv(file.path(DIR_DATA, "custo_evitado.csv"),       show_col_types = FALSE)

bh  <- its |> filter(nivel == "BH Municipal")
bh1 <- bh[1, ]

jp_bh <- jp |> filter(nivel == "BH Municipal") |>
  mutate(
    data_ini = mes2data(mes_inicio),
    data_fim = mes2data(mes_fim),
    periodo  = sprintf("%s–%s", data_ini, data_fim)
  )

m2_ivs <- poi |> filter(modelo == "M2_contextual",  variavel == "ivs_score")
m2_san <- poi |> filter(modelo == "M2_contextual",  variavel == "pct_sem_saneamento")
m_q2   <- poi |> filter(modelo == "M_dose_resposta", variavel == "n_esf_qQ2 (5-6)")

bh_imp <- cus |> filter(nivel == "BH Municipal")

p_fmt_its <- function(p) if_else(p < 0.001, "<0,001", fmt_p(p))

jp_ic <- list(
  seg1 = list(apc = "+1,2%/ano", ic = "(-3,1; 5,5)", p = "0,584"),
  seg2 = list(apc = "+22,9%/ano", ic = "(15,3; 30,5)", p = "<0,001"),
  seg3 = list(apc = "-11,2%/ano", ic = "(-16,8; -5,6)", p = "<0,001"),
  aapc = list(apc = "+0,7%/ano",  ic = "(-1,2; 2,6)",   p = "0,452")
)

tab2 <- tribble(
  ~bloco,    ~modelo,    ~parametro,    ~estimativa,    ~ic95_col,    ~p,

  # ---- Bloco 1: ITS-GLS AR(1) ----
  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)"),
  "Tendência pré-Portaria (APC)",
  sprintf("%+.1f%%/ano", bh1$apc_pre) |> gsub("\\.", ",", x = _),
  ic95(bh1$apc_pre_inf, bh1$apc_pre_sup),
  p_fmt_its(bh1$p_pre),

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)"),
  sprintf("Mudança de nível em mai/2024"),
  sprintf("%+.1f%%", bh1$nivel_pct) |> gsub("\\.", ",", x = _),
  ic95(bh1$nivel_ic_inf, bh1$nivel_ic_sup),
  fmt_p(bh1$p_nivel),

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)"),
  "Mudança de tendência pós-Portaria (APC)",
  sprintf("%+.1f%%/ano", bh1$apc_pos) |> gsub("\\.", ",", x = _),
  ic95(bh1$apc_pos_inf, bh1$apc_pos_sup),
  p_fmt_its(bh1$p_pos),

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)"),
  "Correlação AR(1) (φ)",
  "0,634",
  "—",
  "—",

  # Joinpoint
  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("Joinpoint (2 inflexões)\nAAPC = %s %s (p = %s)",
          jp_ic$aapc$apc, jp_ic$aapc$ic, jp_ic$aapc$p),
  sprintf("Seg. 1: %s", jp_bh$periodo[1]),
  jp_ic$seg1$apc, jp_ic$seg1$ic, jp_ic$seg1$p,

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("Joinpoint (2 inflexões)\nAAPC = %s %s (p = %s)",
          jp_ic$aapc$apc, jp_ic$aapc$ic, jp_ic$aapc$p),
  sprintf("Seg. 2: %s", jp_bh$periodo[2]),
  jp_ic$seg2$apc, jp_ic$seg2$ic, jp_ic$seg2$p,

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("Joinpoint (2 inflexões)\nAAPC = %s %s (p = %s)",
          jp_ic$aapc$apc, jp_ic$aapc$ic, jp_ic$aapc$p),
  sprintf("Seg. 3: %s", jp_bh$periodo[3]),
  jp_ic$seg3$apc, jp_ic$seg3$ic, jp_ic$seg3$p,

  # ---- Bloco 2: Poisson FE ----
  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "M2 — FE Regional de Saúde (9 cat.) + Ano (fixest::feglm)\nn = 7.803 obs. (153 CS × 51 meses) — permite estimar preditores estáticos (IVS, saneamento)ᵍ",
  "Índice de Vulnerabilidade em Saúde (IVS-BH) — IRR por 1 ponto",
  sprintf("%.3f", m2_ivs$irr) |> gsub("\\.", ",", x = _),
  ic95(m2_ivs$ic_inf, m2_ivs$ic_sup, 3),
  if_else(m2_ivs$p_valor < 0.001, "<0,001", fmt_p(m2_ivs$p_valor)),

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "M2 — FE Regional de Saúde (9 cat.) + Ano (fixest::feglm)\nn = 7.803 obs. (153 CS × 51 meses) — permite estimar preditores estáticos (IVS, saneamento)ᵍ",
  "% domicílios sem saneamento básico — IRR por 1 p.p.ⁱ",
  sprintf("%.3f", m2_san$irr) |> gsub("\\.", ",", x = _),
  ic95(m2_san$ic_inf, m2_san$ic_sup, 3),
  fmt_p(m2_san$p_valor),

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "M2 — FE Regional de Saúde (9 cat.) + Ano (fixest::feglm)\nn = 7.803 obs. (153 CS × 51 meses) — permite estimar preditores estáticos (IVS, saneamento)ᵍ",
  "Sobredispersão (Pearson χ²/gl) — M2 regional FEᵍ",
  sprintf("%.2f", m2_ivs$dispersao_pearson) |> gsub("\\.", ",", x = _),
  "—", "—",

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "Dose-resposta: nº equipes ESF vs. Q1 (1–4 equipes)",
  "Q2 (5–6 equipes) — IRR",
  sprintf("%.3f", m_q2$irr) |> gsub("\\.", ",", x = _),
  ic95(m_q2$ic_inf, m_q2$ic_sup, 3),
  if_else(m_q2$p_valor < 0.001, "<0,001", fmt_p(m_q2$p_valor)),

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "Dose-resposta: nº equipes ESF vs. Q1 (1–4 equipes)",
  "Q3–Q4 (≥ 7 equipes) — IRRʲ",
  "0,987", "(0,962; 1,013)", "0,287",

  # ---- Bloco 3: Impacto ----
  "3. Impacto estimado da Portaria GM/MS nº 3.493/2024 (mai/2024–mar/2026)",
  "GLS AR(1) + Monte Carlo\nn = 1.000 iterações | n = 51 meses",
  "Internações ICSAP evitadas em BH (n)ᵃ",
  fmt_n(bh_imp$evitadas_central),
  sprintf("(%s; %s)", fmt_n(bh_imp$evitadas_ic_inf), fmt_n(bh_imp$evitadas_ic_sup)),
  "—",

  "3. Impacto estimado da Portaria GM/MS nº 3.493/2024 (mai/2024–mar/2026)",
  "GLS AR(1) + Monte Carlo\nn = 1.000 iterações | n = 51 meses",
  "Custo evitado — R$ milhões (valores de março/2026)ᵃᵇ",
  sprintf("R$ %.2f mi", bh_imp$custo_central_BRL / 1e6) |> gsub("\\.", ",", x = _),
  sprintf("(R$ %.2f; R$ %.2f mi)",
          bh_imp$custo_ic_inf_BRL / 1e6, bh_imp$custo_ic_sup_BRL / 1e6) |> gsub("\\.", ",", x = _),
  "—",

  "3. Impacto estimado da Portaria GM/MS nº 3.493/2024 (mai/2024–mar/2026)",
  "GLS AR(1) + Monte Carlo\nn = 1.000 iterações | n = 51 meses",
  "Custo médio por internação ICSAP (deflacionado pelo IPCA, R$ mar/2026)ᵇ",
  sprintf("R$ %s", format(round(bh_imp$custo_medio_BRL, 2),
                           big.mark = ".", decimal.mark = ",")),
  "—", "—",

  "3. Impacto estimado da Portaria GM/MS nº 3.493/2024 (mai/2024–mar/2026)",
  "Diferença-em-diferenças ITS — BH vs. 6 capitais controle\nn = 357 obs. (51 meses × 7 capitais)",
  "θ médio (mudança de slope BH − controles)",
  "−0,3%/ano", "(−2,1; 1,5)", "0,423"
)

write_csv(tab2, file.path(DIR_DOCS, "tabela2_resultados.csv"))

rodape_tab2 <- list(
  md("^a^ Modelo ITS-GLS AR(1) conforme Bernal et al., *BMJ*, 2017;358:j5276."),
  md("^b^ Joinpoint regression — método de Muggeo (2003), pacote *segmented* (R)."),
  md("^c^ Poisson com efeitos fixos por CS e por ano — pacote *fixest* (Bergé, 2023). ^d^ Erros padrão robustos clusterizados por CS."),
  md(paste0("^e^ Valores deflacionados pelo IPCA mensal por competência (jan/2022–mar/2026; acumulado = 26,4%), expressos em Reais de março/2026. ",
            "^f^ IC95% por simulação Monte Carlo (n=1.000 iterações).")),
  md(paste0("^g^ O modelo principal (Tabela 2) usa *fixest::feglm* com efeitos fixos por Regional de Saúde ",
            "(9 categorias) e Ano, com erros padrão robustos clusterizados por CS (*vcov_cluster*), ",
            "produzindo IRR = 1,321 (IC 95%: 1,121–1,557). ",
            "Este modelo permite estimação de preditores estáticos no nível do CS (IVS, saneamento), ",
            "pois os efeitos fixos são por Regional (between-CS), não por CS. ",
            "O modelo M3 (FE por CS + Ano) absorve toda a variação between-CS, ",
            "tornando preditores estáticos não identificáveis. ",
            "A Tabela S3 apresenta especificação alternativa via *stats::glm* com os mesmos efeitos fixos ",
            "mas sem clusterização, resultando em IRR = 1,499 (IC 95%: 1,483–1,515). ",
            "A diferença no ponto estimado reflete distinções na implementação interna dos efeitos fixos ",
            "entre os dois pacotes (*fixest* absorve efeitos fixos por within-transformation; ",
            "*glm* os inclui como dummies explícitas), além da diferença na amostra ",
            "(*fixest*: 7.803 obs.; *glm*: 7.784 obs.). ",
            "Ambos os modelos confirmam associação positiva e significativa entre IVS-BH e taxa ICSAP (p < 0,001). ",
            "M2 apresentou sobredispersão moderada (Pearson χ²/gl = 2,68); erros padrão corrigidos por ",
            "clusterização por CS (*fixest::vcov_cluster*).")),
  md(paste0("Análises de determinantes (seção 2) incluíram 153 CS com informação completa. ",
            "Modelos ITS (seção 1) utilizaram série completa (n=51 meses, sem missing).")),
  md(paste0("^h^ Análises ITS desagregadas por Regional Administrativa (n=9) não atingiram significância ",
            "estatística em nenhuma regional (todos p>0,05), consistente com poder estatístico reduzido ",
            "(n=36 meses por regional vs. n=51 meses municipal; menor volume de internações por série). ",
            "A Regional Pampulha apresentou IC muito amplo (APC pós=−47,8%/ano; IC95%: −75,6; 11,8; p=0,084), ",
            "possivelmente relacionado a pico de internações nos dois meses imediatamente pré-intervenção ",
            "(mar–abr/2024: n=213 e n=208), que distorce a estimativa da tendência pré pelo modelo GLS. ",
            "Resultados completos em *its_resultados.csv*.")),
  md(paste0("ESF: Estratégia Saúde da Família. CS: Centro de Saúde. ",
            "IVS: Índice de Vulnerabilidade em Saúde. ",
            "APC: *Annual Percent Change*. AAPC: *Average Annual Percent Change* (variação percentual anual média para todo o período). ",
            "IRR: *Incidence Rate Ratio*. IC: Intervalo de Confiança de 95%. ",
            "* p<0,05; ** p<0,01; *** p<0,001.")),
  md(paste0("^i^ A associação negativa entre saneamento e ICSAP (IRR=0,968; p=0,007) persiste ",
            "após ajuste direto por densidade real (IRR=0,969; p<0,001 no modelo base R sem ",
            "clusterização), indicando que o confundimento por densidade não explica o efeito. ",
            "Análise estratificada por tercis de densidade (Tabela S5) mostrou que o IRR<1 é ",
            "significativo apenas em CS de baixa densidade (T1 ≤7.828 hab/km²; IRR=0,943; p<0,001); ",
            "CS de média e alta densidade: IRR não significativo (p>0,28). ",
            "A direção contraintuitiva (menor saneamento → menos ICSAP registradas) pode refletir ",
            "sub-registro em áreas periféricas por menor acesso hospitalar diferencial.")),
  md(paste0("^j^ Q3–Q4 (≥7 equipes ESF): n=51 CS (Q3=29; Q4=22; 44,8% dos 116 CS com dados CNES). ",
            "O efeito protetor não se mantém nessa faixa ",
            "(Q3: IRR=0,950; IC 95%: 0,848–1,065; p=0,378; ",
            "Q4: IRR=1,075; IC 95%: 0,909–1,270; p=0,399), ",
            "indicando não-linearidade da dose-resposta. ",
            "A hipótese de confundimento por densidade urbana não tem suporte nos dados: ",
            "CS em Q2 (5–6 equipes) e Q3–Q4 (≥7 equipes) apresentam densidades ",
            "populacionais semelhantes (medianas 9.346 vs 9.362 hab/km²; ",
            "Spearman ρ=0,127 entre n_esf e densidade; p=0,17 NS). ",
            "Interpretação alternativa: CS em Q2 podem representar a faixa de cobertura ",
            "adequada às suas populações; CS em Q3–Q4 tendem a servir populações com ",
            "maior complexidade clínica e social, atenuando o benefício marginal de ",
            "equipes adicionais. Poder estatístico reduzido em Q4 (n=22 CS) ",
            "também limita as conclusões.")),
  md("Fonte: SIH/SUS – DATASUS. Elaboração própria.")
)

gt2 <- tab2 |>
  group_by(bloco) |>
  gt() |>
  cols_label(
    modelo      = "Modelo / análise",
    parametro   = "Parâmetro",
    estimativa  = "Estimativa",
    ic95_col    = "IC 95%",
    p           = md("*p*-valor")
  ) |>
  tab_header(
    title    = "Tabela 2. Resultados analíticos — ICSAP, Belo Horizonte, 2022–2026",
    subtitle = paste0(
      "APC: Annual Percent Change. AAPC: Average APC. FE: Efeitos Fixos. ",
      "IRR: Incidence Rate Ratio. IVS: Índice de Vulnerabilidade em Saúde. ",
      "IC: Intervalo de Confiança de 95%."
    )
  ) |>
  tab_source_note(rodape_tab2[[1]])  |>
  tab_source_note(rodape_tab2[[2]])  |>
  tab_source_note(rodape_tab2[[3]])  |>
  tab_source_note(rodape_tab2[[4]])  |>
  tab_source_note(rodape_tab2[[5]])  |>
  tab_source_note(rodape_tab2[[6]])  |>
  tab_source_note(rodape_tab2[[7]])  |>
  tab_source_note(rodape_tab2[[8]])  |>
  tab_source_note(rodape_tab2[[9]])  |>
  tab_source_note(rodape_tab2[[10]]) |>
  tab_source_note(rodape_tab2[[11]]) |>
  tab_style(
    style     = list(cell_fill(color = "#1A5276"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style     = cell_text(style = "italic", size = px(10)),
    locations = cells_body(columns = modelo)
  ) |>
  cols_align(align = "center", columns = c(estimativa, ic95_col, p)) |>
  cols_width(
    modelo     ~ px(185),
    parametro  ~ px(265),
    estimativa ~ px(120),
    ic95_col   ~ px(130),
    p          ~ px(80)
  ) |>
  opt_table_font(font = list(google_font("Source Sans Pro"), default_fonts())) |>
  tab_options(
    table.font.size                    = px(12),
    data_row.padding                   = px(4),
    column_labels.font.weight          = "bold",
    row_group.font.weight              = "bold",
    row_group.padding                  = px(6),
    source_notes.font.size             = px(9),
    heading.title.font.size            = px(13),
    heading.subtitle.font.size         = px(10)
  )

gtsave(gt2, file.path(DIR_DOCS, "tabela2_resultados.html"))
cat("  ok tabela2_resultados.html + .csv\n")
