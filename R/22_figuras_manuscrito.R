# encoding: UTF-8
# R/22_figuras_manuscrito.R
# Figuras e tabelas finais — manuscrito ICSAP-BH
# Padrão Lancet / Cadernos de Saúde Pública — 300 DPI, 170 mm

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(gt)
  library(sf)
  library(viridis)
  library(ggspatial)
  library(classInt)
  library(zoo)
  library(ggrepel)  # anti-sobreposição de labels — auditoria 23/05/2026
  library(ragg)     # renderização de texto Unicode no Windows
})

options(OutDec = ",", scipen = 999)

DPI      <- 300
W_IN     <- 170 / 25.4   # 6.693 pol
DIR_DOCS <- "docs"
DIR_DATA <- "data/processed"
DIR_RAW  <- "data/raw"
DIR_REF  <- "data/ref"

# ---- Helpers ----------------------------------------------------------------
fmt_n   <- function(n) formatC(round(n), format = "d", big.mark = ".")
# fmt_pct: usa formatC para garantir vírgula decimal em qualquer locale
fmt_pct <- function(n, tot, d = 1) {
  pct <- formatC(n / tot * 100, digits = d, format = "f", decimal.mark = ",")
  sprintf("%s (%s%%)", fmt_n(n), pct)
}
# fmt_med: adiciona gsub para garantir vírgula (sprintf não respeita OutDec)
fmt_med <- function(x) {
  q <- quantile(x, c(.25, .5, .75), na.rm = TRUE)
  sprintf("%.1f [%.1f–%.1f]", q[2], q[1], q[3]) |>
    gsub(".", ",", x = _, fixed = TRUE)
}
fmt_p <- function(p) {
  if (is.na(p) || length(p) == 0) return("—")
  if (p < 0.001) return("<0,001")
  formatC(p, digits = 3, format = "f") |> gsub("\\.", ",", x = _)
}
chi_p <- function(var, grp) {
  tab <- table(grp, var)
  if (any(dim(tab) < 2)) return(NA_real_)
  tryCatch(chisq.test(tab)$p.value, error = function(e) NA_real_)
}
mw_p <- function(x, grp) {
  tryCatch(wilcox.test(x[grp == "Pre"], x[grp == "Pos"])$p.value,
           error = function(e) NA_real_)
}

# Quebras de Jenks para mapas (robusta: deduplicada, filtra NA e Inf)
jenks_breaks_safe <- function(x, n = 5) {
  x_valid <- x[!is.na(x) & is.finite(x)]
  if (length(unique(x_valid)) < 2) {
    return(c(min(x_valid, na.rm = TRUE) - 1e-6,
             max(x_valid, na.rm = TRUE) + 1e-6))
  }
  n    <- min(n, length(unique(x_valid)))
  brks <- classIntervals(x_valid, n = n, style = "jenks")$brks
  brks <- unique(brks)
  brks[1]             <- min(x_valid) - 1e-6
  brks[length(brks)]  <- max(x_valid) + 1e-6
  brks
}

# ---- Tema padrão Lancet/CSP -------------------------------------------------
theme_lancet <- function(base_size = 8) {
  theme_classic(base_size = base_size) +
    theme(
      text               = element_text(colour = "black", family = "sans"),
      axis.text          = element_text(size = base_size - 1, colour = "black"),
      axis.title         = element_text(size = base_size),
      plot.title         = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle      = element_text(size = base_size - 1, colour = "grey40"),
      legend.text        = element_text(size = base_size - 1),
      legend.title       = element_text(size = base_size, face = "bold"),
      legend.key.size    = unit(3.5, "mm"),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.25),
      panel.grid.minor   = element_blank(),
      axis.line          = element_line(colour = "black", linewidth = 0.3),
      axis.ticks         = element_line(colour = "black", linewidth = 0.3),
      strip.background   = element_blank(),
      strip.text         = element_text(size = base_size, face = "bold"),
      plot.margin        = margin(4, 4, 4, 4, "mm")
    )
}

# =============================================================================
# FIGURA 1 — Fluxograma STROBE
# =============================================================================
cat("Gerando Figura 1 (STROBE)...\n")

mg_cache <- file.path(DIR_DATA, "mg_totais_strobe.csv")
if (!file.exists(mg_cache)) {
  cat("  Calculando totais MG a partir dos .dbc (primeira vez, ~6 min)...\n")
  suppressPackageStartupMessages(library(read.dbc))
  dbcs <- list.files(DIR_RAW, "\\.dbc$", full.names = TRUE, ignore.case = TRUE)
  totais_lista <- lapply(seq_along(dbcs), function(i) {
    cat(sprintf("    %d/%d  %s\n", i, length(dbcs), basename(dbcs[i])))
    d <- read.dbc(dbcs[i])
    bh_mov  <- as.character(d$MUNIC_MOV) == "310620"
    bh_res  <- as.character(d$MUNIC_RES) == "310620"
    data.frame(
      arquivo   = basename(dbcs[i]),
      n_mg      = nrow(d),
      n_bh_mov  = sum(bh_mov,  na.rm = TRUE),
      n_bh_res  = sum(bh_res,  na.rm = TRUE),
      n_bh_both = sum(bh_mov & bh_res, na.rm = TRUE)
    )
  })
  mg_df <- do.call(rbind, totais_lista)
  write.csv(mg_df, mg_cache, row.names = FALSE)
} else {
  cat("  Cache encontrado:", mg_cache, "\n")
}
mg_df     <- read.csv(mg_cache)
N_MG      <- sum(mg_df$n_mg)
N_BH_MOV  <- sum(mg_df$n_bh_mov)
N_BH      <- 638098L
N_ICSAP   <- 113695L
N_NAICSAP <- 524403L
N_GEO     <- 98192L
N_NGEO    <- 15503L
excl_mov  <- N_MG - N_BH_MOV
excl_res  <- N_BH_MOV - N_BH

cat(sprintf("  MG=%s | BH_mov=%s | BH_ambos=%s\n",
            fmt_n(N_MG), fmt_n(N_BH_MOV), fmt_n(N_BH)))

mk_box <- function(id, x0, y0, x1, y1, fill, lab, text_col = "black") {
  tibble(id = id, x0 = x0, y0 = y0, x1 = x1, y1 = y1, fill = fill,
         lab = lab, xm = (x0 + x1) / 2, ym = (y0 + y1) / 2,
         text_col = text_col)
}

# Layout com mais espaçamento entre caixas (auditoria 23/05/2026)
# Caixas principais: altura 0.085, gap 0.07 entre elas
boxes_main <- bind_rows(
  mk_box("A", 0.10, 0.890, 0.68, 0.975, "#D6EAF8",
         sprintf("Internações hospitalares — Minas Gerais\njaneiro de 2022 a março de 2026\nn = %s", fmt_n(N_MG))),
  mk_box("B", 0.10, 0.745, 0.68, 0.830, "#AED6F1",
         sprintf("Internações ocorridas em BH\n(município de internação = 310620)\nn = %s", fmt_n(N_BH_MOV))),
  mk_box("C", 0.10, 0.600, 0.68, 0.685, "#1B4F72",
         sprintf("Total de internações em BH\n(residentes e internados em BH)\nn = %s", fmt_n(N_BH)),
         "white"),
  mk_box("D1", 0.10, 0.450, 0.68, 0.535, "#2874A6",
         sprintf("ICSAP — Internações por Condições Sensíveis à Atenção Primária\nPortaria SAS/MS nº 221/2008 — 479 códigos CID-10\nn = %s (17,8%%)", fmt_n(N_ICSAP)),
         "white"),
  mk_box("E1", 0.10, 0.295, 0.68, 0.380, "#2980B9",
         sprintf("CEP geocodificado — Centro de Saúde (CS) identificado\nn = %s (86,4%%)", fmt_n(N_GEO)),
         "white")
)

boxes_analysis <- bind_rows(
  mk_box("F1", 0.02, 0.060, 0.43, 0.175, "#1B4F72",
         sprintf("Análise temporal\n(ITS-GLS AR(1) + Joinpoint regression)\nn = %s ICSAP | 51 meses", fmt_n(N_ICSAP)),
         "white"),
  mk_box("F2", 0.45, 0.060, 0.99, 0.175, "#154360",
         sprintf("Análise espacial\n(Índice de Moran + GEE AR(1) + Poisson com efeitos fixos)\n153 CS × 51 meses | n = %s", fmt_n(N_GEO)),
         "white")
)

boxes_excl <- bind_rows(
  mk_box("X1", 0.72, 0.890, 0.99, 0.975, "#BDC3C7",
         sprintf("Excluídos:\nMunicípio de internação ≠ Belo Horizonte\n(código IBGE 310620)\nn = %s", fmt_n(excl_mov))),
  mk_box("X2", 0.72, 0.745, 0.99, 0.830, "#BDC3C7",
         sprintf("Excluídos:\nMunicípio de residência ≠ Belo Horizonte\n(código IBGE 310620)\nn = %s", fmt_n(excl_res))),
  mk_box("X3", 0.72, 0.600, 0.99, 0.685, "#FADBD8",
         sprintf("Não classificadas como ICSAP\n(excluídas da análise)\nn = %s (82,2%%)", fmt_n(N_NAICSAP))),
  mk_box("X4", 0.72, 0.450, 0.99, 0.535, "#FAD7A0",
         sprintf("Sem geocodificação de CEP\n(padrão MNAR — analisado separadamente)\nn = %s (13,6%%)", fmt_n(N_NGEO)))
)

segs_v <- tribble(
  ~x,    ~y,     ~xend, ~yend,
  0.39,  0.890,  0.39,  0.832,
  0.39,  0.745,  0.39,  0.687,
  0.39,  0.600,  0.39,  0.537,
  0.39,  0.450,  0.39,  0.382,
  0.39,  0.295,  0.22,  0.177,
  0.39,  0.295,  0.72,  0.177
)

segs_h <- tribble(
  ~x,    ~y,     ~xend, ~yend,
  0.68,  0.933,  0.72,  0.933,
  0.68,  0.788,  0.72,  0.788,
  0.68,  0.643,  0.72,  0.643,
  0.68,  0.493,  0.72,  0.493
)

fig1 <- ggplot() +
  geom_rect(data = boxes_excl,
            aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill),
            colour = "grey50", linewidth = 0.3) +
  geom_text(data = boxes_excl,
            aes(x = xm, y = ym, label = lab),
            size = 2.0, lineheight = 1.15, family = "sans", colour = "black") +
  geom_rect(data = boxes_analysis,
            aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill),
            colour = "grey25", linewidth = 0.4) +
  geom_text(data = boxes_analysis,
            aes(x = xm, y = ym, label = lab, colour = text_col),
            size = 2.1, lineheight = 1.15, family = "sans") +
  geom_rect(data = boxes_main,
            aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill),
            colour = "grey25", linewidth = 0.45) +
  geom_text(data = boxes_main,
            aes(x = xm, y = ym, label = lab, colour = text_col),
            size = 2.2, lineheight = 1.2, family = "sans") +
  scale_fill_identity() +
  scale_colour_identity() +
  geom_segment(data = segs_v,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
               colour = "grey30", linewidth = 0.4) +
  geom_segment(data = segs_h,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
               colour = "grey40", linewidth = 0.35) +
  annotate("text",
           x = 0.01, y = 0.01,
           hjust = 0, vjust = 1,
           size = 2.1, lineheight = 1.45, colour = "grey30",
           label = paste0(
             "AR(1): estrutura autorregressiva de 1ª ordem. ",
             "BH: Belo Horizonte. CEP: Código de Endereçamento Postal.\n",
             "CID-10: Classificação Internacional de Doenças, 10ª revisão. ",
             "CS: Centro de Saúde. GEE: Equações de Estimação Generalizada.\n",
             "GLS: Mínimos Quadrados Generalizados. ",
             "ICSAP: Internações por Condições Sensíveis à Atenção Primária.\n",
             "ITS: Interrupção de Série Temporal. ",
             "MNAR: dado ausente de forma não aleatória (Missing Not at Random).\n",
             "STROBE: Von Elm et al., Lancet. 2007;370(9596):1453-7."
           )) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.20, 1.02)) +
  theme_void() +
  labs(
    title = paste0(
      "Figura 1. Fluxo de seleção das internações por condições sensíveis\n",
      "à atenção primária (ICSAP). Belo Horizonte, Minas Gerais, Brasil,\n",
      "janeiro de 2022 a março de 2026."
    )
  ) +
  theme(
    plot.title      = element_text(size = 9, face = "bold", hjust = 0,
                                   margin = margin(b = 2)),
    plot.margin     = margin(6, 6, 4, 6, "mm"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ragg::agg_png(
  filename   = file.path(DIR_DOCS, "figura1_fluxograma_strobe.png"),
  width      = 8.5, height = 12, units = "in",
  res        = DPI, background = "white"
)
print(fig1)
dev.off()
cat("  ok figura1_fluxograma_strobe.png\n")

# =============================================================================
# FIGURA 2 — ITS com 4 painéis (2×2)
# =============================================================================
cat("Gerando Figura 2 (ITS 4 painéis)...\n")

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(zoo)
})

ev <- read_csv(file.path(DIR_DATA, "internacoes_evitadas.csv"),
               show_col_types = FALSE) |>
  mutate(
    data = as.Date(data),
    periodo = if_else(data < as.Date("2024-05-01"),
                      "Pré-intervenção", "Pós-intervenção"),
    periodo = factor(periodo, levels = c("Pré-intervenção", "Pós-intervenção")),
    ma3 = rollmean(n_icsap, k = 3, fill = NA, align = "center")
  )

custo_medio <- read_csv(file.path(DIR_DATA, "custo_evitado.csv"),
                        show_col_types = FALSE) |>
  filter(nivel == "BH Municipal") |>
  pull(custo_medio_BRL)
cat(sprintf("  custo_medio (IPCA mensal): R$ %.2f\n", custo_medio))

ev_pos <- filter(ev,
  !is.na(taxa_cf),
  data <= as.Date("2026-03-01")
) |>
  mutate(
    custo_mes_central = evitadas_mes * custo_medio,
    evit_inf = pmax(0, (cf_ic_inf - taxa_obs) * n_total / 100),
    evit_sup = pmax(0, (cf_ic_sup - taxa_obs) * n_total / 100),
    custo_acum        = cumsum(custo_mes_central) / 1e6,
    custo_acum_inf    = cumsum(evit_inf * custo_medio) / 1e6,
    custo_acum_sup    = cumsum(evit_sup * custo_medio) / 1e6
  )

ev_pos_plot <- ev_pos |> filter(data <= as.Date("2026-03-01"))

d_int   <- as.Date("2024-05-01")
y_max_a <- max(ev$n_icsap, na.rm = TRUE)
y_max_b <- ceiling(max(
  ev$taxa_obs[ev$data <= as.Date("2026-03-01")],
  ev_pos_plot$cf_ic_sup,
  na.rm = TRUE)) + 4
y_min_b <- floor(min(ev$taxa_obs, ev_pos_plot$cf_ic_inf, na.rm = TRUE)) - 2

theme_lancet_large <- function(base_size = 9) {
  theme_classic(base_size = base_size) +
    theme(
      text            = element_text(colour = "black", family = "sans"),
      axis.text       = element_text(size = base_size, colour = "black"),
      axis.title      = element_text(size = base_size + 1),
      plot.title      = element_text(size = base_size + 3, face = "bold",
                                     hjust = 0.5),
      legend.text     = element_text(size = base_size - 1),
      legend.title    = element_text(size = base_size, face = "bold"),
      legend.key.size = unit(4, "mm"),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      axis.line  = element_line(colour = "black", linewidth = 0.4),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      plot.margin = margin(12, 15, 10, 10, "mm")
    )
}

# --- Painel A: n absoluto + média móvel ---
p2a <- ggplot(ev, aes(x = data, y = n_icsap, fill = periodo)) +
  geom_col(width = 26, colour = NA, alpha = 0.85) +
  geom_line(aes(y = ma3), colour = "#0D3349", linewidth = 1.0, na.rm = TRUE) +
  geom_vline(xintercept = d_int, linetype = "dashed",
             colour = "#C0392B", linewidth = 0.7) +
  annotate("text",
           x = as.Date("2024-07-01"), y = y_max_a * 0.90,
           label = "Portaria GM/MS nº 3.493/2024",
           size = 3.0, hjust = 0, colour = "#C0392B",
           lineheight = 1.2, fontface = "bold") +
  annotate("text",
           x = as.Date("2024-03-01"),
           y = max(ev$n_icsap) * 0.92,
           label = "Epidemia\ndengue 2024",
           size = 2.0, colour = "#8B0000",
           hjust = 0.5, fontface = "italic") +
  scale_fill_manual(
    values = c("Pré-intervenção" = "#90CAF9", "Pós-intervenção" = "#1565C0"),
    name = NULL) +
  scale_x_date(
    limits      = c(as.Date("2022-01-01"), as.Date("2026-04-01")),
    breaks      = seq(as.Date("2022-01-01"), as.Date("2026-01-01"), by = "6 months"),
    date_labels = "%b/%Y",
    expand      = expansion(mult = 0.01)) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.10))) +
  labs(x = NULL, y = "Internações ICSAP (n)", title = "A") +
  theme_lancet_large() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = c(0.12, 0.85),
        legend.background = element_blank(),
        legend.key.size = unit(3, "mm"))

# --- Painel B: taxa + contrafactual + APCs ---
p2b <- ggplot(ev, aes(x = data)) +
  geom_ribbon(data = ev_pos_plot,
              aes(ymin = cf_ic_inf, ymax = cf_ic_sup),
              fill = "#FFCDD2", alpha = 0.25) +
  geom_line(data = ev_pos_plot,
            aes(y = taxa_cf), colour = "#C0392B",
            linetype = "dashed", linewidth = 0.9) +
  geom_line(aes(y = taxa_obs), colour = "#1565C0", linewidth = 1.0) +
  geom_point(aes(y = taxa_obs, colour = periodo), size = 1.2,
             shape = 16, show.legend = FALSE) +
  geom_vline(xintercept = d_int, linetype = "dashed",
             colour = "#C0392B", linewidth = 0.7) +
  annotate("label",
           x = as.Date("2023-02-01"), y = y_max_b - 2,
           label = "APC pré: +12,3%/ano\n(IC95%: 5,8; 19,2; p<0,001)",
           size = 2.2, hjust = 0, colour = "#1565C0", fill = "white",
           label.padding = unit(1.5, "mm"), label.size = 0.2) +
  annotate("label",
           x = as.Date("2024-10-01"), y = y_max_b - 2,
           label = "APC pós: -8,3%/ano\n(IC95%: -15,2; -0,8; p<0,001)",
           size = 2.2, hjust = 0, colour = "#C0392B", fill = "white",
           label.padding = unit(1.5, "mm"), label.size = 0.2) +
  annotate("text",
           x = as.Date("2024-07-01"), y = y_max_b - 4.0,
           label = "Δ tendência: -20,6%/ano (p<0,001)",
           size = 2.5, hjust = 0, colour = "grey30", fontface = "italic") +
  annotate("text",
           x = as.Date("2025-09-01"),
           y = max(ev_pos_plot$taxa_cf, na.rm = TRUE) * 0.85,
           label = "Contrafactual:\ntendência\npré projetada",
           size = 1.8, colour = "#C0392B", hjust = 0.5, fontface = "italic") +
  scale_x_date(
    limits      = c(as.Date("2022-01-01"), as.Date("2026-04-01")),
    breaks      = seq(as.Date("2022-01-01"), as.Date("2026-01-01"), by = "6 months"),
    date_labels = "%b/%Y",
    expand      = expansion(mult = 0.01)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(y_min_b, y_max_b),
    breaks = seq(floor(y_min_b / 5) * 5, ceiling(y_max_b / 5) * 5, by = 5)) +
  scale_colour_manual(
    values = c("Pré-intervenção" = "#90CAF9", "Pós-intervenção" = "#1565C0")) +
  labs(x = "Competência (mês/ano)", y = "Taxa ICSAP (%)", title = "B") +
  theme_lancet_large() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# --- Painel C: internações evitadas por mês ---
ev_c <- ev |>
  mutate(
    evit_bar = if_else(!is.na(taxa_cf), evitadas_mes, 0),
    bar_pos  = evit_bar >= 0
  )
max_evit <- max(ev_pos$evitadas_mes, na.rm = TRUE)

p2c <- ggplot(ev_c, aes(x = data)) +
  geom_col(aes(y = evit_bar, fill = bar_pos),
           alpha = 0.85, width = 26, colour = NA) +
  scale_fill_manual(
    values = c("TRUE"  = "#2E7D32",
               "FALSE" = "#C62828"),
    labels = c("TRUE"  = "Internações evitadas",
               "FALSE" = "Excesso em mai/2024"),
    name   = NULL,
    breaks = c("TRUE", "FALSE")) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = d_int, linetype = "dashed",
             colour = "#C0392B", linewidth = 0.7) +
  annotate("text",
           x = as.Date("2025-04-01"), y = max_evit * 0.92,
           label = "Total evitado: 13.501\n(IC95%: 5.189–22.575)",
           size = 2.4, hjust = 0.5, colour = "#1B5E20", fontface = "bold") +
  scale_x_date(
    limits      = c(as.Date("2022-01-01"), as.Date("2026-04-01")),
    breaks      = seq(as.Date("2022-01-01"), as.Date("2026-01-01"), by = "6 months"),
    date_labels = "%b/%Y",
    expand      = expansion(mult = 0.01)) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.12, 0.10))) +
  labs(x = "Competência (mês/ano)",
       y = "Internações evitadas (n/mês)", title = "C") +
  theme_lancet_large() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = c(0.20, 0.88),
        legend.background = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 6))

# --- Painel D: custo evitado acumulado ---
p2d <- ggplot(ev_pos, aes(x = data)) +
  geom_ribbon(aes(ymin = custo_acum_inf, ymax = custo_acum_sup),
              fill = "#A5D6A7", alpha = 0.15) +
  geom_line(aes(y = custo_acum), colour = "#2E7D32", linewidth = 1.0) +
  geom_vline(xintercept = d_int, linetype = "dashed",
             colour = "#C0392B", linewidth = 0.7) +
  annotate("label",
           x = as.Date("2025-03-01"), y = 35,
           label = "R$ 29,05 milhões\n(IC95%: 11,16–48,57)",
           hjust = 0.5, fill = "white", colour = "#2E7D32",
           label.size = 0.25, size = 2.6, fontface = "bold") +
  scale_x_date(
    limits      = c(as.Date("2024-05-01"), as.Date("2026-04-01")),
    breaks      = seq(as.Date("2024-07-01"), as.Date("2026-01-01"), by = "3 months"),
    date_labels = "%b/%Y",
    expand      = expansion(mult = 0.02)) +
  scale_y_continuous(
    limits = c(0, 45),
    labels = label_number(accuracy = 0.1, big.mark = ".", decimal.mark = ",")) +
  labs(x = "Competência (mês/ano)",
       y = "Custo evitado acumulado\n(R$ milhões, março/2026)", title = "D") +
  theme_lancet_large() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# --- Composição final ---
fig2_final <- (p2a | p2b) / (p2c | p2d) +
  plot_annotation(
    caption = stringr::str_wrap(
      paste0(
        "Figura 2. Séries temporais de internações por condições sensíveis ",
        "à atenção primária (ICSAP). Belo Horizonte, Minas Gerais, Brasil, ",
        "janeiro de 2022 a março de 2026. ",
        "(A) Número absoluto de internações ICSAP por mês; linha azul escura = ",
        "média móvel de 3 meses. (B) Taxa ICSAP observada (linha sólida azul) e ",
        "contrafactual estimado (linha vermelha tracejada; faixa rosa = IC 95%). ",
        "(C) Internações ICSAP evitadas por mês no período pós-Portaria ",
        "(diferença entre contrafactual e observado). (D) Custo evitado ",
        "acumulado (R$ milhões, valores de março/2026, deflacionados pelo IPCA). ",
        "A linha vertical tracejada indica o início da vigência da Portaria ",
        "GM/MS nº 3.493/2024 (maio/2024). APC: Annual Percent Change. ",
        "IC: Intervalo de Confiança de 95%. Modelo: ITS com GLS e correção AR(1). ",
        "O pico de internações em abril/2024 foi atribuído à epidemia de dengue ",
        "(grupo G04: +3.570% vs média mensal de 2023). Análise de sensibilidade ",
        "excluindo março–abril/2024 produziu APC pós = −10,1%/ano ",
        "(IC 95%: −14,1 a −5,9), indicando que a epidemia atenuou, não criou, ",
        "o efeito estimado da Portaria (ver Tabela S6). ",
        "Fonte: SIH/SUS – DATASUS. Elaboração própria."
      ),
      width = 140
    ),
    theme = theme(
      plot.caption = element_text(size = 7.5, colour = "grey35", hjust = 0,
                                  lineheight = 1.4,
                                  margin = margin(t = 10, b = 5))
    )
  )

ragg::agg_png(
  filename   = file.path(DIR_DOCS, "figura2_its_4paineis.png"),
  width      = 14, height = 14, units = "in",
  res        = 300, background = "white"
)
print(fig2_final)
dev.off()
cat("Figura 2 salva: 14\" x 14\" a 300 DPI\n")

# =============================================================================
# FIGURA 3 — Mapa quádruplo (2×2)
# =============================================================================
cat("Gerando Figura 3 (mapa quádruplo)...\n")

sf_cs <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE) |>
  st_make_valid() |>
  st_transform(4326)

sf_reg <- sf_cs |>
  group_by(regional) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

tv2 <- read_csv(file.path(DIR_DATA, "taxas_padronizadas_v2.csv"),
                show_col_types = FALSE) |>
  group_by(nome_cs) |>
  summarise(taxa_pad_media = mean(taxa_padronizada, na.rm = TRUE), .groups = "drop")

evi_cs <- read_csv(file.path(DIR_DATA, "internacoes_evitadas_cs.csv"),
                   show_col_types = FALSE) |>
  select(nome_cs, regional, evitadas_central)

# Agrega por regional
tv2_reg <- tv2 |>
  left_join(evi_cs |> select(nome_cs, regional), by = "nome_cs") |>
  group_by(regional) |>
  summarise(taxa_pad_media_reg = mean(taxa_pad_media, na.rm = TRUE), .groups = "drop")

evi_reg <- evi_cs |>
  group_by(regional) |>
  summarise(evitadas_reg = sum(evitadas_central, na.rm = TRUE), .groups = "drop")

sf_reg_map <- sf_reg |>
  left_join(tv2_reg, by = "regional") |>
  left_join(evi_reg, by = "regional")

sf_cs_map <- sf_cs |>
  left_join(tv2,    by = "nome_cs") |>
  left_join(evi_cs, by = "nome_cs")

cat(sprintf("  Regionais: taxa=%d, evitadas=%d\n",
            sum(!is.na(sf_reg_map$taxa_pad_media_reg)),
            sum(!is.na(sf_reg_map$evitadas_reg))))
cat(sprintf("  CS: taxa=%d, evitadas=%d (de %d)\n",
            sum(!is.na(sf_cs_map$taxa_pad_media)),
            sum(!is.na(sf_cs_map$evitadas_central)),
            nrow(sf_cs_map)))

# Função para corrigir acentuação dos nomes (GeoJSON usa caixa alta sem diacríticos)
fix_accents_cs <- function(x) {
  x |>
    str_replace_all("AARAO", "AARÃO") |>
    str_replace_all("\\bSAO\\b", "SÃO") |>
    str_replace_all("CRISTOVAO", "CRISTÓVÃO") |>
    str_replace_all("\\bJOAO\\b", "JOÃO") |>
    str_replace_all("\\bJOSE\\b", "JOSÉ") |>
    str_replace_all("\\bOPERARIO\\b", "OPERÁRIO") |>
    str_replace_all("\\bNAZARE\\b", "NAZARÉ") |>
    str_replace_all("\\bGLORIA\\b", "GLÓRIA") |>
    str_replace_all("\\bGOIANIA\\b", "GOIÂNIA") |>
    str_replace_all("\\bEUSTAQUIO\\b", "EUSTÁQUIO") |>
    str_replace_all("\\bTARCISIO\\b", "TARCÍSIO") |>
    str_replace_all("\\bCICERO\\b", "CÍCERO")
}

# Labels para ggrepel — extrair coordenadas explicitamente (evita sobreposição)
top3_taxa <- sf_cs_map |> st_drop_geometry() |>
  filter(taxa_pad_media > quantile(taxa_pad_media, 0.95, na.rm = TRUE)) |>
  slice_max(taxa_pad_media, n = 3, na_rm = TRUE) |>
  mutate(label_cs = fix_accents_cs(str_remove(nome_cs, "^CENTRO DE SAUDE ")))

top5_evit <- sf_cs_map |> st_drop_geometry() |>
  filter(evitadas_central > quantile(evitadas_central, 0.95, na.rm = TRUE)) |>
  slice_max(evitadas_central, n = 3, na_rm = TRUE) |>
  mutate(label_cs = fix_accents_cs(str_remove(nome_cs, "^CENTRO DE SAUDE ")))

# Centroides CS para top labels
sf_lab3 <- suppressWarnings(
  sf_cs_map |> filter(nome_cs %in% top3_taxa$nome_cs) |>
    st_transform(31983) |> st_point_on_surface() |> st_transform(4326)) |>
  mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  left_join(top3_taxa |> select(nome_cs, label_cs), by = "nome_cs")

sf_lab5 <- suppressWarnings(
  sf_cs_map |> filter(nome_cs %in% top5_evit$nome_cs) |>
    st_transform(31983) |> st_point_on_surface() |> st_transform(4326)) |>
  mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry() |>
  left_join(top5_evit |> select(nome_cs, label_cs), by = "nome_cs")

# Pontos dentro dos polígonos regionais para ggrepel
sf_reg_labels <- suppressWarnings(
  sf_reg_map |> st_transform(31983) |> st_point_on_surface() |> st_transform(4326)) |>
  mutate(X = st_coordinates(geometry)[, 1], Y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

# Quebras Jenks para cada variável
brk_taxa_reg  <- jenks_breaks_safe(sf_reg_map$taxa_pad_media_reg,  n = 5)
brk_evit_reg  <- jenks_breaks_safe(sf_reg_map$evitadas_reg,        n = 5)
brk_taxa_cs   <- jenks_breaks_safe(sf_cs_map$taxa_pad_media,       n = 5)
brk_evit_cs   <- jenks_breaks_safe(sf_cs_map$evitadas_central,     n = 5)

fmt_brk <- function(b) {
  # Usa 1 casa decimal para evitar repetição do mesmo valor nos limites adjacentes
  vals <- formatC(b, digits = 1, format = "f", decimal.mark = ",")
  paste0(vals[-length(vals)], "-", vals[-1])
}

sf_reg_map <- sf_reg_map |>
  mutate(
    cl_taxa = cut(taxa_pad_media_reg, breaks = brk_taxa_reg,
                  include.lowest = TRUE, labels = fmt_brk(brk_taxa_reg)),
    cl_evit = cut(evitadas_reg,       breaks = brk_evit_reg,
                  include.lowest = TRUE, labels = fmt_brk(brk_evit_reg))
  )

sf_cs_map <- sf_cs_map |>
  mutate(
    cl_taxa = cut(taxa_pad_media, breaks = brk_taxa_cs,
                  include.lowest = TRUE, labels = fmt_brk(brk_taxa_cs)),
    cl_evit = cut(evitadas_central, breaks = brk_evit_cs,
                  include.lowest = TRUE, labels = fmt_brk(brk_evit_cs))
  )

n_cls_taxa_reg  <- nlevels(sf_reg_map$cl_taxa)
n_cls_evit_reg  <- nlevels(sf_reg_map$cl_evit)
n_cls_taxa_cs   <- nlevels(sf_cs_map$cl_taxa)
n_cls_evit_cs   <- nlevels(sf_cs_map$cl_evit)

pal_viridis <- function(n) viridis::viridis(n, direction = -1)
pal_ylorrd  <- function(n) RColorBrewer::brewer.pal(max(n, 3), "YlOrRd")[seq_len(n)]
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  pal_ylorrd <- function(n) {
    cols <- c("#FFFFB2","#FECC5C","#FD8D3C","#F03B20","#BD0026")
    cols[seq_len(n)]
  }
}

theme_mapa <- function() {
  theme_void(base_size = 8) +
    theme(
      plot.title        = element_text(size = 8, face = "bold", hjust = 0.5),
      legend.title      = element_text(size = 6.5, face = "bold"),
      legend.text       = element_text(size = 6),
      legend.key.height = unit(5, "mm"),
      legend.key.width  = unit(3.5, "mm"),
      plot.margin       = margin(2, 2, 2, 2, "mm"),
      plot.background   = element_rect(fill = "white", colour = NA)
    )
}

# Painel A: taxa por regional
p3a <- ggplot(sf_reg_map) +
  geom_sf(aes(fill = cl_taxa), colour = "grey70", linewidth = 0.3,
          na.rm = FALSE) +
  ggrepel::geom_label_repel(
    data = sf_reg_labels, aes(x = X, y = Y, label = regional),
    size = 2.2, force = 3, max.overlaps = 20,
    fill = "white", alpha = 0.75, label.size = 0.1,
    segment.color = "grey50", min.segment.length = 0.2,
    fontface = "bold", inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name   = "Taxa ICSAP pad.\n(por 10.000 hab./mês)",
    values = setNames(pal_viridis(n_cls_taxa_reg),
                      levels(sf_reg_map$cl_taxa)),
    na.value = "grey85", drop = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.30,
                   bar_cols = c("grey30", "white"), text_cex = 0.50) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_minimal(text_size = 6),
                         height = unit(0.65, "cm"), width = unit(0.45, "cm")) +
  coord_sf(expand = FALSE) +
  labs(title = "A — Taxa ICSAP por Regional Administrativa") +
  theme_mapa()

# Painel B: evitadas por regional
p3b <- ggplot(sf_reg_map) +
  geom_sf(aes(fill = cl_evit), colour = "grey70", linewidth = 0.3,
          na.rm = FALSE) +
  ggrepel::geom_label_repel(
    data = sf_reg_labels, aes(x = X, y = Y, label = regional),
    size = 2.2, force = 3, max.overlaps = 20,
    fill = "white", alpha = 0.75, label.size = 0.1,
    segment.color = "grey50", min.segment.length = 0.2,
    fontface = "bold", inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name   = "Evitadas (n)",
    values = setNames(pal_ylorrd(n_cls_evit_reg),
                      levels(sf_reg_map$cl_evit)),
    na.value = "grey85", drop = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.30,
                   bar_cols = c("grey30", "white"), text_cex = 0.50) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_minimal(text_size = 6),
                         height = unit(0.65, "cm"), width = unit(0.45, "cm")) +
  coord_sf(expand = FALSE) +
  labs(title = "B — Internações evitadas por Regional") +
  theme_mapa() +
  theme(legend.key.width = unit(8, "mm"))

# Painel C: taxa por CS
p3c <- ggplot(sf_cs_map) +
  geom_sf(aes(fill = cl_taxa), colour = "grey80", linewidth = 0.1) +
  geom_sf(data = sf_reg_map, fill = NA, colour = "white", linewidth = 0.5) +
  ggrepel::geom_label_repel(
    data = sf_lab3, aes(x = X, y = Y, label = label_cs),
    size = 1.8, force = 5, max.overlaps = 10,
    fill = "white", alpha = 0.8, label.size = 0.1,
    segment.color = "grey50", min.segment.length = 0.1,
    fontface = "bold", inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name   = "Taxa ICSAP pad.\n(por 10.000 hab./mês)",
    values = setNames(pal_viridis(n_cls_taxa_cs),
                      levels(sf_cs_map$cl_taxa)),
    na.value = "grey85", drop = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.30,
                   bar_cols = c("grey30", "white"), text_cex = 0.50) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_minimal(text_size = 6),
                         height = unit(0.65, "cm"), width = unit(0.45, "cm")) +
  coord_sf(expand = FALSE) +
  labs(title = "C  Taxa ICSAP por Centro de Saúde (n = 153)") +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust    = 0.5,
    keywidth       = unit(6, "mm"),
    keyheight      = unit(4, "mm")
  )) +
  theme_mapa()

# Painel D: evitadas por CS
p3d <- ggplot(sf_cs_map) +
  geom_sf(aes(fill = cl_evit), colour = "grey80", linewidth = 0.1) +
  geom_sf(data = sf_reg_map, fill = NA, colour = "white", linewidth = 0.5) +
  ggrepel::geom_label_repel(
    data = sf_lab5, aes(x = X, y = Y, label = label_cs),
    size = 1.8, force = 5, max.overlaps = 10,
    fill = "white", alpha = 0.8, label.size = 0.1,
    segment.color = "grey50", min.segment.length = 0.1,
    fontface = "bold", inherit.aes = FALSE
  ) +
  scale_fill_manual(
    name   = "Evitadas (n)",
    values = setNames(pal_ylorrd(n_cls_evit_cs),
                      levels(sf_cs_map$cl_evit)),
    na.value = "grey85", drop = FALSE
  ) +
  annotation_scale(location = "bl", width_hint = 0.30,
                   bar_cols = c("grey30", "white"), text_cex = 0.50) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_minimal(text_size = 6),
                         height = unit(0.65, "cm"), width = unit(0.45, "cm")) +
  coord_sf(expand = FALSE) +
  labs(title = "D  Internações evitadas por CS") +
  theme_mapa() +
  theme(
    legend.key.width  = unit(8, "mm"),
    legend.key.height = unit(5, "mm"),
    legend.box.margin = margin(0, 15, 0, 5, "mm"),
    legend.text       = element_text(size = 5.5),
    legend.title      = element_text(size = 6.5, face = "bold")
  )

fig3 <- (p3a | p3b) / (p3c | p3d) +
  plot_annotation(
    caption = paste(
      stringr::str_wrap(
        c(
          "Figura 3. Distribuição espacial da taxa de internações por condições sensíveis à atenção primária (ICSAP) e das internações evitadas após a Portaria GM/MS nº 3.493/2024. Belo Horizonte, Minas Gerais, Brasil, janeiro de 2022 a março de 2026.",
          "(A) Taxa ICSAP padronizada por idade (método direto, população padrão: Brasil, Censo 2022). Média mensal por 10.000 habitantes, por Regional Administrativa de saúde.",
          "(B) Internações ICSAP evitadas por Regional Administrativa, no período pós-Portaria (maio/2024–março/2026). Estimativa baseada no modelo ITS-GLS AR(1).",
          "(C) Taxa ICSAP padronizada por idade (mesmo método), por área de abrangência do Centro de Saúde (CS). Foram incluídos 153 CS com informação completa.",
          "(D) Internações ICSAP evitadas por CS. Estimativa descritiva obtida pela distribuição proporcional do total BH ao período pós-Portaria (mai/2024–mar/2026), usando como peso a participação histórica de cada CS no total de ICSAP geocodificadas no mesmo período. Não representa modelo ITS independente por CS — séries individuais por CS têm poder estatístico insuficiente para inferência causal isolada. Classificação das cores por quebras naturais (método de Jenks).",
          "Escala: 6 km (indicada nos mapas). Norte: seta indicativa.",
          "Fontes: SIH/SUS – DATASUS; SMSA/PBH (áreas de abrangência, 2024); IBGE (Censo 2022). Elaboração própria."
        ),
        width = 210
      ),
      collapse = "\n"
    ),
    theme = theme(
      plot.caption = element_text(size = 6.3, colour = "grey35",
                                  hjust = 0, lineheight = 1.3),
      plot.caption.position = "plot"
    )
  )

suppressWarnings({
  ragg::agg_png(
    filename   = file.path(DIR_DOCS, "figura3_mapa_quadruplo.png"),
    width      = 14, height = 12, units = "in",
    res        = 300, background = "white"
  )
  print(fig3)
  dev.off()
})
cat("  ok figura3_mapa_quadruplo.png\n")

# =============================================================================
# TABELA 1 — Características dos pacientes
# =============================================================================
cat("Gerando Tabela 1 (características pacientes)...\n")

ic <- read_csv(file.path(DIR_DATA, "icsap_bh_regional.csv"),
               show_col_types = FALSE) |>
  mutate(
    periodo  = if_else(ano_cmpt < 2024 | (ano_cmpt == 2024 & mes_cmpt < 5),
                       "Pre", "Pos"),
    sexo_f   = sexo == 3,
    sexo_m   = sexo == 1,
    fx = cut(idade,
             breaks = c(-Inf, 4, 14, 29, 44, 59, 74, Inf),
             labels = c("< 5 anos", "5–14 anos", "15–29 anos",
                        "30–44 anos", "45–59 anos", "60–74 anos",
                        "≥ 75 anos"),
             right = TRUE),
    regional = if_else(is.na(regional) | regional == "", NA_character_, regional),
    com_cs   = !is.na(regional)
  )

lista_icsap <- read_csv(file.path(DIR_REF, "lista_icsap.csv"),
                        show_col_types = FALSE)
# Nomes dos grupos conforme Portaria SAS/MS nº 221/2008.
# O lista_icsap.csv usa 15 grupos; grupos 09 e 10 agregam múltiplos
# grupos da Portaria (cardiovasculares e respiratórios, respectivamente).
grupo_labels <- tibble(
  grupo    = c("01","02","03","04","05","06","07","08",
               "09","10","11","12","13","14","15"),
  label_grp = c(
    "Gastroenterites infecciosas e complicações",
    "Tuberculose",
    "Doenças preveníveis por imunização",
    "Dengue",
    "Doenças relacionadas ao HIV",
    "Anemia",
    "Diabetes mellitus",
    "Deficiências nutricionais",
    "Doenças cardiovasculares (hipertensão, angina, insuficiência cardíaca, acidente vascular cerebral)",
    "Doenças respiratórias (infecções de vias aéreas superiores, pneumonias, bronquite, asma)",
    "Úlcera gastrointestinal",
    "Outras doenças do aparelho digestivo",
    "Infecção do rim e trato urinário",
    "Infecções na gravidez e puerpério",
    "Infecções de pele e tecido subcutâneo"
  )
)

top5_gps <- ic |> count(grupo, sort = TRUE) |> slice_head(n = 5) |>
  left_join(grupo_labels, by = "grupo") |>
  mutate(label_grp = if_else(is.na(label_grp),
                              paste0("Grupo ", grupo), label_grp))

N   <- nrow(ic)
Npr <- sum(ic$periodo == "Pre")
Npo <- sum(ic$periodo == "Pos")

tot_pct <- function(cond) fmt_pct(sum(cond, na.rm = TRUE), N)
pre_pct <- function(cond) fmt_pct(sum(cond[ic$periodo == "Pre"], na.rm = TRUE), Npr)
pos_pct <- function(cond) fmt_pct(sum(cond[ic$periodo == "Pos"], na.rm = TRUE), Npo)

build_row <- function(var, label, p_override = NULL) {
  if (is.logical(var)) {
    pv <- if (!is.null(p_override)) p_override else fmt_p(chi_p(var, ic$periodo))
    tibble(
      Caracteristica = label,
      Total   = tot_pct(var),
      Pre     = pre_pct(var),
      Pos     = pos_pct(var),
      p_valor = pv,
      header  = FALSE
    )
  } else {
    pv <- if (!is.null(p_override)) p_override else fmt_p(mw_p(var, ic$periodo))
    tibble(
      Caracteristica = label,
      Total   = fmt_med(var),
      Pre     = fmt_med(var[ic$periodo == "Pre"]),
      Pos     = fmt_med(var[ic$periodo == "Pos"]),
      p_valor = pv,
      header  = FALSE
    )
  }
}

hdr <- function(label, pval = "") {
  tibble(Caracteristica = label,
         Total = "", Pre = "", Pos = "", p_valor = pval,
         header = TRUE)
}

cat_rows <- function(varname, grplevels = NULL) {
  tbl_tot <- table(ic[[varname]])
  tbl_pre <- table(ic[[varname]][ic$periodo == "Pre"])
  tbl_pos <- table(ic[[varname]][ic$periodo == "Pos"])
  lvls <- if (!is.null(grplevels)) grplevels else names(tbl_tot)
  map_dfr(lvls, function(lv) {
    n_tot <- tbl_tot[as.character(lv)]; if (is.na(n_tot)) n_tot <- 0
    n_pre <- tbl_pre[as.character(lv)]; if (is.na(n_pre)) n_pre <- 0
    n_pos <- tbl_pos[as.character(lv)]; if (is.na(n_pos)) n_pos <- 0
    tibble(Caracteristica = paste0("  ", lv),
           Total = fmt_pct(n_tot, N),
           Pre   = fmt_pct(n_pre, Npr),
           Pos   = fmt_pct(n_pos, Npo),
           p_valor = "", header = FALSE)
  })
}

p_fx   <- fmt_p(chi_p(as.character(ic$fx), ic$periodo))
p_reg  <- fmt_p(chi_p(ic$regional, ic$periodo))
p_grp  <- fmt_p(chi_p(ic$grupo %in% top5_gps$grupo, ic$periodo))
p_sexo <- fmt_p(chi_p(ic$sexo_m, ic$periodo))

tab1 <- bind_rows(
  tibble(Caracteristica = "Total de internações ICSAP",
         Total = fmt_n(N), Pre = fmt_n(Npr), Pos = fmt_n(Npo),
         p_valor = "—", header = FALSE),

  hdr("Sexo — n (%)", p_sexo),
  build_row(ic$sexo_m, "  Masculino", p_override = ""),
  build_row(ic$sexo_f, "  Feminino",  p_override = ""),

  build_row(ic$idade, "Idade (anos) — mediana [IQR]"),

  hdr(paste0("Faixa etária — n (%)"), p_fx),
  cat_rows("fx", levels(ic$fx)),

  hdr(paste0("Top 5 grupos ICSAP (Portaria 221/2008) — n (%)"), p_grp),
  {
    map_dfr(seq_len(nrow(top5_gps)), function(i) {
      g    <- top5_gps$grupo[i]
      lbl  <- top5_gps$label_grp[i]
      cond <- ic$grupo == g
      tibble(Caracteristica = paste0("  ", lbl),
             Total = fmt_pct(sum(cond), N),
             Pre   = fmt_pct(sum(cond & ic$periodo == "Pre"), Npr),
             Pos   = fmt_pct(sum(cond & ic$periodo == "Pos"), Npo),
             p_valor = "", header = FALSE)
    })
  },

  hdr(paste0("Regional de saúde — n (%)^c^"), p_reg),
  {
    regs <- sort(unique(ic$regional[!is.na(ic$regional)]))
    map_dfr(regs, function(r) {
      cond <- ic$regional == r & !is.na(ic$regional)
      tibble(Caracteristica = paste0("  ", r),
             Total = fmt_pct(sum(cond), N),
             Pre   = fmt_pct(sum(cond & ic$periodo == "Pre"), Npr),
             Pos   = fmt_pct(sum(cond & ic$periodo == "Pos"), Npo),
             p_valor = "", header = FALSE)
    })
  },

  build_row(ic$com_cs,    "Com CS identificado — n (%)"),
  build_row(ic$dias_perm, "Dias de permanência — mediana [IQR]"),
  build_row(ic$val_tot,   "Custo por internação (BRL deflacionados, R$ mar/2026) — mediana [IQR]")
)

write_csv(tab1 |> select(-header), file.path(DIR_DOCS, "tabela1_pacientes.csv"))

nota_filtro <- paste0(
  "Incluídos apenas residentes e internados em Belo Horizonte (código IBGE 310620)."
)
nota_rodape <- paste0(
  "CS: Centro de Saúde. IQR: Intervalo Interquartil. \n",
  "^a^Qui-quadrado de Pearson. ^b^Teste de Mann-Whitney. ",
  "^c^Distribuição por regional disponível apenas para internações com Centro de Saúde identificado ",
  "(n = 98.192; 86,4% do total).\n",
  "Valores monetários deflacionados pelo IPCA (jan/2022–mar/2026; acumulado = 26,4%), ",
  "expressos em Reais de março/2026.\n",
  "Fonte: SIH/SUS – DATASUS. ", nota_filtro
)

gt1 <- tab1 |>
  select(-header) |>
  gt() |>
  cols_label(
    Caracteristica = "Característica",
    Total   = md(sprintf("**Total**<br>n = %s", fmt_n(N))),
    Pre     = md(sprintf("**Pré-Portaria**<br>jan/2022–abr/2024<br>n = %s", fmt_n(Npr))),
    Pos     = md(sprintf("**Pós-Portaria**<br>mai/2024–mar/2026<br>n = %s", fmt_n(Npo))),
    p_valor = md("**p**^a,b^")
  ) |>
  tab_header(
    title = "Tabela 1. Características das internações por condições sensíveis à atenção primária (ICSAP)",
    subtitle = "Belo Horizonte, Minas Gerais, Brasil, janeiro de 2022 a março de 2026"
  ) |>
  tab_spanner(
    label   = "Período em relação à Portaria GM/MS nº 3.493/2024",
    columns = c(Pre, Pos, p_valor)
  ) |>
  tab_source_note(md(nota_rodape)) |>
  tab_style(
    style     = list(cell_fill(color = "#EBF5FB"),
                     cell_text(weight = "bold")),
    locations = cells_body(rows = tab1$header)
  ) |>
  tab_style(
    style     = cell_text(indent = px(14)),
    locations = cells_body(rows = !tab1$header &
                             str_starts(tab1$Caracteristica, "  "))
  ) |>
  cols_align(align = "center", columns = c(Total, Pre, Pos, p_valor)) |>
  cols_width(Caracteristica ~ px(265), Total ~ px(115),
             Pre ~ px(120), Pos ~ px(120), p_valor ~ px(65)) |>
  tab_options(
    table.border.top.color            = "black",
    table.border.bottom.color         = "black",
    heading.border.bottom.color       = "black",
    column_labels.border.bottom.color = "black",
    column_labels.font.weight         = "bold",
    table.font.size                   = px(11),
    data_row.padding                  = px(3),
    heading.title.font.size           = px(12),
    heading.subtitle.font.size        = px(10)
  )

gtsave(gt1, file.path(DIR_DOCS, "tabela1_pacientes.html"))
cat("  ok tabela1_pacientes.html + .csv\n")

# =============================================================================
# TABELA 2 — Resultados analíticos
# =============================================================================
cat("Gerando Tabela 2 (resultados analíticos)...\n")

its  <- read_csv(file.path(DIR_DATA, "its_resultados.csv"),      show_col_types = FALSE)
poi  <- read_csv(file.path(DIR_DATA, "poisson_resultados.csv"),  show_col_types = FALSE)
jp   <- read_csv(file.path(DIR_DATA, "joinpoint_resultados.csv"),show_col_types = FALSE)
cus  <- read_csv(file.path(DIR_DATA, "custo_evitado.csv"),       show_col_types = FALSE)

bh   <- its |> filter(nivel == "BH Municipal")
bh1  <- bh[1, ]

mes2data <- function(m) format(as.Date("2022-01-01") + months(m - 1), "%b/%Y")

jp_bh <- jp |> filter(nivel == "BH Municipal") |>
  mutate(
    data_ini = mes2data(mes_inicio),
    data_fim = mes2data(mes_fim),
    periodo  = sprintf("%s–%s", data_ini, data_fim)
  )
aapc_bh <- jp_bh$aapc[1]

m2_ivs  <- poi |> filter(modelo == "M2_contextual",  variavel == "ivs_score")
m2_san  <- poi |> filter(modelo == "M2_contextual",  variavel == "pct_sem_saneamento")
m_q2    <- poi |> filter(modelo == "M_dose_resposta", variavel == "n_esf_qQ2 (5-6)")

bh_imp  <- cus |> filter(nivel == "BH Municipal")

# ic95: usa formatC + gsub para garantir vírgula decimal (sprint não segue OutDec)
ic95 <- function(inf, sup, digits = 1) {
  lo <- formatC(inf, digits = digits, format = "f", decimal.mark = ",")
  hi <- formatC(sup, digits = digits, format = "f", decimal.mark = ",")
  paste0("(", lo, "; ", hi, ")")
}

p_fmt_its <- function(p) if_else(p < 0.001, "<0,001", fmt_p(p))

# Valores Joinpoint (IC95% da análise segmented — especificados pelo pesquisador)
# AAPC: método NCI log-linear (média ponderada das slopes mensais em escala log-linear)
# AAPC corrigido: -0,16%/ano; IC95% ≈ original ± deslocamento de -0,86 pp (mesmo SE)
jp_ic <- list(
  seg1 = list(apc = "+1,2%/ano", ic = "(-3,1; 5,5)", p = "0,584"),
  seg2 = list(apc = "+22,9%/ano", ic = "(15,3; 30,5)", p = "<0,001"),
  seg3 = list(apc = "-11,2%/ano", ic = "(-16,8; -5,6)", p = "<0,001"),
  aapc = list(apc = "-0,2%/ano",  ic = "(-2,1; 1,7)",   p = "0,869")
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
  jp_ic$seg1$apc,
  jp_ic$seg1$ic,
  jp_ic$seg1$p,

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("Joinpoint (2 inflexões)\nAAPC = %s %s (p = %s)",
          jp_ic$aapc$apc, jp_ic$aapc$ic, jp_ic$aapc$p),
  sprintf("Seg. 2: %s", jp_bh$periodo[2]),
  jp_ic$seg2$apc,
  jp_ic$seg2$ic,
  jp_ic$seg2$p,

  "1. Interrupção de Série Temporal (ITS-GLS AR[1]) — BH Municipal",
  sprintf("Joinpoint (2 inflexões)\nAAPC = %s %s (p = %s)",
          jp_ic$aapc$apc, jp_ic$aapc$ic, jp_ic$aapc$p),
  sprintf("Seg. 3: %s", jp_bh$periodo[3]),
  jp_ic$seg3$apc,
  jp_ic$seg3$ic,
  jp_ic$seg3$p,

  # ---- Bloco 2: Poisson FE ----
  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "M2 — FE Regional de Saúde (9 cat.) + Ano (fixest::feglm)\nn = 7.803 obs. (153 CS × 51 meses) — permite estimar preditores estáticos (IVS, saneamento)ᵍ",
  "Índice de Vulnerabilidade em Saúde (IVS-BH) — IRR por 1 ponto",
  sprintf("%.3f", m2_ivs$irr) |> gsub("\\.", ",", x = _),
  ic95(m2_ivs$ic_inf, m2_ivs$ic_sup, 3),
  if_else(m2_ivs$p_valor < 0.001, "<0,001", fmt_p(m2_ivs$p_valor)),

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "M2 — FE Regional de Saúde (9 cat.) + Ano (fixest::feglm)\nn = 7.803 obs. (153 CS × 51 meses) — permite estimar preditores estáticos (IVS, saneamento)ᵍ",
  "% domicílios sem saneamento básico — IRR por 1 p.p.",
  sprintf("%.3f", m2_san$irr) |> gsub("\\.", ",", x = _),
  ic95(m2_san$ic_inf, m2_san$ic_sup, 3),
  fmt_p(m2_san$p_valor),

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "M2 — FE Regional de Saúde (9 cat.) + Ano (fixest::feglm)\nn = 7.803 obs. (153 CS × 51 meses) — permite estimar preditores estáticos (IVS, saneamento)ᵍ",
  "Sobredispersão (Pearson χ²/gl) — M2 regional FEᵍ",
  sprintf("%.2f", m2_ivs$dispersao_pearson) |> gsub("\\.", ",", x = _),
  "—",
  "—",

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "Dose-resposta: nº equipes ESF vs. Q1 (1–4 equipes)",
  "Q2 (5–6 equipes) — IRR",
  sprintf("%.3f", m_q2$irr) |> gsub("\\.", ",", x = _),
  ic95(m_q2$ic_inf, m_q2$ic_sup, 3),
  if_else(m_q2$p_valor < 0.001, "<0,001", fmt_p(m_q2$p_valor)),

  "2. Determinantes da taxa ICSAP — Poisson FE dois sentidos (153 CS)",
  "Dose-resposta: nº equipes ESF vs. Q1 (1–4 equipes)",
  "Q3–Q4 (≥ 7 equipes) — IRRʲ",
  "0,987",
  "(0,962; 1,013)",
  "0,287",

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
  "—",
  "—",

  "3. Impacto estimado da Portaria GM/MS nº 3.493/2024 (mai/2024–mar/2026)",
  "Diferença-em-diferenças ITS — BH vs. 6 capitais controle\nn = 357 obs. (51 meses × 7 capitais)",
  "θ médio (mudança de slope BH − controles)",
  "−0,3%/ano",
  "(−2,1; 1,5)",
  "0,423"
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
  tab_source_note(rodape_tab2[[1]]) |>
  tab_source_note(rodape_tab2[[2]]) |>
  tab_source_note(rodape_tab2[[3]]) |>
  tab_source_note(rodape_tab2[[4]]) |>
  tab_source_note(rodape_tab2[[5]]) |>
  tab_source_note(rodape_tab2[[6]]) |>
  tab_source_note(rodape_tab2[[7]]) |>
  tab_source_note(rodape_tab2[[8]]) |>
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
    p          ~ px(65)
  ) |>
  tab_options(
    table.border.top.color            = "black",
    table.border.bottom.color         = "black",
    heading.border.bottom.color       = "black",
    column_labels.border.bottom.color = "black",
    row_group.border.bottom.color     = "black",
    row_group.border.top.color        = "black",
    column_labels.font.weight         = "bold",
    table.font.size                   = px(11),
    data_row.padding                  = px(4),
    row_group.padding                 = px(5),
    heading.title.font.size           = px(12),
    heading.subtitle.font.size        = px(10)
  )

gtsave(gt2, file.path(DIR_DOCS, "tabela2_resultados.html"))
cat("  ok tabela2_resultados.html + .csv\n")

# =============================================================================
# TABELA S3 — Sensibilidade: Binomial Negativo vs. Poisson (auditoria 23/05/2026)
# =============================================================================
cat("\nGerando Tabela S3 (sensibilidade NB — sobredispersão)...\n")

suppressPackageStartupMessages(library(MASS))
select <- dplyr::select  # MASS mascara dplyr::select

# Lê M2 (Poisson) e M2_NB diretamente de poisson_resultados.csv — MESMA especificação
poi_s3 <- read_csv(file.path(DIR_DATA, "poisson_resultados.csv"), show_col_types = FALSE)

tab_s3_data <- poi_s3 |>
  filter(modelo %in% c("M2_contextual", "M2_NB"),
         variavel %in% c("ivs_score", "pct_sem_saneamento")) |>
  mutate(
    modelo = recode(modelo,
                    "M2_contextual" = "Poisson M2 (FE regional+ano)",
                    "M2_NB"         = "Binomial Negativo M2 (FE regional+ano)"),
    variavel_label = recode(variavel,
                    "ivs_score"          = "IVS-BH (por 1 ponto)",
                    "pct_sem_saneamento" = "% sem saneamento básico (por 1 p.p.)"),
    irr_fmt = formatC(irr, digits = 3, format = "f", decimal.mark = ","),
    ic_fmt  = sprintf("(%s; %s)",
                      formatC(ic_inf, digits = 3, format = "f", decimal.mark = ","),
                      formatC(ic_sup, digits = 3, format = "f", decimal.mark = ",")),
    p_fmt   = if_else(p_valor < 0.001, "<0,001",
                      formatC(p_valor, digits = 3, format = "f", decimal.mark = ","))
  ) |>
  select(modelo, variavel, irr, ic_inf, ic_sup, p_valor,
         variavel_label, irr_fmt, ic_fmt, p_fmt)

write_csv(tab_s3_data, file.path(DIR_DOCS, "tabela_s3_sensibilidade_nb.csv"))

if (nrow(tab_s3_data) > 0) {
  gt_s3 <- tab_s3_data |>
    select(modelo, variavel_label, irr_fmt, ic_fmt, p_fmt) |>
    gt(groupname_col = "modelo") |>
    cols_label(
      variavel_label = "Preditor",
      irr_fmt        = "IRR",
      ic_fmt         = "IC 95%",
      p_fmt          = md("*p*-valor")
    ) |>
    tab_header(
      title    = "Tabela S3. Análise de sensibilidade — Binomial Negativo vs. Poisson",
      subtitle = "Modelo M2: efeitos fixos por Regional e Ano (153 CS × 51 meses)"
    ) |>
    tab_source_note(md(paste0(
      "Poisson e Binomial Negativo ajustados sob a mesma especificação do M2 (contextual): ",
      "FE Regional + Ano, offset log(pop), sazonalidade harmônica e covariáveis contextuais ",
      "(IVS, área de favela, renda, saneamento), erros padrão clusterizados por CS. ",
      "O IRR do Poisson reproduz o do texto principal.\n",
      "IRR: Incidence Rate Ratio. IC: Intervalo de Confiança de 95%."
    ))) |>
    tab_style(
      style = list(cell_fill(color = "#1A5276"),
                   cell_text(color = "white", weight = "bold")),
      locations = cells_row_groups()
    ) |>
    cols_align(align = "center", columns = c(irr_fmt, ic_fmt, p_fmt)) |>
    cols_width(modelo ~ px(220), variavel_label ~ px(230),
               irr_fmt ~ px(90), ic_fmt ~ px(140), p_fmt ~ px(70)) |>
    tab_options(table.font.size = px(11), data_row.padding = px(4),
                table.border.top.color = "black",
                table.border.bottom.color = "black",
                column_labels.font.weight = "bold")

  gtsave(gt_s3, file.path(DIR_DOCS, "tabela_s3_sensibilidade_nb.html"))
  cat("  ok tabela_s3_sensibilidade_nb.html\n")
} else {
  cat("  AVISO: modelos não convergiram — Tabela S3 não gerada\n")
}

cat("\n=== Todos os outputs gerados em docs/ ===\n")
cat("Figuras: figura1_fluxograma_strobe.png, figura2_its_4paineis.png,",
    "figura3_mapa_quadruplo.png\n")
cat("Tabelas: tabela1_pacientes.html/.csv, tabela2_resultados.html/.csv\n")
cat("Suplementar: tabela_s3_sensibilidade_nb.html/.csv\n")
