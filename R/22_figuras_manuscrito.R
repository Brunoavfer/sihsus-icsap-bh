# encoding: UTF-8
# R/22_figuras_manuscrito.R
# Figuras e tabelas finais -- manuscrito ICSAP-BH
# Padrao Lancet / Cadernos de Saude Publica -- 300 DPI, 170 mm

suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(patchwork)
  library(gt)
  library(sf)
  library(viridis)
  library(ggspatial)
  library(zoo)
})

options(OutDec = ",", scipen = 999)

DPI  <- 300
W_IN <- 170 / 25.4   # 6.693 pol
DIR_DOCS <- "docs"
DIR_DATA <- "data/processed"
DIR_RAW  <- "data/raw"
DIR_REF  <- "data/ref"

# ---- Helpers ----------------------------------------------------------------
fmt_n  <- function(n) formatC(round(n), format = "d", big.mark = ".")
fmt_pct <- function(n, tot) sprintf("%s (%.1f%%)", fmt_n(n), n / tot * 100)
fmt_med <- function(x) {
  q <- quantile(x, c(.25, .5, .75), na.rm = TRUE)
  sprintf("%.1f [%.1f–%.1f]", q[2], q[1], q[3])
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

# ---- Tema padrao Lancet/CSP --------------------------------------------------
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
# FIGURA 1 -- Fluxograma STROBE
# =============================================================================
cat("Gerando Figura 1 (STROBE)...\n")

# Cache do total MG: leitura dos 51 .dbc leva ~6 min na primeira execucao
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
  cat("  Cache salvo:", mg_cache, "\n")
} else {
  cat("  Cache encontrado:", mg_cache, "\n")
}
mg_df <- read.csv(mg_cache)
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

# --- Layout: coluna principal x=0.10-0.68, exclusoes x=0.72-0.99 ---
mk_box <- function(id, x0, y0, x1, y1, fill, lab, text_col = "black") {
  tibble(id = id, x0 = x0, y0 = y0, x1 = x1, y1 = y1, fill = fill,
         lab = lab, xm = (x0 + x1) / 2, ym = (y0 + y1) / 2,
         text_col = text_col)
}

boxes_main <- bind_rows(
  mk_box("A", 0.10, 0.88, 0.68, 0.97, "#D6EAF8",
         sprintf("Internacoes hospitalares -- MG\njan/2022 a mar/2026\nn = %s", fmt_n(N_MG))),
  mk_box("B", 0.10, 0.73, 0.68, 0.82, "#AED6F1",
         sprintf("Internacoes ocorridas em BH\n(MUNIC_MOV = 310.620)\nn = %s", fmt_n(N_BH_MOV))),
  mk_box("C", 0.10, 0.57, 0.68, 0.67, "#1B4F72",
         sprintf("Total internacoes BH\n(MUNIC_RES = MUNIC_MOV = 310.620)\nn = %s", fmt_n(N_BH)),
         "white"),
  mk_box("D1", 0.10, 0.40, 0.68, 0.50, "#2874A6",
         sprintf("ICSAP -- Portaria SAS/MS n. 221/2008\n479 CIDs classificadores\nn = %s (17,8%%)", fmt_n(N_ICSAP)),
         "white"),
  mk_box("E1", 0.10, 0.23, 0.68, 0.33, "#2980B9",
         sprintf("CEP geocodificado (CS identificado)\nAwesomeAPI + Nominatim + sf\nn = %s (86,4%%)", fmt_n(N_GEO)),
         "white")
)

boxes_analysis <- bind_rows(
  mk_box("F1", 0.02, 0.04, 0.43, 0.15, "#1B4F72",
         sprintf("Analise temporal\n(ITS-GLS AR(1) + Joinpoint)\nn = %s ICSAP | 51 meses", fmt_n(N_ICSAP)),
         "white"),
  mk_box("F2", 0.45, 0.04, 0.99, 0.15, "#154360",
         sprintf("Analise espacial\n(Moran I + GEE AR(1) + Poisson FE)\n153 CS x 36-51 meses | n = %s", fmt_n(N_GEO)),
         "white")
)

boxes_excl <- bind_rows(
  mk_box("X1", 0.72, 0.88, 0.99, 0.97, "#BDC3C7",
         sprintf("Excluidos:\nMUNIC_MOV /= 310.620\nn = %s", fmt_n(excl_mov))),
  mk_box("X2", 0.72, 0.73, 0.99, 0.82, "#BDC3C7",
         sprintf("Excluidos:\nMUNIC_RES /= 310.620\nn = %s", fmt_n(excl_res))),
  mk_box("X3", 0.72, 0.57, 0.99, 0.67, "#FADBD8",
         sprintf("Nao-ICSAP\n(excluidos da analise)\nn = %s (82,2%%)", fmt_n(N_NAICSAP))),
  mk_box("X4", 0.72, 0.40, 0.99, 0.50, "#FAD7A0",
         sprintf("Sem geocodificacao\n(MNAR -- analisado script 06)\nn = %s (13,6%%)", fmt_n(N_NGEO)))
)

# Setas verticais (fluxo principal)
segs_v <- tribble(
  ~x,    ~y,    ~xend, ~yend,
  0.39,  0.88,  0.39,  0.825,
  0.39,  0.73,  0.39,  0.675,
  0.39,  0.57,  0.39,  0.505,
  0.39,  0.40,  0.39,  0.335,
  0.39,  0.23,  0.22,  0.155,
  0.39,  0.23,  0.72,  0.155
)

# Setas horizontais para exclusoes
segs_h <- tribble(
  ~x,    ~y,    ~xend, ~yend,
  0.68,  0.925, 0.72,  0.925,
  0.68,  0.775, 0.72,  0.775,
  0.68,  0.620, 0.72,  0.620,
  0.68,  0.450, 0.72,  0.450
)

fig1 <- ggplot() +
  # Caixas de exclusao
  geom_rect(data = boxes_excl,
            aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill),
            colour = "grey50", linewidth = 0.3) +
  geom_text(data = boxes_excl,
            aes(x = xm, y = ym, label = lab),
            size = 2.0, lineheight = 1.15, family = "sans", colour = "black") +
  # Caixas de analise
  geom_rect(data = boxes_analysis,
            aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill),
            colour = "grey25", linewidth = 0.4) +
  geom_text(data = boxes_analysis,
            aes(x = xm, y = ym, label = lab, colour = text_col),
            size = 2.1, lineheight = 1.15, family = "sans") +
  # Caixas principais
  geom_rect(data = boxes_main,
            aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1, fill = fill),
            colour = "grey25", linewidth = 0.45) +
  geom_text(data = boxes_main,
            aes(x = xm, y = ym, label = lab, colour = text_col),
            size = 2.2, lineheight = 1.2, family = "sans") +
  scale_fill_identity() +
  scale_colour_identity() +
  # Setas verticais
  geom_segment(data = segs_v,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
               colour = "grey30", linewidth = 0.4) +
  # Setas horizontais para exclusoes
  geom_segment(data = segs_h,
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"),
               colour = "grey40", linewidth = 0.35) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.02, 1.01)) +
  theme_void() +
  labs(
    title    = "Figura 1. Fluxo de selecao da coorte -- ICSAP, Belo Horizonte, jan/2022-mar/2026",
    subtitle = paste0("Segundo recomendacoes STROBE (Von Elm et al., 2007). ",
                      "BH: Belo Horizonte. CS: Centro de Saude. ",
                      "ICSAP: Internacoes por Condicoes Sensiveis a Atencao Primaria. ",
                      "MNAR: Missing Not At Random.")
  ) +
  theme(
    plot.title      = element_text(size = 9, face = "bold", hjust = 0,
                                   margin = margin(b = 2)),
    plot.subtitle   = element_text(size = 6.5, colour = "grey35", hjust = 0,
                                   margin = margin(b = 4), lineheight = 1.3),
    plot.margin     = margin(6, 6, 6, 6, "mm"),
    plot.background = element_rect(fill = "white", colour = NA)
  )

ggsave(file.path(DIR_DOCS, "figura1_fluxograma_strobe.png"),
       fig1, width = W_IN, height = W_IN * 1.1, dpi = DPI, bg = "white")
cat("  ok figura1_fluxograma_strobe.png\n")

# =============================================================================
# FIGURA 2 -- ITS com dois paineis
# =============================================================================
cat("Gerando Figura 2 (ITS paineis)...\n")

ev <- read_csv(file.path(DIR_DATA, "internacoes_evitadas.csv"),
               show_col_types = FALSE) |>
  mutate(
    data    = as.Date(data),
    periodo = if_else(data < as.Date("2024-05-01"),
                      "Pre-intervencao", "Pos-intervencao"),
    periodo = factor(periodo,
                     levels = c("Pre-intervencao", "Pos-intervencao")),
    ma3     = rollmean(n_icsap, k = 3, fill = NA, align = "center")
  )

ev_pos <- filter(ev, !is.na(taxa_cf))
d_int  <- as.Date("2024-05-01")

# Anotacao APC -- posicoes dinamicas
y_max_a <- max(ev$n_icsap, na.rm = TRUE)
x_ann_pre  <- as.Date("2022-10-01")
x_ann_pos  <- as.Date("2024-08-01")

p2a <- ggplot(ev, aes(x = data, y = n_icsap, fill = periodo)) +
  geom_col(width = 26, colour = NA, alpha = 0.80) +
  geom_line(aes(y = ma3), colour = "#0D3349", linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = d_int,
             linetype = "dashed", colour = "#C0392B", linewidth = 0.55) +
  annotate("text",
           x = d_int + 25, y = y_max_a * 0.96,
           label = "Portaria GM/MS\nn. 3.493/2024",
           size = 2.1, hjust = 0, colour = "#C0392B", lineheight = 1.1) +
  scale_fill_manual(
    values = c("Pre-intervencao" = "#90CAF9", "Pos-intervencao" = "#1565C0"),
    name   = NULL
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.07))) +
  labs(x = NULL, y = "Internacoes ICSAP (n)",
       title = "A  Internacoes ICSAP mensais -- BH, jan/2022 a mar/2026") +
  theme_lancet() +
  theme(axis.text.x    = element_text(angle = 45, hjust = 1, size = 6.5),
        legend.position = c(0.13, 0.88),
        legend.background = element_blank())

# Limites Y dinamicos para painel B
y_min_b <- floor(min(ev$taxa_obs, ev$taxa_cf, na.rm = TRUE)) - 1.5
y_max_b <- ceiling(max(ev$taxa_obs, ev_pos$cf_ic_sup, na.rm = TRUE)) + 1.5

p2b <- ggplot(ev, aes(x = data)) +
  geom_ribbon(data = ev_pos,
              aes(ymin = cf_ic_inf, ymax = cf_ic_sup),
              fill = "#FFCDD2", alpha = 0.65) +
  geom_line(data = ev_pos,
            aes(y = taxa_cf),
            colour = "#C0392B", linetype = "dashed", linewidth = 0.70) +
  geom_line(aes(y = taxa_obs),
            colour = "#1565C0", linewidth = 0.80) +
  geom_point(aes(y = taxa_obs, colour = periodo),
             size = 0.9, shape = 16, show.legend = FALSE) +
  geom_vline(xintercept = d_int,
             linetype = "dashed", colour = "#C0392B", linewidth = 0.55) +
  annotate("label",
           x = x_ann_pre, y = y_max_b - 0.8,
           label = "APC pre: +12,3%/ano\n(IC95%: 5,8; 19,2; p<0,001)",
           size = 2.0, hjust = 0, colour = "#1565C0",
           fill = "white", label.padding = unit(1.2, "mm")) +
  annotate("label",
           x = x_ann_pos, y = y_max_b - 0.8,
           label = "APC pos: -8,3%/ano\n(p<0,001)",
           size = 2.0, hjust = 0, colour = "#C0392B",
           fill = "white", label.padding = unit(1.2, "mm")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(y_min_b, y_max_b)) +
  scale_colour_manual(values = c("Pre-intervencao" = "#90CAF9",
                                 "Pos-intervencao" = "#1565C0")) +
  labs(x = "Competencia (mes/ano)", y = "Taxa ICSAP (%)",
       title = "B  Taxa ICSAP observada vs. contrafactual -- ITS-GLS AR(1)") +
  theme_lancet() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5))

fig2 <- (p2a / p2b) +
  plot_annotation(
    caption = paste0(
      "Figura 2. Serie temporal das internacoes por condicoes sensiveis a atencao primaria (ICSAP) em Belo Horizonte, ",
      "jan/2022 a mar/2026.\n",
      "A: Internacoes ICSAP por mes; linha azul escura = media movel de 3 meses. ",
      "B: Taxa ICSAP observada (azul) e contrafactual estimado pelo modelo ITS-GLS AR(1) ",
      "(vermelho tracejado; faixa = IC 95% por Monte Carlo n=1.000 iteracoes).\n",
      "Linha vertical = vigencia da Portaria GM/MS n. 3.493/2024. ",
      "APC: Annual Percentage Change. IC: Intervalo de Confianca. ***p<0,001."
    ),
    theme = theme(plot.caption = element_text(size = 6.3, colour = "grey35",
                                              hjust = 0, lineheight = 1.3))
  )

ggsave(file.path(DIR_DOCS, "figura2_its_paineis.png"),
       fig2, width = W_IN, height = W_IN * 1.35, dpi = DPI, bg = "white")
cat("  ok figura2_its_paineis.png\n")

# =============================================================================
# FIGURA 3 -- Mapa duplo (taxa padronizada + internacoes evitadas por CS)
# =============================================================================
cat("Gerando Figura 3 (mapa duplo)...\n")

sf_cs <- st_read(file.path(DIR_REF, "areas_abrangencia_cs.geojson"), quiet = TRUE) |>
  st_transform(4326)

tv2 <- read_csv(file.path(DIR_DATA, "taxas_padronizadas_v2.csv"),
                show_col_types = FALSE) |>
  group_by(nome_cs) |>
  summarise(taxa_pad_media = mean(taxa_padronizada, na.rm = TRUE),
            .groups = "drop")

evi_cs <- read_csv(file.path(DIR_DATA, "internacoes_evitadas_cs.csv"),
                   show_col_types = FALSE) |>
  select(nome_cs, regional, evitadas_central)

sf_map <- sf_cs |>
  left_join(tv2,    by = "nome_cs") |>
  left_join(evi_cs, by = "nome_cs")

cat(sprintf("  Join: taxa_pad=%d, evitadas=%d (de %d CS)\n",
            sum(!is.na(sf_map$taxa_pad_media)),
            sum(!is.na(sf_map$evitadas_central)),
            nrow(sf_map)))

# Top 3 CS por taxa padronizada (para rotulo)
top3_taxa <- sf_map |>
  st_drop_geometry() |>
  slice_max(taxa_pad_media, n = 3) |>
  select(nome_cs, taxa_pad_media) |>
  mutate(label_cs = str_remove(nome_cs, "^CENTRO DE SAUDE "))

# Top 5 CS por internacoes evitadas
top5_evit <- sf_map |>
  st_drop_geometry() |>
  slice_max(evitadas_central, n = 5) |>
  select(nome_cs, evitadas_central) |>
  mutate(label_cs = str_remove(nome_cs, "^CENTRO DE SAUDE "))

# Centroides para rotulos
sf_labels3 <- suppressWarnings(
  sf_map |>
    filter(nome_cs %in% top3_taxa$nome_cs) |>
    st_centroid() |>
    left_join(top3_taxa |> select(nome_cs, label_cs), by = "nome_cs")
)

sf_labels5 <- suppressWarnings(
  sf_map |>
    filter(nome_cs %in% top5_evit$nome_cs) |>
    st_centroid() |>
    left_join(top5_evit |> select(nome_cs, label_cs), by = "nome_cs")
)

theme_mapa <- function() {
  theme_void(base_size = 8) +
    theme(
      plot.title        = element_text(size = 8.5, face = "bold", hjust = 0.5),
      plot.subtitle     = element_text(size = 7,   colour = "grey40", hjust = 0.5),
      legend.title      = element_text(size = 7,   face = "bold"),
      legend.text       = element_text(size = 6.5),
      legend.key.height = unit(8, "mm"),
      legend.key.width  = unit(3, "mm"),
      plot.margin       = margin(3, 3, 3, 3, "mm"),
      plot.background   = element_rect(fill = "white", colour = NA)
    )
}

p3a <- ggplot(sf_map) +
  geom_sf(aes(fill = taxa_pad_media), colour = "white", linewidth = 0.1) +
  geom_sf_text(data = sf_labels3,
               aes(label = label_cs),
               size = 1.6, colour = "black", fontface = "bold",
               check_overlap = TRUE) +
  scale_fill_viridis_c(
    name      = "Taxa pad.\n(por 10.000\nhab./mes)",
    option    = "plasma", direction = -1, na.value = "grey85",
    labels    = label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")
  ) +
  annotation_scale(location = "bl", width_hint = 0.30,
                   bar_cols = c("grey30", "white"),
                   text_cex = 0.55) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_minimal(text_size = 7),
                         height = unit(0.7, "cm"), width = unit(0.5, "cm")) +
  coord_sf(expand = FALSE) +
  labs(title    = "A  Taxa ICSAP padronizada por idade",
       subtitle = "Media 2022-2026, por Centro de Saude (n = 153)") +
  theme_mapa()

p3b <- ggplot(sf_map) +
  geom_sf(aes(fill = evitadas_central), colour = "white", linewidth = 0.1) +
  geom_sf_text(data = sf_labels5,
               aes(label = label_cs),
               size = 1.6, colour = "black", fontface = "bold",
               check_overlap = TRUE) +
  scale_fill_viridis_c(
    name      = "Internacoes\nevitadas (n)",
    option    = "viridis", direction = 1, na.value = "grey85",
    labels    = label_number(accuracy = 1, big.mark = ".", decimal.mark = ",")
  ) +
  annotation_scale(location = "bl", width_hint = 0.30,
                   bar_cols = c("grey30", "white"),
                   text_cex = 0.55) +
  annotation_north_arrow(location = "tr",
                         style = north_arrow_minimal(text_size = 7),
                         height = unit(0.7, "cm"), width = unit(0.5, "cm")) +
  coord_sf(expand = FALSE) +
  labs(title    = "B  Internacoes ICSAP evitadas por CS",
       subtitle = "mai/2024-mar/2026 (pos-Portaria 3.493/2024)") +
  theme_mapa()

fig3 <- (p3a | p3b) +
  plot_annotation(
    caption = paste0(
      "Figura 3. Distribuicao espacial das internacoes por condicoes sensiveis a atencao primaria (ICSAP) ",
      "nos 153 Centros de Saude (CS) de Belo Horizonte.\n",
      "A: Taxa ICSAP padronizada por idade (metodo direto, Ahmad et al., 2001; populacao padrao BH = 2.310.259, Censo 2022). ",
      "B: Estimativa de internacoes ICSAP evitadas por CS apos a Portaria GM/MS n. 3.493/2024 (mai/2024-mar/2026), ",
      "por simulacao Monte Carlo (n=1.000 iteracoes). Cinza = CS sem dados suficientes.\n",
      "Fonte: SIHSUS/DATASUS; Areas de Abrangencia CS/SMSA-PBH."
    ),
    theme = theme(plot.caption = element_text(size = 6.3, colour = "grey35",
                                              hjust = 0, lineheight = 1.3))
  )

suppressWarnings(
  ggsave(file.path(DIR_DOCS, "figura3_mapa_duplo.png"),
         fig3, width = W_IN, height = W_IN * 0.62, dpi = DPI, bg = "white")
)
cat("  ok figura3_mapa_duplo.png\n")

# =============================================================================
# TABELA 1 -- Caracteristicas dos pacientes: total vs. pre/pos-Portaria
# =============================================================================
cat("Gerando Tabela 1 (caracteristicas pacientes)...\n")

ic <- read_csv(file.path(DIR_DATA, "icsap_bh_regional.csv"),
               show_col_types = FALSE) |>
  mutate(
    periodo = if_else(ano_cmpt < 2024 | (ano_cmpt == 2024 & mes_cmpt < 5),
                      "Pre", "Pos"),
    sexo_f  = sexo == 3,
    fx      = cut(idade,
                  breaks = c(-Inf, 4, 14, 29, 44, 59, 74, Inf),
                  labels = c("< 5 anos", "5-14 anos", "15-29 anos",
                             "30-44 anos", "45-59 anos", "60-74 anos",
                             ">= 75 anos"),
                  right  = TRUE),
    regional = if_else(is.na(regional) | regional == "", NA_character_, regional),
    com_cs  = !is.na(regional)
  )

# Nomes dos 5 principais grupos ICSAP (derivados da lista)
lista_icsap <- read_csv(file.path(DIR_REF, "lista_icsap.csv"),
                         show_col_types = FALSE)
grupo_labels <- lista_icsap |>
  group_by(grupo, subgrupo) |>
  summarise(cond = first(descricao), .groups = "drop") |>
  group_by(grupo) |>
  summarise(label_grp = paste(unique(cond)[seq_len(min(2, n()))],
                               collapse = " / "),
            .groups = "drop") |>
  mutate(label_grp = str_trunc(label_grp, 55))

top5_gps <- ic |> count(grupo, sort = TRUE) |> slice_head(n = 5) |>
  left_join(grupo_labels, by = "grupo") |>
  mutate(label_grp = if_else(is.na(label_grp),
                              paste0("Grupo ", grupo), label_grp))

# Totais
N   <- nrow(ic)
Npr <- sum(ic$periodo == "Pre")
Npo <- sum(ic$periodo == "Pos")

# Funcoes de formatacao por subgrupo
tot_pct <- function(cond) fmt_pct(sum(cond, na.rm = TRUE), N)
pre_pct <- function(cond) fmt_pct(sum(cond[ic$periodo == "Pre"], na.rm = TRUE), Npr)
pos_pct <- function(cond) fmt_pct(sum(cond[ic$periodo == "Pos"], na.rm = TRUE), Npo)

build_row <- function(var, label, cat_ref = NULL) {
  if (is.logical(var)) {
    tibble(
      Caracteristica = label,
      Total   = tot_pct(var),
      Pre     = pre_pct(var),
      Pos     = pos_pct(var),
      p_valor = fmt_p(chi_p(var, ic$periodo)),
      header  = FALSE
    )
  } else {
    tibble(
      Caracteristica = label,
      Total   = fmt_med(var),
      Pre     = fmt_med(var[ic$periodo == "Pre"]),
      Pos     = fmt_med(var[ic$periodo == "Pos"]),
      p_valor = fmt_p(mw_p(var, ic$periodo)),
      header  = FALSE
    )
  }
}

# Linha de cabecalho de secao
hdr <- function(label) tibble(Caracteristica = label,
                               Total = "", Pre = "", Pos = "", p_valor = "",
                               header = TRUE)

# Linhas de categorias
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

tab1 <- bind_rows(
  # -- Header geral
  tibble(Caracteristica = "Total de internacoes ICSAP",
         Total = fmt_n(N),
         Pre   = fmt_n(Npr),
         Pos   = fmt_n(Npo),
         p_valor = "—", header = FALSE),

  # -- Sexo
  hdr("Sexo -- n (%)"),
  build_row(ic$sexo_f, "  Feminino (sexo = 3)"),

  # -- Idade
  build_row(ic$idade, "Idade (anos) -- mediana [IIQ]"),
  hdr("Faixa etaria -- n (%)"),
  cat_rows("fx", levels(ic$fx)),

  # -- Grupos ICSAP
  hdr("Top 5 grupos ICSAP (Portaria 221/2008) -- n (%)"),
  {
    grp_chi_p <- chi_p(ic$grupo %in% top5_gps$grupo, ic$periodo)
    map_dfr(seq_len(nrow(top5_gps)), function(i) {
      g     <- top5_gps$grupo[i]
      lbl   <- top5_gps$label_grp[i]
      cond  <- ic$grupo == g
      p_val <- if (i == 1) fmt_p(grp_chi_p) else ""
      tibble(Caracteristica = paste0("  ", lbl),
             Total = fmt_pct(sum(cond), N),
             Pre   = fmt_pct(sum(cond & ic$periodo == "Pre"), Npr),
             Pos   = fmt_pct(sum(cond & ic$periodo == "Pos"), Npo),
             p_valor = p_val, header = FALSE)
    })
  },

  # -- Regional
  hdr("Regional de saude -- n (%)"),
  {
    regs <- sort(unique(ic$regional[!is.na(ic$regional)]))
    chi_reg <- chi_p(ic$regional, ic$periodo)
    rows_r  <- map_dfr(regs, function(r) {
      cond <- ic$regional == r & !is.na(ic$regional)
      tibble(Caracteristica = paste0("  ", r),
             Total = fmt_pct(sum(cond), N),
             Pre   = fmt_pct(sum(cond & ic$periodo == "Pre"), Npr),
             Pos   = fmt_pct(sum(cond & ic$periodo == "Pos"), Npo),
             p_valor = "", header = FALSE)
    })
    rows_r$p_valor[1] <- fmt_p(chi_reg)
    rows_r
  },

  # -- CS identificado
  build_row(ic$com_cs, "Com CS identificado -- n (%)"),

  # -- Permanencia e custo
  build_row(ic$dias_perm, "Dias de permanencia -- mediana [IIQ]"),
  build_row(ic$val_tot, "Custo por internacao (BRL nominais) -- mediana [IIQ]")
)

write_csv(tab1 |> select(-header), file.path(DIR_DOCS, "tabela1_pacientes.csv"))

gt1 <- tab1 |>
  select(-header) |>
  gt() |>
  cols_label(
    Caracteristica = "Caracteristica",
    Total   = md(sprintf("**Total**<br>n = %s", fmt_n(N))),
    Pre     = md(sprintf("**Pre-Portaria**<br>jan/2022-abr/2024<br>n = %s", fmt_n(Npr))),
    Pos     = md(sprintf("**Pos-Portaria**<br>mai/2024-mar/2026<br>n = %s", fmt_n(Npo))),
    p_valor = md("**p**")
  ) |>
  tab_header(
    title    = "Tabela 1. Caracteristicas das internacoes por condicoes sensiveis a atencao primaria (ICSAP)",
    subtitle = "Belo Horizonte, Minas Gerais, Brasil, jan/2022 a mar/2026"
  ) |>
  tab_spanner(label = "Periodo em relacao a Portaria GM/MS n. 3.493/2024",
              columns = c(Pre, Pos, p_valor)) |>
  tab_source_note(md(
    "Fonte: SIHSUS/DATASUS. Filtro: MUNIC\\_RES = MUNIC\\_MOV = 310.620 (residentes e internados em BH)."
  )) |>
  tab_source_note(md(
    paste0("IIQ: Intervalo Interquartil. CS: Centro de Saude. BRL: valores nominais. ",
           "^a^Qui-quadrado de Pearson. ^b^Teste de Mann-Whitney.")
  )) |>
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
  cols_align(align = "center",
             columns = c(Total, Pre, Pos, p_valor)) |>
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
# TABELA 2 -- Resultados analiticos (3 blocos: ITS/JP | Poisson FE | Impacto)
# =============================================================================
cat("Gerando Tabela 2 (resultados analiticos)...\n")

its  <- read_csv(file.path(DIR_DATA, "its_resultados.csv"),  show_col_types = FALSE)
poi  <- read_csv(file.path(DIR_DATA, "poisson_resultados.csv"), show_col_types = FALSE)
jp   <- read_csv(file.path(DIR_DATA, "joinpoint_resultados.csv"), show_col_types = FALSE)
cus  <- read_csv(file.path(DIR_DATA, "custo_evitado.csv"),    show_col_types = FALSE)

bh  <- its |> filter(nivel == "BH Municipal")
bh1 <- bh[1, ]

# Conversao de mes -> data para Joinpoint
mes2data <- function(m) {
  d <- as.Date("2022-01-01") + months(m - 1)
  format(d, "%b/%Y")
}

jp_bh <- jp |> filter(nivel == "BH Municipal") |>
  mutate(
    data_ini = mes2data(mes_inicio),
    data_fim = mes2data(mes_fim),
    periodo  = sprintf("%s–%s", data_ini, data_fim),
    apc_fmt  = sprintf("%+.1f%%/ano", APC)
  )
aapc_bh <- jp_bh$aapc[1]

# Valores Poisson -- M2 contextual
m2_ivs <- poi |> filter(modelo == "M2_contextual", variavel == "ivs_score")
m2_san <- poi |> filter(modelo == "M2_contextual", variavel == "pct_sem_saneamento")
m_q2   <- poi |> filter(modelo == "M_dose_resposta", variavel == "n_esf_qQ2 (5-6)")

# Impacto -- BH Municipal
bh_imp <- cus |> filter(nivel == "BH Municipal")

# Funcao para IC 95%
ic95 <- function(inf, sup, digits = 1) {
  sprintf("(%.*f; %.*f)", digits, inf, digits, sup)
}

tab2 <- tribble(
  ~bloco,   ~modelo,   ~parametro,   ~estimativa,   ~ic95,   ~p,

  # ---- Bloco 1: ITS-GLS AR(1) ----
  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  "ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)",
  "Tendencia pre-Portaria (APC)",
  sprintf("%+.1f%%/ano", bh1$apc_pre),
  ic95(bh1$apc_pre_inf, bh1$apc_pre_sup),
  if_else(bh1$p_pre < 0.001, "<0,001", fmt_p(bh1$p_pre)),

  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  "ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)",
  sprintf("Mudanca de nivel em mai/2024 (%s%%)", bh1$nivel_pct),
  sprintf("%+.1f%%", bh1$nivel_pct),
  ic95(bh1$nivel_ic_inf, bh1$nivel_ic_sup),
  fmt_p(bh1$p_nivel),

  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  "ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)",
  "Mudanca de tendencia pos-Portaria (APC)",
  sprintf("%+.1f%%/ano", bh1$apc_pos),
  ic95(bh1$apc_pos_inf, bh1$apc_pos_sup),
  if_else(bh1$p_pos < 0.001, "<0,001", fmt_p(bh1$p_pos)),

  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  "ITS-GLS AR(1)\nn = 51 meses (jan/2022–mar/2026)",
  "Correlacao AR(1) (phi)",
  "0,634",
  "—",
  "—",

  # Joinpoint
  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  sprintf("Joinpoint (2 inflexoes)\nAAPC = %+.1f%%/ano", aapc_bh),
  sprintf("Seg. 1: %s (APC)", jp_bh$periodo[1]),
  jp_bh$apc_fmt[1],
  "—",
  "—",

  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  sprintf("Joinpoint (2 inflexoes)\nAAPC = %+.1f%%/ano", aapc_bh),
  sprintf("Seg. 2: %s (APC)", jp_bh$periodo[2]),
  jp_bh$apc_fmt[2],
  "—",
  "<0,001",

  "1. Interrupcao de Serie Temporal (ITS-GLS AR[1]) -- BH Municipal",
  sprintf("Joinpoint (2 inflexoes)\nAAPC = %+.1f%%/ano", aapc_bh),
  sprintf("Seg. 3: %s (APC)", jp_bh$periodo[3]),
  jp_bh$apc_fmt[3],
  "—",
  "<0,001",

  # ---- Bloco 2: Poisson FE ----
  "2. Determinantes da taxa ICSAP -- Poisson FE dois sentidos (153 CS)",
  "M2 -- Efeitos fixos por regional + ano\nn = 7.803 obs. (153 CS x 51 meses)",
  "IVS-BH (IRR por 1 ponto no score)",
  sprintf("%.3f", m2_ivs$irr),
  ic95(m2_ivs$ic_inf, m2_ivs$ic_sup, 3),
  if_else(m2_ivs$p_valor < 0.001, "<0,001", fmt_p(m2_ivs$p_valor)),

  "2. Determinantes da taxa ICSAP -- Poisson FE dois sentidos (153 CS)",
  "M2 -- Efeitos fixos por regional + ano\nn = 7.803 obs. (153 CS x 51 meses)",
  "% domicilios sem saneamento basico (IRR por 1 ponto percentual)",
  sprintf("%.3f", m2_san$irr),
  ic95(m2_san$ic_inf, m2_san$ic_sup, 3),
  fmt_p(m2_san$p_valor),

  "2. Determinantes da taxa ICSAP -- Poisson FE dois sentidos (153 CS)",
  "M2 -- Efeitos fixos por regional + ano\nn = 7.803 obs. (153 CS x 51 meses)",
  "Dispersao de Pearson (M2 regional FE)",
  sprintf("%.2f", m2_ivs$dispersao_pearson),
  "—",
  "—",

  "2. Determinantes da taxa ICSAP -- Poisson FE dois sentidos (153 CS)",
  "Dose-resposta n_esf vs. Q1 (1-4 equipes ESF)",
  "Q2 (5-6 equipes) -- IRR",
  sprintf("%.3f", m_q2$irr),
  ic95(m_q2$ic_inf, m_q2$ic_sup, 3),
  if_else(m_q2$p_valor < 0.001, "<0,001", fmt_p(m_q2$p_valor)),

  "2. Determinantes da taxa ICSAP -- Poisson FE dois sentidos (153 CS)",
  "Dose-resposta n_esf vs. Q1 (1-4 equipes ESF)",
  "Q3-Q4 (>= 7 equipes) -- IRR",
  "NS",
  "—",
  ">0,05",

  # ---- Bloco 3: Impacto ----
  "3. Impacto estimado da Portaria GM/MS n. 3.493/2024 (mai/2024-mar/2026)",
  "GLS AR(1) + Monte Carlo (n = 1.000 iteracoes)",
  "Internacoes ICSAP evitadas em BH (n)",
  fmt_n(bh_imp$evitadas_central),
  sprintf("(%s; %s)", fmt_n(bh_imp$evitadas_ic_inf), fmt_n(bh_imp$evitadas_ic_sup)),
  "—^a",

  "3. Impacto estimado da Portaria GM/MS n. 3.493/2024 (mai/2024-mar/2026)",
  "GLS AR(1) + Monte Carlo (n = 1.000 iteracoes)",
  "Custo evitado -- BRL mar/2026 (R$ milhoes)",
  sprintf("R$ %.2f mi",  bh_imp$custo_central_BRL / 1e6),
  sprintf("(R$ %.2f; R$ %.2f mi)",
          bh_imp$custo_ic_inf_BRL / 1e6, bh_imp$custo_ic_sup_BRL / 1e6),
  "—^a",

  "3. Impacto estimado da Portaria GM/MS n. 3.493/2024 (mai/2024-mar/2026)",
  "GLS AR(1) + Monte Carlo (n = 1.000 iteracoes)",
  "Custo medio por internacao ICSAP (deflacionado pelo IPCA, R$ mar/2026)",
  sprintf("R$ %.2f", bh_imp$custo_medio_BRL),
  "—",
  "—",

  "3. Impacto estimado da Portaria GM/MS n. 3.493/2024 (mai/2024-mar/2026)",
  "DiD-ITS -- BH vs. 6 capitais controle (SP, RJ, Curitiba, Fortaleza, DF, Belem)",
  "Diferenca-na-diferenca na tendencia pos (theta_k, k = 1,...,6)",
  "Nenhum theta_k significativo",
  "—",
  ">0,05"
)

write_csv(tab2, file.path(DIR_DOCS, "tabela2_resultados.csv"))

gt2 <- tab2 |>
  group_by(bloco) |>
  gt() |>
  cols_label(
    modelo     = "Modelo / analise",
    parametro  = "Parametro",
    estimativa = "Estimativa",
    ic95       = "IC 95%",
    p          = md("*p*-valor")
  ) |>
  tab_header(
    title    = "Tabela 2. Resultados analiticos -- ICSAP, Belo Horizonte, 2022-2026",
    subtitle = paste0("APC: Annual Percentage Change. AAPC: Average APC. FE: Efeitos Fixos. ",
                      "IRR: Incidence Rate Ratio. IVS: Indice de Vulnerabilidade em Saude. ",
                      "IC: Intervalo de Confianca de 95%.")
  ) |>
  tab_source_note(md(
    "ITS-GLS AR(1): Kontopantelis et al. (2015). Joinpoint: Muggeo (2003). Poisson FE: Berge (2018) -- pacote *fixest* v0.14."
  )) |>
  tab_source_note(md(
    "DiD-ITS: GLS pooled com interacao capital x tempo\\_pos. Controles: SP, RJ, Curitiba, Fortaleza, DF, Belem."
  )) |>
  tab_source_note(md(
    "^a^ IC 95% calculado por simulacao Monte Carlo (n=1.000 iteracoes via MASS::mvrnorm). Valores deflacionados pelo IPCA (jan/2022-mar/2026; delta=26,4%)."
  )) |>
  tab_style(
    style     = list(cell_fill(color = "#1A5276"),
                     cell_text(color = "white", weight = "bold")),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style     = cell_text(style = "italic", size = px(10)),
    locations = cells_body(columns = modelo)
  ) |>
  cols_align(align = "center", columns = c(estimativa, ic95, p)) |>
  cols_width(
    modelo     ~ px(185),
    parametro  ~ px(265),
    estimativa ~ px(120),
    ic95       ~ px(130),
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

cat("\n=== Todos os outputs gerados em docs/ ===\n")
cat("Figuras: figura1_fluxograma_strobe.png, figura2_its_paineis.png, figura3_mapa_duplo.png\n")
cat("Tabelas: tabela1_pacientes.html/.csv, tabela2_resultados.html/.csv\n")
