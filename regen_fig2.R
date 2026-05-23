# encoding: UTF-8
# regen_fig2.R — Regenera apenas Figura 2 com correções v5 (auditoria 23/05/2026)
# Painel B: margem superior +4, anotações APC reposicionadas (y_max_b-2.5 / -4.5)
# Confirmado: evitadas_mes é mensal (não acumulado), geom_hline já presente

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(patchwork)
  library(scales)
  library(zoo)
  library(ggrepel)
  library(ragg)
  library(lubridate)
})

options(OutDec = ",", scipen = 999)

DPI    <- 300
W_IN   <- 170 / 25.4
DIR_DATA <- "data/processed"
DIR_DOCS <- "docs"

# --- Theme Lancet ---
theme_lancet <- function(base_size = 8) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      text             = element_text(family = "sans"),
      axis.line        = element_line(colour = "grey40", linewidth = 0.35),
      axis.ticks       = element_line(colour = "grey40", linewidth = 0.25),
      axis.title       = element_text(size = base_size * 0.95),
      axis.text        = element_text(size = base_size * 0.85, colour = "grey20"),
      legend.text      = element_text(size = base_size * 0.85),
      legend.key.size  = unit(3.5, "mm"),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.25),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(size = base_size, face = "bold",
                                        hjust = 0, margin = margin(b = 3)),
      plot.caption       = element_text(size = base_size * 0.80, colour = "grey35",
                                        hjust = 0, lineheight = 1.3),
      strip.background   = element_blank(),
      strip.text         = element_text(size = base_size * 0.90, face = "bold")
    )
}

cat("Gerando Figura 2 (ITS 4 painéis) — v5...\n")

ev <- read_csv(file.path(DIR_DATA, "internacoes_evitadas.csv"),
               show_col_types = FALSE) |>
  mutate(
    data    = as.Date(data),
    periodo = if_else(data < as.Date("2024-05-01"),
                      "Pré-intervenção", "Pós-intervenção"),
    periodo = factor(periodo,
                     levels = c("Pré-intervenção", "Pós-intervenção")),
    ma3 = rollmean(n_icsap, k = 3, fill = NA, align = "center")
  )

custo_medio <- read_csv(file.path(DIR_DATA, "custo_evitado.csv"),
                        show_col_types = FALSE) |>
  filter(nivel == "BH Municipal") |>
  pull(custo_medio_BRL)
cat(sprintf("  custo_medio (IPCA mensal): R$ %.2f\n", custo_medio))

ev_pos <- filter(ev, !is.na(taxa_cf)) |>
  mutate(
    custo_mes_central = evitadas_mes * custo_medio,
    evit_inf = pmax(0, (cf_ic_inf - taxa_obs) * n_total / 100),
    evit_sup = pmax(0, (cf_ic_sup - taxa_obs) * n_total / 100),
    custo_mes_inf     = evit_inf * custo_medio,
    custo_mes_sup     = evit_sup * custo_medio,
    custo_acum        = cumsum(custo_mes_central) / 1e6,
    custo_acum_inf    = cumsum(custo_mes_inf)     / 1e6,
    custo_acum_sup    = cumsum(custo_mes_sup)     / 1e6
  )

d_int      <- as.Date("2024-05-01")
y_max_a    <- max(ev$n_icsap, na.rm = TRUE)
x_ann_pre  <- as.Date("2022-09-01")
x_ann_pos  <- as.Date("2024-09-01")
# v5: margem +4 (era +1) para acomodar rótulos APC sem truncagem
y_max_b    <- ceiling(max(ev$taxa_obs, ev_pos$cf_ic_sup, na.rm = TRUE)) + 4
y_min_b    <- floor(min(ev$taxa_obs, ev_pos$cf_ic_inf, na.rm = TRUE)) - 1

cat(sprintf("  y_max_b = %.1f, y_min_b = %.1f\n", y_max_b, y_min_b))
cat(sprintf("  evitadas_mes pós (primeiros 5): %.1f, %.1f, %.1f, %.1f, %.1f\n",
            ev_pos$evitadas_mes[1], ev_pos$evitadas_mes[2], ev_pos$evitadas_mes[3],
            ev_pos$evitadas_mes[4], ev_pos$evitadas_mes[5]))
cat(sprintf("  custo_acum final: R$ %.2f mi | ribbon sup: R$ %.2f mi\n",
            max(ev_pos$custo_acum), max(ev_pos$custo_acum_sup)))

lbl_interv <- "Portaria GM/MS\nnº 3.493/2024"

# --- Painel A: n absoluto + média móvel ---
p2a <- ggplot(ev, aes(x = data, y = n_icsap, fill = periodo)) +
  geom_col(width = 26, colour = NA, alpha = 0.80) +
  geom_line(aes(y = ma3), colour = "#0D3349", linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = d_int,
             linetype = "dashed", colour = "#C0392B", linewidth = 0.55) +
  annotate("text",
           x = d_int + 25, y = y_max_a * 0.96,
           label = lbl_interv,
           size = 2.1, hjust = 0, colour = "#C0392B", lineheight = 1.1) +
  scale_fill_manual(
    values = c("Pré-intervenção" = "#90CAF9",
               "Pós-intervenção"  = "#1565C0"),
    name = NULL
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.07))) +
  labs(x = NULL,
       y = "Internações ICSAP (n)",
       title = "A") +
  theme_lancet() +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1, size = 6.5),
        legend.position  = c(0.13, 0.88),
        legend.background = element_blank(),
        plot.title        = element_text(size = 9, face = "bold"))

# --- Painel B: taxa + contrafactual + APCs ---
# v5: y_max_b+4; APC labels em y_max_b-2.5; Delta em y_max_b-4.5; size=1.8
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
           x = x_ann_pre, y = y_max_b - 2.5,
           label = "APC pré: +12,3%/ano\n(IC95%: 5,8; 19,2; p<0,001)",
           size = 1.8, hjust = 0, colour = "#1565C0",
           fill = "white", label.padding = unit(1.2, "mm")) +
  annotate("label",
           x = x_ann_pos, y = y_max_b - 2.5,
           label = "APC pós: -8,3%/ano\n(IC95%: -12,1; -4,5; p<0,001)",
           size = 1.8, hjust = 0, colour = "#C0392B",
           fill = "white", label.padding = unit(1.2, "mm")) +
  annotate("text",
           x = x_ann_pos, y = y_max_b - 4.5,
           label = "Δ tendência: -20,6%/ano (p<0,001)",
           size = 1.8, hjust = 0, colour = "grey25") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(y_min_b, y_max_b)) +
  scale_colour_manual(values = c("Pré-intervenção" = "#90CAF9",
                                 "Pós-intervenção"  = "#1565C0")) +
  labs(x = "Competência (mês/ano)",
       y = "Taxa ICSAP (%)",
       title = "B") +
  theme_lancet() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
        plot.title  = element_text(size = 9, face = "bold"))

# --- Painel C: internações evitadas por mês — v5.1: barra negativa mai/2024 ---
# mai/2024: evitadas_mes = -115,6 (mais internações do que o contrafactual previa)
ev_c <- ev |>
  mutate(
    evit_bar = if_else(!is.na(taxa_cf), evitadas_mes, 0),
    bar_pos  = evit_bar >= 0   # TRUE = verde, FALSE = vermelho
  )

p2c <- ggplot(ev_c, aes(x = data)) +
  geom_col(aes(y = evit_bar, fill = bar_pos),
           alpha = 0.80, width = 26, colour = NA) +
  scale_fill_manual(
    values = c("TRUE" = "#2E7D32", "FALSE" = "#C62828"),
    guide  = "none"
  ) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
  geom_vline(xintercept = d_int,
             linetype = "dashed", colour = "#C0392B", linewidth = 0.55) +
  annotate("text",
           x = as.Date("2025-04-01"),
           y = max(ev_pos$evitadas_mes, na.rm = TRUE) * 0.92,
           label = "Total: 13.501\n(IC95%: 5.189–22.575)",
           size = 2.1, hjust = 0.5, colour = "#1B5E20", fontface = "bold") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0.12, 0.10))) +
  labs(x = "Competência (mês/ano)",
       y = "Internações evitadas (n/mês)",
       title = "C",
       caption = "Barra vermelha (mai/2024): mais internações\nobservadas do que o contrafactual previu") +
  theme_lancet() +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 6.5),
        plot.title   = element_text(size = 9, face = "bold"),
        plot.caption = element_text(size = 5.5, colour = "grey50", hjust = 0,
                                    margin = margin(t = 3)))

# --- Painel D: custo evitado acumulado (R$ milhões, valores mar/2026) ---
p2d <- ggplot(ev_pos, aes(x = data)) +
  geom_ribbon(aes(ymin = custo_acum_inf, ymax = custo_acum_sup),
              fill = "#A5D6A7", alpha = 0.60) +
  geom_line(aes(y = custo_acum),
            colour = "#2E7D32", linewidth = 0.9) +
  geom_vline(xintercept = d_int,
             linetype = "dashed", colour = "#C0392B", linewidth = 0.55) +
  annotate("label",
           x = max(ev_pos$data) - 15,
           y = 21.8,
           label = "R$ 29,05 mi\n(IC95%: 11,16–48,57)",
           hjust = 1,
           fill = "white", colour = "#2E7D32",
           linewidth = 0.3, size = 2.5,
           fontface = "bold") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b/%Y",
               expand = expansion(mult = 0.01)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, big.mark = ".",
                                            decimal.mark = ",",
                                            suffix = ""),
                     expand = expansion(mult = c(0, 0.10))) +
  labs(x = "Competência (mês/ano)",
       y = "Custo evitado acumulado\n(R$ milhões, mar/2026)",
       title = "D") +
  theme_lancet() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5),
        plot.title  = element_text(size = 9, face = "bold"))

fig2 <- (p2a | p2b) / (p2c | p2d) +
  plot_annotation(
    caption = paste0(
      "Figura 2. Séries temporais de internações por condições sensíveis à atenção primária (ICSAP). ",
      "Belo Horizonte, Minas Gerais, Brasil, janeiro de 2022 a março de 2026.\n",
      "(A) Número absoluto de internações ICSAP por mês; linha azul escura = média móvel de 3 meses. ",
      "(B) Taxa ICSAP observada (linha sólida azul) e contrafactual estimado pelo modelo ITS ",
      "(linha vermelha tracejada; faixa = IC 95% sombreado).\n",
      "(C) Internações ICSAP evitadas por mês no período pós-Portaria (diferença entre contrafactual e observado). ",
      "(D) Custo evitado acumulado (R$ milhões, valores de março/2026, deflacionados pelo IPCA).\n",
      "A linha vertical tracejada indica o início da vigência da Portaria GM/MS nº 3.493/2024 (maio/2024). ",
      "APC: Annual Percent Change. IC: Intervalo de Confiança de 95%. ",
      "Modelo: séries temporais interrompidas (ITS) com mínimos quadrados generalizados (GLS) e correção autorregressiva AR(1).\n",
      "Fonte: SIH/SUS – DATASUS. Elaboração própria."
    ),
    theme = theme(plot.caption = element_text(size = 6.3, colour = "grey35",
                                              hjust = 0, lineheight = 1.3))
  )

out_fig2 <- file.path(DIR_DOCS, "figura2_its_4paineis.png")
ggsave(out_fig2,
       fig2,
       width  = W_IN,
       height = W_IN * 1.45,
       dpi    = DPI,
       device = ragg::agg_png,
       bg     = "white")
cat(sprintf("  ok %s\n", out_fig2))
