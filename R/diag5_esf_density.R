suppressPackageStartupMessages({ library(sf); library(dplyr); library(readr) })
sf::sf_use_s2(FALSE)
select <- dplyr::select
cs_sf <- st_read('data/ref/areas_abrangencia_cs.geojson', quiet=TRUE)
cs_areas <- cs_sf |>
  mutate(area_km2=as.numeric(st_area(st_transform(geometry,31983)))/1e6) |>
  st_drop_geometry() |> select(nome_cs, area_km2)
variaveis <- read_csv('data/ref/variaveis_cs.csv', show_col_types=FALSE)
static_cs <- variaveis |>
  filter(ano_cmpt>=2023, ano_cmpt<=2025, !is.na(n_esf), n_esf>=1) |>
  group_by(nome_cs) |>
  summarise(n_esf_modal=as.numeric(names(sort(table(n_esf),decreasing=TRUE))[1]),
            pop_total=first(pop_total_censo), .groups='drop') |>
  left_join(cs_areas, by='nome_cs') |>
  mutate(densidade=pop_total/area_km2,
         quartil=case_when(n_esf_modal<=4~'Q1 (1-4)',
                           n_esf_modal<=6~'Q2 (5-6)',
                           TRUE~'Q3-Q4 (>=7)'))
cat('=== DENSIDADE POR QUARTIL ESF ===\n')
print(
  static_cs |> group_by(quartil) |>
    summarise(n_cs=n(),
              den_med=round(median(densidade,na.rm=TRUE)),
              den_p25=round(quantile(densidade,.25,na.rm=TRUE)),
              den_p75=round(quantile(densidade,.75,na.rm=TRUE)),
              .groups='drop')
)
rho <- cor.test(static_cs[['n_esf_modal']], static_cs[['densidade']],
                method='spearman', exact=FALSE)
cat(sprintf('\nSpearman n_esf x densidade: rho=%.3f | p=%.4f\n',
            rho[['estimate']], rho[['p.value']]))
# Q2 vs Q3-Q4 density comparison
cat('\nQ2 densidade mediana:', round(median(static_cs$densidade[static_cs$quartil=='Q2 (5-6)'],na.rm=TRUE)), '\n')
cat('Q3-Q4 densidade mediana:', round(median(static_cs$densidade[static_cs$quartil=='Q3-Q4 (>=7)'],na.rm=TRUE)), '\n')
