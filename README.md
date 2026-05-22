# Painel ICSAP-BH

[![Dados Abertos](https://img.shields.io/badge/dados-abertos-brightgreen)](https://datasus.saude.gov.br/)
[![Pesquisa Reprodutível](https://img.shields.io/badge/pesquisa-reprodut%C3%ADvel-blue)](docs/reproducibilidade.md)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.XXXXXXX-blue)](https://zenodo.org)
[![Licença MIT](https://img.shields.io/badge/licen%C3%A7a-MIT-green)](LICENSE)
[![Shiny App](https://img.shields.io/badge/painel-shinyapps.io-orange)](https://brunoavferreira.shinyapps.io/sihsus-icsap-bh/)
[![Reprodutibilidade](https://img.shields.io/badge/reprodutibilidade-100%25-brightgreen)](https://github.com/Brunoavfer/sihsus-icsap-bh)

Painel interativo e pipeline de dados para monitoramento das **Internações por Condições Sensíveis à Atenção Primária (ICSAP)** no município de Belo Horizonte, desagregadas por **regional administrativa** e **Centro de Saúde**.

> **Este projeto é inteiramente baseado em dados públicos do governo brasileiro.**
> Acreditamos que dados abertos salvam vidas.

## Objetivo

Disponibilizar de forma acessível, visual e reproduzível os dados de internações evitáveis ocorridas em BH, permitindo análises por regional, Centro de Saúde, condição de saúde e período — e subsidiar pesquisa científica sobre a efetividade da atenção primária no SUS.

## Documentação completa

**Site:** [brunoavfer.github.io/sihsus-icsap-bh](https://brunoavfer.github.io/sihsus-icsap-bh/)

**Painel interativo:** [brunoavferreira.shinyapps.io/sihsus-icsap-bh](https://brunoavferreira.shinyapps.io/sihsus-icsap-bh/)

## Fontes de Dados

| Fonte | Dado | Acesso |
|---|---|---|
| DATASUS / SIHSUS | Internações hospitalares no SUS — série RD (MG) | [ftp.datasus.gov.br](ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/) |
| DATASUS / CNES | Equipes e profissionais de saúde por estabelecimento | [ftp.datasus.gov.br/CNES](ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/) |
| PBH / SMSA | Polígonos das 153 áreas de abrangência dos CS (CC-BY) | [dados.pbh.gov.br](https://dados.pbh.gov.br/) |
| e-Gestor AB / MS | Cobertura histórica da Atenção Primária em BH | [egestorab.saude.gov.br](https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/) |
| IBGE Censo 2022 | Setores censitários: população, renda, saneamento | [ibge.gov.br](https://www.ibge.gov.br/estatisticas/sociais/populacao/22827-censo-demografico-2022.html) |
| Portaria SAS/MS nº 221/2008 | Lista oficial ICSAP (93 grupos de condições) | [Saúde Legis](https://saude.gov.br/) |

Todos os dados são públicos, gratuitos e sem informações individuais identificáveis.

## Tecnologias

| Tecnologia | Uso |
|---|---|
| R ≥ 4.5.0 | Pipeline de dados e painel interativo |
| `read.dbc` | Leitura de arquivos .dbc do DATASUS |
| `sf`, `geobr`, `censobr` | Geocodificação e análise espacial |
| `spdep` | Autocorrelação espacial (Moran's I, LISA) |
| Shiny + shinydashboard | Painel interativo |
| plotly + leaflet | Gráficos e mapas interativos |
| GitHub Actions | Atualização automática dos dados (dia 10 de cada mês) |

## Regionais de BH

Barreiro · Centro-Sul · Leste · Nordeste · Noroeste · Norte · Oeste · Pampulha · Venda Nova

## Como Reproduzir

```r
# 1. Clone e entre no diretório
git clone https://github.com/Brunoavfer/sihsus-icsap-bh.git

# 2. Pipeline de dados (execute em ordem)
source("R/01_download.R")               # Baixa dados SIHSUS do DATASUS (FTP)
source("R/02_process.R")               # Filtra BH, identifica ICSAP, gera CSVs
source("R/03_cep_regional.R")          # Geocodifica CEP → CS/Regional (APIs + sf)
source("R/04_melhora_cobertura.R")     # Melhora cobertura de geocodificação (opcional)
source("R/05_coleta_variaveis.R")      # Coleta variáveis independentes (CNES, eGestor, Censo)

# 3. Análises descritivas e diagnósticas
source("R/06_analise_missing.R")       # Padrão de missing — MAR/MNAR (13,9% sem geocod.)
source("R/07_padronizacao_taxa.R")     # Taxa ICSAP por CS (nota: taxa bruta — ver protocolo)
source("R/08_autocorrelacao_espacial.R") # Moran's I = 0,283 (p<0,001); 10 clusters HH

# 4. Enriquecimento e alocação
source("R/13_incorpora_ivs.R")         # IVS-BH por CS (SMSA/PBH)
source("R/14_alocacao_proporcional.R") # Alocação proporcional de CEPs limítrofes

# 5. Análises estatísticas principais
source("R/09_glm_gama.R")             # GEE AR-1 painel mensal (φ≈0,96); stepwise
source("R/10_joinpoint.R")            # Joinpoint: AAPC BH = +1,1%/ano; inflexão abr/2024
source("R/11_its.R")                  # ITS GLS AR(1): Portaria 3.493/2024
source("R/12_its_controle.R")         # ITS com controle: BH × SP, RJ, Curitiba, Fortaleza
source("R/15_subgrupos_ivs.R")        # GEE por estrato IVS: CS Muito Elevado não se beneficiou

# 6. Tabelas para o manuscrito
source("R/16_tabela1.R")              # Tabela 1: características dos 153 CS

# 7. Rode o painel interativo
shiny::runApp("app")
```

Consulte [docs/reproducibilidade.md](docs/reproducibilidade.md) para requisitos detalhados, validação dos resultados e checklist de reprodutibilidade.

## Como Citar

### ABNT

FERREIRA, Bruno Ávila. **Painel ICSAP-BH: Internações por Condições Sensíveis à Atenção Primária em Belo Horizonte** [software e conjunto de dados]. Versão 1.0.0. GitHub, 2026. Disponível em: https://github.com/Brunoavfer/sihsus-icsap-bh. Acesso em: [data de acesso].

### APA

Ferreira, B. A. (2026). *ICSAP-BH Dashboard: Hospitalizations for Ambulatory Care-Sensitive Conditions in Belo Horizonte* (Version 1.0.0) [Software and dataset]. GitHub. https://github.com/Brunoavfer/sihsus-icsap-bh

O arquivo [`CITATION.cff`](CITATION.cff) na raiz do repositório permite citação automática pelo GitHub (botão "Cite this repository").

## Citação e Arquivo Permanente

Uma versão arquivada permanentemente estará disponível no Zenodo após a publicação do artigo (DOI a ser atualizado).

## Contribuições

Contribuições são muito bem-vindas! Abra uma [*issue*](https://github.com/Brunoavfer/sihsus-icsap-bh/issues) ou envie um *pull request*.

Consulte [docs/dados_publicos.md](docs/dados_publicos.md) para o manifesto de ciência aberta do projeto.

## Licença

MIT License — veja o arquivo [LICENSE](LICENSE).

Os dados de entrada são de domínio público (DATASUS, IBGE) ou licenciados CC-BY (PBH/SMSA).
