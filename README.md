# Painel ICSAP-BH

[![Dados Abertos](https://img.shields.io/badge/dados-abertos-brightgreen)](https://datasus.saude.gov.br/)
[![Pesquisa Reprodutível](https://img.shields.io/badge/pesquisa-reprodut%C3%ADvel-blue)](docs/reproducibilidade.md)
[![DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.XXXXXXX-blue)](https://zenodo.org)
[![Licença MIT](https://img.shields.io/badge/licen%C3%A7a-MIT-green)](LICENSE)
[![Shiny App](https://img.shields.io/badge/painel-shinyapps.io-orange)](https://brunoavferreira.shinyapps.io/sihsus-icsap-bh/)

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

# 2. Execute os scripts em ordem
source("R/01_download.R")          # Baixa dados do DATASUS
source("R/02_process.R")           # Processa e identifica ICSAP
source("R/03_cep_regional.R")      # Geocodifica CEP → CS/Regional
source("R/04_melhora_cobertura.R") # Melhora cobertura (opcional)
source("R/05_coleta_variaveis.R")  # Coleta variáveis independentes
source("R/06_analise_missing.R")   # Analisa padrão de dados ausentes
source("R/07_padronizacao_taxa.R") # Padroniza taxas por idade/sexo
source("R/08_autocorrelacao_espacial.R") # Moran's I e LISA

# 3. Rode o painel
shiny::runApp("app")
```

Consulte [docs/reproducibilidade.md](docs/reproducibilidade.md) para requisitos detalhados, validação dos resultados e checklist de reprodutibilidade.

## Como Citar

### ABNT

FERREIRA, Bruno Ávila. **Painel ICSAP-BH: Internações por Condições Sensíveis à Atenção Primária em Belo Horizonte** [software e conjunto de dados]. Versão 1.0.0. GitHub, 2026. Disponível em: https://github.com/Brunoavfer/sihsus-icsap-bh. Acesso em: [data de acesso].

### APA

Ferreira, B. A. (2026). *ICSAP-BH Dashboard: Hospitalizations for Ambulatory Care-Sensitive Conditions in Belo Horizonte* (Version 1.0.0) [Software and dataset]. GitHub. https://github.com/Brunoavfer/sihsus-icsap-bh

O arquivo [`CITATION.cff`](CITATION.cff) na raiz do repositório permite citação automática pelo GitHub (botão "Cite this repository").

## Contribuições

Contribuições são muito bem-vindas! Abra uma [*issue*](https://github.com/Brunoavfer/sihsus-icsap-bh/issues) ou envie um *pull request*.

Consulte [docs/dados_publicos.md](docs/dados_publicos.md) para o manifesto de ciência aberta do projeto.

## Licença

MIT License — veja o arquivo [LICENSE](LICENSE).

Os dados de entrada são de domínio público (DATASUS, IBGE) ou licenciados CC-BY (PBH/SMSA).
