# Manifesto de Ciência Aberta em Saúde Pública

**Projeto ICSAP-BH — Internações por Condições Sensíveis à Atenção Primária em Belo Horizonte**

---

## Por que dados públicos são fundamentais para a saúde pública brasileira

O Brasil construiu, ao longo de décadas, um dos sistemas de informação em saúde mais abrangentes do mundo. O DATASUS, o IBGE e os portais de dados abertos municipais como o da PBH reúnem centenas de milhões de registros que documentam o nascimento, o adoecimento, o cuidado e a morte de toda a população brasileira.

Esses dados existem porque cidadãos e profissionais de saúde os produziram — em cada internação registrada, em cada formulário do CNES preenchido, em cada questionário do Censo respondido. São dados públicos, financiados com dinheiro público, para o benefício público.

Quando pesquisadores, gestores e cidadãos os utilizam para entender onde o sistema falha e onde triunfa, esse ciclo se fecha. **Dados abertos salvam vidas — mas apenas quando são usados.**

Este projeto existe para demonstrar que é possível transformar dados brutos e dispersos em conhecimento acionável, de forma transparente e reproduzível, sem custos e sem barreiras.

---

## Como este projeto usa dados públicos

Todos os dados utilizados são integralmente públicos, gratuitos e disponíveis sem restrição de acesso.

| Fonte | O que usamos | Link |
|---|---|---|
| **DATASUS / SIHSUS** | Registros de internações hospitalares no SUS — série Reduzida de Morbidade Hospitalar (RD) para Minas Gerais, jan/2023–mar/2026 | [ftp.datasus.gov.br](ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/) |
| **DATASUS / CNES** | Equipes de saúde (ESF, eMulti, ESB) e profissionais (médicos, ACS) por estabelecimento e competência | [ftp.datasus.gov.br/CNES](ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/200508_/Dados/) |
| **PBH / SMSA** | Polígonos das 153 áreas de abrangência dos Centros de Saúde municipais (licença CC-BY 4.0) | [dados.pbh.gov.br](https://dados.pbh.gov.br/) |
| **e-Gestor AB / DAB/MS** | Cobertura histórica da Atenção Primária à Saúde em Belo Horizonte | [egestorab.saude.gov.br](https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/) |
| **IBGE Censo 2022** | Setores censitários: população, estrutura etária, renda e saneamento | [ibge.gov.br](https://www.ibge.gov.br/estatisticas/sociais/populacao/22827-censo-demografico-2022.html) |
| **IBGE Aglomerados Subnormais 2022** | Polígonos de favelas e comunidades urbanas de BH | [geoftp.ibge.gov.br](https://geoftp.ibge.gov.br/organizacao_do_territorio/estrutura_territorial/aglomerados_subnormais/) |
| **ViaCEP / BrasilAPI** | Geocodificação de CEPs: logradouro e coordenadas geográficas | [viacep.com.br](https://viacep.com.br/) · [brasilapi.com.br](https://brasilapi.com.br/) |
| **Nominatim / OpenStreetMap** | Geocodificação de endereços: lat/lon para cruzamento espacial | [nominatim.openstreetmap.org](https://nominatim.openstreetmap.org/) |

**Nenhum dado individual identificável foi utilizado.** O SIHSUS não contém nome, CPF ou qualquer identificador direto de pacientes. Os dados de Censo são agregados por setor censitário. Este estudo é dispensado de apreciação pelo CEP conforme a Resolução CNS nº 510/2016.

---

## Agradecimento institucional

Este projeto não seria possível sem o investimento público brasileiro em infraestrutura de dados de saúde. Agradecemos a:

- **DATASUS / Ministério da Saúde** — pela manutenção do FTP público e dos sistemas SIHSUS, CNES, e-Gestor AB e TabNet, que permitem o acesso livre e gratuito a décadas de informações em saúde.

- **Secretaria Municipal de Saúde de Belo Horizonte (SMSA/PBH)** — pela disponibilização, sob licença aberta (CC-BY), dos polígonos de área de abrangência dos 153 Centros de Saúde de BH no Portal de Dados Abertos da PBH. Esses polígonos são a espinha dorsal da análise espacial deste projeto.

- **Instituto Brasileiro de Geografia e Estatística (IBGE)** — pelo Censo Demográfico 2022 e pela malha de Aglomerados Subnormais, disponibilizados gratuitamente para a sociedade brasileira.

- **OpenStreetMap e seus colaboradores voluntários** — pela cartografia aberta que torna possível a geocodificação via Nominatim.

- **Desenvolvedores dos pacotes R** utilizados neste projeto: `sf`, `read.dbc`, `shiny`, `leaflet`, `plotly`, `censobr`, `geobr`, `spdep` e todos os demais.

---

## Como reproduzir este estudo do zero

Para replicar completamente os resultados, siga os passos abaixo. Para detalhes, consulte [`docs/reproducibilidade.md`](reproducibilidade.md).

```
1. Clone o repositório:
   git clone https://github.com/Brunoavfer/sihsus-icsap-bh.git

2. Instale os pacotes R (R >= 4.5.0):
   install.packages(c("read.dbc","dplyr","readr","sf","leaflet",
                       "plotly","shiny","shinydashboard","lubridate",
                       "httr","jsonlite","stringr","curl",
                       "censobr","geobr","spdep","stringdist","readxl"))

3. Execute os scripts em ordem (a partir da raiz do projeto):
   source("R/01_download.R")          # Baixa .dbc do DATASUS
   source("R/02_process.R")           # Filtra BH e identifica ICSAP
   source("R/03_cep_regional.R")      # Geocodifica CEPs → CS/Regional
   source("R/04_melhora_cobertura.R") # Melhora geocodificação (opcional)
   source("R/05_coleta_variaveis.R")  # Coleta variáveis independentes
   source("R/06_analise_missing.R")   # Análise de missingness
   source("R/07_padronizacao_taxa.R") # Padronização direta das taxas
   source("R/08_autocorrelacao_espacial.R") # Moran's I e LISA

4. Rode o painel:
   shiny::runApp("app")
```

**Resultado esperado:** 90.869 internações ICSAP, taxa bruta ~18,2%, 153 CS identificados, cobertura de geocodificação ~86%.

---

## Como contribuir

Contribuições são muito bem-vindas! Este é um projeto de código e dados abertos.

- **Reportar erros ou sugerir melhorias:** abra uma [*issue*](https://github.com/Brunoavfer/sihsus-icsap-bh/issues)
- **Contribuir com código:** faça um *fork*, crie um branch e abra um *pull request*
- **Sugerir novas análises ou fontes de dados:** abra uma *issue* com a tag `enhancement`
- **Reportar problemas com APIs ou FTPs do DATASUS:** abra uma *issue* com a tag `data-source`

Ao contribuir, você concorda que suas contribuições serão licenciadas sob os mesmos termos deste projeto (MIT).

---

## Como citar

### ABNT

FERREIRA, Bruno Ávila. **Painel ICSAP-BH: Internações por Condições Sensíveis à Atenção Primária em Belo Horizonte** [software e conjunto de dados]. Versão 1.0.0. GitHub, 2026. Disponível em: https://github.com/Brunoavfer/sihsus-icsap-bh. Acesso em: [data de acesso].

### APA

Ferreira, B. A. (2026). *ICSAP-BH Dashboard: Hospitalizations for Ambulatory Care-Sensitive Conditions in Belo Horizonte* (Version 1.0.0) [Software and dataset]. GitHub. https://github.com/Brunoavfer/sihsus-icsap-bh

### BibTeX

```bibtex
@software{Ferreira2026icsapbh,
  author  = {Ferreira, Bruno {\'A}vila},
  title   = {Painel {ICSAP-BH}: Interna{\c{c}}{\~o}es por Condi{\c{c}}{\~o}es
             Sens{\'i}veis {\`a} Aten{\c{c}}{\~a}o Prim{\'a}ria em
             Belo Horizonte},
  year    = {2026},
  version = {1.0.0},
  url     = {https://github.com/Brunoavfer/sihsus-icsap-bh},
  license = {MIT}
}
```

---

## Declaração de transparência

- **Todos os dados utilizados são públicos**, gratuitos e produzidos ou disponibilizados pelo governo brasileiro.
- **Nenhuma informação individual identificável** foi utilizada. Os dados do SIHSUS são anonimizados por lei.
- **Todo o código** que gera os resultados está disponível neste repositório, sob licença MIT.
- **Não há conflito de interesses.** Esta é uma pesquisa independente, sem financiamento de fontes com interesse nos resultados.
- **Os dados de geocodificação** (cache de CEPs e coordenadas) são derivados de fontes abertas (ViaCEP, Nominatim/OSM, BrasilAPI) e estão disponíveis em `data/ref/cache_cep.csv`.

---

## O valor dos dados abertos para quem toma decisões

> *"Não podemos melhorar o que não medimos. E não podemos medir com equidade o que não compartilhamos."*

**Para gestores de saúde:** os dados ICSAP mostram, com precisão geográfica, onde o sistema de atenção primária precisa de reforço — antes que as internações evitáveis aconteçam.

**Para pesquisadores:** um dataset limpo, geocodificado e com 153 unidades de análise × 36 competências mensais é uma oportunidade rara de estudo ecológico longitudinal em escala subnacional no SUS.

**Para cidadãos e pacientes:** saber que a taxa de internações evitáveis em sua regional de BH é mais alta do que a média não é um dado abstrato — é informação que pode e deve orientar a mobilização comunitária por mais equipes ESF, mais médicos de família e mais investimento na atenção básica.

**Para o SUS:** cada internação ICSAP evitada representa não apenas o sofrimento humano poupado, mas também recursos que podem ser redirecionados para a prevenção. O estudo deste indicador é, em si mesmo, um ato de defesa do sistema público de saúde.

---

*Este manifesto é parte do projeto [ICSAP-BH](https://github.com/Brunoavfer/sihsus-icsap-bh), desenvolvido como pesquisa científica independente. O painel interativo está disponível em: [brunoavferreira.shinyapps.io/sihsus-icsap-bh](https://brunoavferreira.shinyapps.io/sihsus-icsap-bh/).*
