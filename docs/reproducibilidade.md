# Guia de Reprodutibilidade — ICSAP-BH

**Versão:** 1.1.0  
**Data:** maio de 2026  
**Padrão de referência:** Nature Reporting Standards · TOP Guidelines · STROBE Checklist

---

## Requisitos de Software

### R e versão mínima

| Software | Versão mínima | Versão testada |
|---|---|---|
| R | 4.5.0 | 4.5.3 |
| RStudio (opcional) | 2023.06 | — |

### Pacotes R obrigatórios

```r
install.packages(c(
  # Pipeline de dados
  "read.dbc",     # >= 1.0.6  — leitura de arquivos .dbc (DATASUS)
  "dplyr",        # >= 1.1.0  — manipulação de dados
  "readr",        # >= 2.1.4  — leitura e escrita de CSV
  "stringr",      # >= 1.5.0  — manipulação de strings
  "lubridate",    # >= 1.9.3  — manipulação de datas
  "purrr",        # >= 1.0.2  — programação funcional

  # Geoespacial
  "sf",           # >= 1.0.14 — operações espaciais (st_join, st_union)
  "geobr",        # >= 1.8.0  — malhas geográficas brasileiras (IBGE)
  "censobr",      # >= 0.3.0  — dados do Censo 2022 por setor censitário

  # Análise estatística
  "spdep",        # >= 1.3.1  — autocorrelação espacial (Moran's I, LISA)

  # Visualização
  "ggplot2",      # >= 3.4.4  — gráficos estáticos (mapas LISA)
  "plotly",       # >= 4.10.3 — gráficos interativos
  "leaflet",      # >= 2.2.0  — mapas interativos web

  # Painel Shiny
  "shiny",        # >= 1.7.5  — framework do painel interativo
  "shinydashboard",# >= 0.7.2 — value boxes e layout do painel

  # Coleta de dados externos
  "httr",         # >= 1.4.7  — requisições HTTP (APIs)
  "jsonlite",     # >= 1.8.7  — parse de JSON
  "curl",         # >= 5.1.0  — download via FTP
  "readxl",       # >= 1.4.3  — leitura de planilhas Excel (e-Gestor AB)
  "stringdist"    # >= 0.9.10 — similaridade textual (de-para CNES)
))
```

### Verificar versões instaladas

```r
pkg <- c("read.dbc","dplyr","readr","stringr","lubridate","sf",
         "geobr","censobr","spdep","ggplot2","plotly","leaflet",
         "shiny","shinydashboard","httr","jsonlite","curl","readxl","stringdist")
sapply(pkg, function(p) as.character(packageVersion(p)))
```

---

## Passo a Passo Completo — Do Zero ao Painel

Execute todos os comandos a partir do **diretório raiz** do projeto (`sihsus-icsap-bh/`).

### Passo 0 — Clonar o repositório

```bash
git clone https://github.com/Brunoavfer/sihsus-icsap-bh.git
cd sihsus-icsap-bh
```

### Passo 1 — Download dos dados SIHSUS

```r
source("R/01_download.R")
```

**O que faz:** Baixa os arquivos `.dbc` da série Reduzida de Morbidade Hospitalar (RDMG) do FTP do DATASUS para `data/raw/`. Competências: jan/2023 a dez/2025 (36 arquivos, ~500 MB total).

**Dependência externa:** FTP DATASUS (`ftp.datasus.gov.br`). Se o servidor estiver offline, tente novamente após algumas horas — o DATASUS realiza manutenções periódicas. Os arquivos já baixados não são re-baixados (cache).

**Tempo estimado:** 10–30 min (depende da conexão).

### Passo 2 — Processamento e identificação ICSAP

```r
source("R/02_process.R")
```

**O que faz:** Lê os `.dbc`, aplica o filtro duplo de município (residente **e** internado em BH, código 310620), cruza com a lista ICSAP (Portaria SAS/MS nº 221/2008) e salva:
- `data/processed/internacoes_bh.csv` — todas as internações (denominador)
- `data/processed/icsap_bh.csv` — apenas ICSAP (numerador)

**Tempo estimado:** 5–15 min.

### Passo 3 — Geocodificação CEP → Centro de Saúde

```r
source("R/03_cep_regional.R")
```

**O que faz:** Para cada CEP único nas internações ICSAP:
1. Busca endereço via ViaCEP
2. Geocodifica via Nominatim/OpenStreetMap
3. Cruza ponto × polígono (st_join) com as áreas de abrangência dos CS

**Dependências externas:** ViaCEP e Nominatim (ambos gratuitos, com rate limiting). O script respeita os limites e usa cache incremental.

**Saídas:**
- `data/processed/icsap_bh_regional.csv` — ICSAP com CS/Regional
- `data/ref/cache_cep.csv` — cache de geocodificação

**Tempo estimado:** 2–8 horas (depende do número de CEPs novos e velocidade das APIs).

### Passo 4 — Melhoria da cobertura (opcional)

```r
source("R/04_melhora_cobertura.R")
```

**O que faz:** Retenta CEPs que falharam no Passo 3 com cascata de APIs alternativas (BrasilAPI, Photon/Komoot, Nominatim por CEP). Melhora a cobertura de geocodificação de ~82% para ~86%.

**Tempo estimado:** 1–3 horas.

### Passo 5 — Coleta de variáveis independentes

```r
# ATENÇÃO: baixe primeiro o e-Gestor manualmente (ver instruções no script)
source("R/05_coleta_variaveis.R")
```

**Pré-requisito manual:** Baixar o relatório de cobertura histórica da APS em Belo Horizonte no [e-Gestor AB](https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/) e salvar como `data/ref/egestor_cobertura_bh.xlsx`.

**O que faz:** Coleta e integra CNES (equipes/profissionais), e-Gestor AB (cobertura), Censo 2022 (censobr) e Aglomerados Subnormais (geobr). Salva `data/ref/variaveis_cs.csv`.

**Tempo estimado:** 30–120 min (Censo e AGSN requerem download volumoso na primeira execução).

### Passo 6 — Análise de sensibilidade dos CEPs ausentes

```r
source("R/06_analise_missing.R")
```

**Saídas:**
- `data/processed/tabela_missing.csv`
- `data/processed/conclusao_missing.txt`

**Tempo estimado:** < 1 min.

### Passo 7 — Padronização direta das taxas ICSAP

```r
source("R/07_padronizacao_taxa.R")
```

**Saída:** `data/processed/taxas_padronizadas.csv`

**Tempo estimado:** < 5 min.

### Passo 8 — Autocorrelação espacial (Moran's I e LISA)

```r
source("R/08_autocorrelacao_espacial.R")
```

**Saídas:**
- `data/processed/moran_resultados.csv`
- `docs/mapa_lisa.png`

**Tempo estimado:** < 5 min.

### Passo 9 — Rodar o painel interativo

```r
shiny::runApp("app")
```

O painel abre no navegador padrão em `http://localhost:XXXX`.

---

## Como Validar os Resultados

Após executar os scripts 1–4, os valores esperados são:

| Indicador | Valor esperado | Script |
|---|---|---|
| Internações ICSAP (total) | 90.869 | `02_process.R` |
| Total de internações BH | 498.246 | `02_process.R` |
| Taxa ICSAP bruta | ~18,2% | `02_process.R` |
| CEPs únicos geocodificados | ~86% | `03_cep_regional.R` / `04_melhora_cobertura.R` |
| CS com pelo menos 1 internação | 153 | `03_cep_regional.R` |
| Competências cobertas | 36 (jan/2023–dez/2025) | `01_download.R` |
| Regionais identificadas | 9 | `03_cep_regional.R` |

Pequenas variações são esperadas se novas competências forem baixadas ou se as APIs de geocodificação retornarem resultados diferentes.

---

## Checklist de Reprodutibilidade (10 itens)

Baseado nos padrões Nature/Science e TOP Guidelines (Nosek et al., 2015).

| # | Item | Status |
|---|---|---|
| 1 | **Código fonte aberto:** Todo o pipeline e o painel estão em repositório público sob licença MIT | ✅ |
| 2 | **Dados de entrada documentados:** Fontes, URLs, versões e datas de acesso documentadas em `CLAUDE.md` e `docs/dados_publicos.md` | ✅ |
| 3 | **Dados intermediários versionados:** `icsap_bh_regional.csv`, `cache_cep.csv` e `variaveis_cs.csv` estão no repositório | ✅ |
| 4 | **Ambiente de software documentado:** Versões mínimas de R e todos os pacotes listadas neste documento | ✅ |
| 5 | **Sementes aleatórias fixadas:** Não há aleatoriedade nos scripts (geocodificação e análises são determinísticas dado o mesmo input) | ✅ |
| 6 | **Resultados numéricos validáveis:** Valores de referência documentados na seção "Como Validar" | ✅ |
| 7 | **Fluxo de análise linear:** Scripts numerados (01–08) executáveis em sequência, sem dependências circulares | ✅ |
| 8 | **Cache de dados externos:** CEPs geocodificados, setores censitários e AGSN são cacheados localmente após o primeiro download | ✅ |
| 9 | **Análise de sensibilidade documentada:** Script 06 avalia o padrão de missingness; script 07 padroniza por idade/sexo | ✅ |
| 10 | **Citação formal:** `CITATION.cff` na raiz do repositório com metadados completos para citação automática pelo GitHub | ✅ |

---

## Limitações Conhecidas à Reprodutibilidade

1. **FTP DATASUS:** O servidor ftp.datasus.gov.br realiza manutenções periódicas e pode ficar temporariamente indisponível. Se o download falhar, aguarde e tente novamente.

2. **APIs de geocodificação (Nominatim):** Sujeitas a rate limiting. O script respeita os limites (~1 req/s) e usa cache. Em conexões lentas, o Passo 3 pode levar várias horas.

3. **e-Gestor AB:** Não possui endpoint REST público documentado. O download do relatório de cobertura histórica requer acesso manual ao portal web (Passo 5).

4. **censobr e geobr:** Dependem de servidores externos do IBGE. Na primeira execução, baixam dados volumosos (~100–500 MB). Execuções subsequentes usam cache local do pacote.

5. **Versões de pacotes:** O pacote `censobr` ainda está em desenvolvimento ativo (< 1.0). A API pode mudar entre versões. Caso `read_tracts()` falhe, verifique a documentação da versão instalada.

6. **Geocodificação não determinística:** Em raros casos, a API Nominatim pode retornar coordenadas ligeiramente diferentes para o mesmo endereço em execuções distintas, afetando quais CS recebem cada internação na margem. A cobertura total (~86%) é estável.

7. **Windows vs. Linux:** O pacote `read.dbc` e a maioria dos pacotes geoespaciais (`sf`, `spdep`) funcionam em ambos os sistemas. Em caso de erros no Windows, verifique se a biblioteca GDAL está instalada corretamente via `sf::sf_extSoftVersion()`.

---

*Para dúvidas ou problemas de reprodutibilidade, abra uma [issue no GitHub](https://github.com/Brunoavfer/sihsus-icsap-bh/issues).*
