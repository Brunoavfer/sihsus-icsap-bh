# CLAUDE.md — Briefing do Projeto ICSAP-BH

## Objetivo

Painel interativo de monitoramento das **Internações por Condições Sensíveis à Atenção Primária (ICSAP)** no município de Belo Horizonte, desagregadas por **regional administrativa** e **Centro de Saúde (CS)**.

ICSAP são internações que poderiam ter sido evitadas caso a atenção primária à saúde (APS) tivesse atuado de forma efetiva. O indicador é definido pela **Portaria SAS/MS nº 221/2008** e serve como proxy da qualidade da APS municipal.

**Documentação completa:** https://brunoavfer.github.io/sihsus-icsap-bh/

---

## Estrutura de Arquivos

```
sihsus-icsap-bh/
├── R/
│   ├── 01_download.R          # Baixa arquivos .dbc do FTP do DATASUS
│   ├── 02_process.R           # Filtra BH, cruza com lista ICSAP, gera CSVs
│   ├── 03_cep_regional.R      # CEP → lat/lon → CS/Regional (via APIs + sf)
│   ├── 04_melhora_cobertura.R # Retenta CEPs que falharam com cascata de APIs
│   ├── 05_coleta_variaveis.R  # Coleta variáveis independentes (CNES, e-Gestor, Censo)
│   ├── 06_analise_missing.R   # Análise de sensibilidade dos 13,9% sem geocodificação
│   ├── 07_padronizacao_taxa.R # Padronização direta por idade e sexo (Ahmad et al., 2001)
│   └── 08_autocorrelacao_espacial.R  # Moran's I global e LISA (Anselin, 1995)
├── app/
│   ├── global.R               # Carrega pacotes, dados e variáveis globais
│   ├── ui.R                   # Interface Shiny (filtros, abas, componentes)
│   └── server.R               # Lógica reativa (gráficos, mapa, tabela)
├── data/
│   ├── raw/                   # Arquivos .dbc brutos do DATASUS (não versionados)
│   ├── processed/             # CSVs tratados (versionados)
│   │   ├── internacoes_bh.csv         # Todas as internações BH (denominador)
│   │   ├── icsap_bh.csv               # Apenas ICSAP (numerador)
│   │   ├── icsap_bh_regional.csv      # ICSAP + CS/Regional (produto final)
│   │   ├── ceps_nao_encontrados.csv   # Log de falhas na geocodificação
│   │   ├── taxas_padronizadas.csv     # Taxa ICSAP padronizada por CS × ano (script 07)
│   │   ├── moran_resultados.csv       # Classificação LISA por CS (script 08)
│   │   ├── tabela_missing.csv         # Comparação COM vs SEM regional (script 06)
│   │   └── conclusao_missing.txt      # Interpretação MAR/MNAR (script 06)
│   └── ref/                   # Referências estáticas
│       ├── lista_icsap.csv            # CIDs ICSAP (Portaria 221/2008)
│       ├── cache_cep.csv              # Cache de geocodificação por CEP
│       ├── areas_abrangencia_cs.geojson  # Polígonos oficiais PBH/SMSA
│       ├── area_abrangencia_saude.csv    # Fonte CSV dos polígonos (PBH)
│       ├── bairros_regional_bh.csv       # Tabela bairro → regional (auxiliar)
│       ├── variaveis_cs.csv           # Variáveis independentes por CS × competência (script 05)
│       ├── censo2022_cs_bh.csv        # Censo 2022 agregado por CS (censobr)
│       ├── depara_cnes_cs.csv         # De-para CNES ↔ CS (via CEP + sf)
│       ├── favelas_cs_bh.csv          # % área de favela por CS (code_favela/geobr)
│       ├── egestor_cobertura_bh.xlsx  # Cobertura ESF BH — e-Gestor AB (manual)
│       └── egestor_cobertura_bh.csv   # Idem em CSV
├── docs/
│   ├── index.html             # Site de documentação (GitHub Pages)
│   ├── metodologia_cep_cs.md  # Documentação detalhada do pipeline de geocodificação
│   └── mapa_lisa.png          # Mapa LISA de autocorrelação espacial (script 08)
└── .github/
    └── workflows/
        └── atualizar_dados.yml  # GitHub Actions — executa dia 10 de cada mês
```

---

## Pipeline de Dados

Os scripts R devem ser executados em ordem:

```
01_download.R → 02_process.R → 03_cep_regional.R → [04_melhora_cobertura.R]
```

| Script | Entrada | Saída |
|---|---|---|
| `01_download.R` | FTP DATASUS | `data/raw/RDMG*.dbc` |
| `02_process.R` | `.dbc` + `lista_icsap.csv` | `internacoes_bh.csv`, `icsap_bh.csv` |
| `03_cep_regional.R` | `icsap_bh.csv` + polígonos PBH | `icsap_bh_regional.csv`, `cache_cep.csv` |
| `04_melhora_cobertura.R` | `cache_cep.csv` | `cache_cep.csv` atualizado, `icsap_bh_regional.csv` regerado |

O script `04_melhora_cobertura.R` é opcional — usado para melhorar a cobertura de geocodificação retentando CEPs que falharam no script 03 com uma cascata de APIs alternativas.

---

## Decisões Metodológicas Importantes

### 1. Filtro duplo de município (02_process.R)

São incluídas **apenas** internações que satisfazem **ambos** os critérios:
- `MUNIC_RES == 310620` — paciente **reside** em BH
- `MUNIC_MOV == 310620` — internação **ocorreu** em BH

Isso garante que o indicador avalia a efetividade da APS de BH para sua própria população, excluindo residentes de BH internados fora e não-residentes internados em BH.

### 2. Identificação do CS por geoprocessamento (03_cep_regional.R)

A alocação CEP → Centro de Saúde é feita por **cruzamento espacial ponto × polígono**, não por tabela de bairros. Isso é necessário porque um mesmo bairro pode ser coberto por mais de um CS (ex.: bairro "da Graça" é dividido entre CS Alcides Lins e CS Cidade Ozanan).

**Fluxo de geocodificação:**
1. **ViaCEP** — CEP → logradouro + bairro + cidade
2. **Nominatim (OSM)** — endereço → lat/lon (WGS 84 / EPSG:4326)
3. **st_join (pacote `sf`)** — ponto × polígonos oficiais PBH/SMSA → CS + Regional

**Fonte dos polígonos:** Portal de Dados Abertos da PBH — o script busca automaticamente a versão mais recente via web scraping; se falhar, usa arquivo local em `data/ref/`.

### 3. Sistema de cache de CEPs (03_cep_regional.R)

O arquivo `data/ref/cache_cep.csv` persiste os resultados de geocodificação por CEP entre execuções. Nas atualizações mensais, apenas CEPs **novos** (não presentes no cache) são consultados nas APIs. O cache é salvo incrementalmente a cada 50 CEPs para evitar perda de progresso.

### 4. Cascata de APIs para recuperação de falhas (04_melhora_cobertura.R)

CEPs que falharam no Nominatim são retentados na seguinte ordem:
1. **BrasilAPI** — retorna coordenadas diretamente quando disponível
2. **BrasilAPI + Photon/Komoot** — endereço via BrasilAPI → geocodificação via Photon
3. **Nominatim por CEP postal** — busca pelo código CEP em vez do endereço textual
4. **Photon direto** — busca pelo CEP como string

CEPs de outros municípios (erro de cadastro no SIHSUS) não são retentados.

### 5. Taxa ICSAP (%)

```
Taxa ICSAP = (internações ICSAP / total de internações BH) × 100
```

O denominador é o total de internações de residentes **e** internados em BH — mesma base do filtro duplo. Isso permite calcular a taxa corretamente mesmo quando os filtros do app reduzem o numerador.

---

## Tecnologias

| Tecnologia | Uso |
|---|---|
| R ≥ 4.3 | Linguagem principal do pipeline e do app |
| `read.dbc` | Leitura de arquivos .dbc do DATASUS |
| `dplyr`, `readr`, `lubridate` | Manipulação de dados |
| `sf` | Operações espaciais (st_join, st_transform, st_as_sf) |
| `httr`, `jsonlite` | Consultas às APIs (ViaCEP, Nominatim, BrasilAPI, Photon) |
| `rvest` | Web scraping do Portal de Dados Abertos da PBH |
| Shiny | Framework do painel interativo |
| `plotly` | Gráficos interativos (evolução, regional, condição, ranking CS) |
| `leaflet` | Mapa interativo com polígonos coloridos por taxa ICSAP |
| GitHub Actions | Atualização automática dos dados todo dia 10 de cada mês |

**APIs externas utilizadas (todas gratuitas/open source):**
- ViaCEP (`viacep.com.br`) — CEP → endereço
- Nominatim / OpenStreetMap (`nominatim.openstreetmap.org`) — geocodificação
- BrasilAPI (`brasilapi.com.br`) — coordenadas diretas por CEP
- Photon/Komoot (`photon.komoot.io`) — geocodificação alternativa

---

## Interface do Painel (app/)

O app Shiny possui 5 abas:

| Aba | Conteúdo |
|---|---|
| Visão Geral | 3 value boxes (total ICSAP, taxa ICSAP, regional líder) + evolução temporal + gráfico por regional |
| Mapa | Mapa coroplético leaflet por regional ou por CS (selecionável) |
| Por Condição | Barras horizontais com top 15 condições ICSAP |
| Ranking CS | Top 20 Centros de Saúde por taxa ICSAP, coloridos por regional |
| Dados | Tabela filtrável + botão de download CSV |

**Filtros disponíveis:** período (slider), regional, Centro de Saúde (atualizado dinamicamente conforme regional), condição ICSAP.

O filtro de CS é dependente do filtro de regional — ao selecionar uma regional, a lista de CS é filtrada para exibir apenas os CS daquela regional.

---

## Status Atual (maio de 2026)

### Dados SIHSUS
- **Dados baixados:** janeiro/2023 a março/2026 — 39 competências (`RDMG2301.dbc` a `RDMG2603.dbc`)
- **Dados processados:** `internacoes_bh.csv` e `icsap_bh.csv` com série completa jan/2023–mar/2026
  - 498.246 internações de residentes e internados em BH
  - 90.869 internações ICSAP | taxa média: 18,2%
- **`icsap_bh_regional.csv`:** ainda reflete apenas jan/2023–mai/2025 (aguarda re-execução do script 03 com série completa)
- **Cobertura de geocodificação:** 86,1% (78.251/90.869 geocodificados)

### Scripts concluídos
- ✅ **Scripts 01–04** — pipeline de download, processamento e geocodificação
- ✅ **Script 05** — variáveis independentes coletadas em `data/ref/variaveis_cs.csv`
  - 5.508 obs (153 CS × 36 competências); cobertura: n_esf 76,5%, cobertura_aps_pct 100%, pop_total_censo 100%, pct_sem_saneamento 100%, renda_media 100%, pct_area_favela 100%
  - Limitação: pct_idosos/pct_criancas = 0% (censobr Basico não tem faixas etárias por setor)
  - Limitação: n_medicos/n_mfc = NA (arquivos PF/MG causam SIGSEGV no read.dbc)
- ✅ **Script 06** — análise de missing: 13,9% sem geocodificação, classificação MNAR (5/6 testes significativos, mas diferenças de magnitude pequena — reportar como limitação)
- ✅ **Script 07** — taxas padronizadas por CS × ano: 612 obs; correlação bruta = padronizada = 1,000 (esperado: mesma distribuição etária de BH aplicada a todos os CS por ausência de faixas por setor)
- ✅ **Script 08** — autocorrelação espacial: **Moran's I = 0,283 (p < 0,001)**, 10 clusters High-High (6,5%); mapa LISA em `docs/mapa_lisa.png`; resultado indica necessidade de GEE AR-1 ou SAR em vez de GLM-Gama padrão

### Infraestrutura
- **App Shiny** implementado com todas as abas
- **GitHub Actions** configurado para atualização automática dia 10 de cada mês
- **`01_download.R`** parametrizado para baixar de jan/2023 ao ano corrente automaticamente

---

## Protocolo Científico

O projeto evoluiu de painel de monitoramento para **estudo científico com protocolo formal**, visando publicação em periódico Qualis A1.

### Desenho

**Estudo ecológico longitudinal de séries temporais**
- Unidade de análise: área de abrangência dos 153 Centros de Saúde de BH
- Período: jan/2023–dez/2025 (36 competências mensais)
- Painel: 153 CS × 36 meses = 5.508 observações potenciais

### Desfecho

Taxa ICSAP **padronizada por idade e sexo** por 10.000 habitantes, por área de abrangência de CS (padronização direta, população-padrão: BH Censo 2022).

### Variáveis Independentes

| Variável | Fonte | Status |
|---|---|---|
| Cobertura ESF (%) | e-Gestor AB/DAB | ✅ Coletada (nível municipal) |
| Nº equipes ESF / eMulti / ESB | CNES EP/DATASUS | ✅ Coletada (76,5% CS) |
| Médicos de família (CH) | CNES PF/DATASUS | ❌ Indisponível (SIGSEGV no read.dbc) |
| % domicílios sem rede geral de água | Censo IBGE 2022 (censobr) | ✅ Coletada (100% CS) |
| Renda per capita (salários mínimos) | Censo IBGE 2022 (censobr) | ✅ Coletada (100% CS) |
| % área de favela por CS | Censo IBGE 2022 (code_favela/geobr) | ✅ Coletada (100% CS) |
| População total por CS | Censo IBGE 2022 (censobr V0001) | ✅ Coletada (100% CS) |
| % idosos / % crianças por CS | Censo IBGE 2022 (censobr Pessoa) | ❌ Não disponível no dataset Basico |
| IVS-BH | SMSA/PBH | ❌ A coletar manualmente |

### Método Estatístico

- **GLM-Gama** (link log) — replica Oliveira et al. (2025) para o nível de CS
- **Joinpoint regression** — APC e AAPC para análise de tendência temporal
- **GEE** (AR-1 ou estrutura espacial) — **indicado**: Moran's I = 0,283 (p < 0,001) detectado no script 08; autocorrelação positiva viola independência do GLM padrão

### Periódico Alvo

*Cadernos de Saúde Pública* (Fiocruz) — Qualis A1 · 4.000 palavras · Vancouver

### Documentação do Protocolo

| Documento | Localização |
|---|---|
| Protocolo completo | `docs/protocolo_pesquisa.md` |
| Checklist STROBE | `docs/checklist_strobe.md` |
| Tabela de variáveis | `docs/variaveis_estudo.md` |
| Referências BibTeX | `docs/referencias.bib` |

### Próximos Passos Metodológicos

1. ✅ ~~Coletar variáveis independentes~~ — concluído (script 05)
2. ✅ ~~Análise de missing~~ — concluído (script 06, MNAR leve)
3. ✅ ~~Padronização de taxas~~ — concluído (script 07)
4. ✅ ~~Autocorrelação espacial~~ — concluído (script 08, Moran's I = 0,283)
5. **Re-executar script 03** com série completa jan/2023–dez/2025 para atualizar `icsap_bh_regional.csv`
6. **Implementar GLM-Gama** + GEE AR-1 (indicado pelo Moran's I positivo) — `R/09_glm_gama.R`
7. **Implementar joinpoint regression** para APC/AAPC — `R/10_joinpoint.R`
8. **Coletar IVS-BH** manualmente do portal SMSA/PBH e incorporar ao `variaveis_cs.csv`
9. **Redigir manuscrito** para submissão ao *Cadernos de Saúde Pública* (meta: jan/2027)

---

## Próximos Passos

### Prioritários (para o estudo científico)
- **Re-executar script 03** com série completa jan/2023–dez/2025 — `icsap_bh_regional.csv` atual cobre apenas jan/2023–mai/2025
- **Implementar GLM-Gama + GEE** — `R/09_glm_gama.R` com estrutura AR-1 (indicado pelo Moran's I = 0,283)
- **Implementar joinpoint regression** — `R/10_joinpoint.R` para APC e AAPC por CS e regional
- **Coletar IVS-BH** — Índice de Vulnerabilidade em Saúde do portal SMSA/PBH; incorporar ao `variaveis_cs.csv`

### App e infraestrutura
- **Melhorar cobertura para ≥90%** — re-executar script 04 após script 03 atualizado; avaliar uso da API Claude para CEPs irrecuperáveis pelas APIs open source
- **Publicar o app** — deploy do Shiny app (shinyapps.io ou Posit Connect)
- **Incluir script 03 no GitHub Actions** — atualmente o workflow automatiza apenas os scripts 01 e 02
- **Documentar seção "Como Usar"** no README.md

---

## Fonte de Dados

- **SIHSUS** (Sistema de Informações Hospitalares do SUS) — DATASUS
  - Arquivo: Reduzida de Morbidade Hospitalar — série RDMG (Minas Gerais)
  - FTP: `ftp://ftp.datasus.gov.br/dissemin/publicos/SIHSUS/200801_/Dados/`
- **Lista ICSAP** — Portaria SAS/MS nº 221/2008
- **Polígonos de área de abrangência** — Portal de Dados Abertos PBH/SMSA (Licença CC Attribution)
- **Código IBGE de BH:** `310620`
