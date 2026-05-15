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
│   ├── 08_autocorrelacao_espacial.R  # Moran's I global e LISA (Anselin, 1995)
│   ├── 09_glm_gama.R         # GLM-Gama + GEE AR-1 (Zeger & Liang, 1986) + VIF + stepwise
│   ├── 10_joinpoint.R        # Joinpoint regression — APC e AAPC (Muggeo, 2003)
│   ├── 11_its.R              # ITS GLS AR(1): Portaria 3.493/2024 — BH municipal + 9 regionais
│   ├── 12_its_controle.R     # ITS com controle: BH × SP, RJ, Curitiba, Fortaleza (especificidade)
│   ├── 13_incorpora_ivs.R    # Integra IVS-BH (SMSA/PBH) em ivs_por_cs.csv
│   ├── 14_alocacao_proporcional.R  # Alocação proporcional CEPs limítrofes entre CS
│   ├── 15_subgrupos_ivs.R    # GEE AR-1 estratificado por nível IVS (Baixo/Médio/Elevado/M.Elevado)
│   ├── 16_tabela1.R          # Tabela 1 descritiva dos 153 CS para o manuscrito
│   ├── 17_did_its.R          # DiD-ITS formal: BH × 4 capitais (θ = slope change diferencial)
│   └── 18_its_ivs.R          # ITS × IVS: interação ivs_z:tempo_pos (heterogeneidade pós-Portaria)
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
│   │   ├── conclusao_missing.txt      # Interpretação MAR/MNAR (script 06)
│   │   ├── glm_resultados.csv         # Coeficientes, RR, IC 95% por modelo (script 09)
│   │   ├── glm_diagnosticos.csv       # QIC, correlação AR-1, N por modelo (script 09)
│   │   ├── joinpoint_resultados.csv   # APC/AAPC por segmento, BH e regional (script 10)
│   │   ├── cep_pesos_cs.csv           # Tabela CEP × CS × peso proporcional (script 14)
│   │   ├── n_icsap_cs_mes_prop.csv    # n_icsap por CS × mês com alocação proporcional (script 14)
│   │   ├── alocacao_impacto.txt       # Relatório de impacto da alocação proporcional (script 14)
│   │   ├── gee_subgrupos_ivs.csv      # Coeficientes GEE por estrato IVS (script 15)
│   │   ├── its_resultados.csv         # ITS BH + 9 regionais: β₁,β₂,β₃, APC pré/pós (script 11)
│   │   ├── its_controle_resultados.csv # ITS comparativo BH × 4 capitais (script 12)
│   │   ├── serie_controles.csv        # Séries mensais por capital (script 12)
│   │   ├── did_its_resultados.csv     # DiD-ITS BH × controles: θ nível e slope (script 17)
│   │   └── its_ivs_resultados.csv     # GEE AR-1 ivs_z:tempo_pos por modelo (script 18)
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
│       ├── egestor_cobertura_bh.csv   # Idem em CSV
│       ├── ivs_bh.csv                 # Setores censitários com IVS-BH (SMSA/PBH, WKT EPSG:31983)
│       └── ivs_por_cs.csv             # IVS agregado por CS (script 13): score, predominante, % por categoria
├── docs/
│   ├── index.html             # Site de documentação (GitHub Pages)
│   ├── metodologia_cep_cs.md  # Documentação detalhada do pipeline de geocodificação
│   ├── mapa_lisa.png          # Mapa LISA de autocorrelação espacial (script 08)
│   ├── tendencia_bh.png       # Joinpoint BH municipal com linha ajustada (script 10)
│   ├── tendencia_regional.png # Joinpoint por regional — facet_wrap 3×3 (script 10)
│   ├── its_bh.png             # ITS BH: observado, ajustado, contrafactual (script 11)
│   ├── its_regional.png       # ITS por regional: facet 3×3 (script 11)
│   ├── its_comparativo.png    # ITS comparativo BH × 4 capitais (script 12)
│   ├── subgrupos_ivs.png      # Forest plot GEE AR-1 por estrato IVS (script 15)
│   ├── tabela1.csv            # Tabela 1 tidy: 153 CS por regional/IVS/variáveis (script 16)
│   ├── tabela1_formatada.html # Tabela 1 HTML formatada (gt) para o manuscrito (script 16)
│   ├── did_its.png            # Forest plot DiD-ITS: θ slope change por capital (script 17)
│   └── its_ivs.png            # Série por quartil IVS + curva efeito marginal (script 18)
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
- **`icsap_bh_regional.csv`:** série completa jan/2023–mar/2026 (90.869 registros = 100% do icsap_bh.csv); não há lacuna de meses
- **Cobertura de geocodificação:** 86,1% (78.251/90.869 geocodificados); 12.618 sem regional são genuinamente irrecuperáveis (CEPs inexistentes, fora de BH ou sem coordenadas — script 04 já exauriu as tentativas)

### Scripts concluídos
- ✅ **Scripts 01–04** — pipeline de download, processamento e geocodificação
- ✅ **Script 05** — variáveis independentes coletadas em `data/ref/variaveis_cs.csv`
  - 5.508 obs (153 CS × 36 competências); cobertura: n_esf 76,5%, cobertura_aps_pct 100%, pop_total_censo 100%, pct_sem_saneamento 100%, renda_media 100%, pct_area_favela 100%
  - Limitação: pct_idosos/pct_criancas = 0% (censobr Basico não tem faixas etárias por setor)
  - Limitação: n_medicos/n_mfc = NA (arquivos PF/MG causam SIGSEGV no read.dbc)
- ✅ **Script 06** — análise de missing: 13,9% sem geocodificação, classificação MNAR (5/6 testes significativos, mas diferenças de magnitude pequena — reportar como limitação)
- ✅ **Script 07** — taxas padronizadas por CS × ano: 612 obs; correlação bruta = padronizada = 1,000 (esperado: mesma distribuição etária de BH aplicada a todos os CS por ausência de faixas por setor)
- ✅ **Script 08** — autocorrelação espacial: **Moran's I = 0,283 (p < 0,001)**, 10 clusters High-High (6,5%); mapa LISA em `docs/mapa_lisa.png`; resultado indica necessidade de GEE AR-1 ou SAR em vez de GLM-Gama padrão
- ✅ **Script 09** — GEE AR-1 painel mensal (5.492 obs, 153 CS × 36 meses: jan/2023–dez/2025) + VIF + stepwise
  - **Redesenho para painel mensal**: substituiu análise annual (459 obs) por painel mensal com termos Fourier (sin12/cos12) para sazonalidade
  - **VIF**: todos os preditores VIF ≤ 5 (max: renda_media=3,82, ivs_score=3,69); sem multicolinearidade
  - **Stepwise Forward (p<0,20)**: n_esf (p=0,076) e pct_sem_saneamento (p=0,0003) selecionados; ivs_score, pct_area_favela, renda_media não selecionados
  - **cobertura_aps_pct / n_esf_egestor**: nível municipal → não usáveis como preditores CS-nível
  - **M1 GEE AR-1** (base, φ≈0,96): mes_num NS (p=0,585); sin12 RR=1,031\*\*\*, cos12 RR=0,927\*\*\*; QIC=265
  - **M2 GEE + CNES** (117 CS): n_esf p=0,076 NS; QIC=464; φ=0,965
  - **M3 GEE + contexto** (153 CS, modelo principal): pct_sem_saneamento **RR=0,968 (IC95%: 0,946–0,990), p=0,005**; demais preditores NS; QIC=460; φ=0,960
  - **M4 GEE completo** (exchangeable, 117 CS): n_esf RR=1,032 (p=0,049\*), mes_num p<0,001 (APC=+3,05%/ano); QIC=472
  - **Backward stepwise** (exchangeable, 117 CS): pct_sem_saneamento p=0,255 → removido; n_esf p=0,064 → removido; **modelo final = M1-base** (nenhum preditor contextual sobreviveu p≤0,05 no subsample CNES de 117 CS)
  - **Interpretação pct_sem_saneamento** (M3): RR<1 contraintuitivo — CS em áreas com pior saneamento têm MENOS ICSAP registradas; possível viés de acesso/sub-registro em periferias; a discutir como limitação no manuscrito
- ✅ **Script 10** — joinpoint regression (APC/AAPC) nível municipal e regional
  - **BH**: 1 joinpoint em ~abr/2024 (mês 16); seg 1 APC = **+19,2%/ano**; seg 2 APC = **-10,8%/ano**; **AAPC = +1,1%/ano**
  - **Regionais**: todas as 9 com padrão bimodal similar (joinpoint em ~abr–mai/2024); AAPC de +2,4% (Noroeste/Oeste) a +8,5% (Barreiro/Centro-Sul)
  - Inversão de tendência em ~abr/2024 consistente em todo o município — possível efeito de intervenção ou mudança na codificação

- ✅ **Script 13** — IVS-BH incorporado: 153/153 CS com ivs_score (1,00–3,86), ivs_predominante e % por categoria; join via centroide setor → polígono CS (sf_use_s2=FALSE); mais vulnerável: CS Granja de Freitas (LESTE, ivs_score=3,86)
- ✅ **Script 14** — Alocação proporcional de CEPs limítrofes (buffer 100m)
  - 9.974 CEPs geocodificados; 3.206 limítrofes (32,3%); 6.720 internos (67,7%)
  - 2.403 internações redistribuídas = **3,31%** do total geocodificado (2023-2025)
  - Diferença máxima por CS×mês: 7,9 internações; CS mais afetado: CS Paraúna/Venda Nova (88,5 total acumulado)
  - Saídas: `cep_pesos_cs.csv`, `n_icsap_cs_mes_prop.csv`, `alocacao_impacto.txt`
- ✅ **Script 11** — ITS GLS AR(1) BH municipal + 9 regionais (Portaria GM/MS 3.493/2024)
  - **BH municipal**: nível -5,1% NS (p=0,307); APC pré=+22,8%/ano; APC pós líquida=-9,2%/ano
  - **Barreiro**: único regional com mudança de nível sig (-18,4%, p=0,025); demais NS
  - **Todas as regionais**: slope change pós significativo (p<0,05) = desaceleração universal
  - Saída: `its_resultados.csv`, `docs/its_bh.png`, `docs/its_regional.png`
  - Nota: `apc_pos` = APC líquida pós (β₁+β₃) — não só β₃; já calculado corretamente no CSV
- ✅ **Script 15** — GEE AR-1 estratificado por IVS (ITS com Portaria GM/MS 3.493/2024)
  - Modelo: taxa_cs ~ mes_num + interv + tempo_pos + sin12 + cos12 + pct_sem_saneamento
  - Portaria efetiva maio/2024 = mes_num 17 (interv=0→1; tempo_pos=ramp 0,0,...,1,2,...)
  - **Tendência pré-intervenção**: similar entre estratos (RR/mês ≈ 1,027–1,031, APC≈38–45%/ano, p<0,001)
  - **Mudança de nível (interv)**: Baixo -17,7%\*\*\*, Médio -15,8%\*\*\*, Elevado -22,1%\*\*\*, **Muito Elevado -7,1% NS** (p=0,315)
  - **Mudança de slope pós (tempo_pos)**: similar, sem diferencial entre estratos (~-37 a -40%/ano)
  - **Conclusão**: Portaria não reduziu desigualdades — CS Muito Elevado não se beneficiou do efeito abrupt level change; possível AMPLIAÇÃO de desigualdades
  - Saída: `gee_subgrupos_ivs.csv`, `docs/subgrupos_ivs.png`
- ✅ **Script 12** — ITS com controle: BH × SP, RJ, Curitiba, Fortaleza (concluído)
  - Modelo idêntico ao script 11; métrica: % ICSAP das internações totais
  - Saída: `its_controle_resultados.csv`, `serie_controles.csv`, `docs/its_comparativo.png`
- ✅ **Script 16** — Tabela 1 dos 153 CS (concluído)
  - Distribuição por regional, IVS, variáveis contínuas (mediana [IQR]), estratificado por IVS
  - Saída: `docs/tabela1.csv`, `docs/tabela1_formatada.html`
- ✅ **Script 17** — DiD-ITS formal: BH × 4 capitais (concluído)
  - GLS pooled com interação capital×tempo_pos; θ_k = slope change controle − slope change BH
  - **Nenhum θ_slope significativo** — efeito da Portaria 3.493/2024 é **nacional**, não específico de BH
  - BH APC pós = **-7,1%/ano**; capitais controle com desaceleração similar (θ_k NS)
  - Saída: `did_its_resultados.csv`, `docs/did_its.png`
- ✅ **Script 18** — ITS × IVS: interação ivs_z:tempo_pos (concluído)
  - GEE AR-1; 3 modelos: M1-base, M2-interação, M3-completo
  - **ivs_z RR=1,317 (p<0,001)** — CS mais vulneráveis têm 31,7% mais ICSAP (efeito principal)
  - **ivs_z:tempo_pos NS (p=0,179)** — efeito da Portaria homogêneo entre CS por vulnerabilidade
  - Conclusão: Portaria reduziu ICSAP uniformemente — sem ampliação nem redução de desigualdades pós
  - Saída: `its_ivs_resultados.csv`, `docs/its_ivs.png`

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

Taxa ICSAP **bruta** por 10.000 habitantes, por área de abrangência de CS. **Nota:** padronização por faixas etárias não foi possível — dados etários por setor censitário não estão disponíveis no censobr Basico (Censo 2022). Taxa bruta utilizada como proxy; limitação documentada em `docs/protocolo_pesquisa.md` (seção "Limitação Implementada").

### Variáveis Independentes

| Variável | Fonte | Status | Nível |
|---|---|---|---|
| Cobertura ESF (%) | e-Gestor AB/DAB | ⚠️ Municipal — não usável como preditor CS | Municipal |
| Nº equipes ESF / eMulti / ESB | CNES EP/DATASUS | ✅ Coletada (76,5% CS) | CS |
| Médicos de família (CH) | CNES PF/DATASUS | ❌ Indisponível (SIGSEGV no read.dbc) | CS |
| % domicílios sem rede geral de água | Censo IBGE 2022 (censobr) | ✅ Coletada (100% CS) | CS |
| Renda per capita (salários mínimos) | Censo IBGE 2022 (censobr) | ✅ Coletada (100% CS) | CS |
| % área de favela por CS | Censo IBGE 2022 (code_favela/geobr) | ✅ Coletada (100% CS) | CS |
| População total por CS | Censo IBGE 2022 (censobr V0001) | ✅ Coletada (100% CS) | CS |
| % idosos / % crianças por CS | Censo IBGE 2022 (censobr Pessoa) | ❌ Não disponível no dataset Basico | CS |
| IVS-BH | SMSA/PBH | ✅ Coletada (100% CS, script 13) | CS |

### Método Estatístico

- **GLM-Gama** (link log) — baseline, ignora correlação; subestima erros padrão (p.ex. pct_sem_saneamento p=0,015 → p=0,096 no GEE)
- **GEE AR-1 painel mensal** ✅ (script 09) — φ≈0,96; VIF todos ≤5; M3 (153 CS, AR-1): pct_sem_saneamento RR=0,968 (p=0,005), demais NS; backward stepwise elimina todos (modelo final=M1-base no subsample CNES de 117 CS)
- **GEE ITS por estrato IVS** ✅ (script 15) — Portaria 3.493/2024: redução de nível significativa em Baixo/Médio/Elevado, NS em Muito Elevado → possível ampliação de desigualdades
- **ITS GLS AR(1)** ✅ (script 11) — BH: nível -5,1% NS (p=0,307); APC pré=+22,8%/ano → pós=-9,2%/ano; apenas Barreiro com nível sig; desaceleração universal em todas as regionais
- **ITS com controle** ✅ (script 12) — compara β₃ de BH com SP, RJ, Curitiba, Fortaleza; responde se inflexão é específica de BH ou tendência nacional
- **DiD-ITS formal** ✅ (script 17) — GLS pooled; nenhum θ_slope significativo → efeito nacional (não específico de BH); BH APC pós=-7,1%/ano
- **ITS × IVS** ✅ (script 18) — GEE AR-1; ivs_z RR=1,317\*\*\* (CS vulneráveis têm 31,7% mais ICSAP); ivs_z:tempo_pos NS (p=0,179) → efeito homogêneo da Portaria entre CS
- **Alocação proporcional de CEPs** ✅ (script 14) — 3,31% das internações redistribuídas com buffer 100m; 32,3% CEPs limítrofes; impacto marginal mas methodologicamente relevante
- **Joinpoint regression** ✅ (script 10) — AAPC BH = +1,1%/ano; padrão bimodal com inflexão em abr/2024 em todas as 9 regionais

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
3. ✅ ~~Padronização de taxas~~ — script 07 executado; taxa bruta = proxy adequado (limitação documentada)
4. ✅ ~~Autocorrelação espacial~~ — concluído (script 08, Moran's I = 0,283)
5. ✅ ~~GLM-Gama + GEE AR-1~~ — concluído (script 09; painel mensal; φ≈0,96; VIF ≤5; M3 é modelo principal)
6. ✅ ~~Joinpoint regression~~ — concluído (script 10; AAPC BH=+1,1%/ano; inflexão abr/2024)
7. ✅ ~~Re-executar script 03~~ — `icsap_bh_regional.csv` já estava completo (jan/2023–mar/2026)
8. ✅ ~~Coletar IVS-BH~~ — concluído (script 13; 100% CS; ivs_score 1,00–3,86)
9. ✅ ~~Alocação proporcional de CEPs limítrofes~~ — concluído (script 14; 3,31% redistribuídas; buffer 100m)
10. ✅ ~~Análise de subgrupos por IVS (Portaria 3.493)~~ — concluído (script 15; Muito Elevado NS → ampliação desigualdades)
11. ✅ ~~Finalizar backward stepwise~~ — modelo final confirmado: M3 (153 CS, AR-1) é o modelo principal
12. ✅ ~~ITS BH + regionais~~ — concluído (script 11; BH nível NS p=0,307; Barreiro único sig)
13. ✅ ~~ITS com grupo controle~~ — script 12 criado (aguarda execução com download de SP/RJ/PR/CE)
14. ✅ ~~Tabela 1 descritiva~~ — script 16 criado (aguarda execução)
15. ✅ ~~Padronização documentada como limitação~~ — `protocolo_pesquisa.md` + `checklist_strobe.md` atualizados
16. ✅ ~~Scripts 12 e 16 executados~~ — ITS controle e Tabela 1 concluídos
17. ✅ ~~DiD-ITS formal~~ — concluído (script 17; nenhum θ_slope sig → efeito nacional; BH APC pós=-7,1%/ano)
18. ✅ ~~ITS × IVS~~ — concluído (script 18; ivs_z RR=1,317\*\*\*; interação NS → efeito homogêneo)
19. ✅ ~~Executar scripts 17 e 18~~ — **pipeline analítico 100% completo**
20. **Investigar inflexão de abr/2024** — padrão bimodal em todas as regionais e todos os estratos IVS; checar mudanças de codificação SIHSUS ou portaria federal
21. **Redigir manuscrito** para submissão ao *Cadernos de Saúde Pública* (meta: jan/2027)

---

## Próximos Passos

### Prioritários (para o estudo científico)

**Pipeline analítico 100% completo** — scripts 01–18 executados e versionados.

- **Tornar repositório privado** — GitHub Settings → Danger Zone → "Change repository visibility" → Private (antes de submeter o manuscrito)
- **Iniciar redação do manuscrito** — *Cadernos de Saúde Pública* (Fiocruz, Qualis A1)
  - Template: título em PT/EN/ES, resumo estruturado (Objetivo / Métodos / Resultados / Conclusão), corpo IMRD
  - Seguir checklist STROBE (`docs/checklist_strobe.md`) item a item
  - Vancouver, 4.000 palavras, até 5 tabelas/figuras
  - Figuras centrais: `docs/its_bh.png`, `docs/subgrupos_ivs.png`, `docs/did_its.png`, `docs/its_ivs.png`
- **Investigar inflexão de abr/2024** — padrão bimodal em todas as regionais E em todos os estratos IVS; checar mudanças de codificação SIHSUS ou portaria federal anterior

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
