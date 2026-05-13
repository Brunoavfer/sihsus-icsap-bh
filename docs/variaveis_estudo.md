# Tabela de Variáveis do Estudo ICSAP-BH

**Projeto:** Estudo Ecológico Longitudinal, jan/2023–dez/2025  
**Unidade de análise:** Área de abrangência dos Centros de Saúde (CS) de BH  
**Última atualização:** maio de 2026

---

## 1. Variável Desfecho

| Campo | Detalhe |
|---|---|
| **Nome** | `taxa_icsap_pad` |
| **Descrição** | Taxa ICSAP padronizada por idade e sexo por 10.000 habitantes |
| **Tipo** | Quantitativa contínua, não-negativa (distribuição Gama) |
| **Papel no modelo** | Variável dependente (Y) |
| **Unidade** | Internações ICSAP por 10.000 hab./mês por área de abrangência do CS |
| **Fonte** | SIHSUS (numerador) + IBGE Censo 2022 / projeção (denominador) |
| **Forma de obtenção** | Numerador: scripts `01_download.R` → `02_process.R` → `03_cep_regional.R`; padronização direta em novo script `06_padronizacao.R` |
| **Período disponível** | Jan/2023 a Mar/2026 (numerador); denominador por Censo 2022 + projeção linear |
| **Incorporação no pipeline** | Novo arquivo `data/processed/icsap_bh_regional_pad.csv` com coluna `taxa_icsap_pad` |
| **Transformação** | Logarítmica no modelo (link log do GLM-Gama) |
| **Valores esperados** | 0 a ~80 internações/10.000 hab./mês (estimativa baseada em taxas nacionais) |

### Variável auxiliar: Taxa ICSAP bruta

| Campo | Detalhe |
|---|---|
| **Nome** | `taxa_icsap_bruta` |
| **Descrição** | Taxa bruta de ICSAP (sem padronização) por 10.000 hab./mês |
| **Tipo** | Quantitativa contínua |
| **Papel no modelo** | Análise descritiva; comparação com taxa padronizada |
| **Fonte** | `icsap_bh_regional.csv` (atual) + denominador IBGE |
| **Status** | ⚠️ Parcial — contagens disponíveis; denominador por CS pendente |

---

## 2. Variáveis Independentes Principais

### 2.1 Cobertura ESF (%)

| Campo | Detalhe |
|---|---|
| **Nome** | `cobertura_esf_pct` |
| **Descrição** | Proporção da população da área de abrangência do CS cadastrada em equipes ESF ativas, em determinado mês |
| **Tipo** | Quantitativa contínua (0–100%) |
| **Papel no modelo** | Variável independente principal (exposição de interesse) |
| **Fonte** | e-Gestor Atenção Básica (DAB/MS) — relatórios mensais de cobertura |
| **URL de acesso** | https://egestorab.saude.gov.br |
| **Forma de obtenção** | Download manual de relatórios mensais (formato CSV/XLSX) ou raspagem web; desagregação por CNES do CS |
| **Período disponível** | Mensal, 2013–atual (verificar disponibilidade por CS individual) |
| **Granularidade** | Por estabelecimento (CNES) por mês → cruzar com lista de CS municipais de BH |
| **Incorporação no pipeline** | Novo script `R/05_variaveis_aps.R`; saída em `data/processed/aps_mensal.csv` |
| **Transformação** | Usar como contínua; avaliar categorização em tertis para análise de subgrupos |
| **Valores esperados** | 70–100% (BH tem alta cobertura ESF) |
| **Status** | ❌ Pendente — coleta não iniciada |

### 2.2 Número de Equipes ESF Ativas

| Campo | Detalhe |
|---|---|
| **Nome** | `n_equipes_esf` |
| **Descrição** | Número de equipes ESF com status ativo no CNES, vinculadas ao CS, no mês de competência |
| **Tipo** | Quantitativa discreta |
| **Papel no modelo** | Variável independente (proxy de capacidade instalada) |
| **Fonte** | CNES — Cadastro Nacional de Estabelecimentos de Saúde (FTP DATASUS) |
| **URL de acesso** | `ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/` |
| **Forma de obtenção** | Download de arquivos `.dbc` da competência mensal, tabela EQ (equipes); filtrar por CO_MUNICIPIO = 310620 e tipo de equipe ESF |
| **Período disponível** | Mensal, 2005–atual |
| **Granularidade** | Por CNES do estabelecimento por mês |
| **Incorporação no pipeline** | Script `R/05_variaveis_aps.R`; coluna em `data/processed/aps_mensal.csv` |
| **Transformação** | Usar como contínua; considerar razão equipes/1.000 hab. |
| **Status** | ❌ Pendente — coleta não iniciada |

### 2.3 Carga Horária de Médicos de Família

| Campo | Detalhe |
|---|---|
| **Nome** | `ch_medico_familia` |
| **Descrição** | Carga horária semanal total de profissionais com CBO de médico de família e comunidade (CBO 2251-05 ou 2251-10) vinculados ao CS |
| **Tipo** | Quantitativa contínua (horas/semana) |
| **Papel no modelo** | Variável independente (dimensão qualitativa da APS) |
| **Fonte** | CNES — tabela PF (profissionais) do FTP DATASUS |
| **URL de acesso** | `ftp://ftp.datasus.gov.br/dissemin/publicos/CNES/` |
| **Forma de obtenção** | Download arquivos `.dbc` tabela PF; filtrar CBO = "225105" ou "225110" e CO_MUNICIPIO = 310620; agregar CH_AMB + CH_HOSP por CNES do CS |
| **Período disponível** | Mensal, 2005–atual |
| **Granularidade** | Por CNES do estabelecimento por mês |
| **Incorporação no pipeline** | Script `R/05_variaveis_aps.R`; coluna em `data/processed/aps_mensal.csv` |
| **Transformação** | Razão CH/equipe ou CH/1.000 hab.; avaliar distribuição |
| **Status** | ❌ Pendente — coleta não iniciada |

### 2.4 Índice de Vulnerabilidade em Saúde (IVS-BH)

| Campo | Detalhe |
|---|---|
| **Nome** | `ivs_bh` |
| **Descrição** | Índice de Vulnerabilidade em Saúde da Prefeitura de BH, por área de abrangência de CS. Índice composto que agrega dimensões socioeconômicas, ambientais e de acesso a serviços |
| **Tipo** | Quantitativa contínua (0 = menor vulnerabilidade; valores maiores = maior vulnerabilidade) |
| **Papel no modelo** | Variável independente (confundidora/modificadora de efeito) |
| **Fonte** | Secretaria Municipal de Saúde de BH (SMSA) / Portal de Dados Abertos PBH |
| **URL de acesso** | https://dados.pbh.gov.br — buscar "Índice de Vulnerabilidade em Saúde" |
| **Forma de obtenção** | Download de shapefile ou CSV com IVS por área de abrangência; cruzamento geoespacial com polígonos dos CS |
| **Período disponível** | Pontual (versão mais recente disponível; verificar ano de referência) |
| **Granularidade** | Por área de abrangência de CS (nível de agregação nativo) |
| **Incorporação no pipeline** | Arquivo `data/ref/ivs_bh.csv`; join com `icsap_bh_regional.csv` via nome do CS |
| **Transformação** | Usar como contínua no modelo; categorizar em tertis (baixo/médio/alto) para análises descritivas |
| **Limitação** | Dado estático — não capta variações temporais de vulnerabilidade no período 2023–2025 |
| **Status** | ❌ Pendente — coleta não iniciada |

---

## 3. Variáveis de Controle / Covariáveis

### 3.1 População Total da Área de Abrangência

| Campo | Detalhe |
|---|---|
| **Nome** | `pop_total` |
| **Descrição** | Total de residentes na área de abrangência do CS |
| **Tipo** | Quantitativa discreta |
| **Papel no modelo** | Denominador da taxa; *offset* no GLM ou covariável de porte |
| **Fonte** | IBGE — Censo Demográfico 2022 + projeção intercensitária para 2023–2025 |
| **Forma de obtenção** | Agregação de setores censitários IBGE 2022 dentro de cada polígono de área de abrangência (operação espacial `st_join` no R) |
| **Período disponível** | Censo 2022; projeção linear anual |
| **Incorporação no pipeline** | Arquivo `data/ref/pop_cs.csv`; join via nome do CS |
| **Status** | ❌ Pendente — coleta não iniciada |

### 3.2 Distribuição Etária por Área de Abrangência

| Campo | Detalhe |
|---|---|
| **Nome** | `pop_faixa_etaria` |
| **Descrição** | População por faixa etária e sexo em cada área de abrangência do CS |
| **Tipo** | Quantitativa discreta (8 faixas × 2 sexos = 16 estratos por CS) |
| **Papel no modelo** | Insumo para padronização direta da taxa ICSAP |
| **Fonte** | IBGE — Censo 2022, tabelas por setor censitário |
| **Faixas** | <1 ano; 1–4; 5–14; 15–29; 30–44; 45–59; 60–74; ≥75 anos |
| **Forma de obtenção** | Agregação espacial de setores censitários dentro de polígonos dos CS |
| **Incorporação no pipeline** | Arquivo `data/ref/pop_cs_faixa_etaria.csv`; insumo para `06_padronizacao.R` |
| **Status** | ❌ Pendente |

### 3.3 Proporção de Idosos (≥60 anos)

| Campo | Detalhe |
|---|---|
| **Nome** | `pct_idosos` |
| **Descrição** | Proporção de residentes com 60 anos ou mais na área de abrangência |
| **Tipo** | Quantitativa contínua (%) |
| **Papel no modelo** | Covariável de composição demográfica |
| **Fonte** | Derivado de `pop_cs_faixa_etaria.csv` |
| **Incorporação no pipeline** | Calculado em `06_padronizacao.R` |
| **Status** | ❌ Pendente |

### 3.4 Regional Administrativa

| Campo | Detalhe |
|---|---|
| **Nome** | `regional` |
| **Descrição** | Regional administrativa de BH à qual o CS pertence (9 regionais) |
| **Tipo** | Qualitativa nominal (9 categorias) |
| **Papel no modelo** | Variável de efeito fixo (cluster) ou *random intercept* no modelo misto |
| **Fonte** | `icsap_bh_regional.csv` (já disponível) |
| **Incorporação no pipeline** | Já presente no pipeline atual |
| **Status** | ✅ Disponível |
| **Categorias** | Barreiro, Centro-Sul, Leste, Nordeste, Noroeste, Norte, Oeste, Pampulha, Venda Nova |

### 3.5 Mês de Competência e Tendência Temporal

| Campo | Detalhe |
|---|---|
| **Nome** | `mes_cmpt`, `tempo` |
| **Descrição** | Competência mensal (YYYYMM) e variável numérica de tendência (1 = jan/2023, ..., 36 = dez/2025) |
| **Tipo** | Data / Quantitativa discreta |
| **Papel no modelo** | Controle de tendência secular e sazonalidade |
| **Fonte** | Derivado de `icsap_bh_regional.csv` |
| **Incorporação no pipeline** | Calculado na análise |
| **Status** | ✅ Disponível |

---

## 4. Síntese e Prioridade de Coleta

| Variável | Status | Prioridade | Bloqueador |
|---|---|---|---|
| Taxa ICSAP (numerador) | ✅ Disponível | — | — |
| Regional | ✅ Disponível | — | — |
| Mês/Tendência | ✅ Disponível | — | — |
| **Taxa padronizada** | ⚠️ Parcial | 🔴 Alta | Denominador por faixa etária e CS |
| **Pop. total por CS** | ❌ Pendente | 🔴 Alta | Agrega setores censitários IBGE nos polígonos dos CS |
| **Pop. faixa etária por CS** | ❌ Pendente | 🔴 Alta | Idem; insumo para padronização |
| **IVS-BH** | ❌ Pendente | 🔴 Alta | Download PBH; verificar granularidade |
| **Cobertura ESF** | ❌ Pendente | 🟠 Média-Alta | e-Gestor AB; desagregação por CNES |
| **Nº equipes ESF** | ❌ Pendente | 🟠 Média-Alta | CNES/DATASUS; script de leitura de .dbc |
| **Médicos de família (CH)** | ❌ Pendente | 🟠 Média-Alta | CNES/DATASUS; tabela PF |
| Proporção idosos | ❌ Pendente | 🟡 Média | Derivado da população por faixa etária |

---

## 5. Estrutura de Dados Final Esperada

O arquivo analítico final (`data/processed/dados_analise.csv`) terá a seguinte estrutura:

```
cs_nome          | Regional        | mes_cmpt | tempo | n_icsap | pop_total | taxa_icsap_bruta | taxa_icsap_pad | cobertura_esf_pct | n_equipes_esf | ch_medico_familia | ivs_bh | pct_idosos
-----------------|-----------------|----------|-------|---------|-----------|-----------------|----------------|-------------------|---------------|-------------------|--------|------------
CS Alcides Lins  | Noroeste        | 202301   | 1     | 12      | 18.450    | 6.5             | 7.2            | 94.3              | 4             | 160               | 0.42   | 14.2
CS Cidade Ozanan | Noroeste        | 202301   | 1     | 8       | 12.200    | 6.6             | 6.1            | 87.1              | 3             | 120               | 0.51   | 13.8
...
```

**Dimensões esperadas:** 153 CS × 36 meses = 5.508 linhas (painel balanceado); potencialmente ~5.200 após exclusões.

---

## 6. Novas Dependências de Pacotes R

Os novos scripts de coleta e padronização requererão os seguintes pacotes adicionais:

| Pacote | Uso |
|---|---|
| `read.dbc` | Leitura dos arquivos .dbc do CNES |
| `sf` | Agregação espacial de setores censitários IBGE nos polígonos dos CS (já disponível) |
| `censobr` | Acesso programático aos dados do Censo IBGE 2022 por setor censitário |
| `aopdata` | Dados de acessibilidade e população por área de planejamento (alternativa ao `censobr`) |
| `MASS` | GLM com distribuição Gama |
| `geepack` | GEE para autocorrelação temporal |
| `lme4` | Modelos mistos (efeitos aleatórios por CS/regional) |
| `segmented` | Joinpoint regression em R (alternativa ao software NCI) |
