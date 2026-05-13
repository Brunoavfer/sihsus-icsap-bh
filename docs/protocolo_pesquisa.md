# Protocolo de Pesquisa — ICSAP-BH

**Versão:** 1.0  
**Data:** maio de 2026  
**Status:** Em elaboração

---

## Título Provisório

**Português:**
Efetividade da atenção primária à saúde e internações por condições sensíveis nos Centros de Saúde de Belo Horizonte: estudo ecológico longitudinal, 2023–2025

**Inglês (para submissão):**
Primary health care effectiveness and ambulatory care sensitive conditions in Belo Horizonte health centers: a longitudinal ecological study, 2023–2025

---

## Pergunta de Pesquisa (PEO — adaptação PICO para ecológico)

| Componente | Definição |
|---|---|
| **P** — Unidades ecológicas | Áreas de abrangência dos 153 Centros de Saúde municipais de Belo Horizonte, MG |
| **E** — Exposição | Cobertura e estrutura da Estratégia Saúde da Família (equipes ESF ativas, médicos de família, cobertura cadastrada) e vulnerabilidade socioeconômica da área (IVS-BH) |
| **O** — Desfecho | Taxa de internações por condições sensíveis à atenção primária (ICSAP) padronizada por idade e sexo por 10.000 habitantes |

**Pergunta central:** A maior cobertura e estrutura da ESF está associada a menores taxas de ICSAP nas áreas de abrangência dos Centros de Saúde de Belo Horizonte, após controle pela vulnerabilidade social da população adscrita?

---

## Objetivos

### Objetivo Geral

Analisar os fatores associados à variação das taxas de ICSAP nas áreas de abrangência dos 153 Centros de Saúde de Belo Horizonte no período 2023–2025, controlando por vulnerabilidade social e estrutura da atenção primária.

### Objetivos Específicos

1. Descrever a distribuição espacial e temporal das taxas ICSAP padronizadas por Centro de Saúde e regional administrativa de BH no período 2023–2025.

2. Analisar a tendência temporal das ICSAP pelo método *joinpoint regression*, estimando a variação percentual anual (APC) e a variação percentual anual média (AAPC) para o município e por regional.

3. Identificar os fatores associados à taxa ICSAP por meio de modelo linear generalizado com distribuição Gama, controlando por cobertura ESF, número de equipes, carga horária de médicos de família e Índice de Vulnerabilidade em Saúde (IVS-BH).

4. Comparar as taxas ICSAP padronizadas entre as nove regionais administrativas de Belo Horizonte, identificando desigualdades intramunicipais.

5. Avaliar a contribuição relativa dos grupos de condições ICSAP (Portaria SAS/MS nº 221/2008) na composição da taxa municipal ao longo do período.

---

## Desenho do Estudo

**Tipo:** Estudo ecológico longitudinal de séries temporais (painel de dados agregados).

**Unidade de análise:** Área de abrangência de cada um dos 153 Centros de Saúde da rede municipal de Belo Horizonte, delimitada pelos polígonos oficiais da Secretaria Municipal de Saúde (SMSA/BH).

**Período:** Janeiro de 2023 a dezembro de 2025 (36 competências mensais).

**Estrutura dos dados:** Painel longitudinal com observações repetidas por unidade. Dimensões esperadas: 153 CS × 36 meses = 5.508 observações (potencialmente desbalanceado após exclusões).

**Referencial metodológico:** Replicação e extensão de Oliveira et al. (2025) para o nível de Centro de Saúde, incorporando análise de tendência temporal ausente no artigo de referência.

---

## Variável Desfecho

**Taxa ICSAP padronizada por idade e sexo por 10.000 habitantes**

- **Numerador:** Internações ICSAP de residentes em BH e internados em BH (filtro duplo MUNIC_RES = MUNIC_MOV = 310620), com CID-10 constante da lista da Portaria SAS/MS nº 221/2008, alocadas ao CS pela área de abrangência (geoprocessamento).
- **Denominador:** População estimada de residentes na área de abrangência do CS, por faixa etária e sexo (Censo IBGE 2022 + projeção intercensitária para 2023–2025).
- **Método de padronização:** Padronização direta, usando como população-padrão a distribuição etária e de sexo do município de Belo Horizonte (Censo IBGE 2022).
- **Faixas etárias:** <1 ano; 1–4; 5–14; 15–29; 30–44; 45–59; 60–74; ≥75 anos (8 grupos, conforme padrão RIPSA).
- **Unidade:** Por 10.000 habitantes por área de abrangência por mês de competência.

---

## Variáveis Independentes

| Variável | Operacionalização | Fonte | Nível |
|---|---|---|---|
| **Cobertura ESF (%)** | Proporção da população cadastrada em equipes ESF ativas, por CS por mês | e-Gestor AB (DAB/MS) | CS × mês |
| **Número de equipes ESF** | Contagem de equipes ESF com status ativo no mês, por CS | CNES (FTP DATASUS) | CS × mês |
| **Médicos de família (CH)** | Carga horária total de profissionais com CBO 2251-05 ou 2251-10 vinculados ao CS | CNES (FTP DATASUS) | CS × mês |
| **IVS-BH** | Índice de Vulnerabilidade em Saúde da PBH — versão mais recente disponível, por área de abrangência de CS | SMSA/BH (dados abertos) | CS (estático) |

### Variáveis de Controle / Covariáveis

| Variável | Operacionalização | Fonte |
|---|---|---|
| **Proporção de idosos** | % da população ≥60 anos na área de abrangência | IBGE Censo 2022 |
| **Proporção de mulheres** | % de mulheres na área de abrangência | IBGE Censo 2022 |
| **Porte poblacional** | Total de residentes na área de abrangência (quartis) | IBGE Censo 2022 |
| **Regional administrativa** | 9 regionais de BH (variável de efeito fixo ou *cluster*) | SMSA/BH |

---

## Método Estatístico

### 1. Análise Descritiva

- Medidas de tendência central (mediana, IQR) e dispersão para as taxas ICSAP por CS e por regional.
- Mapas coropléticos das taxas padronizadas por período (ano e período pré/pós).
- Boxplots das taxas por regional para visualizar desigualdades intramunicipais.

### 2. Análise de Tendência Temporal — *Joinpoint Regression*

- Software: Joinpoint v4.9.1.0 (National Cancer Institute, EUA) ou pacote `segmented` do R.
- Método: *joinpoint regression* com permutation test para identificação do número ótimo de pontos de inflexão.
- Métricas reportadas: APC (Annual Percentage Change) e AAPC (Average Annual Percentage Change) com IC 95%.
- Aplicação: série municipal agregada + por regional administrativa.
- Referência: Kim et al. (2000) *Statistics in Medicine*.

### 3. Modelo de Associação — GLM com Distribuição Gama

Replicação e extensão de Oliveira et al. (2025):

$$\ln(\mu_{it}) = \beta_0 + \beta_1 \cdot \text{CobESF}_{it} + \beta_2 \cdot \text{nEquipes}_{it} + \beta_3 \cdot \text{MédFam}_{it} + \beta_4 \cdot \text{IVS}_i + \beta_5 \cdot \text{Tempo}_t + u_i$$

Onde:
- $\mu_{it}$: taxa ICSAP padronizada do CS $i$ no mês $t$
- Distribuição: Gama (adequada para resposta contínua positiva com variância proporcional à média²)
- Link: logarítmico
- $u_i$: efeito aleatório por CS (se modelo misto) ou efeito fixo por regional
- **Autocorrelação temporal:** verificada pelo teste de Durbin-Watson; se detectada, usar GEE (*Generalized Estimating Equations*) com estrutura de correlação autorregressiva AR(1)

**Pressupostos a verificar:**
- Distribuição dos resíduos (gráficos Q-Q, deviance residuals)
- Superdispersão (razão deviance/gl; teste de Pearson)
- Multicolinearidade (VIF < 5 para todas as variáveis independentes)
- Influência de outliers (distância de Cook, leverage)

**Análise de sensibilidade:**
- Modelo 1: apenas variáveis ESF (sem IVS)
- Modelo 2: com IVS (modelo completo)
- Modelo 3: estratificado por regional administrativa
- Modelo 4: restrito ao subperíodo 2023 (para comparação com dados históricos)

### 4. Software

| Análise | Software |
|---|---|
| Pipeline de dados | R ≥ 4.3 (`dplyr`, `readr`, `sf`, `lubridate`) |
| GLM-Gama e descritiva | R (`MASS`, `lme4`, `geepack`, `ggplot2`, `leaflet`) |
| *Joinpoint regression* | Joinpoint v4.9 (NCI) ou R `segmented` |
| Mapas | R (`sf`, `leaflet`, `tmap`) |
| Relatório final | R Markdown / Quarto |

---

## Critérios de Inclusão

1. Internações com `MUNIC_RES = 310620` **e** `MUNIC_MOV = 310620` (paciente reside **e** foi internado em BH).
2. Competência entre janeiro de 2023 e dezembro de 2025.
3. CID-10 constante da lista ICSAP (Portaria SAS/MS nº 221/2008, 93 subgrupos).
4. CEP geocodificado com sucesso e alocado a um CS por geoprocessamento.
5. CS com pelo menos 12 meses de dados no período (para estabilidade da estimativa da taxa).

---

## Critérios de Exclusão

1. Registros com CEP ausente, inválido ou pertencente a município diferente de BH (erro de cadastro no SIHSUS).
2. CEPs que, após todas as etapas do pipeline de geocodificação (scripts 03 e 04), não foram alocados a nenhum CS.
3. CS com menos de 12 meses de dados válidos no período analisado.
4. Internações com `DIAG_PRINC` ausente ou inválido.

---

## Limitações Previstas

1. **Falácia ecológica:** Associações detectadas em nível agregado (área de abrangência do CS) não podem ser inferidas para o nível individual. Os resultados descrevem padrões populacionais, não causalidade individual.

2. **Cobertura de geocodificação:** A geocodificação cobre ~84–85% dos registros ICSAP. Os 15–16% não geocodificados são excluídos da análise por CS/regional, o que pode introduzir viés se a distribuição dos CEPs não geocodificados não for aleatória (e.g., concentração em áreas periféricas com logradouros mal cadastrados).

3. **Ausência de dados individuais:** O SIHSUS não contém variáveis sobre renda, escolaridade, presença de plano de saúde ou multimorbidade — fatores potencialmente confundidores não controlados.

4. **IVS estático:** O IVS-BH disponível é de período único; variações na vulnerabilidade social ao longo de 2023–2025 não são capturadas.

5. **Estabilidade das áreas de abrangência:** Revisões nos limites das áreas de abrangência dos CS ao longo do período podem criar descontinuidades nas séries.

6. **Portaria 221/2008:** A lista ICSAP foi construída em 2008 e pode não capturar adequadamente mudanças epidemiológicas recentes (e.g., doenças crônicas emergentes).

7. **Dados de APS incompletos:** A disponibilidade e qualidade dos dados do CNES e e-Gestor AB para desagregação ao nível de CS precisam ser verificadas.

---

## Aspectos Éticos

- **Dispensa de CEP:** Pesquisa com dados secundários públicos e anonimizados. Conforme a **Resolução CNS nº 510, de 7 de abril de 2016**, art. 1º, parágrafo único, inciso V, são dispensadas de registro e avaliação pelo CEP "pesquisas com bancos de dados, cujas informações são agregadas, sem possibilidade de identificação individual".
- **Uso dos dados SIHSUS:** Autorizado pela Portaria MS/SVS nº 204/2016. Os dados do SIHSUS não contêm nome, CPF ou qualquer identificador direto de pacientes.
- **Dados PBH:** Polígonos de área de abrangência disponibilizados sob licença Creative Commons Attribution (CC-BY), Portal de Dados Abertos PBH.
- **Declaração de conflito de interesses:** Pesquisa independente, sem financiamento de fontes com interesse nos resultados.

---

## Periódico Alvo

**Primeira opção:** *Cadernos de Saúde Pública / Reports in Public Health* (Fiocruz)
- Qualis CAPES 2017-2020: A1 (Saúde Coletiva)
- ISSN: 0102-311X (impresso) / 1678-4464 (eletrônico)
- Fator de impacto: ~2,5 (JCR 2023)
- Limite: 4.000 palavras (artigo original, excluindo resumo, tabelas e referências)
- Referências: estilo Vancouver, máximo 35
- Idioma de submissão: português (com resumo em inglês e espanhol)
- Estrutura obrigatória: Introdução, Métodos, Resultados, Discussão, Conclusão

**Segunda opção:** *Ciência & Saúde Coletiva* (ABRASCO)
- Qualis CAPES 2017-2020: A1 (Saúde Coletiva)
- ISSN: 1413-8123 / 1678-4561
- Limite: 6.000 palavras

**Terceira opção (internacional):** *International Journal for Equity in Health* (BioMed Central)
- Permite submissão em inglês com foco em desigualdades em saúde

### Cronograma Estimado de Submissão

| Etapa | Prazo estimado |
|---|---|
| Coleta de variáveis independentes (CNES, e-Gestor, IVS) | Ago/2026 |
| Padronização das taxas ICSAP | Set/2026 |
| Análises estatísticas (GLM-Gama + joinpoint) | Out/2026 |
| Redação do manuscrito | Nov/2026 |
| Revisão por pares interno (co-autores) | Dez/2026 |
| Submissão | Jan/2027 |
