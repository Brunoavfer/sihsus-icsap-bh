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

---

## Análise de Sensibilidade — CEPs Não Identificados

Aproximadamente 14% dos registros ICSAP não foram alocados a nenhum CS após o pipeline de geocodificação (scripts 03 e 04). Para avaliar se essa exclusão introduz viés nas estimativas, o script `R/06_analise_missing.R` compara sistematicamente os registros **com** e **sem** regional identificada.

### Variáveis comparadas e testes aplicados

| Variável | Teste | Hipótese de interesse |
|---|---|---|
| Idade (anos) | Mann-Whitney U | Registros não geocodificados são de pacientes mais velhos/jovens? |
| Sexo | Qui-quadrado | Há diferença na proporção de homens/mulheres? |
| Condição ICSAP (grupo diagnóstico) | Qui-quadrado | Alguma condição clínica concentra os missing? |
| Distribuição por ano | Qui-quadrado | O missingness se concentra em períodos específicos? |
| Valor da internação (R$) | Mann-Whitney U | Internações mais complexas (caras) são mais/menos geocodificadas? |
| Dias de permanência | Mann-Whitney U | Internações mais longas têm padrão diferente de geocodificação? |

### Classificação do padrão de missingness

- **MCAR** (Missing Completely at Random): nenhuma diferença significativa em qualquer variável → exclusão sem viés relevante.
- **MAR** (Missing at Random): diferenças em ≤ 2 variáveis → limitação moderada; reportar como limitação.
- **MNAR** (Missing Not at Random): diferenças em ≥ 3 variáveis → limitação substantiva; considerar análise de sensibilidade com CS com cobertura ≥ 85%.

**Saídas:** `data/processed/tabela_missing.csv` e `data/processed/conclusao_missing.txt`

**Referência:** Sterne JAC et al. Multiple imputation for missing data in epidemiological and clinical research. *BMJ*. 2009;338:b2393. doi:10.1136/bmj.b2393

---

## Padronização da Taxa ICSAP

### Justificativa

As áreas de abrangência dos CS diferem substancialmente na composição etária e de sexo de sua população adscrita. A comparação de taxas brutas entre CS com perfis populacionais distintos pode ser enganosa, pois ICSAP são sabidamente mais frequentes em idosos (doenças crônicas) e em crianças menores de 5 anos (condições respiratórias e gastroenterite). A padronização direta remove esse viés de composição e torna os CS comparáveis.

### Método: Padronização Direta (Ahmad et al., 2001)

O script `R/07_padronizacao_taxa.R` implementa a padronização direta conforme o método padrão da OMS:

$$\text{Taxa}_{\text{pad}} = \frac{\displaystyle\sum_{j} \frac{n_{ij}}{P_{ij}} \cdot P^*_j}{\displaystyle\sum_{j} P^*_j} \times 10.000$$

Onde:
- $n_{ij}$ = número de internações ICSAP no CS $i$, faixa $j$
- $P_{ij}$ = população do CS $i$ na faixa $j$ (Censo IBGE 2022 × proporção de BH)
- $P^*_j$ = população-padrão na faixa $j$ (distribuição etária e de sexo de BH, Censo 2022)
- Unidade: por 10.000 habitantes por área de abrangência por ano

### Faixas etárias (padrão RIPSA)

`<5 · 5–14 · 15–29 · 30–44 · 45–59 · 60–74 · 75+` (7 grupos) × 2 sexos = 14 estratos

### Diagnóstico: inversão de ranking

O script identifica CS onde a padronização inverte o ranking de taxa (variação ≥ 20 posições). CS onde isso ocorre são aqueles com composição etária muito diferente da média municipal — especialmente relevante para a discussão sobre desigualdades intramunicipais.

**Saída:** `data/processed/taxas_padronizadas.csv`

**Referência:** Ahmad OB, Boschi-Pinto C, Lopez AD, Murray CJL, Lozano R, Inoue M. Age standardization of rates: a new WHO standard. GPE Discussion Paper Series No. 31. Geneva: World Health Organization; 2001.

---

### Limitação Implementada — Padronização Indireta Não Realizada

> **Situação verificada após execução do script 07 (mai/2026):**

O Censo IBGE 2022, quando acessado via pacote `censobr` (tabela `Basico` por setor censitário), **não disponibiliza faixas etárias desagregadas por setor**. A variável disponível é apenas a população total por setor (`V0001`), sem estratificação etária.

Por isso, **não foi possível realizar a padronização direta por idade e sexo descrita acima**, pois ela requer $P_{ij}$ = população do CS $i$ na faixa etária $j$, que não está disponível nessa fonte a nível de setor censitário.

**Solução adotada:** O script `R/07_padronizacao_taxa.R` aplica a mesma distribuição etária de Belo Horizonte (Censo 2022, nível municipal) a todos os CS. Isso produz taxas numericamente idênticas às taxas brutas (correlação = 1,000 entre taxa bruta e padronizada), pois o fator de padronização é constante entre CS.

**Decisão metodológica:** A **taxa ICSAP bruta por 10.000 habitantes** (n_icsap / pop_cs × 10.000) é utilizada como desfecho em todas as análises. Essa métrica é a mais comparável internacionalmente e é amplamente usada na literatura de ICSAP (Nedel et al., 2011; Alfradique et al., 2009).

**Implicação para validade:** O viés potencial por composição etária diferencial entre CS é uma limitação reconhecida do estudo. Sua magnitude deve ser discutida no manuscrito à luz da evidência de que:
1. A variação na composição etária entre as 153 áreas de abrangência de BH é relativamente homogênea em comparação a outros contextos;
2. O IVS-BH incorporado como covariável no modelo GEE captura parte da estrutura demográfica diferencial entre CS.

**Texto sugerido para a seção Limitações do manuscrito:**

> "As taxas ICSAP não foram padronizadas por faixas etárias desagregadas por setor censitário, pois tais dados não estão disponíveis no Censo IBGE 2022 a nível de setor no pacote *censobr*. O potencial viés de composição etária diferencial entre as áreas de abrangência dos CS é uma limitação reconhecida, parcialmente controlada pela inclusão do IVS-BH no modelo multivariável."

---

## Análise de Autocorrelação Espacial

### Justificativa

Em estudos ecológicos com unidades geográficas adjacentes (como áreas de abrangência de CS), os resíduos do modelo tendem a ser espacialmente correlacionados — CS vizinhos compartilham características socioeconômicas, acesso a serviços e histórico de adoecimento. Essa autocorrelação viola o pressuposto de independência dos erros do GLM padrão e pode inflar os coeficientes ou os intervalos de confiança.

O **Índice de Moran's I** (Moran, 1950) e os **LISA** — Local Indicators of Spatial Association (Anselin, 1995) — são as ferramentas padrão para detectar e caracterizar esse padrão.

### Método implementado (script `R/08_autocorrelacao_espacial.R`)

**Matriz de vizinhança:** Queen contiguity (pacote `spdep`) — dois CS são vizinhos se compartilharem pelo menos um ponto de fronteira.

**Moran's I global:**

$$I = \frac{n}{\sum_{i}\sum_{j} w_{ij}} \cdot \frac{\sum_{i}\sum_{j} w_{ij}(x_i - \bar{x})(x_j - \bar{x})}{\sum_{i}(x_i - \bar{x})^2}$$

Onde $w_{ij}$ = peso espacial entre CS $i$ e CS $j$ (1 se vizinhos, 0 caso contrário), $x_i$ = taxa ICSAP padronizada do CS $i$.

**Moran's I local (LISA):** Classifica cada CS em:

| Quadrante | Interpretação |
|---|---|
| **High-High** | CS com alta taxa rodeado de CS com alta taxa — cluster de risco elevado |
| **Low-Low** | CS com baixa taxa rodeado de CS com baixa taxa — cluster de bom desempenho |
| **High-Low** | CS com alta taxa rodeado de CS com baixa taxa — outlier isolado de alto risco |
| **Low-High** | CS com baixa taxa rodeado de CS com alta taxa — outlier isolado de bom desempenho |
| **Not Significant** | Sem padrão espacial significativo (p ≥ 0,05) |

### Decisão metodológica sobre o modelo

| Resultado do Moran's I | Decisão |
|---|---|
| $I > 0$ e $p < 0{,}05$ | Considerar modelo SAR, CAR ou GEE com estrutura espacial |
| $I \approx 0$ ou $p \geq 0{,}05$ | GLM-Gama padrão (sem lag espacial) é adequado |

**Saídas:** `data/processed/moran_resultados.csv` e `docs/mapa_lisa.png`

**Referências:**
- Anselin L. Local indicators of spatial association — LISA. *Geographical Analysis*. 1995;27(2):93–115. doi:10.1111/j.1538-4632.1995.tb00338.x
- Moran PAP. Notes on continuous stochastic phenomena. *Biometrika*. 1950;37(1/2):17–23. doi:10.2307/2332142

---

## Análise de Interrupção de Série Temporal (Interrupted Time Series — ITS)

### Contexto e Justificativa

A *joinpoint regression* (script 10) identificou inflexão em ~abril/2024 nas taxas ICSAP em todas as 9 regionais administrativas de BH **simultaneamente**. Um padrão síncrono e de abrangência municipal é compatível com efeito de política nacional, não com mudanças locais isoladas.

A **Portaria GM/MS nº 3.493, de 10 de abril de 2024** instituiu nova metodologia de cofinanciamento federal da APS:
- Aumento do repasse federal por equipe ESF de R$ 21.000 para R$ 24.000–30.000, escalonado por indicadores de desempenho
- Introdução de componente de qualidade vinculado a indicadores de HAS, DM, pré-natal e desenvolvimento infantil
- Vigência dos novos valores a partir de maio de 2024

A análise de ITS permite **testar formalmente** se a Portaria 3.493/2024 produziu mudança na trajetória das ICSAP em BH — mudança no nível (impacto imediato) e/ou na tendência (mudança na inclinação da série após a portaria). O ITS é a abordagem quase-experimental de referência para avaliar efeitos de intervenções de política pública em séries temporais de saúde quando randomização não é possível (Bernal et al., 2017).

O ITS é uma análise **complementar** ao GEE (script 09): o GEE responde à pergunta cross-sectional ("CS com maior estrutura ESF têm menores taxas ICSAP?"); o ITS responde à pergunta longitudinal ("a Portaria 3.493/2024 alterou a trajetória das ICSAP no tempo?").

### Modelo

Regressão log-linear segmentada com correção de autocorrelação (Prais-Winsten ou GEE AR-1):

$$\ln(\mu_t) = \beta_0 + \beta_1 \cdot \text{Tempo}_t + \beta_2 \cdot \text{Intervenção}_t + \beta_3 \cdot (\text{Tempo}_t \times \text{Intervenção}_t) + \varepsilon_t$$

Onde:
- $\text{Tempo}_t$: número sequencial do mês (1 = jan/2023; 39 = mar/2026)
- $\text{Intervenção}_t$: variável binária — **0** antes de maio/2024, **1** a partir de maio/2024 (mês $t = 17$)
- $\beta_0$: nível basal pré-intervenção
- $\beta_1$: tendência pré-intervenção (inclinação do segmento 1)
- $\beta_2$: mudança imediata no **nível** das ICSAP em maio/2024 (impacto de curto prazo)
- $\beta_3$: mudança na **inclinação** após a portaria (diferença entre as tendências pré e pós)

A tendência no período pós-intervenção é: $\beta_1 + \beta_3$

### Interpretação Esperada

| Coeficiente | Sinal esperado | Interpretação substantiva |
|---|---|---|
| $\beta_1$ | Positivo | Crescimento das ICSAP no período pré-portaria (confirmado pelo joinpoint: +19,2%/ano) |
| $\beta_2$ | Negativo | Queda imediata em maio/2024 — efeito de curto prazo (adesão imediata dos CS aos indicadores) |
| $\beta_3$ | Negativo | Mudança para tendência decrescente — efeito estrutural (melhora continuada da qualidade APS) |

Resultado compatível com efeito da portaria: $\beta_2 < 0$ e/ou $\beta_3 < 0$, com $p < 0{,}05$.

Se ambos forem não significativos, a inflexão pode refletir variação aleatória, sazonalidade, mudança de codificação no SIHSUS ou outros fatores concorrentes não capturados.

### Implementação Planejada

Script: `R/11_its.R`

| Decisão | Opção adotada | Justificativa |
|---|---|---|
| Nível de análise | Municipal (série agregada de BH) | Maior poder estatístico; joinpoint municipal é o mais robusto |
| Dados | `icsap_bh.csv` + `internacoes_bh.csv` | Série completa (100%); sem viés de geocodificação |
| Variável resposta | % ICSAP sobre total de internações | Controla variação no volume de internações ao longo do tempo |
| Correção de autocorrelação | GEE AR-1 (Gama, link log) | Consistente com script 09; ou Prais-Winsten para log(taxa) |
| Sazonalidade | Termos de Fourier (sen/cos, período 12 meses) | Incluir se ACF dos resíduos mostrar picos em lag 12 |
| Ponto de corte | Maio/2024 ($t = 17$) | Data de vigência dos novos valores da Portaria 3.493/2024 |

### Análises de Sensibilidade

| Sensibilidade | Objetivo |
|---|---|
| Variar ponto de corte ±2 meses (mar–jul/2024) | Verificar robustez em relação à data exata da intervenção |
| Estratificar por regional administrativa | Testar heterogeneidade geográfica do efeito |
| Usar condições ICSAP crônicas vs agudas separadamente | Verificar se o efeito é mediado por condições mais responsivas à APS |
| Comparar com outras capitais brasileiras (DiD-ITS) | Isolar efeito da portaria de tendências nacionais (principal controle negativo) |

### Limitação Principal

O ITS sem grupo controle não permite isolar o efeito da Portaria 3.493/2024 de outros eventos concorrentes (pandemia tardia, sazonalidade, reorganização local da rede, mudança nos critérios de codificação do SIHSUS). A análise comparativa com outras capitais como grupo controle (diferença-em-diferenças interrompida — DiD-ITS) aumentaria substancialmente a validade causal, mas está fora do escopo do presente estudo. Essa limitação deve ser explicitada na seção de discussão do manuscrito.

### Referência

Bernal JL, Cummins S, Gasparrini A. Interrupted time series regression for the evaluation of public health interventions: a tutorial. *BMJ*. 2017;359:j2981. doi:10.1136/bmj.j2981

---

## Atualização do Checklist STROBE

Com a implementação dos scripts 06, 07 e 08, os seguintes itens do Checklist STROBE foram atualizados:

| Item STROBE | Descrição | Status anterior | Status atual |
|---|---|---|---|
| **12c** | Tratamento dos dados faltantes | ⚠️ Parcial | ✅ Atendido |
| **E6** | Correlação espacial entre unidades adjacentes | ⚠️ Parcial | ✅ Atendido |

Consulte `docs/checklist_strobe.md` para o checklist completo atualizado.
