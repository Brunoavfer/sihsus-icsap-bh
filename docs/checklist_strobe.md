# Checklist STROBE — Estudos Observacionais (Ecológico Longitudinal)

**Referência:** von Elm E, Altman DG, Egger M, Pocock SJ, Gøtzsche PC, Vandenbroucke JP; STROBE Initiative. The Strengthening the Reporting of Observational Studies in Epidemiology (STROBE) Statement: guidelines for reporting observational studies. *PLoS Medicine*. 2007;4(10):e296. doi:10.1371/journal.pmed.0040296

**Adaptação ecológica:** Morgenstern H. Ecologic studies in epidemiology: concepts, principles, and methods. *Annu Rev Public Health*. 1995;16:61-81.

**Projeto:** ICSAP-BH — Estudo Ecológico Longitudinal, 2023–2025  
**Avaliação:** maio de 2026

**Legenda de status:**
- ✅ **Atendido** — item contemplado pelo protocolo ou dados disponíveis
- ⚠️ **Parcial** — item iniciado, mas incompleto ou dependente de etapas futuras
- ❌ **Pendente** — não iniciado; requer ação específica

---

## TÍTULO E RESUMO

| Item | Descrição STROBE | Status | Observação |
|------|-----------------|--------|-----------|
| **1a** | Indicar o desenho do estudo no título ou resumo | ⚠️ Parcial | Título provisório menciona "estudo ecológico longitudinal"; confirmar na versão final |
| **1b** | Fornecer no resumo uma síntese informativa e equilibrada do que foi feito e encontrado | ❌ Pendente | Resumo estruturado a ser elaborado após análises |

---

## INTRODUÇÃO

| Item | Descrição STROBE | Status | Observação |
|------|-----------------|--------|-----------|
| **2** | **Contexto/Justificativa:** Explicar o embasamento científico e a razão da realização do estudo | ⚠️ Parcial | Contextualização presente no protocolo; seção de introdução do manuscrito a ser redigida |
| **3** | **Objetivos:** Declarar objetivos específicos, incluindo hipóteses pré-especificadas | ✅ Atendido | Objetivo geral e 5 objetivos específicos definidos em `protocolo_pesquisa.md` |

---

## MÉTODOS

| Item | Descrição STROBE | Status | Observação |
|------|-----------------|--------|-----------|
| **4** | **Desenho do estudo:** Apresentar os elementos-chave do desenho no início da seção de métodos | ✅ Atendido | Estudo ecológico longitudinal, painel CS × mês, descrito no protocolo |
| **5** | **Cenário:** Descrever o cenário, locais e datas relevantes, incluindo períodos de recrutamento, exposição, acompanhamento e coleta | ✅ Atendido | BH, 153 CS, jan/2023–dez/2025; fontes SIHSUS, CNES, e-Gestor AB, IVS-BH |
| **6** | **Participantes / Unidades ecológicas:** Descrever as unidades de análise e o processo de seleção | ✅ Atendido | 153 áreas de abrangência dos CS municipais; polígonos oficiais SMSA/BH |
| **7** | **Variáveis:** Definir claramente desfechos, exposições, preditores, confundidores e modificadores de efeito | ✅ Atendido | Definidas no protocolo e detalhadas em `variaveis_estudo.md` |
| **8** | **Fontes de dados / Mensuração:** Para cada variável de interesse, descrever fontes e métodos de avaliação | ✅ Atendido | SIHSUS, CNES, e-Gestor AB, IVS-BH, IBGE — descritos em `variaveis_estudo.md` |
| **9** | **Viés:** Descrever qualquer esforço para abordar potenciais fontes de viés | ⚠️ Parcial | Limitações listadas no protocolo; análises de sensibilidade planejadas; seção de discussão a desenvolver |
| **10** | **Tamanho da amostra:** Explicar como o tamanho do estudo foi calculado | ⚠️ Parcial | 153 CS × 36 meses = 5.508 obs. (censo, não amostra); cálculo de poder para o GLM-Gama pendente |
| **11** | **Variáveis quantitativas:** Explicar como variáveis quantitativas foram tratadas (categorização, pontos de corte) | ⚠️ Parcial | IVS: contínuo e categorizado em tertis; cobertura ESF: contínua; categorização detalhada pendente |
| **12a** | **Métodos estatísticos:** Descrever todos os métodos estatísticos, incluindo os usados para controlar confundimento | ✅ Atendido | GLM-Gama (link log) + joinpoint regression + GEE para autocorrelação; detalhado no protocolo |
| **12b** | Descrever métodos para análise de subgrupos e interações | ⚠️ Parcial | Estratificação por regional planejada; análise de interação ESF × IVS a definir |
| **12c** | Explicar como foram tratados os dados faltantes | ✅ Atendido | CEPs não geocodificados excluídos; análise de padrão de missingness implementada em `R/06_analise_missing.R` — comparação COM vs SEM regional em idade, sexo, condição ICSAP, valor, dias de permanência e distribuição temporal |
| **12d** | *Para estudos de coorte:* Abordar perda de seguimento | ✅ Atendido | Não aplicável (dados administrativos secundários) |
| **12e** | Descrever análises de sensibilidade | ✅ Atendido | 4 modelos de sensibilidade descritos no protocolo |

---

## RESULTADOS

| Item | Descrição STROBE | Status | Observação |
|------|-----------------|--------|-----------|
| **13a** | **Participantes:** Reportar o número de indivíduos em cada etapa do estudo (fluxograma) | ❌ Pendente | Fluxograma de inclusão/exclusão de internações e CS a elaborar |
| **13b** | Fornecer razões para não participação em cada etapa | ❌ Pendente | Quantificação das exclusões por critério (CEP inválido, CS com < 12 meses, etc.) |
| **13c** | Considerar o uso de um diagrama de fluxo | ❌ Pendente | Fluxograma PRISMA-adaptado para estudos ecológicos |
| **14a** | **Dados descritivos:** Descrever características dos participantes (unidades) e informações sobre exposições e confundidores | ❌ Pendente | Tabela 1: distribuição dos CS por regional, porte, IVS, cobertura ESF |
| **14b** | Indicar o número de participantes com dados faltantes | ❌ Pendente | % CEPs não geocodificados por CS e regional |
| **15** | **Dados do desfecho:** Reportar número de eventos desfecho ou medidas-resumo ao longo do tempo | ⚠️ Parcial | Contagens brutas disponíveis (`icsap_bh_regional.csv`); taxas padronizadas pendentes |
| **16a** | **Principais resultados:** Apresentar estimativas não ajustadas e, se aplicável, ajustadas e seus IC 95% | ❌ Pendente | Análises estatísticas não iniciadas |
| **16b** | Reportar os limites das categorias quando variáveis contínuas forem categorizadas | ❌ Pendente | Depende da categorização final das variáveis |
| **16c** | Se pertinente, transformar estimativas de risco relativo em risco absoluto por período de tempo relevante | ❌ Pendente | Diferença de taxas absolutas entre CS de alto/baixo desempenho |
| **17** | **Outras análises:** Reportar análises adicionais realizadas (subgrupos, interações, sensibilidade) | ❌ Pendente | Análises de sensibilidade planejadas; resultados pendentes |

---

## DISCUSSÃO

| Item | Descrição STROBE | Status | Observação |
|------|-----------------|--------|-----------|
| **18** | **Principais resultados:** Resumir os resultados-chave com referência aos objetivos do estudo | ❌ Pendente | Seção de discussão a redigir após análises |
| **19** | **Limitações:** Discutir limitações do estudo, considerando fontes de viés ou imprecisão | ✅ Atendido | 7 limitações mapeadas em `protocolo_pesquisa.md`; discussão a detalhar no manuscrito |
| **20** | **Interpretação:** Apresentar interpretação cautelosa dos resultados, considerando objetivos, limitações, multiplicidade de análises e evidências de outros estudos relevantes | ❌ Pendente | Pendente de resultados; referências mapeadas em `referencias.bib` |
| **21** | **Generalização:** Discutir a generalização (validade externa) dos resultados do estudo | ❌ Pendente | BH como estudo de caso; implicações para outros municípios de grande porte |

---

## OUTRAS INFORMAÇÕES

| Item | Descrição STROBE | Status | Observação |
|------|-----------------|--------|-----------|
| **22** | **Financiamento:** Fornecer informações sobre financiamento e papel dos financiadores | ⚠️ Parcial | Pesquisa independente, sem financiamento externo; declarar na submissão |

---

## Itens Específicos para Estudos Ecológicos

Os seguintes aspectos adicionais são recomendados para estudos ecológicos na literatura (Morgenstern 1995; Leyland & Groenewegen 2003):

| Item Ecológico | Descrição | Status | Observação |
|---|---|---|---|
| **E1** | Justificar a escolha das unidades ecológicas | ✅ Atendido | Área de abrangência dos CS: unidade natural de gestão da APS em BH |
| **E2** | Discutir a falácia ecológica e limitações de inferência | ✅ Atendido | Descrita nas limitações do protocolo |
| **E3** | Avaliar viés de confusão ecológico (*cross-level confounding*) | ⚠️ Parcial | IVS como proxy; outros confundidores ecológicos a avaliar |
| **E4** | Descrever a variabilidade entre unidades ecológicas como informação substantiva | ❌ Pendente | Análise de variância entre CS e entre regionais |
| **E5** | Considerar o problema de *modifiable areal unit problem* (MAUP) | ⚠️ Parcial | Área de abrangência dos CS é unidade definida institucionalmente, reduzindo arbitrariedade |
| **E6** | Abordar a correlação espacial entre unidades adjacentes | ✅ Atendido | Moran's I global e LISA (queen contiguity) implementados em `R/08_autocorrelacao_espacial.R`; resultado orienta decisão sobre lag espacial no GLM-Gama |

---

## Resumo Executivo do Status

| Seção | Total de itens | ✅ Atendido | ⚠️ Parcial | ❌ Pendente |
|-------|---------------|------------|-----------|------------|
| Título e Resumo | 2 | 0 | 1 | 1 |
| Introdução | 2 | 1 | 1 | 0 |
| Métodos | 12 | 7 | 4 | 1 |
| Resultados | 9 | 1 | 1 | 7 |
| Discussão | 4 | 1 | 0 | 3 |
| Outras Informações | 1 | 0 | 1 | 0 |
| **Itens ecológicos** | **6** | **3** | **2** | **1** |
| **TOTAL** | **36** | **13 (36%)** | **10 (28%)** | **13 (36%)** |

**Interpretação:** Os 36% atendidos cobrem a infraestrutura metodológica completa (desenho, variáveis, fontes, métodos estatísticos, análise de missing e autocorrelação espacial). A implementação dos scripts 06, 07 e 08 migrou dois itens de ⚠️ Parcial para ✅ Atendido (12c e E6). Os 36% pendentes concentram-se em resultados e discussão — esperados nesta etapa de protocolo. A prioridade imediata é executar as análises estatísticas (GLM-Gama, joinpoint) para migrar os itens de Resultados de ❌ para ✅.
