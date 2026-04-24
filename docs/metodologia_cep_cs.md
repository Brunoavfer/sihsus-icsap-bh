# Metodologia de Identificação do Centro de Saúde pelo CEP

## Contexto

O Sistema de Informações Hospitalares do SUS (SIHSUS) registra o **CEP de residência** de cada paciente internado. No presente projeto, utilizamos essa informação para identificar qual Centro de Saúde (CS) da rede municipal de Belo Horizonte é responsável pelo endereço de residência do paciente — permitindo a desagregação das internações por condições sensíveis à atenção primária (ICSAP) no nível do CS e da regional administrativa.

---

## Fundamento Metodológico

A Secretaria Municipal de Saúde de Belo Horizonte (SMSA) define a área de responsabilidade de cada CS por meio de **polígonos geográficos oficiais**, publicados mensalmente no Portal de Dados Abertos da PBH:

> **Fonte:** Portal de Dados Abertos da PBH — Área de Abrangência Saúde (SMSA)
> **URL:** https://dados.pbh.gov.br/dataset/area-de-abrangencia-saude
> **Licença:** Creative Commons Attribution
> **Periodicidade:** Mensal

Essa delimitação por polígono é mais precisa do que abordagens baseadas em listas de bairros, pois um mesmo bairro pode ser atendido por mais de um CS, dependendo do endereço exato do paciente.

**Exemplo:**
O bairro **"da Graça"** (Regional Nordeste) é parcialmente atendido pelo C.S. Alcides Lins e parcialmente pelo C.S. Cidade Ozanan. Apenas a coordenada geográfica do endereço permite a alocação correta.

---

## Método de Geolocalização — Três Etapas

A identificação do CS de referência para cada CEP é realizada em três etapas sequenciais:

### Etapa 1 — Consulta ao ViaCEP

Para cada CEP único presente nos dados do SIHSUS, é realizada uma consulta à **API ViaCEP** (https://viacep.com.br), que retorna o logradouro, bairro e município correspondentes.

```
Entrada:  CEP (ex: 30140-071)
Saída:    Logradouro: Rua dos Aimorés
          Bairro: Boa Viagem
          Cidade: Belo Horizonte
```

Nesta etapa, são descartados os CEPs pertencentes a outros municípios (erro de cadastro no SIHSUS) e os CEPs inválidos ou inexistentes.

### Etapa 2 — Geocodificação via Nominatim

O endereço retornado pelo ViaCEP é enviado à **API Nominatim** (OpenStreetMap — https://nominatim.openstreetmap.org), que converte o endereço textual em coordenadas geográficas (latitude e longitude no sistema WGS 84 / EPSG:4326).

```
Entrada:  Rua dos Aimorés, Boa Viagem, Belo Horizonte, MG
Saída:    Latitude:  -19.9294534
          Longitude: -43.9337935
```

### Etapa 3 — Cruzamento com Polígonos da PBH

As coordenadas obtidas na Etapa 2 são cruzadas com os **polígonos oficiais de área de abrangência** dos Centros de Saúde da PBH, utilizando operação de interseção espacial ponto × polígono (função `st_join` do pacote `sf` no R, com projeção EPSG:4326).

```
Entrada:  Ponto: lat = -19.929, lon = -43.933
          Polígonos: 153 áreas de abrangência dos CS de BH
Saída:    Centro de Saúde: C.S. Carlos Chagas
          Regional: Centro-Sul
```

---

## Fluxo Completo

```
CEP do paciente (SIHSUS)
        │
        ▼
┌───────────────────────┐
│  ETAPA 1 — ViaCEP     │
│  CEP → Endereço       │
└──────────┬────────────┘
           │ logradouro + bairro + cidade
           ▼
┌───────────────────────┐
│  ETAPA 2 — Nominatim  │
│  Endereço → lat/lon   │
└──────────┬────────────┘
           │ latitude + longitude (WGS 84)
           ▼
┌────────────────────────────────────┐
│  ETAPA 3 — Cruzamento espacial     │
│  Ponto × Polígonos oficiais PBH    │
│  (st_join — pacote sf — R)         │
└──────────┬─────────────────────────┘
           │
           ▼
   Centro de Saúde + Regional
```

---

## Otimização — Sistema de Cache

Para evitar consultas redundantes às APIs externas e reduzir o tempo de processamento nas execuções mensais automatizadas, o pipeline implementa um **sistema de cache por CEP**:

- Na primeira execução, todos os CEPs únicos são consultados e os resultados (coordenadas ou motivo de falha) são armazenados no arquivo `data/ref/cache_cep.csv`.
- Nas execuções subsequentes, apenas os CEPs novos (não presentes no cache) são consultados.
- O cache é salvo incrementalmente a cada 50 CEPs processados, garantindo que o progresso não seja perdido em caso de interrupção.

Essa estratégia reduz significativamente o número de consultas às APIs a cada atualização mensal, respeitando os limites de uso dos serviços e reduzindo o tempo total de processamento.

---

## Tratamento de CEPs Não Identificados

Quatro situações podem resultar na não identificação do CS de referência:

| Situação | Motivo | Tratamento |
|---|---|---|
| CEP inválido | Erro de digitação no SIHSUS (menos de 8 dígitos, zerado) | Excluído da consulta |
| CEP não encontrado | CEP inexistente ou não cadastrado no ViaCEP | Registrado no log |
| CEP de outro município | Inconsistência de cadastro no SIHSUS | Excluído — somente BH |
| Endereço não geocodificado | Logradouro não reconhecido pelo Nominatim | Registrado no log |

Todos os casos não identificados são registrados no arquivo `data/processed/ceps_nao_encontrados.csv`, contendo o CEP, o motivo da falha e o número de pacientes afetados. Esse arquivo subsidia melhorias contínuas na cobertura do pipeline.

---

## Cobertura Alcançada

Resultados obtidos para o ano de 2025 (janeiro a dezembro):

| Indicador | Valor |
|---|---|
| Total de internações ICSAP | 28.420 |
| CEPs únicos consultados | 8.195 |
| Com CS identificado | 23.859 |
| Sem CS identificado | 4.561 |
| **Taxa de cobertura** | **84,0%** |

A meta estabelecida para o projeto é de **85 a 90% de cobertura**. Para os CEPs não geocodificados pelo Nominatim, está em avaliação o uso de **inteligência artificial** (API Anthropic/Claude) como etapa adicional de geocodificação, estimando o CS mais provável com base no logradouro e bairro retornados pelo ViaCEP.

---

## Limitações

1. **Dependência de APIs externas:** O pipeline depende da disponibilidade do ViaCEP e do Nominatim. Falhas temporárias nessas APIs podem reduzir a cobertura em execuções específicas.

2. **Desatualização dos polígonos:** As áreas de abrangência são atualizadas mensalmente pela PBH. Internações de períodos anteriores à última atualização podem ser alocadas a CS com delimitações ligeiramente diferentes das vigentes à época da internação.

3. **Qualidade do CEP no SIHSUS:** O SIHSUS é um sistema de informação administrativo e pode conter erros de digitação nos campos de CEP, especialmente em registros mais antigos.

4. **Bairros limítrofes:** Em endereços localizados próximos aos limites entre áreas de abrangência de dois CS, pequenas imprecisões na geocodificação podem resultar em alocação incorreta.

---

## Ferramentas Utilizadas

| Ferramenta | Versão | Finalidade |
|---|---|---|
| R | ≥ 4.5.0 | Linguagem principal |
| pacote `sf` | ≥ 1.0 | Operações espaciais (st_join, st_transform) |
| pacote `httr` | ≥ 1.4 | Consultas às APIs (ViaCEP, Nominatim) |
| pacote `jsonlite` | ≥ 1.8 | Parsing das respostas JSON |
| API ViaCEP | — | Conversão CEP → endereço |
| API Nominatim (OSM) | — | Geocodificação endereço → lat/lon |
| Polígonos PBH/SMSA | Mensal | Áreas de abrangência dos CS |

---

## Referências

- BRASIL. Ministério da Saúde. Departamento de Informática do SUS (DATASUS). *Sistema de Informações Hospitalares do SUS (SIHSUS)*. Disponível em: http://datasus.saude.gov.br

- PREFEITURA DE BELO HORIZONTE (PBH). *Área de Abrangência Saúde — Portal de Dados Abertos*. Disponível em: https://dados.pbh.gov.br/dataset/area-de-abrangencia-saude

- VIACEP. *API de Consulta de CEP*. Disponível em: https://viacep.com.br

- NOMINATIM. *OpenStreetMap Geocoding API*. Disponível em: https://nominatim.openstreetmap.org

- PEBESMA, E. *Simple Features for R: Standardized Support for Spatial Vector Data*. The R Journal, v. 10, n. 1, p. 439-446, 2018.
