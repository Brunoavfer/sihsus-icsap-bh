# =============================================================================
# ui.R
#
# O que faz:
#   - Define a interface visual do painel
#   - Organiza filtros, gráficos e tabelas
# =============================================================================

ui <- fluidPage(

  # Título e estilo
  titlePanel(
    title = div(
      h2("Internações por Condições Sensíveis à Atenção Primária"),
      h4("Belo Horizonte — por Regional Administrativa"),
      style = "color: #2c3e50;"
    )
  ),

  # Barra lateral com filtros
  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Filtros"),

      # Filtro de ano
      sliderInput(
        inputId = "filtro_ano",
        label   = "Período:",
        min     = min(ANOS),
        max     = max(ANOS),
        value   = c(min(ANOS), max(ANOS)),
        sep     = ""
      ),

      # Filtro de regional
      selectInput(
        inputId  = "filtro_regional",
        label    = "Regional:",
        choices  = REGIONAIS,
        selected = "Todas"
      ),

      # Filtro de condição ICSAP
      selectInput(
        inputId  = "filtro_condicao",
        label    = "Condição ICSAP:",
        choices  = c("Todas", GRUPOS_ICSAP),
        selected = "Todas"
      ),

      hr(),

      # Informações sobre os dados
      p("Fonte: SIHSUS/DATASUS"),
      p("Lista ICSAP: Portaria SAS/MS nº 221/2008"),
      p(em("Atualizado automaticamente todo mês."))
    ),

    # Painel principal com abas
    mainPanel(
      width = 9,

      tabsetPanel(

        # Aba 1 — Visão Geral
        tabPanel(
          title = "Visão Geral",
          br(),
          fluidRow(
            valueBoxOutput("box_total"),
            valueBoxOutput("box_regional_mais"),
            valueBoxOutput("box_condicao_mais")
          ),
          br(),
          plotlyOutput("grafico_evolucao", height = "350px"),
          br(),
          plotlyOutput("grafico_regional", height = "350px")
        ),

        # Aba 2 — Por Regional
        tabPanel(
          title = "Por Regional",
          br(),
          plotlyOutput("grafico_mapa_regional", height = "500px")
        ),

        # Aba 3 — Por Condição
        tabPanel(
          title = "Por Condição",
          br(),
          plotlyOutput("grafico_condicao", height = "500px")
        ),

        # Aba 4 — Dados
        tabPanel(
          title = "Dados",
          br(),
          downloadButton("baixar_dados", "Baixar CSV"),
          br(), br(),
          dataTableOutput("tabela_dados")
        )
      )
    )
  )
)
