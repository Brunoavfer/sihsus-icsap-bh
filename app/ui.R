# =============================================================================
# ui.R — Interface do painel ICSAP-BH
# =============================================================================

ui <- fluidPage(

  # Título
  titlePanel(
    title = div(
      h2("Internações por Condições Sensíveis à Atenção Primária"),
      h4("Belo Horizonte — por Regional e Centro de Saúde"),
      style = "color: #2c3e50;"
    )
  ),

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

      # Filtro de Centro de Saúde
      selectInput(
        inputId  = "filtro_cs",
        label    = "Centro de Saúde:",
        choices  = CENTROS_SAUDE,
        selected = "Todos"
      ),

      # Filtro de condição ICSAP
      selectInput(
        inputId  = "filtro_condicao",
        label    = "Condição ICSAP:",
        choices  = c("Todas", CONDICOES),
        selected = "Todas"
      ),

      hr(),
      p("Fonte: SIHSUS/DATASUS"),
      p("Polígonos: Portal Dados Abertos PBH/SMSA"),
      p("Lista ICSAP: Portaria SAS/MS nº 221/2008"),
      p(em("Atualizado automaticamente todo dia 10."))
    ),

    mainPanel(
      width = 9,

      tabsetPanel(

        # Aba 1 — Visão Geral
        tabPanel(
          title = "Visão Geral",
          br(),
          fluidRow(
            valueBoxOutput("box_total"),
            valueBoxOutput("box_taxa"),
            valueBoxOutput("box_regional_mais")
          ),
          br(),
          plotlyOutput("grafico_evolucao", height = "300px"),
          br(),
          plotlyOutput("grafico_regional", height = "300px")
        ),

        # Aba 2 — Mapa
        tabPanel(
          title = "Mapa",
          br(),
          fluidRow(
            column(12,
              radioButtons(
                inputId  = "mapa_nivel",
                label    = "Visualizar por:",
                choices  = c("Regional" = "regional", "Centro de Saúde" = "cs"),
                selected = "regional",
                inline   = TRUE
              )
            )
          ),
          leafletOutput("mapa_interativo", height = "550px")
        ),

        # Aba 3 — Por Condição
        tabPanel(
          title = "Por Condição",
          br(),
          plotlyOutput("grafico_condicao", height = "500px")
        ),

        # Aba 4 — Ranking Centros de Saúde
        tabPanel(
          title = "Ranking CS",
          br(),
          plotlyOutput("grafico_ranking_cs", height = "600px")
        ),

        # Aba 5 — Dados
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
