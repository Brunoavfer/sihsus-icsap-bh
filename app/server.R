# =============================================================================
# server.R
#
# O que faz:
#   - Define a lógica do painel
#   - Filtra os dados conforme seleção do usuário
#   - Gera os gráficos e tabelas
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Dados filtrados reativamente
  # ---------------------------------------------------------------------------

  dados_filtrados <- reactive({

    df <- dados

    # Filtro de ano
    df <- df %>%
      filter(ano_cmpt >= input$filtro_ano[1],
             ano_cmpt <= input$filtro_ano[2])

    # Filtro de regional
    if (input$filtro_regional != "Todas") {
      df <- df %>% filter(regional == input$filtro_regional)
    }

    # Filtro de condição
    if (input$filtro_condicao != "Todas") {
      df <- df %>% filter(descricao == input$filtro_condicao)
    }

    df
  })

  # ---------------------------------------------------------------------------
  # Caixas de resumo
  # ---------------------------------------------------------------------------

  output$box_total <- renderValueBox({
    valueBox(
      value    = format(nrow(dados_filtrados()), big.mark = "."),
      subtitle = "Total de Internações ICSAP",
      icon     = icon("hospital"),
      color    = "blue"
    )
  })

  output$box_regional_mais <- renderValueBox({
    regional <- dados_filtrados() %>%
      filter(!is.na(regional)) %>%
      count(regional, sort = TRUE) %>%
      slice(1) %>%
      pull(regional)

    valueBox(
      value    = ifelse(length(regional) > 0, regional, "—"),
      subtitle = "Regional com mais internações",
      icon     = icon("map-marker"),
      color    = "orange"
    )
  })

  output$box_condicao_mais <- renderValueBox({
    condicao <- dados_filtrados() %>%
      filter(!is.na(descricao)) %>%
      count(descricao, sort = TRUE) %>%
      slice(1) %>%
      pull(descricao)

    valueBox(
      value    = ifelse(length(condicao) > 0, condicao, "—"),
      subtitle = "Condição mais frequente",
      icon     = icon("stethoscope"),
      color    = "red"
    )
  })

  # ---------------------------------------------------------------------------
  # Gráfico: evolução mensal
  # ---------------------------------------------------------------------------

  output$grafico_evolucao <- renderPlotly({

    df <- dados_filtrados() %>%
      count(data_internacao) %>%
      arrange(data_internacao)

    plot_ly(df,
      x = ~data_internacao,
      y = ~n,
      type = "scatter",
      mode = "lines",
      line = list(color = "#2980b9")
    ) %>%
      layout(
        title  = "Evolução das Internações ICSAP",
        xaxis  = list(title = ""),
        yaxis  = list(title = "Internações")
      )
  })

  # ---------------------------------------------------------------------------
  # Gráfico: internações por regional
  # ---------------------------------------------------------------------------

  output$grafico_regional <- renderPlotly({

    df <- dados_filtrados() %>%
      filter(!is.na(regional)) %>%
      count(regional, sort = TRUE)

    plot_ly(df,
      x    = ~reorder(regional, n),
      y    = ~n,
      type = "bar",
      marker = list(color = "#27ae60")
    ) %>%
      layout(
        title  = "Internações por Regional",
        xaxis  = list(title = ""),
        yaxis  = list(title = "Internações")
      )
  })

  # ---------------------------------------------------------------------------
  # Gráfico: internações por condição
  # ---------------------------------------------------------------------------

  output$grafico_condicao <- renderPlotly({

    df <- dados_filtrados() %>%
      filter(!is.na(descricao)) %>%
      count(descricao, sort = TRUE) %>%
      slice_head(n = 15)

    plot_ly(df,
      x    = ~reorder(descricao, n),
      y    = ~n,
      type = "bar",
      orientation = "h",
      marker = list(color = "#8e44ad")
    ) %>%
      layout(
        title  = "Top 15 Condições ICSAP",
        xaxis  = list(title = "Internações"),
        yaxis  = list(title = "")
      )
  })

  # ---------------------------------------------------------------------------
  # Tabela de dados
  # ---------------------------------------------------------------------------

  output$tabela_dados <- renderDataTable({
    dados_filtrados() %>%
      select(
        data_internacao,
        regional,
        descricao,
        idade,
        sexo,
        dias_perm,
        val_tot
      ) %>%
      rename(
        Data       = data_internacao,
        Regional   = regional,
        Condição   = descricao,
        Idade      = idade,
        Sexo       = sexo,
        `Dias Internado` = dias_perm,
        `Valor (R$)`     = val_tot
      )
  })

  # ---------------------------------------------------------------------------
  # Download dos dados
  # ---------------------------------------------------------------------------

  output$baixar_dados <- downloadHandler(
    filename = function() {
      paste0("icsap_bh_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(dados_filtrados(), file)
    }
  )
}
