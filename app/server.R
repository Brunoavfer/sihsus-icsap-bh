# =============================================================================
# server.R — Lógica do painel ICSAP-BH
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # Atualiza filtro de CS quando regional muda
  # ---------------------------------------------------------------------------

  observeEvent(input$filtro_regional, {
    if (input$filtro_regional == "Todas") {
      choices_cs <- CENTROS_SAUDE
    } else {
      choices_cs <- c("Todos", sort(unique(na.omit(
        dados$nome_cs[dados$regional == input$filtro_regional]
      ))))
    }
    updateSelectInput(session, "filtro_cs", choices = choices_cs)
  })

  # ---------------------------------------------------------------------------
  # Dados filtrados reativamente
  # ---------------------------------------------------------------------------

  dados_filtrados <- reactive({
    df <- dados %>%
      filter(ano_cmpt >= input$filtro_ano[1],
             ano_cmpt <= input$filtro_ano[2])

    if (input$filtro_regional != "Todas")
      df <- df %>% filter(regional == input$filtro_regional)

    if (input$filtro_cs != "Todos")
      df <- df %>% filter(nome_cs == input$filtro_cs)

    if (input$filtro_condicao != "Todas")
      df <- df %>% filter(descricao == input$filtro_condicao)

    df
  })

  # ---------------------------------------------------------------------------
  # Total de internações no período (denominador da taxa)
  # ---------------------------------------------------------------------------

  total_periodo <- reactive({
    dados %>%
      filter(ano_cmpt >= input$filtro_ano[1],
             ano_cmpt <= input$filtro_ano[2]) %>%
      nrow()
  })

  # ---------------------------------------------------------------------------
  # Caixas de resumo
  # ---------------------------------------------------------------------------

  output$box_total <- renderValueBox({
    valueBox(
      value    = format(nrow(dados_filtrados()), big.mark = "."),
      subtitle = "Internações ICSAP",
      icon     = icon("hospital"),
      color    = "blue"
    )
  })

  output$box_taxa <- renderValueBox({
    taxa <- round(nrow(dados_filtrados()) / total_periodo() * 100, 1)
    valueBox(
      value    = paste0(taxa, "%"),
      subtitle = "Taxa ICSAP",
      icon     = icon("percent"),
      color    = "orange"
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
      subtitle = "Regional com mais ICSAP",
      icon     = icon("map-marker"),
      color    = "red"
    )
  })

  # ---------------------------------------------------------------------------
  # Gráfico: evolução temporal
  # ---------------------------------------------------------------------------

  output$grafico_evolucao <- renderPlotly({
    df <- dados_filtrados() %>%
      count(data_internacao) %>%
      arrange(data_internacao)

    plot_ly(df, x = ~data_internacao, y = ~n,
            type = "scatter", mode = "lines",
            line = list(color = "#2980b9")) %>%
      layout(title  = "Evolução das Internações ICSAP",
             xaxis  = list(title = ""),
             yaxis  = list(title = "Internações"))
  })

  # ---------------------------------------------------------------------------
  # Gráfico: por regional
  # ---------------------------------------------------------------------------

  output$grafico_regional <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(regional)) %>%
      count(regional, sort = TRUE)

    plot_ly(df, x = ~reorder(regional, n), y = ~n,
            type = "bar", marker = list(color = "#27ae60")) %>%
      layout(title = "Internações ICSAP por Regional",
             xaxis = list(title = ""),
             yaxis = list(title = "Internações"))
  })

  # ---------------------------------------------------------------------------
  # Mapa interativo
  # ---------------------------------------------------------------------------

  output$mapa_interativo <- renderLeaflet({

    if (is.null(poligonos_cs)) {
      return(leaflet() %>% addTiles() %>%
               setView(lng = -43.94, lat = -19.92, zoom = 11))
    }

    if (input$mapa_nivel == "regional") {

      # Agrega por regional
      taxa_regional <- dados_filtrados() %>%
        filter(!is.na(regional)) %>%
        group_by(regional) %>%
        summarise(n_icsap = n(), .groups = "drop") %>%
        mutate(taxa = round(n_icsap / total_periodo() * 100, 1))

      # Junta com polígonos — dissolve por regional
      mapa_data <- poligonos_cs %>%
        group_by(regional) %>%
        summarise(geometry = st_union(geometry)) %>%
        left_join(taxa_regional, by = "regional")

      pal <- colorNumeric("YlOrRd", domain = mapa_data$taxa, na.color = "#cccccc")

      leaflet(mapa_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor   = ~pal(taxa),
          fillOpacity = 0.7,
          color       = "white",
          weight      = 2,
          popup       = ~paste0(
            "<b>", regional, "</b><br>",
            "Internações ICSAP: ", n_icsap, "<br>",
            "Taxa ICSAP: ", taxa, "%"
          )
        ) %>%
        addLegend(
          pal      = pal,
          values   = ~taxa,
          title    = "Taxa ICSAP (%)",
          position = "bottomright"
        )

    } else {

      # Agrega por Centro de Saúde
      taxa_cs <- dados_filtrados() %>%
        filter(!is.na(nome_cs)) %>%
        group_by(nome_cs) %>%
        summarise(n_icsap = n(), .groups = "drop") %>%
        mutate(taxa = round(n_icsap / total_periodo() * 100, 1))

      mapa_data <- poligonos_cs %>%
        left_join(taxa_cs, by = c("nome_cs"))

      pal <- colorNumeric("YlOrRd", domain = mapa_data$taxa, na.color = "#cccccc")

      leaflet(mapa_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor   = ~pal(taxa),
          fillOpacity = 0.7,
          color       = "white",
          weight      = 1,
          popup       = ~paste0(
            "<b>", nome_cs, "</b><br>",
            "Regional: ", regional, "<br>",
            "Internações ICSAP: ", ifelse(is.na(n_icsap), 0, n_icsap), "<br>",
            "Taxa ICSAP: ", ifelse(is.na(taxa), "—", paste0(taxa, "%"))
          )
        ) %>%
        addLegend(
          pal      = pal,
          values   = ~taxa,
          title    = "Taxa ICSAP (%)",
          position = "bottomright"
        )
    }
  })

  # ---------------------------------------------------------------------------
  # Gráfico: por condição
  # ---------------------------------------------------------------------------

  output$grafico_condicao <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(descricao)) %>%
      count(descricao, sort = TRUE) %>%
      slice_head(n = 15)

    plot_ly(df, x = ~n, y = ~reorder(descricao, n),
            type = "bar", orientation = "h",
            marker = list(color = "#8e44ad")) %>%
      layout(title = "Top 15 Condições ICSAP",
             xaxis = list(title = "Internações"),
             yaxis = list(title = ""))
  })

  # ---------------------------------------------------------------------------
  # Ranking de Centros de Saúde
  # ---------------------------------------------------------------------------

  output$grafico_ranking_cs <- renderPlotly({
    df <- dados_filtrados() %>%
      filter(!is.na(nome_cs)) %>%
      group_by(nome_cs, regional) %>%
      summarise(n_icsap = n(), .groups = "drop") %>%
      mutate(taxa = round(n_icsap / total_periodo() * 100, 2)) %>%
      arrange(desc(taxa)) %>%
      slice_head(n = 20)

    plot_ly(df,
            x    = ~taxa,
            y    = ~reorder(nome_cs, taxa),
            type = "bar",
            orientation = "h",
            color = ~regional,
            text  = ~paste0(regional, "<br>", n_icsap, " internações"),
            hoverinfo = "text+x") %>%
      layout(
        title  = "Top 20 Centros de Saúde por Taxa ICSAP (%)",
        xaxis  = list(title = "Taxa ICSAP (%)"),
        yaxis  = list(title = ""),
        legend = list(title = list(text = "Regional"))
      )
  })

  # ---------------------------------------------------------------------------
  # Tabela de dados
  # ---------------------------------------------------------------------------

  output$tabela_dados <- renderDataTable({
    dados_filtrados() %>%
      select(
        data_internacao, regional, nome_cs,
        descricao, idade, sexo, dias_perm, val_tot
      ) %>%
      rename(
        Data           = data_internacao,
        Regional       = regional,
        `Centro Saúde` = nome_cs,
        Condição       = descricao,
        Idade          = idade,
        Sexo           = sexo,
        `Dias Internado` = dias_perm,
        `Valor (R$)`     = val_tot
      )
  })

  # ---------------------------------------------------------------------------
  # Download
  # ---------------------------------------------------------------------------

  output$baixar_dados <- downloadHandler(
    filename = function() paste0("icsap_bh_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(dados_filtrados(), file)
  )
}
