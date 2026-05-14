# =============================================================================
# server.R — Lógica do painel ICSAP-BH
# =============================================================================

# Mensagem padrão para gráficos sem dados
sem_dados_plot <- function(msg = "Sem dados para os filtros selecionados") {
  plot_ly() %>%
    layout(
      title      = msg,
      xaxis      = list(visible = FALSE),
      yaxis      = list(visible = FALSE),
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    )
}

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
  # Total de internações no período (denominador da taxa ICSAP)
  # Usa internacoes_bh.csv (todas as internações), não apenas ICSAP.
  # Fallback para dados ICSAP se internacoes_bh.csv não estiver disponível.
  # ---------------------------------------------------------------------------

  total_periodo <- reactive({
    base <- if (!is.null(total_internacoes_ref)) total_internacoes_ref else dados
    base %>%
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
    n_total <- total_periodo()
    taxa <- if (n_total > 0) round(nrow(dados_filtrados()) / n_total * 100, 1) else 0
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

    if (nrow(df) == 0) return(sem_dados_plot())

    plot_ly(df, x = ~data_internacao, y = ~n,
            type = "scatter", mode = "lines",
            line = list(color = "#2980b9")) %>%
      layout(title  = "Evolução Mensal das Internações ICSAP",
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

    if (nrow(df) == 0) return(sem_dados_plot("Regional não disponível nos dados atuais"))

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
      return(
        leaflet() %>%
          addTiles() %>%
          setView(lng = -43.94, lat = -19.92, zoom = 11) %>%
          addControl(
            html     = "<b>Polígonos não disponíveis</b><br>Verifique a conexão.",
            position = "topright"
          )
      )
    }

    legenda_titulo <- HTML(
      "Taxa ICSAP (%)<br>
       <small style='font-weight:normal'>
         Quanto mais escuro,<br>maior a taxa ICSAP
       </small>"
    )

    if (input$mapa_nivel == "regional") {

      taxa_regional <- dados_filtrados() %>%
        filter(!is.na(regional)) %>%
        group_by(regional) %>%
        summarise(n_icsap = n(), .groups = "drop") %>%
        mutate(taxa = round(n_icsap / max(total_periodo(), 1) * 100, 1))

      mapa_data <- poligonos_cs %>%
        group_by(regional) %>%
        summarise(geometry = st_union(geometry), .groups = "drop") %>%
        left_join(taxa_regional, by = "regional")

      pal <- colorNumeric("YlOrRd", domain = mapa_data$taxa, na.color = "#dddddd")

      leaflet(mapa_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor   = ~pal(taxa),
          fillOpacity = 0.75,
          color       = "white",
          weight      = 2,
          highlightOptions = highlightOptions(
            weight      = 3,
            color       = "#333",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>Regional ", regional, "</b><br>",
            "Internações ICSAP: <b>",
            ifelse(is.na(n_icsap), "sem dados", format(n_icsap, big.mark = ".")),
            "</b><br>",
            "Taxa ICSAP: <b>",
            ifelse(is.na(taxa), "—", paste0(taxa, "%")),
            "</b>"
          )
        ) %>%
        addLegend(
          pal      = pal,
          values   = ~taxa,
          title    = legenda_titulo,
          position = "bottomright",
          na.label = "Sem dados"
        )

    } else {

      taxa_cs <- dados_filtrados() %>%
        filter(!is.na(nome_cs)) %>%
        group_by(nome_cs) %>%
        summarise(n_icsap = n(), .groups = "drop") %>%
        mutate(taxa = round(n_icsap / max(total_periodo(), 1) * 100, 1))

      mapa_data <- poligonos_cs %>%
        left_join(taxa_cs, by = "nome_cs")

      pal <- colorNumeric("YlOrRd", domain = mapa_data$taxa, na.color = "#dddddd")

      leaflet(mapa_data) %>%
        addTiles() %>%
        addPolygons(
          fillColor   = ~pal(taxa),
          fillOpacity = 0.75,
          color       = "white",
          weight      = 1,
          highlightOptions = highlightOptions(
            weight      = 2,
            color       = "#333",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          popup = ~paste0(
            "<b>", nome_cs, "</b><br>",
            "Regional: ", regional, "<br>",
            "Internações ICSAP: <b>",
            ifelse(is.na(n_icsap), "0", format(n_icsap, big.mark = ".")),
            "</b><br>",
            "Taxa ICSAP: <b>",
            ifelse(is.na(taxa), "—", paste0(taxa, "%")),
            "</b>"
          )
        ) %>%
        addLegend(
          pal      = pal,
          values   = ~taxa,
          title    = legenda_titulo,
          position = "bottomright",
          na.label = "Sem dados"
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

    if (nrow(df) == 0) return(sem_dados_plot())

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
      mutate(taxa = round(n_icsap / max(total_periodo(), 1) * 100, 2)) %>%
      arrange(desc(taxa)) %>%
      slice_head(n = 20)

    if (nrow(df) == 0) return(sem_dados_plot("CS não disponível — dados sem geocodificação de regional"))

    plot_ly(df,
            x    = ~taxa,
            y    = ~reorder(nome_cs, taxa),
            type = "bar",
            orientation = "h",
            color = ~regional,
            text  = ~paste0(regional, "<br>", format(n_icsap, big.mark = "."), " internações"),
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
        Data             = data_internacao,
        Regional         = regional,
        `Centro de Saúde` = nome_cs,
        `Condição ICSAP` = descricao,
        Idade            = idade,
        Sexo             = sexo,
        `Dias Internado` = dias_perm,
        `Valor (R$)`     = val_tot
      )
  }, options = list(pageLength = 15, language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json")))

  # ---------------------------------------------------------------------------
  # Download
  # ---------------------------------------------------------------------------

  output$baixar_dados <- downloadHandler(
    filename = function() paste0("icsap_bh_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(dados_filtrados(), file)
  )
}
