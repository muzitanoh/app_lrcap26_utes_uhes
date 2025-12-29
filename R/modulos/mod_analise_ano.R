# mod_analise_ano.R ------------------------------------------------------------
# Análises detalhadas por ano (produto e margem considerada)

mod_analise_ano_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "analise_ano",

    fluidRow(
      column(
        width = 12,
        h2("Análises por Ano", style = "color: #486018;"),
        br()
      )
    ),

    fluidRow(
      box(
        title = "Projetos e margem por ano-início (produto)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "500px",
        plotlyOutput(ns("plot_ano_inicio_dual"), height = "430px")
      ),
      box(
        title = "Margem considerada por ano (UF selecionável)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "500px",
        uiOutput(ns("filtro_uf_ui")),
        plotlyOutput(ns("plot_uf_ano"), height = "380px")
      )
    ),

    fluidRow(
      box(
        title = "Tabela (margem considerada por UF e ano)",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        DTOutput(ns("tabela_uf_ano"))
      )
    )
  )
}

mod_analise_ano_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    opcoes_uf <- reactive({
      sort(unique(dados()$uf_do_ponto_de_conexao))
    })

    output$filtro_uf_ui <- renderUI({
      selectInput(
        ns("uf_sel"),
        label = "UF:",
        choices = c("Todas" = "", opcoes_uf()),
        selected = ""
      )
    })

    output$plot_ano_inicio_dual <- renderPlotly({
      df <- dados()
      req(nrow(df) > 0)

      base <- agg_por_ano_inicio(df)

      # gráfico de barras (margem) + labels no hover com projetos
      g <- ggplot(base, aes(
        x = factor(ano_inicio),
        y = margem_total_mw,
        text = paste0(
          "Ano-início: ", ano_inicio, "<br>",
          "Projetos: ", n_projetos, "<br>",
          "Margem (MW): ", round(margem_total_mw, 1)
        )
      )) +
        geom_col(fill = "#486018") +
        theme_projetos() +
        labs(x = "Ano (produto)", y = "Margem total (MW)")

      ggplotly(g, tooltip = "text") |>
        config(displayModeBar = FALSE)
    })

    output$plot_uf_ano <- renderPlotly({
      df <- dados()
      req(nrow(df) > 0)

      base <- agg_por_uf_ano(df, ano_fim = 2031)

      if (!is.null(input$uf_sel) && input$uf_sel != "") {
        base <- base %>% filter(uf_do_ponto_de_conexao == input$uf_sel)
      }

      g <- ggplot(base, aes(
        x = ano,
        y = margem_total_mw,
        color = uf_do_ponto_de_conexao,
        group = uf_do_ponto_de_conexao,
        text = paste0(
          "UF: ", uf_do_ponto_de_conexao, "<br>",
          "Ano: ", ano, "<br>",
          "Margem (MW): ", round(margem_total_mw, 1)
        )
      )) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        theme_projetos() +
        labs(x = "Ano", y = "Margem total (MW)", color = "UF")

      ggplotly(g, tooltip = "text") |>
        config(displayModeBar = FALSE)
    })

    output$tabela_uf_ano <- renderDT({
      df <- dados()
      base <- agg_por_uf_ano(df, ano_fim = 2031)

      datatable(
        base,
        rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE),
        class = "cell-border stripe"
      )
    })
  })
}
