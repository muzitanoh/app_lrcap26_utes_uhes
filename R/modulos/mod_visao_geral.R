# mod_visao_geral.R ------------------------------------------------------------
# Visão Geral: KPIs + Mapa Brasil por UF + gráficos agregados
# Observação: recebe o dataframe já filtrado (filtros globais do app).

mod_visao_geral_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "visao_geral",

    fluidRow(
      column(
        width = 12,
        h2("Análise de Projetos", style = "color: #486018;"),
        h4("LRCAP 2026 — UTE/UHE (visão consolidada)", style = "color: #486018; font-weight: normal;"),
        br()
      )
    ),

    fluidRow(
      valueBoxOutput(ns("box_projetos"), width = 4),
      valueBoxOutput(ns("box_margem"),   width = 4),
      valueBoxOutput(ns("box_pot"),      width = 4)
      # valueBoxOutput(ns("box_produtos"), width = 3)
    ),

    fluidRow(
      box(
        title = "Brasil por UF (Nº de Projetos e Margem Solicitada)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "600px",
        uiOutput(ns("metrica_ui")),
        plotlyOutput(ns("mapa_brasil"), height = "480px")
      ),
      box(
        title = "Resumo por UF",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "600px",
        DTOutput(ns("tabela_resumo_uf"), height = "600px")
      )
    ),

    fluidRow(
      box(
        title = "Potência instalada considerada por ano (Valor acumulativo)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "500px",
        plotlyOutput(ns("plot_potencia_ano"), height = "420px")
      ),
      box(
        title = "Margem solicitada por ano (Valor acumulativo)",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        height = "500px",
        plotlyOutput(ns("plot_margem_ano"), height = "420px")
      )
    )
  )
}

mod_visao_geral_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$metrica_ui <- renderUI({
      radioButtons(
        ns("metrica_mapa"),
        label = NULL,
        choices = c("Nº projetos" = "n", "Margem Solicitada (MW)" = "m"),
        selected = "n",
        inline = TRUE
      )
    })

    # ---------------- KPIs ----------------
    output$box_projetos <- renderValueBox({
      df <- dados()
      valueBox(
        value = fmt_br(n_distinct(df$id_projeto)),
        subtitle = "Nº de Projetos (distintos)",
        icon = icon("folder-open"),
        color = "green"
      )
    })

    output$box_margem <- renderValueBox({
      df <- dados()
      valueBox(
        value = fmt_br(sum(df$margem_solicitada_mw, na.rm = TRUE)),
        subtitle = "Margem total solicitada (MW)",
        icon = icon("bolt"),
        color = "aqua"
      )
    })

    output$box_pot <- renderValueBox({
      df <- dados()
      valueBox(
        value = fmt_br(sum(df$potencia_final_instalada_k_w, na.rm = TRUE)/10^3),
        subtitle = "Potência final instalada (MW)",
        icon = icon("map"),
        color = "purple"
      )
    })

    # output$box_produtos <- renderValueBox({
    #   df <- dados()
    #   valueBox(
    #     value = n_distinct(df$ano_inicio),
    #     subtitle = "Anos-início (produtos)",
    #     icon = icon("calendar"),
    #     color = "yellow"
    #   )
    # })

    # ---------------- Mapa ----------------
    output$mapa_brasil <- renderPlotly({
      df <- dados()
      req(nrow(df) > 0)
      
      base_uf <- agg_por_uf(df)
      
      sf_join <- br_states %>%
        left_join(base_uf, by = c("abbrev_state" = "uf_do_ponto_de_conexao")) %>%
        mutate(
          n_projetos        = if_else(is.na(n_projetos), 0L, n_projetos),
          margem_total_mw   = if_else(is.na(margem_total_mw), 0,  margem_total_mw),
          pot_inst_total_mw = if_else(is.na(pot_inst_total_mw), 0,  pot_inst_total_mw),
          fill_var = if (input$metrica_mapa == "m") margem_total_mw else as.numeric(n_projetos),
          tooltip = paste0(
            "UF: ", abbrev_state, "<br>",
            "Nº de Projetos: ", fmt_br(n_projetos, digits = 0), "<br>",
            "Margem solicitada (MW): ", fmt_br(margem_total_mw, digits = 2), "<br>",
            "Potência instalada final (MW): ", fmt_br(pot_inst_total_mw, digits = 2)
          )
        )
      
      max_v <- max(sf_join$fill_var, na.rm = TRUE)
      brks  <- scales::breaks_pretty(n = 5)(c(0, max_v))   # 4–6 ticks costuma ficar bom
      brks  <- brks[brks >= 0 & brks <= max_v]
      
      labels_fun <- if (input$metrica_mapa == "m") {
        function(x) fmt_br(x, digits = 0)
      } else {
        function(x) fmt_br(x, digits = 0)
      }
      
      p <- ggplot(sf_join) +
        geom_sf(aes(fill = fill_var, text = tooltip), color = "white", size = 0.2) +
        scale_fill_gradient(
          low   = "#e2e3e1",
          high  = "#3e6b00",
          name  = ifelse(input$metrica_mapa == "m", "Margem Solicitada (MW)", "Nº projetos"),
          limits = c(0, max_v),
          trans  = "log1p",
          breaks = brks,
          labels = labels_fun
        ) +
        guides(fill = guide_colorbar(
          barheight = unit(120, "pt"),
          barwidth  = unit(12, "pt")
        )) +
        theme_minimal() +
        theme(
          axis.title = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = "right"
        )
      
      ggplotly(p, tooltip = "text") |>
        config(displayModeBar = FALSE)
    })
    
    # ---------------- Tabela resumo por UF ----------------
    output$tabela_resumo_uf <- renderDT({
      df <- dados()
      base_uf <- agg_por_uf(df) %>%
        arrange(desc(margem_total_mw), desc(n_projetos))
      
      datatable(
        base_uf,
        rownames = FALSE,
        colnames = c("UF", "Nº de Projetos", "Margem Solicitada (MW)", "Potência instalada final (MW)"),
        options = list(pageLength = 12, dom = "tip", scrollX = TRUE),
        class = "cell-border stripe"
      ) %>%
        formatRound(
          columns = c("margem_total_mw", "pot_inst_total_mw"),
          digits  = 2,
          mark    = ".",   # milhar
          dec.mark = ","   # decimal
        )
    })

    # ------------- Barras (pot inst por ano) -------------
    output$plot_potencia_ano <- renderPlotly({
      df <- dados()
      req(nrow(df) > 0)

      base <- agg_por_ano(df)

      g <- ggplot(base, aes(x = factor(ano), y = pot_inst_total_mw,
                            text = paste0(
                              "Ano: ", ano, "<br>",
                              "Nº de Projetos: ", n_projetos, "<br>",
                              "Potência instalada final (MW): ", fmt_br(pot_inst_total_mw)
                            ))) +
        geom_col(fill = "#486018") +
        theme_projetos() +
        labs(x = "Ano", y = "Potência instalada final (MW)") +
        scale_y_continuous(labels = function(x) fmt_br(x, digits = 0))

      ggplotly(g, tooltip = "text") |>
        config(displayModeBar = FALSE)
    })

    # ---------------- Barras (margem por ano) ----------------
    output$plot_margem_ano <- renderPlotly({
      df <- dados()
      req(nrow(df) > 0)

      base <- agg_por_ano(df, ano_fim = 2031)

      g <- ggplot(base, aes(x = factor(ano), y = margem_total_mw,
                            text = paste0(
                              "Ano: ", ano, "<br>",
                              "Nº de Projetos: ", n_projetos, "<br>",
                              "Margem solicitada (MW): ", fmt_br(margem_total_mw)
                            ))) +
        geom_col(fill = "#486018") +
        theme_projetos() +
        labs(x = "Ano", y = "Margem total solicitada (MW)") +
        scale_y_continuous(labels = function(x) fmt_br(x, digits = 0))

      ggplotly(g, tooltip = "text") |>
        config(displayModeBar = FALSE)
    })

  })
}
