# mod_tabela_completa.R ---------------------------------------------------------
# Tabela completa com download

mod_tabela_completa_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = "tabela_completa",

    fluidRow(
      column(
        width = 12,
        h2("Tabela Completa", style = "color: #486018;"),
        br()
      )
    ),

    fluidRow(
      box(
        title = "Base completa",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        div(style = "overflow-x: auto;", DTOutput(ns("tabela"))),
        br(),
        downloadButton(ns("download_xlsx"), "Download Excel", class = "btn-download")
      )
    )
  )
}

mod_tabela_completa_server <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$tabela <- renderDT({
      df <- dados() %>% 
        select("nom_leilao", "processo", "te", "municipio", "uf_do_ponto_de_conexao", "nome_empreendimento", "empreendedor_razao_social",
               "ponto_de_conexao", "tensao_do_ponto_de_conexao_k_v", "proprietaria", "potencia_instalada_k_w", "ampliacao_k_w", "potencia_final_instalada_k_w",
               "potencia_injetavel_max_k_w", "ceg", "submercado", "classificacao", "empreendimento", "distancia_ate_o_seccionamento_km", "se_mais_proxima", 
               "extensao_da_lt_do_ponto_de_seccionamento_a_se_mais_proxima", "extensao_da_lte_km", "produtos",
               "margem_solicitada_mw", "desconsiderar_projeto", "comentario"                                                       
        )

      datatable(
        df,
        rownames = FALSE,
        filter = "top",
        extensions = c("Buttons"),
        options = list(
          scrollX = TRUE,
          pageLength = 25,
          dom = "Bfrtip",
          buttons = c("copy", "csv")
        ),
        class = "cell-border stripe"
      )
    })

    output$download_xlsx <- downloadHandler(
      filename = function() {
        paste0("analise_projetos_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(dados(), file)
      }
    )
  })
}
