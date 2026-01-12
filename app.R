# app.R ------------------------------------------------------------------------
# Dashboard Shiny: Análise de Projetos (LRCAP 2026 — UTE/UHE)

source("R/util.R")
source("R/data_prep.R")

source("R/modulos/mod_visao_geral.R")
source("R/modulos/mod_tabela_completa.R")

options(shiny.maxRequestSize = 1024^3)

# Carrega base (no startup)
base_projetos <- load_projetos("data/dados_tratados_lrcap26_ute_uhe_input_analistas.xlsx")

produto_cols_base <- get_produto_cols(base_projetos)
opcoes_prod_base  <- sort(as.integer(produto_cols_base))

opcoes_uf_base            <- sort(unique(base_projetos$uf_do_ponto_de_conexao))
opcoes_subsistema_base    <- sort(unique(base_projetos$submercado))
opcoes_tipo_rede_base     <- sort(unique(base_projetos$classificacao))
opcoes_tipo_usina_base    <- sort(unique(base_projetos$te))
opcoes_leilao_base        <- sort(unique(base_projetos$nom_leilao))
opcoes_empreendimento_base<- sort(unique(base_projetos$empreendimento))

# Remove vazios/NA das opções (para não poluir choices)
drop_empty <- function(x) {
  x <- x[!is.na(x)]
  x <- trimws(as.character(x))
  x <- x[x != ""]
  unique(x)
}

opcoes_uf_base             <- drop_empty(opcoes_uf_base)
opcoes_subsistema_base     <- drop_empty(opcoes_subsistema_base)
opcoes_tipo_rede_base      <- drop_empty(opcoes_tipo_rede_base)
opcoes_tipo_usina_base     <- drop_empty(opcoes_tipo_usina_base)
opcoes_leilao_base         <- drop_empty(opcoes_leilao_base)
opcoes_empreendimento_base <- drop_empty(opcoes_empreendimento_base)

ui <- shinydashboard::dashboardPage(
  skin = "green",
  header = shinydashboard::dashboardHeader(title = "Análise Projetos"),
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      
      # =========================
      # NAVEGAÇÃO (EM CIMA)
      # =========================
      tags$li(class = "header_", "Navegação"),
      shinydashboard::menuItem("Visão Geral", tabName = "visao_geral", icon = icon("tachometer-alt")),
      # shinydashboard::menuItem("Análises por Ano", tabName = "analise_ano", icon = icon("chart-line")),
      shinydashboard::menuItem("Tabela Completa", tabName = "tabela_completa", icon = icon("table")),
      
      tags$hr(style = "margin: 8px 0;"),
      
      # =========================
      # FILTROS GLOBAIS (EMBAIXO)
      # =========================
      tags$li(class = "header_", "Filtros Globais"),
      
      # Recomendo selectizeInput para listas grandes
      selectizeInput(
        "filtro_leilao_global",
        label = "Leilão:",
        choices = opcoes_leilao_base,
        multiple = TRUE,
        options = list(placeholder = "Todos")
      ),
      
      selectizeInput(
        "filtro_subsistema_global",
        label = "Subsistema:",
        choices = opcoes_subsistema_base,
        multiple = TRUE,
        options = list(placeholder = "Todos")
      ),
      
      selectizeInput(
        "filtro_uf_global",
        label = "UF:",
        choices = opcoes_uf_base,
        multiple = TRUE,
        options = list(placeholder = "Todas")
      ),
      
      selectizeInput(
        "filtro_tipo_rede_global",
        label = "Tipo de rede:",
        choices = opcoes_tipo_rede_base,
        multiple = TRUE,
        options = list(placeholder = "Todos")
      ),
      
      selectizeInput(
        "filtro_tipo_usina_global",
        label = "Tipo de usina:",
        choices = opcoes_tipo_usina_base,
        multiple = TRUE,
        options = list(placeholder = "Todos")
      ),
      
      selectizeInput(
        "filtro_empreendimento_global",
        label = "Novo/Existente:",
        choices = opcoes_empreendimento_base,
        multiple = TRUE,
        options = list(placeholder = "Todos", maxOptions = 5000)
      ),
      
      selectizeInput(
        "filtro_produtos_global",
        label = "Produtos (anos):",
        choices = opcoes_prod_base,
        multiple = TRUE,
        options = list(placeholder = "Todos")
      ),
      
      checkboxInput(
        "excluir_desconsiderados_global",
        "Excluir projetos desconsiderados pelos analistas",
        value = TRUE
      ),
      
      actionButton(
        "limpar_filtros_global",
        "Limpar filtros",
        icon = icon("eraser"),
        style = "width:100%; margin-top:6px;"
      )
    )
  ),
  body = shinydashboard::dashboardBody(
    includeCSS("www/style.css"),
    shinydashboard::tabItems(
      mod_visao_geral_ui("vg"),
      mod_tabela_completa_ui("tb")
    )
  )
)

server <- function(input, output, session) {
  
  dados_base <- reactiveVal(base_projetos)
  
  observeEvent(input$limpar_filtros_global, {
    updateSelectizeInput(session, "filtro_uf_global", selected = character(0))
    updateSelectizeInput(session, "filtro_produtos_global", selected = character(0))
    updateSelectizeInput(session, "filtro_subsistema_global", selected = character(0))
    updateSelectizeInput(session, "filtro_tipo_rede_global", selected = character(0))
    updateSelectizeInput(session, "filtro_tipo_usina_global", selected = character(0))
    updateSelectizeInput(session, "filtro_leilao_global", selected = character(0))
    updateSelectizeInput(session, "filtro_empreendimento_global", selected = character(0))
    updateCheckboxInput(session, "excluir_desconsiderados_global", value = TRUE)
  })
  
  dados_filtrados <- reactive({
    filter_projetos(
      df = dados_base(),
      uf = input$filtro_uf_global,
      produtos = input$filtro_produtos_global,
      subsistema = input$filtro_subsistema_global,
      tipo_rede = input$filtro_tipo_rede_global,
      tipo_usina = input$filtro_tipo_usina_global,
      leilao = input$filtro_leilao_global,
      empreendimento = input$filtro_empreendimento_global,
      excluir_desconsiderados = input$excluir_desconsiderados_global
    )
  })
  
  mod_visao_geral_server("vg", dados = dados_filtrados)
  mod_tabela_completa_server("tb", dados = dados_filtrados)
}

shinyApp(ui, server)
