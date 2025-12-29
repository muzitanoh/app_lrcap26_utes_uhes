library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(purrr)
library(ggplot2)
library(plotly)
library(DT)
library(geobr)
library(writexl)

br_states <- geobr::read_state(showProgress = FALSE)

# Tema base (no estilo do dashboard de referência)
theme_projetos <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.title.x      = ggplot2::element_text(margin = margin(t = 8)),
      axis.title.y      = ggplot2::element_text(margin = margin(r = 8)),
      legend.title      = ggplot2::element_text(face = "bold"),
      legend.position   = "right"
    )
}

fmt_br <- function(x, digits = 2, suffix = NULL, na_txt = "-") {
  out <- ifelse(
    is.na(x),
    na_txt,
    formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  )
  if (!is.null(suffix)) out <- paste0(out, suffix)
  out
}

produtos_disponiveis <- function(df) {
  produto_cols <- get_produto_cols(df)
  if (length(produto_cols) == 0) return(integer(0))
  
  disponiveis <- produto_cols[
    vapply(produto_cols, function(col) {
      any(toupper(trimws(as.character(df[[col]]))) == "X", na.rm = TRUE)
    }, logical(1))
  ]
  
  sort(as.integer(disponiveis))
}