library(shiny)
library(bslib)
library(dplyr)
library(DT)

# UI Module for summary data display
atsummaryModuleUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Count of unique parameters per assessment unit"),
    card_body(
      DTOutput(ns("param_summary_table"))
    )
  )
}

# Server Module for summary data
atsummaryModuleServer <- function(id, data_values) {
  moduleServer(id, function(input, output, session) {
    # Display the parameter summary using DT
    output$param_summary_table <- renderDT({
      req(data_values$summary_data)
      
      datatable(
        data_values$summary_data,
        options = list(
          pageLength = 10,
          searchHighlight = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        rownames = FALSE,
        class = 'cell-border stripe',
        caption = 'Count of unique parameters per assessment unit'
      )
    })
  })
}