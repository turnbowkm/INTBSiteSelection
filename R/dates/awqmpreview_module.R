library(shiny)
library(DT)
library(bslib)

# UI Module for data preview
awqmdataPreviewUI <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Preview of Uploaded Data"),
    DT::dataTableOutput(ns("preview_table"))
  )
}

# Server Module for data preview
awqmdataPreviewServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Generate a preview of the uploaded data
    output$preview_table <- DT::renderDataTable({
      req(data_reactive())
      
      # Show the first 10 rows of the data
      DT::datatable(
        head(data_reactive(), 10),
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}