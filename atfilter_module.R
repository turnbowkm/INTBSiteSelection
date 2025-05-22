library(shiny)
library(bslib)
library(dplyr)
library(DT)

# UI Module for filtered data display
atfilterModuleUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("PARAM_STATUS_NAME != 'Meeting Criteria'"),
    card_body(
      DTOutput(ns("filtered_table"))
    )
  )
}

# Server Module for filtered data
atfilterModuleServer <- function(id, data_values) {
  moduleServer(id, function(input, output, session) {
    # Display the filtered data using DT for interactive table
    output$filtered_table <- renderDT({
      req(data_values$filtered_data)
      
      # Create interactive datatable
      datatable(
        data_values$filtered_data,
        options = list(
          pageLength = data_values$input$num_rows,
          searchHighlight = TRUE,
          scrollX = TRUE,
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        filter = 'top',  # Add filters at the top of each column
        rownames = FALSE,
        class = 'cell-border stripe',
        caption = 'Records excluding "Meeting Criteria" status'
      )
    })
  })
}