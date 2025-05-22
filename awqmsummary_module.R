library(shiny)
library(dplyr)
library(DT)
library(bslib)

# UI Module for date summary
awqmdateSummaryUI <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Date Range Summary"),
    DT::dataTableOutput(ns("date_summary_table"))
  )
}

# Server Module for date summary
awqmdateSummaryServer <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Generate summary table with date ranges by Monitoring_Location_ID
    output$date_summary_table <- DT::renderDataTable({
      req(data_reactive())
      
      df <- data_reactive()
      
      # Compute the date ranges for each Monitoring_Location_ID
      date_ranges <- df %>%
        group_by(Monitoring_Location_ID) %>%
        summarize(
          Earliest_Date = min(Activity_Start_Date, na.rm = TRUE),
          Latest_Date = max(Activity_Start_Date, na.rm = TRUE),
          Number_of_Dates = n_distinct(Activity_Start_Date)  # Added count of unique dates
        ) %>%
        arrange(Monitoring_Location_ID)
      
      # Return the data table
      DT::datatable(
        date_ranges,
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
  })
}