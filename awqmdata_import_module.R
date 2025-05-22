library(shiny)
library(readr)
library(dplyr)
library(lubridate)
library(janitor)

# UI Module for data import
awqmdataImportUI <- function(id) {
  ns <- NS(id)
  
  fileInput(ns("csv_file"), "Upload Stations CSV File",
            accept = c(".csv"))
}

# Server Module for data import
awqmdataImportServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive function to read the CSV file
    data_reactive <- reactive({
      req(input$csv_file)
      
      # Show a notification while reading the file
      withProgress(
        message = "Reading file...",
        value = 0.5,
        {
          df <- read_csv(input$csv_file$datapath)
          
          # Check if required columns exist
          if(!("Monitoring_Location_ID" %in% names(df)) || 
             !("Activity_Start_Date" %in% names(df))) {
            showModal(modalDialog(
              title = "Error",
              "The CSV file must contain both 'Monitoring_Location_ID' and 'Activity_Start_Date' columns.",
              easyClose = TRUE
            ))
            return(NULL)
          }
          
          # Try to convert the date column to proper date format
          tryCatch({
            df$Activity_Start_Date <- as_date(df$Activity_Start_Date)
          }, error = function(e) {
            showModal(modalDialog(
              title = "Date Format Error",
              "Could not parse 'Activity_Start_Date' column as date. Please check the format.",
              easyClose = TRUE
            ))
            return(NULL)
          })
          
          # Remove duplicate combinations of Monitoring_Location_ID and Activity_Start_Date
          df <- df %>% 
            janitor::get_dupes(Monitoring_Location_ID, Activity_Start_Date) %>%
            select(-dupe_count) %>%
            bind_rows(
              anti_join(df, 
                        janitor::get_dupes(df, Monitoring_Location_ID, Activity_Start_Date) %>% select(-dupe_count),
                        by = names(df))
            ) %>%
            distinct(Monitoring_Location_ID, Activity_Start_Date, .keep_all = TRUE)
          
          return(df)
        }
      )
    })
    
    return(data_reactive)
  })
}