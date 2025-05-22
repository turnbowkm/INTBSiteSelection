library(shiny)
library(bslib)

# UI Module for file upload
atfileModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("csv_file"), "Upload Attains CSV File", accept = c(".csv")),
    #downloadButton(ns("download_data"), "Download Filtered Data"),
    #downloadButton(ns("download_summary"), "Download Summary Data"),
    #numericInput(ns("num_rows"), "Number of rows to display", value = 10, min = 5, max = 100)
    
  )
}

# Server Module for file upload
atfileModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store all data and outputs
    values <- reactiveValues(
      raw_data = NULL,
      filtered_data = NULL,
      summary_data = NULL
    )
    
    # Update data when file is uploaded
    observeEvent(input$csv_file, {
      req(input$csv_file)
      
      # Try to read the CSV file
      tryCatch({
        df <- read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
        values$raw_data <- df
        
        # Create filtered data
        if("PARAM_STATUS_NAME" %in% colnames(df)) {
          values$filtered_data <- df %>% 
            dplyr::filter(PARAM_STATUS_NAME != "Meeting Criteria")
        } else {
          showNotification("Column 'PARAM_STATUS_NAME' not found in the data", type = "warning")
          values$filtered_data <- df
        }
        
        # Create summary data
        if(all(c("ASSESSMENT_UNIT_ID", "PARAM_NAME") %in% colnames(values$filtered_data))) {
          summary <- values$filtered_data %>%
            dplyr::group_by(ASSESSMENT_UNIT_ID) %>%
            dplyr::summarize(Counto_303d = length(unique(PARAM_NAME)))
          
          # Rename columns to match shapefile column names
          colnames(summary) <- c('AUID', 'PARAM_NAME')
          
          values$summary_data <- summary
        } else {
          showNotification("Required columns 'ASSESSMENT_UNIT_ID' or 'PARAM_NAME' not found", type = "warning")
          values$summary_data <- data.frame(AUID = character(0), PARAM_NAME = integer(0))
        }
        
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
      })
    })
    
    # Download handler for filtered data
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(values$filtered_data)
        write.csv(values$filtered_data, file, row.names = FALSE)
      }
    )
    
    # Download handler for summary data
    output$download_summary <- downloadHandler(
      filename = function() {
        paste0("summary_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(values$summary_data)
        write.csv(values$summary_data, file, row.names = FALSE)
      }
    )
    
    # Return reactive values to be used by other modules
    return(values)
  })
}