library(shiny)
library(sf)
library(bslib)

# UI Module for file upload and filtering
streammapfileUploadUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("zipfile"), "Streams Zipped Shapefile (.zip)",
              accept = ".zip"),
    conditionalPanel(
      condition = paste0("output['", ns("fileUploaded"), "']"),
      selectInput(ns("milesCol"), "Select Miles Column", choices = NULL),
      numericInput(ns("minMiles"), "Minimum Miles", value = 0),
      actionButton(ns("filterBtn"), "Apply Filter", class = "btn-primary"),
      hr(),
      # Add random selection controls
      numericInput(ns("sampleSize"), "Number of Random Streams", value = 5, min = 1),
      actionButton(ns("randomBtn"), "Select Random Streams", class = "btn-primary")
    ),
    hr(),
    conditionalPanel(
      condition = paste0("output['", ns("fileUploaded"), "']"),
      #downloadButton(ns("downloadRandom"), "Download Random Selection")
    )
  )
}

# Server Module for file upload and data processing
streammapfileUploadServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Function to safely convert values to numeric
    safe_as_numeric <- function(x) {
      result <- suppressWarnings(as.numeric(as.character(x)))
      if(all(is.na(result))) {
        # If all values converted to NA, return original values
        return(x)
      } else {
        return(result)
      }
    }
    
    # Process uploaded zip file
    observeEvent(input$zipfile, {
      req(input$zipfile)
      
      # Create a temporary directory to extract files
      temp_dir <- tempdir()
      values$temp_dir <- temp_dir
      
      # Try to unzip the file
      tryCatch({
        # Unzip the shapefile
        unzip(input$zipfile$datapath, exdir = temp_dir)
        
        # Find the .shp file
        shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
        
        if(is.na(shp_file)) {
          showNotification("No shapefile found in the zip archive", type = "error")
          return(NULL)
        }
        
        # Read the shapefile
        shapefile <- st_read(shp_file, quiet = TRUE)
        
        # Ensure CRS is suitable for leaflet (transform to WGS84 if necessary)
        if (!is.na(st_crs(shapefile)) && !is.null(st_crs(shapefile)$epsg) && st_crs(shapefile)$epsg != 4326) {
          shapefile <- st_transform(shapefile, 4326)
        }
        
        values$shapefile <- shapefile
        values$filtered_shapefile <- shapefile
        values$display_data <- "filtered"  # Default to showing filtered data
        
        # Update the miles column dropdown
        miles_cols <- grep("mile|dist", names(shapefile), ignore.case = TRUE, value = TRUE)
        if(length(miles_cols) > 0) {
          default_col <- miles_cols[1]
        } else {
          default_col <- names(shapefile)[1]
        }
        
        updateSelectInput(session, "milesCol", 
                          choices = names(shapefile),
                          selected = default_col)
        
      }, error = function(e) {
        showNotification(paste("Error loading shapefile:", e$message), type = "error")
      })
    })
    
    # Filter data when the filter button is clicked
    observeEvent(input$filterBtn, {
      req(values$shapefile, input$milesCol)
      
      tryCatch({
        # Convert the column to numeric if it's not already
        miles_values <- safe_as_numeric(values$shapefile[[input$milesCol]])
        
        # Check if conversion worked
        if(all(is.na(miles_values))) {
          showNotification("Selected column cannot be converted to numeric values for filtering", type = "warning")
          values$filtered_shapefile <- values$shapefile
          values$display_data <- "filtered"
          return()
        }
        
        # Create a logical vector for filtering
        filter_condition <- miles_values >= input$minMiles
        
        # Apply the filter
        values$filtered_shapefile <- values$shapefile[filter_condition, ]
        values$display_data <- "filtered"
        
        # Notify user
        showNotification(paste0("Filtered to ", nrow(values$filtered_shapefile), " features"), type = "message")
      }, error = function(e) {
        showNotification(paste("Error filtering data:", e$message), type = "error")
      })
    })
    
    # Select random streams when the random button is clicked
    observeEvent(input$randomBtn, {
      req(values$filtered_shapefile)
      
      tryCatch({
        # Get the number of features to select
        n_select <- min(input$sampleSize, nrow(values$filtered_shapefile))
        
        if(n_select <= 0) {
          showNotification("Invalid number of streams to select", type = "error")
          return()
        }
        
        if(nrow(values$filtered_shapefile) == 0) {
          showNotification("No features available to select from", type = "warning")
          return()
        }
        
        # Randomly select n features
        set.seed(Sys.time()) # Use current time as seed for true randomness
        random_indices <- sample(1:nrow(values$filtered_shapefile), n_select)
        values$random_shapefile <- values$filtered_shapefile[random_indices, ]
        values$display_data <- "random"
        
        # Notify user
        showNotification(paste0("Randomly selected ", n_select, " streams"), type = "message")
      }, error = function(e) {
        showNotification(paste("Error selecting random streams:", e$message), type = "error")
      })
    })
    
    
    
    # Download handler for the random shapefile
    output$downloadRandom <- downloadHandler(
      filename = function() {
        paste0("random_streams_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        req(values$random_shapefile)
        
        # Create a temporary directory for the new shapefile
        out_dir <- tempfile()
        dir.create(out_dir)
        
        # Create a name for the new shapefile
        out_shapefile <- file.path(out_dir, "random_streams.shp")
        
        # Write the random shapefile
        st_write(values$random_shapefile, out_shapefile, append = FALSE)
        
        # Get all the files that were created (shp, dbf, shx, prj, etc.)
        all_files <- list.files(out_dir, full.names = TRUE)
        
        # Create a zip file containing all those files
        zip(file, all_files)
      }
    )
    
    # Flag to indicate if a file has been uploaded
    output$fileUploaded <- reactive({
      return(!is.null(values$shapefile))
    })
    outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  })
}