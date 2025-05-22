# UI Module for file upload
stationsuploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("zipfile"), "Upload Zipped Shapefile", accept = ".zip"),
    hr()
  )
}

# Server Module for file upload
stationsuploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store shapefile data
    sf_data <- reactiveVal(NULL)
    
    # Function to handle the file upload and processing automatically
    observeEvent(input$zipfile, {
      req(input$zipfile)
      
      # Create a temporary directory to extract the zip file
      temp_dir <- tempdir()
      
      # Unzip the uploaded file
      zip_file <- input$zipfile$datapath
      utils::unzip(zip_file, exdir = temp_dir)
      
      # Find the .shp file in the extracted files
      shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
      
      if (length(shp_file) == 0) {
        showNotification("No shapefile (.shp) found in the zip file", type = "error")
        return(NULL)
      }
      
      # Try to read the shapefile
      tryCatch({
        # Read shapefile without transforming CRS
        data <- sf::st_read(shp_file[1])
        
        # Check if it's a point dataset
        if (!all(sf::st_geometry_type(data) %in% c("POINT", "MULTIPOINT"))) {
          showNotification("The shapefile does not contain point data", type = "warning")
        }
        
        # Store data in reactive value
        sf_data(data)
        showNotification("Shapefile loaded successfully", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading shapefile:", e$message), type = "error")
        sf_data(NULL)
      })
    })
    
    return(sf_data)
  })
}