library(shiny)
library(DT)
library(sf)
library(bslib)

# UI module for the table display
streamshptableUI <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Attribute Table"),
    card_body(
      DTOutput(ns("attributeTable"))
    )
  )
}

# Server module for the table display
streamshptableServer <- function(id, values) {
  moduleServer(id, function(input, output, session) {
    
    # Get the current display data
    get_display_data <- reactive({
      if(values$display_data == "random" && !is.null(values$random_shapefile)) {
        return(values$random_shapefile)
      } else {
        return(values$filtered_shapefile)
      }
    })
    
    # Render the attribute table
    output$attributeTable <- renderDT({
      req(get_display_data())
      
      # Check if shapefile has any features
      if (nrow(get_display_data()) == 0) {
        return(NULL)
      }
      
      # Remove geometry column for display and convert to data frame
      attribute_data <- st_drop_geometry(get_display_data())
      
      # Create the data table with options
      datatable(
        attribute_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 'Blfrtip',
          buttons = c('copy', 'csv', 'excel'),
          lengthMenu = list(c(10, 25, 50, 100, -1), 
                            c('10', '25', '50', '100', 'All'))
        ),
        extensions = c('Buttons', 'Responsive'),
        filter = 'top',
        rownames = FALSE,
        class = 'cell-border stripe'
      )
    })
  })
}