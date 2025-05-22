# UI Module for table
stationstableUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Shapefile Attributes"),
    tableOutput(ns("attributes")),
    card_footer("Attribute table will display after loading shapefile.")
  )
}

# Server Module for table
stationstableServer <- function(id, sf_data) {
  moduleServer(id, function(input, output, session) {
    # Display attribute table
    output$attributes <- renderTable({
      data <- sf_data()
      if (is.null(data)) return(NULL)
      
      # Return only attribute data, not geometry
      head(sf::st_drop_geometry(data), 10)
    })
  })
}