# UI Module for map
stationsmapUI <- function(id) {
  ns <- NS(id)
  card(
    card_header("Point Shapefile Map"),
    leafletOutput(ns("map"), height = 500),
    card_footer("Point data will appear on map after uploading a valid zipped shapefile.")
  )
}

# Server Module for map
stationsmapServer <- function(id, sf_data) {
  moduleServer(id, function(input, output, session) {
    # Render the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0, lat = 0, zoom = 2)
    })
    
    # Update map when shapefile is loaded
    observe({
      data <- sf_data()
      if (is.null(data)) return()
      
      # Convert to 4326 for Leaflet if needed
      if (sf::st_crs(data)$epsg != 4326) {
        data_4326 <- sf::st_transform(data, 4326)
      } else {
        data_4326 <- data
      }
      
      # Create popup content from attributes
      popup_content <- apply(sf::st_drop_geometry(data_4326), 1, function(row) {
        paste0("<strong>", names(row), ":</strong> ", row, "<br>")
      })
      
      leafletProxy("map", session = session, data = data_4326) %>%
        clearMarkers() %>%
        clearShapes() %>%
        addCircleMarkers(
          popup = popup_content,
          radius = 5,
          color = "#1E88E5",
          stroke = FALSE,
          fillOpacity = 0.7
        ) %>%
        fitBounds(
          lng1 = min(sf::st_coordinates(data_4326)[,1]),
          lat1 = min(sf::st_coordinates(data_4326)[,2]),
          lng2 = max(sf::st_coordinates(data_4326)[,1]),
          lat2 = max(sf::st_coordinates(data_4326)[,2])
        )
    })
  })
}
