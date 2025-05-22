library(shiny)
library(sf)
library(leaflet)
library(bslib)

# UI Module for map
stream_mapUI <- function(id) {
  ns <- NS(id)
  
  card(
    card_header("Interactive Map"),
    card_body(
      leafletOutput(ns("map"), height = "500px")
    )
  )
}

# Server Module for map
stream_mapServer <- function(id, values) {
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
    
    # Function to get the current display data
    get_display_data <- reactive({
      if(values$display_data == "random" && !is.null(values$random_shapefile)) {
        return(values$random_shapefile)
      } else {
        return(values$filtered_shapefile)
      }
    })
    
    # Render the leaflet map
    output$map <- renderLeaflet({
      req(get_display_data())
      
      # Check if shapefile has any features
      if (nrow(get_display_data()) == 0) {
        return(leaflet() %>% 
                 addProviderTiles(providers$CartoDB.Positron) %>%
                 addControl("No features to display after filtering", position = "topright"))
      }
      
      # Create the map
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      # Create a palette based on the miles column if available
      miles_col <- input$milesCol
      
      if (!is.null(miles_col) && miles_col %in% names(get_display_data())) {
        # Try to use the miles column for coloring
        tryCatch({
          miles_values <- safe_as_numeric(get_display_data()[[miles_col]])
          
          # Only create a color palette if we have numeric values
          if(!all(is.na(miles_values))) {
            pal <- colorNumeric(
              palette = "viridis",
              domain = miles_values,
              na.color = "grey"
            )
            
            # Add polylines with color based on miles
            map <- map %>% 
              addPolylines(
                data = get_display_data(),
                color = ~pal(safe_as_numeric(get_display_data()[[miles_col]])),
                weight = 3,
                opacity = 1,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  opacity = 1,
                  bringToFront = TRUE
                ),
                label = ~if("NAME" %in% names(get_display_data())) as.character(NAME) else row.names(get_display_data()),
                layerId = ~row.names(get_display_data())
              ) %>%
              addLegend(
                position = "bottomright",
                pal = pal,
                values = miles_values,
                title = miles_col,
                opacity = 0.7
              )
          } else {
            # No numeric values, use default coloring
            map <- map %>% 
              addPolylines(
                data = get_display_data(),
                color = "steelblue",
                weight = 3,
                opacity = 1,
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  opacity = 1,
                  bringToFront = TRUE
                ),
                label = ~if("NAME" %in% names(get_display_data())) as.character(NAME) else row.names(get_display_data()),
                layerId = ~row.names(get_display_data())
              )
          }
        }, error = function(e) {
          # If coloring fails, use default coloring
          map <- map %>% 
            addPolylines(
              data = get_display_data(),
              color = "steelblue",
              weight = 3,
              opacity = 1,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                opacity = 1,
                bringToFront = TRUE
              ),
              label = ~if("NAME" %in% names(get_display_data())) as.character(NAME) else row.names(get_display_data()),
              layerId = ~row.names(get_display_data())
            )
        })
      } else {
        # No miles column, use default coloring
        map <- map %>% 
          addPolylines(
            data = get_display_data(),
            color = "steelblue",
            weight = 3,
            opacity = 1,
            highlight = highlightOptions(
              weight = 5,
              color = "#666",
              opacity = 1,
              bringToFront = TRUE
            ),
            label = ~if("NAME" %in% names(get_display_data())) as.character(NAME) else row.names(get_display_data()),
            layerId = ~row.names(get_display_data())
          )
      }
      
      # Automatically fit bounds to the data
      map %>% fitBounds(
        lng1 = min(st_bbox(get_display_data())[1], na.rm = TRUE),
        lat1 = min(st_bbox(get_display_data())[2], na.rm = TRUE),
        lng2 = max(st_bbox(get_display_data())[3], na.rm = TRUE),
        lat2 = max(st_bbox(get_display_data())[4], na.rm = TRUE)
      )
    })
  })
}
