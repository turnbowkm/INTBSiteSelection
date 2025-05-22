library(shiny)
library(sf)
library(leaflet)
library(bslib)
library(utils)

# Source all modules
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/Stations modules/upload_module.R")
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/Stations modules/map_module.R")
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/Stations modules/table_module.R")

# Main UI
ui <- page_sidebar(
  title = "Shapefile Viewer",
  sidebar = sidebar(
    uploadUI("upload")
  ),
  mapUI("map_section"),
  tableUI("table_section")
)

# Main server
server <- function(input, output, session) {
  # Call the upload server module and get the reactive sf_data
  sf_data <- uploadServer("upload")
  
  # Call the map and table server modules, passing sf_data to them
  mapServer("map_section", sf_data)
  tableServer("table_section", sf_data)
}

shinyApp(ui, server)