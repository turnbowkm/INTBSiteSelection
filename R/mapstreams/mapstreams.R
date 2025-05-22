library(shiny)
library(bslib)

base_path <- "C:/Users/kevin.turnbow/siteselect/SiteSelection"
# Source all modules
# Streams/maps modules
source(file.path(base_path, "mapstreams/streamsmap_upload_module.R"))
source(file.path(base_path, "mapstreams/streammap_viz_module.R"))
source(file.path(base_path, "mapstreams/streamshptable_module.R"))

# Main UI
ui <- page_sidebar(
  title = "Shapefile Miles Filter",
  sidebar = sidebar(
    width = 300,
    fileUploadUI("file_upload")
  ),
  
  layout_column_wrap(
    width = 1,
    mapUI("map_view"),
    tableUI("table_view")
  )
)

# Main server
server <- function(input, output, session) {
  # Initialize shared reactive values
  data_vals <- reactiveValues(
    shapefile = NULL,
    filtered_shapefile = NULL,
    random_shapefile = NULL,
    temp_dir = NULL,
    display_data = NULL  # Track which data to display (filtered or random)
  )
  
  # Call file upload module server and connect it to shared data
  fileUploadServer("file_upload", data_vals)
  
  # Call map module server with the shared data
  mapServer("map_view", data_vals)
  
  # Call table module server with the shared data
  tableServer("table_view", data_vals)
}

shinyApp(ui, server)