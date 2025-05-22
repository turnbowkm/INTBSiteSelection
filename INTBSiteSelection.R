library(shiny)
library(bslib)

# Define paths for module sourcing
base_path <- "C:/Users/kevin.turnbow/siteselect/SiteSelection"

# Source all modules
# Streams/maps modules
source(file.path(base_path, "mapstreams/streamsmap_upload_module.R"))
source(file.path(base_path, "mapstreams/streammap_viz_module.R"))
source(file.path(base_path, "mapstreams/streamshptable_module.R"))

# ATTAINS modules
source(file.path(base_path, "attains/atfile_module.R"))
source(file.path(base_path, "attains/atfilter_module.R"))
source(file.path(base_path, "attains/atsummary_module.R"))

# AWQM modules
source(file.path(base_path, "dates/awqmdata_import_module.R"))
source(file.path(base_path, "dates/awqmsummary_module.R"))
source(file.path(base_path, "dates/awqmpreview_module.R"))

# Stations modules
source(file.path(base_path, "Stations modules/stationsupload_module.R"))
source(file.path(base_path, "Stations modules/stationsmap_module.R"))
source(file.path(base_path, "Stations modules/stationstable_module.R"))

# Main UI
ui <- page_sidebar(
  title = "Shapefile Miles Filter",
  sidebar = sidebar(
    width = 300,
    streammapfileUploadUI("file_upload"),
    atfileModuleUI("file_input"),
    stationsuploadUI("upload"),
    awqmdataImportUI("data_import")
  ),
  
  # Use navset_tab for the main content area
  navset_tab(
    nav_panel(
      title = "Stream Viewer",
      stream_mapUI("map_view")
    ),
    nav_panel(
      title = "Stream Table (filtered)",
      streamshptableUI("table_view")
    ),
    nav_panel(
      title = "Filtered Dataset",
      atfilterModuleUI("filter_data")
    ),
    nav_panel(
      title = "Parameter Count Summary",
      atsummaryModuleUI("summary_data")
    ),
    nav_panel(
      title = "Stations Viewer",
      stationsmapUI("map_section")
    ),
    nav_panel(
      title = "Station Table",
      stationstableUI("table_section")
    ),
    nav_panel(
      title = "AWQM Summary",
      awqmdateSummaryUI("date_summary")
    ),
    nav_panel(
      title = "AWQM Preview",
      awqmdataPreviewUI("data_preview")
    )
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
  streammapfileUploadServer("file_upload", data_vals)
  
  # Call map module server with the shared data
  stream_mapServer("map_view", data_vals)
  
  # Call table module server with the shared data
  streamshptableServer("table_view", data_vals)
  # Load file data from file module
  data_result <- atfileModuleServer("file_input")
  
  # Pass data to filter module
  atfilterModuleServer("filter_data", data_result)
  
  # Pass data to summary module  
  atsummaryModuleServer("summary_data", data_result)
  
  # Call the upload server module and get the reactive sf_data
  sf_data <- stationsuploadServer("upload")
  
  # Call the map and table server modules, passing sf_data to them
  stationsmapServer("map_section", sf_data)
  stationstableServer("table_section", sf_data)
  # Import and process data
  data_reactive <- awqmdataImportServer("data_import")
  
  # Generate summary table
  awqmdateSummaryServer("date_summary", data_reactive)
  
  # Generate preview table
  awqmdataPreviewServer("data_preview", data_reactive)
}

shinyApp(ui, server)
