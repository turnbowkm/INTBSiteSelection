library(shiny)
library(bslib)

# Source all modules
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/attains/file_module.R")
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/attains/filter_module.R")
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/attains/summary_module.R")

# Main UI
ui <- page_sidebar(
  title = "CSV Data Filter",
  sidebar = sidebar(
    fileModuleUI("file_input")
  ),
  
  # Convert cards to tabs
  tabsetPanel(
    tabPanel(
      title = "Filtered Dataset",
      filterModuleUI("filter_data")
    ),
    tabPanel(
      title = "Parameter Count Summary",
      summaryModuleUI("summary_data")
    )
  )
)

# Main server
server <- function(input, output, session) {
  # Load file data from file module
  data_result <- fileModuleServer("file_input")
  
  # Pass data to filter module
  filterModuleServer("filter_data", data_result)
  
  # Pass data to summary module  
  summaryModuleServer("summary_data", data_result)
}

shinyApp(ui, server)