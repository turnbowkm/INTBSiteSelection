library(shiny)
library(bslib)

# Source all modules
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/dates/data_import_module.R")
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/dates/summary_module.R")
source("C:/Users/kevin.turnbow/siteselect/SiteSelection/dates/preview_module.R")

# Main UI
ui <- page_sidebar(
  title = "Date Range Finder for Monitoring Locations",
  sidebar = sidebar(
    width = 300,
    dataImportUI("data_import"),
    hr(),
    helpText("This app will find the earliest and most recent dates for each Monitoring_Location_ID based on the Activity_Start_Date column in your CSV file.")
  ),
  dateSummaryUI("date_summary"),
  dataPreviewUI("data_preview")
)

# Main server
server <- function(input, output, session) {
  # Import and process data
  data_reactive <- dataImportServer("data_import")
  
  # Generate summary table
  dateSummaryServer("date_summary", data_reactive)
  
  # Generate preview table
  dataPreviewServer("data_preview", data_reactive)
}

shinyApp(ui, server)