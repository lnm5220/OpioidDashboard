library(maptools)
library(leaflet)    
library(shiny)
library(remotes)
library(devtools)
library(data.table)
library(shinydashboard)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyhelper)
library(tidyr)
library(plotly)
library(dplyr)
library(sf)
library(sp)
library(DT)
library(janitor)
library(rmapshaper)
library(shiny)
library(usdata)
library(shinyWidgets)
library(markdown)
library(shinythemes)

#UI
ui <- navbarPage("OPADD",
                 theme = shinytheme("flatly"),
                 useShinydashboard(),
                 tabPanel("Test"),
                 tabPanel("Sources",
                          box(title = NULL, width = 12,
                              h1("OPADD Sources & Citations", align = 'center'),
                              p(HTML("The OPADD Team used a variety of sources in the creation of this dashboard. The team has provided a downloadable pdf organized with citations for background information, policy data, and measure specific data. Click the button below to view a full list of these sources.", align = 'center')),
                              hr(),
                              fluidRow(
                                align = 'center',
                                tags$h2("PDF Download", align = 'center'),
                                actionButton(
                                  inputId = "pdf",
                                  label = "OPADD Citations",
                                  icon = icon("download", align = 'center'),
                                  onclick = "window.open('')"),
                              ))),
                 footer = includeHTML("footer2.html")
)
                 
server <- function(input, output) { 
  
  observeEvent(input$pdf, {
    # Absolute path to a pdf, replace with Ally's pdf citation file
    file.show(file.path(R.home(), "doc", "NEWS.pdf"))
  })
  
}

shinyApp(ui,server)
