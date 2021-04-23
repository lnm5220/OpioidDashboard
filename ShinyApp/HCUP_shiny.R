library(shiny)
library(data.table)
library(dplyr)
library(naniar)
library(ggplot2)
library(plotly)


#Load in HCUP pilot data
HCUPdata <- fread("C:/Users/koons/OneDrive/Documents/OpioidDashboard/Pilot_data_0130.csv", quote="")

#filter data to only get annual results
HCUP_annual <- HCUPdata %>%
  filter(TimeInterval == "Annual")

#create dataframe for options for state dropdown
dropdown <- data.frame(unique(HCUP_annual$State))
boxes <- HCUP_annual %>%
  select(IP, ED)

ui <- fluidPage(
  titlePanel("Opioid Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select a State:", choices = dropdown),
      
      checkboxGroupInput("measure", "Measures:", choices = names(boxes))
    ),
    
    mainPanel(plotlyOutput("plot")
              )
  )
)



server <- function(input, output) {

  output$plot <- renderPlotly({
    
    statedf <- HCUP_annual %>%
      filter(State == input$state)
    
    if(length(input$measure) > 1){
      plot_ly(statedf, x = ~Year, y = statedf[[input$measure[1]]], type = 'scatter', mode = 'lines', name = "IP") %>%
        add_trace(statedf, x = ~Year, y = statedf[[input$measure[2]]], type = 'scatter', mode = 'lines', name = "ED")
    }
    else{
      plot_ly(statedf, x = ~Year, y = statedf[[input$measure[1]]], type = 'scatter', mode = 'lines', name = "IP") %>%
        add_trace(statedf, x = ~Year, y = statedf[[input$measure[1]]], type = 'scatter', mode = 'lines',name = "ED")
      
    }
  })
  
}

shinyApp(ui = ui, server = server)