library(shiny)
library(data.table)
library(dplyr)
library(naniar)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(timevis)
library(shinydashboard)

dt <- fread("./PolicyDates.csv", quote="")
HCUPKFF <- fread("https://raw.githubusercontent.com/lnm5220/OpioidDashboard/main/DashboardIntegration/HCUPKFFAnnual.csv", quote = "")



#dt2 <- tibble::rownames_to_column(as.data.frame(t(dt)), "content")

timeline_df <- function(state){
  temp <- filter(dt, dt$State == state)
  temp <- tibble::rownames_to_column(as.data.frame(t(temp)), "content")
  colnames(temp)[2] <- "start"
  temp$start <- as.Date(temp$start, format = "%m/%d/%Y")
  temp <- na.omit(temp)
  return(temp)
}

ui <- dashboardPage(
  
  dashboardHeader(title = "Opioid Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Multi-State Comparison", tabName = "Comparison", icon = icon("dashboard")),
      menuItem("Single State Analysis", tabName = "Analysis", icon = icon("th"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Comparison",
              fluidRow(
                box(
                  title = "Controls",
                  width = 4,
                  tags$style(HTML(".js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00006900; border-top: 1px solid #00003900; border-bottom: 1px solid #00003900 ;}")),
                  tags$style(HTML(".js-irs-0 .irs-single { font-size: 110%; color: #000000; background: #ffffff }")),
                  tags$style(HTML(".js-irs-0 .irs-min, .js-irs-0 .irs-max { font-size: 110%; color: #000000; background: #808080}")),
                  #tags$style(HTML(".js-irs-0 .irs-grid-pol {}")),
                  sliderInput("dropdown_year", "Select Year",min = 2005, max = 2018, value = 2018, step=1, sep = "", animate=TRUE)
                )
              )
              ),
      tabItem(tabName = "Analysis",
            fluidRow(
              box(
                title = "State Metrics Analysis",
                width = 8,
                plotlyOutput("single.state.plot")
              ),
              box(
                title = "Controls",
                width = 4,
                selectInput("dropdown_input", "Select State", c("Select a State" = "", dt$State)),
                checkboxGroupButtons("checkbox", "Select Metrics to compare on Line Graph",
                                     choices = list("Inpatient Opioid Related Hospitalizations" = "IP",
                                                    "Emergency Department Opioid Related Hospitalizations" = "ED",
                                                    "Natural Semisynthetic Overdose Deaths (e.g. Oxycodone)"="OverdoseDeathsNaturalSemisynthetic",
                                                    "Methadone Overdose Deaths"="OverdoseDeathsMethadone",
                                                    "Heroin Overdose Deaths"="OverdoseDeathsHeroin",
                                                    "Synthetic Opioid Overdose Deaths"="OverdoseDeathsSyntheticOpioids")),
                #checkboxGroupInput("checkbox", "Select Metrics to compare on Line Graph",
                                     #choices = list("Inpatient Opioid Related Hospitalizations" = "IP",
                                                    #"Emergency Department Opioid Related Hospitalizations" = "ED",
                                                    #"Natural Semisynthetic Overdose Deaths (e.g. Oxycodone)"="OverdoseDeathsNaturalSemisynthetic",
                                                    #"Methadone Overdose Deaths"="OverdoseDeathsMethadone",
                                                    #"Heroin Overdose Deaths"="OverdoseDeathsHeroin",
                                                    #"Synthetic Opioid Overdose Deaths"="OverdoseDeathsSyntheticOpioids"))
              )
            ),
            fluidRow(
              box(
                title = "State Policy Timeline",
                width = 8,
                timevisOutput("timeline")
              )
            )
            )
    )
    
  )
)
#setSliderColor("#00006900",1),
  #chooseSliderSkin("Flat"),
  
  #sliderInput("dropdown_year", "Select Year",min = 2005, max = 2018, value = 2018, step=1, sep = "", animate=TRUE),







server <- function(input, output, session) {
#  filter_state_data <- reactive({
#    # Filter data based on input year and then select measure
#    data <-  HCUPKFF %>% filter(State %in% input$dropdown_input)
#  })
  
#  ss_data <- filter_state_data()
  
  
 # line.graph <- reactive({
  #  y <- list(title = "Count")
   # plot_ly(ss_data, x = ~Year,y = ss_data[[input$checkbox]],color = ~State, type = 'scatter', mode = 'lines', name = ~State, connectgaps = FALSE) %>% 
    #  layout(paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF",yaxis=y) %>% 
     # add_annotations(
      #  x= 0.1,
       # y= 1,
        #xref = "paper",
#        yref = "paper",
 #       text = "Note: Incomplete lines represent missing data.",
  #      showarrow = F,
   #     font= list(size=15)
    #  )})
  
 # output$single.state.plot <- renderPlotly({line.graph})
    
    
  
  output$timeline <- renderTimevis({
    timevis(timeline_df(input$dropdown_input))
  })
}

shinyApp(ui, server)