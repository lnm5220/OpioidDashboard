library(shiny)
library(data.table)
library(dplyr)
library(naniar)
library(ggplot2)
library(plotly)

#load in data
HCUPdata = fread("./cleanIP2017.csv",quote="")
KFFdata = fread("./cleanKFF.csv",quote="")

#fix up data a bit
names(KFFdata)[names(KFFdata) == 'abbr'] <- 'State'
#join together kff and HCUP (eventually will not have to do this bc back end will provide master data csv)
full_data <- left_join(HCUPdata, KFFdata)
full_data <- within(full_data, rm(Location))

#create options dataframe that just has the dropdown information
options <- full_data[,!"State"]

# specify some map projection/options
g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
)

#Shiny app
server <- function(input, output) {
    #title function this def needs to be better lmao
    title <- reactive(
        ifelse(
            input$y == "Heroin", "Heroin Overdose Deaths",
            ifelse(
                input$y =="Methadone", "Methadone Overdose Deaths", 
                ifelse(
                    input$y =="IP","In Patient Opioid Related Hospitalizations",
                    ifelse(
                        input$y =="NaturalandSemisyntheticOpioids","Natural and Semisynthetic Opioids (e.g. oxycodone, hydrocodone)",
                        ifelse(
                            input$y =="SyntheticOpioids","Synthetic Opioids, other than Methadone (e.g. fentanyl, tramadol)","Title"
                        ))))))
    
    output$p <- renderPlotly({
        plot_geo() %>%
            add_trace(z = full_data[[input$y]], span = I(0), locations = full_data$State, locationmode = 'USA-states', colors="Purples") %>% 
            layout(geo=g) %>% 
            colorbar(title="Count") %>% 
            layout(title=title())
        })
}


ui <- fluidPage(
    selectInput(
        "y", "Measures", 
        choices = names(options)
    ),
    plotlyOutput("p")
)


shinyApp(ui = ui, server = server)
