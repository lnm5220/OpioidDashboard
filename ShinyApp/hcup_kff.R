library(maps)
library(maptools)
library(leaflet)    
library(shiny)
library(data.table)
library(shinydashboard )
library(plotly)
library(shinydashboardPlus)
library(dplyr)

# get a SpatialPolygonsDataFrame of US states
#need to add alaska & hawaii
usa <- map("state", fill = TRUE)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa <- SpatialPolygonsDataFrame(usa,data = data.frame(unique(IDs),row.names = unique(IDs)))

#Load in HCUP pilot data
HCUPKFFdata <- fread("./dt.KFF_HCUP.csv", quote="")

#filter data to only get annual results
HCUP_KFF_annual <- HCUPKFFdata %>%
  filter(TimeInterval == "Annual")
HCUP_KFF_annual <- HCUP_KFF_annual %>%
  filter(!State %in% c(NA, "AK","PR","HI","AS","CZ","GM","NB","VI"))
HCUP_KFF_annual <- HCUP_KFF_annual[order(HCUP_KFF_annual$State),]
state_abbreviations <- c("AL","AZ","AR","CA","CO","CT","DE","DC","FL","GA","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
usa$abbr <- state_abbreviations
dropdown_options <- c("IP","ED","OverdoseDeathsNaturalSemisynthetic","OverdoseDeathsMethadone","OverdoseDeathsHeroin","OverdoseDeathsSyntheticOpioids" )
#usa$IP <- HCUP_annual$IP
year_list <- c(unique(HCUP_KFF_annual$Year))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("State Comparison: Opioid Data"),
  sidebarLayout(
    sidebarPanel(
      helpText("View different measures and years on the choropleth map. Select states by clicking on the map to see a comparison on the plot below."),
      radioButtons("dropdown", "Select one Measure", choices = list("In Patient Opioid Related Hospitalizations" = "IP", "Emergecy Department Opioid Related Hospitalizations" = "ED","Natural Semisynthetic Overdose Deaths (e.g. Oxycodone)"="OverdoseDeathsNaturalSemisynthetic","Methadone Overdose Deaths"="OverdoseDeathsMethadone","Heroin Overdose Deaths"="OverdoseDeathsHeroin","Synthetic Opioid Overdose Deaths"="OverdoseDeathsSyntheticOpioids"), selected = "IP"),
      #selectInput("dropdown_year", "Year", choices = year_list, selected = "2018"),
      sliderInput("dropdown_year", "Select Year",min = 2005, max = 2018, value = 2018, step=1, animate=TRUE),
      actionButton(
        inputId = "clearHighlight",
        label = "Clear selections",
        style = "color: #fff; background-color: #D75453; border-color: #C73232"),
      helpText("The states you have selected will populate below:"),
      textOutput("list")),
    mainPanel(
      box(
        h3(textOutput("title")),
        width = 12,
        leaflet::leafletOutput(
          outputId = "myMap",
          height = 450)),
      box(
        background = 'blue',
        h3(textOutput("title_line")),
        width = 12,
        plotlyOutput("plot")
      )
    )
  ))


# create the server
server <- function( input, output, session ){
  #create palette for the data chosen
  #i have no idea how this palette works bc chosen isnt a variable i use oop
  #function to create foundation map
  output$plot <- renderPlotly(plot_ly() %>%
                                layout(paper_bgcolor="#F5F5F5",plot_bgcolor="#F5F5F5") %>% 
                                add_annotations(
                                  x= 1,
                                  y= 1,
                                  text = "Select states on the choropleth map to populate this plot!",
                                  showarrow=FALSE,
                                  font = list(size = 20)
                                ))
  foundational.map <- reactive({
    if (input$dropdown == "IP") {
      output$title <- renderText({paste("Chloropleth Map of In Patient Hospitalizations for ", input$dropdown_year)})
      output$title_line <- renderText({paste("In Patient Hospitalizations over time for selected states")})
    } else {
      output$title <- renderText({paste("Chloropleth Map of Emergecy Department Hospitalizations for ", input$dropdown_year)})
      output$title_line <- renderText({paste("Emergecy Department Hospitalizations over time for selected states")})
    }
    
    
    #filter data 
    filter_data <- reactive({
      #filter data based on input year and then select measure
      data <- HCUP_KFF_annual %>% filter(HCUP_KFF_annual$Year == input$dropdown_year)
      data <- data %>% select(State,Year,input$dropdown)
    })
    data <- filter_data()
    create_pal <- reactive ({pal <- colorNumeric("Purples",HCUP_KFF_annual[[input$dropdown]],na.color = "#808080",alpha = FALSE)
    })
    
    pal <- create_pal()
    
    
    #set labels
    labels <- sprintf("<strong>%s</strong><br/>%g",usa$unique.IDs., data[[input$dropdown]]) %>% lapply(htmltools::HTML)
    leaflet() %>%
      addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      setView( lng= -95.712891,lat=37.09024, zoom=4.2) %>%
      addLegend(pal = pal, values = HCUP_KFF_annual[[input$dropdown]], opacity = 1, title = paste0(input$dropdown_year," ",input$dropdown),
                position = "bottomright") %>% 
      addPolygons(data=usa,
                  fillColor = ~pal(data[[input$dropdown]]),
                  color = "#000000",
                  fillOpacity = 1,
                  opacity=1,
                  weight = 2,
                  layerId = usa$abbr,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"),
      )
  })
  
  #Render leaflet and push to ouput
  output$myMap <- renderLeaflet({foundational.map()})
  
  #Store the list of clicked polygons in a vector
  click.list <- reactiveValues(ids = vector())
  
  #get clicked polygons & highlight
  observeEvent(
    input$myMap_shape_click, {
      click <- input$myMap_shape_click
      click.list$ids <- c( click.list$ids, click$id )
      output$list <- renderText(click.list$ids)
      lines.of.interest <- usa[ which( usa$abbr %in% click.list$ids ) , ]
      
      filter_state_data <- reactive({
        #filter data based on input year and then select measure
        data <-  HCUP_KFF_annual %>% filter(State %in% click.list$ids)
      })
      st_data <- filter_state_data()
      
      line.graph <- reactive({
        y <- list(title = "Count")
        plot_ly(st_data, x = ~Year,y = st_data[[input$dropdown]],color = ~State, type = 'scatter', mode = 'lines', name = ~State, connectgaps = FALSE) %>% 
          layout(paper_bgcolor="#F5F5F5",plot_bgcolor="#F5F5F5",yaxis=y) %>% 
          add_annotations(
            x= 0.1,
            y= 1,
            xref = "paper",
            yref = "paper",
            text = "Note: Incomplete lines represent missing data.",
            showarrow = F,
            font= list(size=15)
          )
      })
      
      output$plot <- renderPlotly({line.graph()})
      
      #if statement
      if( is.null( click$id ) ){
        req( click$id )
      } else if( !click$id %in% lines.of.interest@data$id ){
        leafletProxy( mapId = "myMap" ) %>%
          addPolylines( data = lines.of.interest
                        , layerId = lines.of.interest@data$id
                        , color = "#87FCFC"
                        , weight = 3
                        , opacity = 1)
      }
    })
  
  # Create the logic for the "Clear the map" action button which will clear the map of all user-created highlights and display a clean version of the leaflet map
  observeEvent( input$clearHighlight, {
    blank.graph <- reactive({plot_ly()})
    output$plot <- renderPlotly(blank.graph())
    output$myMap <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      # second recall the foundational.map() object
      foundational.map()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


