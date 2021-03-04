library(maps)
library(maptools)
library(leaflet)    
library(shiny)
library(data.table)
library(shinydashboard )
library(plotly)
library(shinydashboardPlus)
library(dplyr)

### CREATE SPATIAL DATA
# get a SpatialPolygonsDataFrame of US states using map package
usa <- map("state",fill=TRUE)
#get "IDS" this is the name of the state, we have to filter out some states that have cities after like new york:staten island
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
#take map data and turn to spatialpolygons (necessary for clicking/leaflet)
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
#take spatialpolygon to spatial polygon dataframe
usa <- SpatialPolygonsDataFrame(usa,data = data.frame(unique(IDs),row.names = unique(IDs)))
###use state.abb instead when we add alaska & hawaii
#list of state abbreviations we need, add to USA spatial data
state_abbreviations <- c("AL","AZ","AR","CA","CO","CT","DE","DC","FL","GA","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
usa$abbr <- state_abbreviations

#Maybe rename column names in data to shortname\longname then use regex to split names and rename columns, make dictionary
###Load in KFF/HCUP
HCUPKFFdata <- fread("./dt.KFF_HCUP.csv", quote="")

#filter data to only get annual results and get rid of extra territories
#CONDENSE THIS TO ONE FILTER STATEMENT
HCUP_KFF_annual <- HCUPKFFdata %>%
  filter(TimeInterval == "Annual" & !State %in% c(NA, "AK","PR","HI","AS","CZ","GM","NB","VI"))

#Define year list by getting unique values from dataframe
year_list <- c(unique(HCUP_KFF_annual$Year))

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("State Comparison: Opioid Data"),
  sidebarLayout(
    sidebarPanel(
      helpText("View different measures and years on the choropleth map. Select states by clicking on the map to see a comparison on the plot below."),
      radioButtons("dropdown", "Select one Measure", choices = list("In Patient Opioid Related Hospitalizations" = "IP", "Emergecy Department Opioid Related Hospitalizations" = "ED","Natural Semisynthetic Overdose Deaths (e.g. Oxycodone)"="OverdoseDeathsNaturalSemisynthetic","Methadone Overdose Deaths"="OverdoseDeathsMethadone","Heroin Overdose Deaths"="OverdoseDeathsHeroin","Synthetic Opioid Overdose Deaths"="OverdoseDeathsSyntheticOpioids"), selected = "IP"),
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
        h3(textOutput("title_line")),
        width = 12,
        plotlyOutput("plot")
      )
    )
  ))


# create the server
server <- function( input, output, session ){
  
  #Set blank output line graph
  output$plot <- renderPlotly(
    plot_ly() %>%
      layout(paper_bgcolor="#F5F5F5",plot_bgcolor="#F5F5F5") %>% 
      add_annotations(x=1, y=1, text="Select states on the choropleth map to populate this plot!", showarrow=FALSE, font=list(size=20))
    )

  
  #choropleth map function
  foundational.map <- reactive({
    #set title using these if else statements (that stink)
    if (input$dropdown == "IP") {
      output$title <- renderText({paste("Chloropleth Map of In Patient Hospitalizations for ", input$dropdown_year)})
      output$title_line <- renderText({paste("In Patient Hospitalizations over time for selected states")})
    } 
    else if (input$dropdown == "OverdoseDeathsMethadone") {
      output$title <- renderText({paste("Chloropleth Map of Methadone Overdose Deaths for ", input$dropdown_year)})
      output$title_line <- renderText({paste("Methadone Overdose Deaths over time for selected states")})
    }
    else if (input$dropdown == "OverdoseDeathsNaturalSemisynthetic") {
      output$title <- renderText({paste("Chloropleth Map of Natural Semisynthetic Opioid Overdose Deaths (e.g. Oxycodone) for ", input$dropdown_year)})
      output$title_line <- renderText({paste("Natural Semisynthetic Opioid Overdose Deaths over time for selected states")})
    }
    else if (input$dropdown == "OverdoseDeathsSyntheticOpioids") {
      output$title <- renderText({paste("Chloropleth Map of Synthetic Opioid (e.g. fentanyl, tramadol) Overdose Deaths for ", input$dropdown_year)})
      output$title_line <- renderText({paste("Synthetic Opioid Overdose Deaths over time for selected states")})
    }
    else if (input$dropdown == "OverdoseDeathsHeroin") {
      output$title <- renderText({paste("Chloropleth Map of Heroin Overdose Deaths for ", input$dropdown_year)})
      output$title_line <- renderText({paste("Heroin Overdose Deaths over time for selected states")})
    }
    else {
      output$title <- renderText({paste("Chloropleth Map of Emergency Department Hospitalizations for ", input$dropdown_year)})
      output$title_line <- renderText({paste("Emergecy Department Hospitalizations over time for selected states")})
    }
    
    #filter data function based on the input
    filter_data <- reactive({
      #filter data based on input year and then select measure
      data <- HCUP_KFF_annual %>% filter(HCUP_KFF_annual$Year == input$dropdown_year)
      data <- data %>% select(State,Year,input$dropdown)
    })
    
    #create pallette function based on the input
    create_pal <- reactive({
      pal <- colorNumeric("Purples",HCUP_KFF_annual[[input$dropdown]],na.color = "#808080",alpha = FALSE)
    })
    
    
    #call filter data
    data <- filter_data()
    pal <- create_pal()
    
    #set hover labels
    labels <- sprintf("<strong>%s</strong><br/>%g",usa$unique.IDs., data[[input$dropdown]]) %>% lapply(htmltools::HTML)
    
    #Call Leaflet to produce map
    leaflet() %>%
      #add tiles using carto db pre made map backgrounds
      addTiles( urlTemplate = "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png") %>%
      #set view zooms us in
      setView( lng= -95.712891,lat=37.09024, zoom=4.2) %>%
      #legend uses pallette and values and updates title
      addLegend(pal = pal, values = HCUP_KFF_annual[[input$dropdown]], opacity = 1, title = paste0(input$dropdown_year," ",input$dropdown),position = "bottomright") %>% 
      #add the polygons using spatial data (usa dataframe)
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
  
  #END OF FOUNDATIONAL MAP CREATION
  
  #Render leaflet and push to output UI
  output$myMap <- renderLeaflet({foundational.map()})
  
  #set reactive variable
  click.list <- reactiveValues(ids = vector())
  
  #This event will update every time there is a click- we update the choropleth and the line graph below
  #Store the list of clicked polygons in a vector, picks up id of clicked place, in this case its the state abbrevation
  observeEvent(
    input$myMap_shape_click, {
      #get click and add to list
      click <- input$myMap_shape_click
      click.list$ids <- c( click.list$ids, click$id )
      #output the states to text in UI
      output$list <- renderText(click.list$ids)
      #get the lines of those selected states, this makes the higlight color when you click
      lines.of.interest <- usa[ which( usa$abbr %in% click.list$ids ) , ]
      
      #filter the states that have been clicked so we can show them in line graph
      filter_state_data <- reactive({
        #filter data based on input year and then select measure
        data <-  HCUP_KFF_annual %>% filter(State %in% click.list$ids)
      })
      
      st_data <- filter_state_data()
      
      #set line graph function
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
      
      #call line graph and sent to output UI
      output$plot <- renderPlotly({line.graph()})
      
      
      #if statement to check if something is clicked that doesnt have an ID (like the ocean) this req() makes it so the app doesnt break
      if( is.null( click$id ) ){
        req( click$id )
      #if the click is valid, get the lines of interest and add it to leaflet using leaflet proxy (it updates the leaflet map)
      } else if( !click$id %in% lines.of.interest@data$id ){
        leafletProxy( mapId = "myMap" ) %>%
          addPolylines( data = lines.of.interest
                        , layerId = lines.of.interest@data$id
                        , color = "#87FCFC"
                        , weight = 3
                        , opacity = 1)
      }
    })
  
  #Observe the event of clicking clear highlight, when this is clicked, we sent blank graphs to line graph, and clear the highlights on choropleth
  observeEvent( input$clearHighlight, {
    #render blank line graph
    blank.graph <- reactive({plot_ly()})
    output$plot <- renderPlotly(blank.graph())
    #render blank leaflet 
    output$myMap <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      # second recall the foundational.map() object
      foundational.map()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


