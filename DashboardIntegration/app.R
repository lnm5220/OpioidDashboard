library(maps)
library(maptools)
library(leaflet)    
library(shiny)
library(data.table)
library(shinydashboard )
library(plotly)
library(dplyr)
library(sf)
library(sp)
library(albersusa)
library(rmapshaper)
library(shiny)
library(usdata)

#Load Data
HCUPKFFdata <- fread("./HCUPKFFAnnual.csv", quote="")
HCUPKFFdata <- HCUPKFFdata %>% 
    mutate(name = abbr2state(State))

#Call USA sf to get USA data
spdf <- ms_simplify(usa_sf(), keep = 0.1) #spatial dataframe using usa_sf()
spdf <- spdf %>% select(-c('pop_2010','pop_2011','pop_2012','pop_2013','pop_2014','census','pop_estimataes_base','lsad'))


#Set basemap projection using leaflet CRS (allowing us to see AK & HI)
epsg2163 <- leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:2163",
    proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
    resolutions = 2^(16:7))


ui <- dashboardPage(
    dashboardHeader(title = "Opioid dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("State Comparison", tabName = "Comparison", icon = icon("dashboard")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "Comparison",
                    fluidRow(
                        box(
                          title= "Choropleth Map",
                          width = 8,
                          leafletOutput(
                            outputId = "map",
                            height = 450)
                        ),
                        
                        box(
                            width = 4,
                            title = "Controls",
                            radioButtons("measure", "Select a Measure to visualize",
                                         choices = list("In Patient Opioid Related Hospitalizations" = "IP",
                                                        "Emergecy Department Opioid Related Hospitalizations" = "ED",
                                                        "Natural Semisynthetic Overdose Deaths (e.g. Oxycodone)"="OverdoseDeathsNaturalSemisynthetic",
                                                        "Methadone Overdose Deaths"="OverdoseDeathsMethadone",
                                                        "Heroin Overdose Deaths"="OverdoseDeathsHeroin",
                                                        "Synthetic Opioid Overdose Deaths"="OverdoseDeathsSyntheticOpioids"),
                                         selected = "IP"),
                            sliderInput("dropdown_year", "Select Year",min = 2005, max = 2018, value = 2018, step=1, animate=TRUE),
                            actionButton(
                              inputId = "clearHighlight",
                              label = "Clear selections",
                              style = "color: #fff; background-color: #D75453; border-color: #C73232"),
                            helpText("The states you have selected will populate below:"),
                            textOutput("list")
                        ),
                    ),
                    fluidRow(
                      box(
                        width=8,
                        title="Line Graph here",
                        plotlyOutput("plot")),
                      box(width=4,
                          title="Policy")
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    h2("Widgets tab content")
            )
        )
    )
)

server <- function(input, output) {
  
  #filter data function based on the input
  filter_data <- reactive({
    
    #get the year & measure from main data
    test_data <- HCUPKFFdata %>% filter(HCUPKFFdata$Year == input$dropdown_year) %>% select(input$measure,"name")
    
    #make spatial dataframe & drop unnessescay columns & add our target measure to the df
    spdf <- merge(spdf,test_data,by="name")
    spd <- as_Spatial(st_geometry(spdf), IDs = as.character(1:nrow(spdf))) #make is spatial using sf package
    
    df <- spdf #back to df & get rid of geometry
    df$geometry <- NULL
    df <- as.data.frame(df)
    spd <- SpatialPolygonsDataFrame(spd, data = df)
    return(spd)
  })
  
  #Foundational heat map 
  foundational.map <- reactive({
    spd <- filter_data()
    pal <- colorNumeric("Blues", domain = HCUPKFFdata[[input$measure]],na.color = "#808080")
    leaflet(spd, options = leafletOptions(crs = epsg2163)) %>%
      #set view zooms us in
      setView( lng= -95.712891,lat=37.09024, zoom=3) %>%
      addLegend(pal = pal, values = HCUPKFFdata[[input$measure]], opacity = 1, title = "legend",position = "bottomright") %>% 
      addPolygons(weight = 1,
                  layerId = ~name,
                  color = "#444444",
                  opacity = 1,
                  fillColor = ~pal(spd[[input$measure]]),
                  fillOpacity = 0.7,
                  smoothFactor = 0.5,
                  label = ~paste(name, spd[[input$measure]]),
                  labelOptions = labelOptions(direction = "auto"),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE))})
  
  #Render heat graph map
  output$map <- renderLeaflet({foundational.map()})
  
  # store the list of clicked polygons in a vector
  click.list <- reactiveValues(ids = vector())
  
  observeEvent(input$map_shape_click, {
    spd <- filter_data()
    click <- input$map_shape_click
    click.list$ids <- c(click.list$ids, click$id)
    output$list <- renderText(click.list$ids)
    lines.of.interest <- spd[which( spd$name %in% click.list$ids), ]
    
    
    #filter the states that have been clicked so we can show them in line graph
    filter_state_data <- reactive({
      #filter data based on input year and then select measure
      data <-  HCUPKFFdata %>% filter(name %in% click.list$ids)
    })
    
    st_data <- filter_state_data()
    
    #set line graph function
    line.graph <- reactive({
      y <- list(title = "Count")
      plot_ly(st_data, x = ~Year,y = st_data[[input$measure]],color = ~State, type = 'scatter', mode = 'lines', name = ~State, connectgaps = FALSE) %>% 
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
    
    
    if(is.null(click$id)){
      req( click$id )}
    else if(!click$id %in% lines.of.interest@data$id ){
      leafletProxy( mapId = "map" ) %>%
        addPolylines(data = lines.of.interest,
                     layerId = lines.of.interest@data$id,
                     color = "#BDFFFF",
                     weight = 5,
                     opacity = 1)}})
  
  observeEvent( input$clearHighlight, {
    #render blank line graph
    blank.graph <- reactive({
      plotly_empty(type = "scatter", mode = "markers") %>%
        config(displayModeBar = FALSE) %>%
        layout(title = list(text = "Click states in order to see the line graph.", yref = "paper", y = 0.5))})
    output$plot <- renderPlotly(blank.graph())
    
    output$map <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      foundational.map()})})
  
}

shinyApp(ui, server)
