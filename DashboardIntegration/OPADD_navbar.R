library(maps)
library(maptools)
library(leaflet)    
library(shiny)
library(remotes)
library(devtools)
#devtools::install_github("https://github.com/hrbrmstr/albersusa")
library(albersusa)
library(data.table)
library(shinydashboard)
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
library(shinythemes)
library(shinyWidgets)
library(timevis)

# DASHBOARD IN NAVBARPAGE INSTEAD OF SHINYDASHBOARD-----------------------------

# NOTES: The only real functioning part of this dashboard is the timeline on the single state page.
## The map and line graphs wouldn't work when I tried them (some issues with the spatial polygons and what not), so will have to work that out.


Policy <- fread("./PolicyDates.csv", quote="")
HCUPKFFdata <- fread("https://raw.githubusercontent.com/lnm5220/OpioidDashboard/main/DashboardIntegration/HCUPKFFAnnual.csv", quote = "")


#Call USA sf to get USA data
spdf <- ms_simplify(usa_sf(), keep = 0.1) #spatial dataframe using usa_sf()
spdf <- spdf %>% select(-c('pop_2010','pop_2011','pop_2012','pop_2013','pop_2014','census','pop_estimataes_base','lsad'))

#Set basemap projection using leaflet CRS (allowing us to see AK & HI)
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

timeline_df <- function(state){
  temp <- filter(Policy, Policy$State == state)
  temp <- tibble::rownames_to_column(as.data.frame(t(temp)), "content")
  colnames(temp)[2] <- "start"
  temp$start <- as.Date(temp$start, format = "%m/%d/%Y")
  temp <- na.omit(temp)
  return(temp)
}


ui <- navbarPage(
  "OPADD",
  theme = shinytheme("slate"),
  tabPanel("Multi-State Comparison",
           useShinydashboard(),
           fluidRow(
             box(
               width=12,
               title="Welcome to OPAD (Opioid Policy and Data Dashboard)",
               h5("Use this dashboard to explore data about the opioid epdimeic.
                         Customize the data shown on the map and then select states on the map to compare them.")                    )),
           fluidRow(
             box(
               title= "State Measure Heat Map",
               width = 8,
               leafletOutput(
                 outputId = "map",
                 height = 450)),
             box(
               width = 4,
               title = "Controls",
               radioButtons("measure", "Select a Measure to visualize",
                            choices = list("Inpatient Opioid Related Hospitalizations" = "IP",
                                           "Emergency Department Opioid Related Hospitalizations" = "ED",
                                           "Natural Semisynthetic Overdose Deaths (e.g. Oxycodone)"="OverdoseDeathsNaturalSemisynthetic",
                                           "Methadone Overdose Deaths"="OverdoseDeathsMethadone",
                                           "Heroin Overdose Deaths"="OverdoseDeathsHeroin",
                                           "Synthetic Opioid Overdose Deaths"="OverdoseDeathsSyntheticOpioids"),
                            selected = "IP") %>% helper(type = "inline",
                                                        title = "What do these variables mean?",
                                                        content = c("<b>Inpatient Opioid Related Hospitalizations</b> refers to the number of inpatient hospital stays that related to the use of an opioid.",
                                                                    "<b>Emergency Department Opioid Related Hospitalizations</b> refers to the number of emergency department visits that were related to the use of an opioid.",
                                                                    "<b>Natural Semisynthetic Overdose Deaths </b> refers to overdose deaths caused by natural and semisynthetic opioids including but not limited to morphine, codeine, oxycodone, hydrocodone, hydromorphone, and oxymorphone.",
                                                                    "<b>Methadone Overdose Deaths</b> refers to overdose deaths caused by methadone, a synthetic opioid.",
                                                                    "<b>Heroin Overdose Deaths</b> refers to overdose deaths caused by heroin.",
                                                                    "<b>Synthetic Opioid Overdose Deaths</b> refers to overdose deaths caused by synthetic opioids, drugs made in laboratories to mimic the effect of natural opiates.")),
               tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00006900; border-top: 1px solid #00003900; border-bottom: 1px solid #00003900 ;}")),
               sliderInput("dropdown_year", "Select Year",min = 2005, max = 2018, value = 2018, step=1, sep = "", animate=TRUE),
               actionButton(
                 inputId = "clearHighlight",
                 label = "Clear selections",
                 style = "color: #fff; background-color: #D75453; border-color: #C73232"),
               helpText("The states you have selected will populate below:"),
               textOutput("list"),
               actionButton("jump.to.ss", "Single-State for XXX")
             ),
           ),
           fluidRow(
             box(
               width=6,
               title="Line Graph",
               plotlyOutput("plot")),
             box(width=6,
                 title="Policy",
                 dataTableOutput("policy_table"))
           )
  ),
  tabPanel("Single-State Analysis",
           fluidRow(
             box(
               title = "State Metrics Analysis",
               width = 8,
               plotlyOutput("single.state.plot")
             ),
             box(
               title = "Controls",
               width = 4,
               selectInput("dropdown_input", "Select State", c("Select a State" = "", Policy$State)),
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
  ),
  tabPanel("Background"),
  tab
  
)

server <- function(input, output) {
  
  #Tells Rshiny we have helper buttons
  observe_helpers()
  
  #filter data function based on the input and return dataframe
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
  
  #Foundational heat map Function ----------------------------------------------------------------------------------------------------
  foundational.map <- reactive({
    #call filter
    spd <- filter_data()
    #set palette based on input measure
    pal <- colorNumeric("Blues", domain = HCUPKFFdata[[input$measure]],na.color = "#808080")
    #start leaflet object (using projections defined in the beginning)
    leaflet(spd, options = leafletOptions(crs = epsg2163)) %>%
      #set view (allows us to see USA)
      setView( lng= -95.712891,lat=37.09024, zoom=3) %>%
      #adds legend based on input and palette
      addLegend(pal = pal, values = HCUPKFFdata[[input$measure]], opacity = 1, title = "Counts",position = "bottomright") %>% 
      #actually adds the states, layerid refers to the hover info associated with each shape, in our case its state name
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
                                                      bringToFront = FALSE))
  })
  
  #Render heat graph map
  output$map <- renderLeaflet({foundational.map()})
  
  #Observe Click Event ----------------------------------------------------------------------------------------------------
  # store the list of clicked polygons in a vector
  click.list <- reactiveValues(ids = vector())
  #use shinys "observe" to see what is clicked, and get the geometry for those lines so we can draw the blue highlights
  observeEvent(input$map_shape_click, {
    spd <- filter_data()
    click <- input$map_shape_click
    click.list$ids <- c(click.list$ids, click$id)
    output$list <- renderText(click.list$ids)
    lines.of.interest <- spd[which( spd$name %in% click.list$ids), ]
    #filter state function to get data for *clicked* states (we need this for plotting line graph)
    filter_state_data <- reactive({
      #filter data based on input year and then select measure
      data <-  HCUPKFFdata %>% filter(name %in% click.list$ids)
    })
    
    #filter the data for clicked states so we can acce
    st_data <- filter_state_data()
    
    #filter policy data
    filter_policy <- reactive({
      if (length(click.list$ids) == 1) 
      {policy_table <- data.table(Policy)
      data_wide <- data.frame(t(policy_table))
      data_wide <- data_wide %>%row_to_names(row_number = 1)
      policy_data <- data.frame(data_wide[,click.list$ids])
      names(policy_data)[names(policy_data) == "data_wide...click.list.ids."] <- click.list$ids[1]
      return(policy_data)
      }
      else{
        policy_table <- data.table(Policy)
        data_wide <- data.frame(t(policy_table))
        data_wide <- data_wide %>%row_to_names(row_number = 1)
        policy_data <- data.frame(data_wide[,click.list$ids])
        return(policy_data)
      }
    })
    
    policy_data <- filter_policy()
    
    #set line graph function
    line.graph <- reactive({
      y <- list(title = "Count")
      plot_ly(st_data, x = ~Year,y = st_data[[input$measure]],color = ~State, type = 'scatter', mode = 'lines', name = ~State, connectgaps = FALSE) %>% 
        layout(paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF",yaxis=y) %>% 
        add_annotations(
          x= 0.1,
          y= 1,
          xref = "paper",
          yref = "paper",
          text = "Note: Incomplete lines represent missing data.",
          showarrow = F,
          font= list(size=15)
        )})
    
    
    #call line graph and sent to output UI
    output$plot <- renderPlotly({line.graph()})
    
    output$policy_table <- DT::renderDataTable(policy_data, server = TRUE, selection='single')
    
    observeEvent(input$policy_table_rows_selected, {
      if (length(input$policy_table_rows_selected)) {
        annotations = list()
        shapes = list()
        states = names(policy_data)
        for (i in 1:ncol(policy_data)){
          if (policy_data[input$policy_table_rows_selected,i] != "") {
            date = policy_data[input$policy_table_rows_selected,i]
            year = substr(date, nchar(date) - 3, nchar(date))
            annotation <- list(x = year,
                               y = max(st_data[[input$measure]],na.rm=TRUE)-(max(st_data[[input$measure]],na.rm=TRUE)/ncol(policy_data))*(i-1),
                               text = paste0(row.names(policy_data)[input$policy_table_rows_selected],"(",colnames(policy_data)[i],")"),
                               showarrow = FALSE)
            shape <- list(type='line',
                          x0=year,
                          x1=year,
                          y0=0,
                          y1=max(st_data[[input$measure]],na.rm=TRUE),
                          line=list(dash='dot', width=1)) 
            annotations[[i]] <- annotation
            shapes[[i]] <- shape}
          
          annotated.graph <- reactive({
            y <- list(title = "Count")
            plot_ly(st_data, x = ~Year,y = st_data[[input$measure]],
                    color = ~State, type = 'scatter', mode = 'lines',
                    name = ~State, connectgaps = FALSE) %>% 
              layout(paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF",yaxis=y,annotations=annotations, shapes=shapes)
          })
          output$plot <- renderPlotly({annotated.graph()})
        }} else {
          output$plot <- renderPlotly({line.graph()})
        }
    })
    
    if(is.null(click$id)){
      req( click$id )}
    else if(!click$id %in% lines.of.interest@data$id ){
      leafletProxy( mapId = "map" ) %>%
        addPolylines(data = lines.of.interest,
                     layerId = lines.of.interest@data$id,
                     color = "#BDFFFF",
                     weight = 5,
                     opacity = 1)}
  })  # End Click observation -----------------------------------------------------------------------------------------------------------
  
  # Observe clear highlight button & reset everything ----------------------------------------------------------------------------------------
  observeEvent( input$clearHighlight, {
    #render blank line graph
    blank.graph <- reactive({
      plotly_empty(type = "scatter", mode = "markers") %>%
        config(displayModeBar = FALSE) %>%
        layout(title = list(text = "Click states in order to see the line graph.", yref = "paper", y = 0.5))})
    output$plot <- renderPlotly(blank.graph())
    policy_data <- data.table()
    output$policy_table <- DT::renderDataTable(policy_data, server = TRUE, selection='single')
    output$map <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      foundational.map()})})
  

  
  #SINGLE STATE PAGE ----------------------------------
  output$timeline <- renderTimevis({
    timevis(timeline_df(input$dropdown_input), showZoom = FALSE)
  })
}

shinyApp(ui, server)