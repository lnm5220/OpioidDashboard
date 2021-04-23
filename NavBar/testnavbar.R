library(maps)
library(maptools)
library(leaflet)    
library(shiny)
library(data.table)
library(shinydashboard)
library(shinyhelper)
library(tidyr)
library(plotly)
library(dplyr)
library(stringr)
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

#LOAD IN STATE DATA
Policy <- fread("./PolicyDates.csv", quote="")
#state_opioid_data <- fread("./data.csv", quote="")
state_opioid_data <- fread("https://raw.githubusercontent.com/lnm5220/OpioidDashboard/main/NavBar/OpioidData.CSV")
#spdf <- st_read("./spdf.shp")
policy_info <- fread("./policyinfo.csv", quote = "")




#Set basemap projection using leaflet CRS (allowing us to see AK & HI)
epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))




ui <- navbarPage(
  "OPADD",
  theme = shinytheme("flatly"),
  tabPanel("Multi-State Comparison",
           tags$style(HTML("
                    .box.box-solid.box-primary>.box-header {
                    color:#FFFFFF;
                    background:#2C3E50;
                    }
                    
                    .box.box-solid.box-primary{
                    border-bottom-color:#2C3E50;
                    border-left-color:#2C3E50;
                    border-right-color:#2C3E50;
                    border-top-color:#2C3E50;
                    background:#FFFFFF
                    }
                    
                    .box.box-solid.box-warning>.box-header {
                    color:#FFFFFF;
                    background:#18BC9C;
                    }
                    
                    .box.box-solid.box-warning{
                    border-bottom-color:#18BC9C;
                    border-left-color:#18BC9C;
                    border-right-color:#18BC9C;
                    border-top-color:#18BC9C;
                    background:#FFFFFF
                    }
                    
                    .box.box-solid.box-success>.box-header {
                    color:#FFFFFF;
                    background:#F39C12;
                    }
                    
                    .box.box-solid.box-success{
                    border-bottom-color:#F39C12;
                    border-left-color:#F39C12;
                    border-right-color:#F39C12;
                    border-top-color:#F39C12;
                    background:#FFFFFF
                    }
                    ")),
           useShinydashboard(),
           fluidRow(
             box(
               width=12,
               status = "primary",
               solidHeader = TRUE,
               title="Welcome to OPADD (Opioid Policy and Data Dashboard)",
               h5("Use this dashboard to explore data about the opioid epdimeic.
                         Customize the data shown on the map and then select states on the map to compare them.")                    )),
           fluidRow(
             box(
               title= "Select a state on the map",
               width = 8,
               status = "primary",
               solidHeader = TRUE,
               leafletOutput(
                 outputId = "statemap",
                 height = 450)),
             box(
               width = 4,
               title = "Controls",
               status = "warning",
               solidHeader = TRUE,
               selectInput("measure", "Select a Measure to visualize",
                           choices = list(    "Opioid Overdose Deaths by Natural and Semisynthetic Opioid",             
                                              "Opioid Overdose Deaths by Synthetic Opioids",
                                              "Opioid Overdose Deaths by Methadone",                                                        
                                              "Opioid Overdose Deaths by Heroin",                                                          
                                              "All Opioid Overdose Deaths",                                                                    
                                              "All Drug Overdose Deaths",                                                    
                                              "Opioid Overdose Deaths as Perctange of all Overdose Deaths",                   
                                              "Opioid Overdose Death Rate Per 100,000 (Age Adjusted)",
                                              "All Drug Overdose Death Rate Per 100,00 (Age Adjusted)",                        
                                              "Rate of Opioid Related Inpatient Hospital Visits",                                                                                     
                                              "Rate of Ppioid Related Emergency Department Visits",                                                                                  
                                              "Rate of Opioid Overdose Deaths (Natural & Semisynthetic)",                             
                                              "Rate of Opioid Overdose Deaths (Synthetic Opioids)",                          
                                              "Rate of Opioid Overdose Deaths (Methadone)",                                                   
                                              "Rate of Opioid Overdose Deaths (Herion)",                                                        
                                              "Counts of Opioid Related Inpatient Hospital Visits",                                                                           
                                              "Counts of Opioid Related Emergency Department Visits",
                                              "Opioid Prescription Rate Per 100 population",                      
                                              "Number of Crime Incident Related to Any Drug Type",                          
                                              "Number of Crime Incident Related to Heroin",                             
                                              "Number of Crime Incident Related to Narcotics",                       
                                              "Number of Crime Incident Related to All Types of Opioid",                    
                                              "Number of Crime Incident Related to Cocaine",                         
                                              "Number of Crime Incident Related to Heroin Morphine And Opium",               
                                              "Number of Crime Incident Related to Other Opioid"),
                           selected = "Opioid Overdose Deaths by Natural and Semisynthetic Opioid") %>% helper(type = "inline",
                                                                                                               title = "What do these variables mean?",
                                                                                                               content = c("<b>Inpatient Opioid Related Hospitalizations</b> refers to the number of inpatient hospital stays that related to the use of an opioid.",
                                                                                                                           "<b>Emergency Department Opioid Related Hospitalizations</b> refers to the number of emergency department visits that were related to the use of an opioid.",
                                                                                                                           "<b>Natural Semisynthetic Overdose Deaths </b> refers to overdose deaths caused by natural and semisynthetic opioids including but not limited to morphine, codeine, oxycodone, hydrocodone, hydromorphone, and oxymorphone.",
                                                                                                                           "<b>Methadone Overdose Deaths</b> refers to overdose deaths caused by methadone, a synthetic opioid.",
                                                                                                                           "<b>Heroin Overdose Deaths</b> refers to overdose deaths caused by heroin.",
                                                                                                                           "<b>Synthetic Opioid Overdose Deaths</b> refers to overdose deaths caused by synthetic opioids, drugs made in laboratories to mimic the effect of natural opiates.")),
               tags$style(HTML(".js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00006900; border-top: 1px solid #00003900; border-bottom: 1px solid #00003900 ;}")),
               tags$style(HTML(".js-irs-0 .irs-single { font-size: 12px; font-weight: bold; color: #2C3E50; background: #FFFFFF }")),
               tags$style(HTML(".js-irs-0 .irs-min, .js-irs-0 .irs-max { font-size: 12px; color: #FFFFFF; background: #2C3E50}")),
               tags$style(HTML(".js-irs-0 .irs-grid-pol.small { display: none;}")),
               tags$style(HTML(".js-irs-0 .irs-grid-text { font-size: 10px;}")),
               tags$style(HTML(".js-irs-0 .irs-handle { background: #18BC9C;}")),
               sliderInput("dropdown_year", "Select Year",min = 2005, max = 2018, value = 2018, step=1, sep = "", animate=TRUE),
               actionButton(
                 inputId = "clearHighlight",
                 label = "Clear selections",
                 style = "color: #fff; background-color: #D75453; border-color: #C73232"),
               helpText("The states you have selected will appear below:"),
               textOutput("list")
             ),
           ),
           fluidRow(
             box(
               width=6,
               title="Line Graph",
               status = "primary",
               solidHeader = TRUE,
               plotlyOutput("plot")),
             box(width=6,
                 title="Policy",
                 status = "success",
                 solidHeader = TRUE,
                 dataTableOutput("policy_table"))
           )
  ),
  tabPanel("Single-State Analysis",
           fluidRow(
             box(
               title = textOutput("ss.graph.title"),
               width = 8,
               status = "primary",
               solidHeader = TRUE,
               plotOutput("single.state.plot")
             ),
             fluidRow(
               box(
                 title = "Controls",
                 width = 4,
                 status = "warning",
                 solidHeader = TRUE,
                 selectInput("dropdown_state_input", selected = "Alabama","Select State", c("Select a State" = "", Policy$State)),
                 pickerInput("single_state_checkbox", "Select Metrics to compare (You may select up to 4)",selected="Opioid Overdose Deaths by Natural and Semisynthetic Opioid",multiple=TRUE,
                             options = list("max-options" = 4, "max-options-text" = "A maximum of 4 metrics are able to be selected."),
                             choices = list(    "Opioid Overdose Deaths by Natural and Semisynthetic Opioid",             
                                                "Opioid Overdose Deaths by Synthetic Opioids",
                                                "Opioid Overdose Deaths by Methadone",                                                        
                                                "Opioid Overdose Deaths by Heroin",                                                          
                                                "All Opioid Overdose Deaths",                                                                    
                                                "All Drug Overdose Deaths",                                                    
                                                "Opioid Overdose Deaths as Perctange of all Overdose Deaths",                   
                                                "Opioid Overdose Death Rate Per 100,000 (Age Adjusted)",
                                                "All Drug Overdose Death Rate Per 100,00 (Age Adjusted)",                        
                                                "Rate of Opioid Related Inpatient Hospital Visits",                                                                                     
                                                "Rate of Ppioid Related Emergency Department Visits",                                                                                  
                                                "Rate of Opioid Overdose Deaths (Natural & Semisynthetic)",                             
                                                "Rate of Opioid Overdose Deaths (Synthetic Opioids)",                          
                                                "Rate of Opioid Overdose Deaths (Methadone)",                                                   
                                                "Rate of Opioid Overdose Deaths (Herion)",                                                        
                                                "Counts of Opioid Related Inpatient Hospital Visits",                                                                           
                                                "Counts of Opioid Related Emergency Department Visits",
                                                "Opioid Prescription Rate Per 100 population",                      
                                                "Number of Crime Incident Related to Any Drug Type",                          
                                                "Number of Crime Incident Related to Heroin",                             
                                                "Number of Crime Incident Related to Narcotics",                       
                                                "Number of Crime Incident Related to All Types of Opioid",                    
                                                "Number of Crime Incident Related to Cocaine",                         
                                                "Number of Crime Incident Related to Heroin Morphine And Opium",               
                                                "Number of Crime Incident Related to Other Opioid"))
               ))
           ),
           fluidRow(
             box(
               title = textOutput("timeline.title"),
               width = 8,
               status = "success",
               solidHeader = TRUE,
               timevisOutput("timeline")
             ),
             box(
               title="Policy Information",
               width = 4,
               status = "success",
               solidHeader = TRUE,
               htmlOutput("policy.message"),
               htmlOutput("ss.policy.name"),
               htmlOutput("ss.policy.date"),
               htmlOutput("ss.policy.desc"),
               htmlOutput("ss.date.desc")
             )
           )
  ),
  tabPanel("County",
           fluidRow(
             box(
               title= "County Measure Heat Map",
               width = 8,
               status = "primary",
               solidHeader = TRUE,
               leafletOutput(
                 outputId = "county_map",
                 height = 450)),
             box(
               width = 4,
               title = "Controls",
               status = "warning",
               solidHeader = TRUE,
               radioButtons("county_measure", "Select a Measure to visualize",
                            choices = list("Count of Overdose Deaths Related to Any Drug"="Count.of.Overdose.Death.Related.to.any.Drug.Type",
                                           "Count of Court of Common Please Opioid Related Cases"="CourtofCommonPleasOpioidCases",
                                           "Rate of Overdose Deaths Related to Any Drug "="Rate.of.Overdose.Death.Related.to.any.Drug.Type",
                                           "Rate of Court of Common Please Opioid Related Cases"="CourtofCommonPleasOpioidRate"),
                            selected = "Count.of.Overdose.Death.Related.to.any.Drug.Type") %>% helper(type = "inline",
                                                                                                      title = "What do these variables mean?",
                                                                                                      content = c("INFO HERE ABOUT COUNTY VARIABLES")),
               tags$style(HTML(".js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #00006900; border-top: 1px solid #00003900; border-bottom: 1px solid #00003900 ;}")),
               tags$style(HTML(".js-irs-1 .irs-single { font-size: 12px; font-weight: bold; color: #2C3E50; background: #FFFFFF }")),
               tags$style(HTML(".js-irs-1 .irs-min, .js-irs-1 .irs-max { font-size: 12px; color: #FFFFFF; background: #2C3E50}")),
               tags$style(HTML(".js-irs-1 .irs-grid-pol.small { display: none;}")),
               tags$style(HTML(".js-irs-1 .irs-grid-text { font-size: 10px;}")),
               tags$style(HTML(".js-irs-1 .irs-handle { background: #18BC9C;}")),
               sliderInput("county_year_slider", "Select Year",min = 2012, max = 2020, value = 2020, step=1, sep = "", animate=TRUE),
               actionButton(
                 inputId = "clearCountyHighlight",
                 label = "Clear selections",
                 style = "color: #fff; background-color: #D75453; border-color: #C73232"),
               helpText("The counties you have selected will appear below:"),
               textOutput("county_list")
             ),
           ),
           fluidRow(box(plotlyOutput('county_line_plot')),
                    box(dataTableOutput("county_policy_table")),
                    box(textOutput("policy_selected_county"))))
)

server <- function(input, output,session) {
  
  #--------------------------------------------- General Server --------------------------------------------
  
  #Tells Rshiny we have helper buttons
  observe_helpers()
  
  
  #--------------------------------------------- County Server --------------------------------------------
  
 
  
  #---------------------------------------------County Server--------------------------------------------
  
  #---------------------------------------------State Comparison Server--------------------------------------------
  
  
  #filter data function based on the input and return dataframe
  filter_data <- reactive({
    #get the year & measure from main data
    test_data <- state_opioid_data %>% filter(state_opioid_data$Year == input$dropdown_year) %>% select(input$measure,"name")
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
    pal <- colorNumeric("Blues", domain = state_opioid_data[[input$measure]],na.color = "#808080")
    #start leaflet object (using projections defined in the beginning)
    leaflet(spd, options = leafletOptions(crs = epsg2163)) %>%
      #set view (allows us to see USA)
      setView(lng= -95.712891,lat=37.09024, zoom=3) %>%
      #adds legend based on input and palette
      addLegend(pal = pal, values = state_opioid_data[[input$measure]], opacity = 1, title = "Counts",position = "bottomright") %>% 
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
  output$statemap <- renderLeaflet({foundational.map()})
  
  #Observe Click Event ----------------------------------------------------------------------------------------------------
  # store the list of clicked polygons in a vector
  click.list <- reactiveValues(ids = vector())
  #use shinys "observe" to see what is clicked, and get the geometry for those lines so we can draw the blue highlights
  observeEvent(input$statemap_shape_click, {
    spd <- filter_data()
    click <- input$statemap_shape_click
    click.list$ids <- c(click.list$ids, click$id)
    output$list <- renderText(click.list$ids)
    lines.of.interest <- spd[which( spd$name %in% click.list$ids), ]
    #filter state function to get data for *clicked* states (we need this for plotting line graph)
    filter_state_data <- reactive({
      #filter data based on input year and then select measure
      data <-  state_opioid_data %>% filter(name %in% click.list$ids)
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
                               text = paste0(colnames(policy_data)[i]),
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
      leafletProxy( mapId = "statemap" ) %>%
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
    output$statemap <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      foundational.map()})})
  
  #---------------------------------------------State Comparison Server--------------------------------------------
  
  #---------------------------------------------Single State Server--------------------------------------------
  output$ss.graph.title <- renderText({paste(input$dropdown_state_input, " State Metrics Analysis")})
  output$timeline.title <- renderText({paste(input$dropdown_state_input, " State Policy Timeline")})
  
  observeEvent(input$single_state_checkbox, {
    #filter data function based on the input and return dataframe
    filter_single_data <- reactive({
      if (length(input$single_state_checkbox)==0) {
        return(NULL)
      }
      else{
        #get the year & measure from main data
        single_data <- state_opioid_data %>% filter(state_opioid_data$name == input$dropdown_state_input) %>% select(State,Year,input$single_state_checkbox)
        melted <- single_data %>% pivot_longer(cols = input$single_state_checkbox, names_to="measure",values_to="value")
        return(melted)
      }
    })
    
    single.line.graph <- reactive({
      single_data <- filter_single_data()
      if (length(single_data) == 0){
        ggplot()
      }
      else{
        ggplot(single_data,aes(x=Year,y=value))+geom_line()+facet_wrap(~measure,scales="free_y") + theme_bw()
      }
    })
    
    output$single.state.plot <- renderPlot(single.line.graph())
    
  })
  
  timeline_react <- reactive({
    temp <- filter(Policy, Policy$State == input$dropdown_state_input)
    temp <- tibble::rownames_to_column(as.data.frame(t(temp)), "content")
    colnames(temp)[2] <- "start"
    temp$start <- as.Date(temp$start, format = "%m/%d/%Y")
    temp$id <- c(1,2,3,4,5,6,7,8)
    temp <- na.omit(temp)
    return(temp)
  })
  
  output$timeline <- renderTimevis({
    timevis(timeline_react(),showZoom = FALSE)
  })
  
  #Policy Info Box output based on timeline selections, reactive functions needed for when no policy is selected
  message <- reactive({
    if (length(input$timeline_selected) == 0) {
      return(paste(h4("Select a Policy from the Timeline to find out more.")))
    } else{
      return(NULL)
    }
  })
  policy_name <- reactive({
    req(input$timeline_selected)
    return(paste(h4("Selected Policy Date: "), filter(policy_info, id == input$timeline_selected)$name))
  })
  policy_date <- reactive({
    req(input$timeline_selected)
    return(paste("Took place in ", format(filter(timeline_react(), id == input$timeline_selected)$start, "%B %Y")))
  })
  policy_desc <- reactive({
    req(input$timeline_selected)
    return(paste(h4("About the Policy:"), filter(policy_info, id == input$timeline_selected)$policy.desc))
  })
  date_desc <- reactive({
    req(input$timeline_selected)
    return(paste(h4("What does this date mean?"), filter(policy_info, id == input$timeline_selected)$date.desc))
  })
  output$policy.message <- renderText({message()})
  output$ss.policy.name <- renderText({policy_name()})
  output$ss.policy.date <- renderText({policy_date()})
  output$ss.policy.desc <- renderText({policy_desc()})
  output$ss.date.desc <- renderText({date_desc()})
  
  #---------------------------------------------Single State Server--------------------------------------------
  
}

shinyApp(ui, server)