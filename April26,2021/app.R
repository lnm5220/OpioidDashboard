library(maps)
library(maptools)
library(leaflet)    
library(shiny)
library(data.table)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyhelper)
library(tidyr)
library(plotly)
library(dplyr)
library(leaflet.extras)
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
single_state_opioid_data <- fread("./OpioidData.CSV")
state_opioid_data <- fread("./new_state_data.csv")
spdf <- st_read("./spdf.shp")
policy_info <- fread("./policyinfo.csv", quote = "")

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
  temp$id <- c(1,2,3,4,5,6,7,8)
  temp <- na.omit(temp)
  return(temp)
}


ui <- navbarPage(
  id="main",
  "",
  theme = shinytheme("flatly"),
  tabPanel(
    div(
      img(src="penn.png",
          height=30, style="padding: 0px 0px;"), "OPADD"),
    value="a",
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
    fluidPage(
      fluidRow(
        id = "toprow",
        style = "height:300px; background-color: #F9F9F9;",
        br(),
        column(width= 8,
               style = "padding-left: 50px;",
            align = "center",
            tags$h1("Opioid Policy and Data Dashboard (OPADD)", align = "left",style="font-weight:bold"),
            p("In this dashbaord you can analyze metrics about the opioid epidemic.",align="left"),
            br(),
            br()),
        column(width = 3,
            align = "center",
            br(),
            actionLink(
          "gif2",
          div(img(src ="Opioid2.gif", style = "width: 100%; height: 100%;"),
          )))),
      fluidRow(
        column(width = 4,
               offset=2,
               align = "center",
               br(),
               h3("Multi-State Comparison",style="font-weight:bold;"),
               br(),
               p("Use this page to compare metrics about the opioid epidemic between different states.",style="color:#706f6f"),
               br(),
               icon("balance-scale", "fa-10x"),
               br(),
               br(),
               div( style = "padding: 10px 0px 10px 0px;",
                    actionBttn(
                      inputId = "Multi.State.Static",
                      label = "    Launch     "))
               
        ),
        column(width =4, 
               align = "center",
               br(),
               h3("Single-State Analysis",style="font-weight:bold;"),
               br(),
               p('Use this page to choose a state and analyze data about the opioid epidemic in that state.',style="color:#706f6f"),
               br(),
               icon("search", "fa-10x"),
               br(),
               br(),
               div( style = "padding: 10px 0px 10px 0px;",
                    actionBttn(
                      inputId = "Single.State.Static",
                      label = "    Launch    "))
               
        )),
      fluidRow(
        style = "padding-top: 50px;",
        h3("Overview of the Opioid Epidemic", align = "center",style="font-weight:bold;"),
        br(),
        height = "450px",
        column(width = 4, align = "center",offset=2,
               p("The opioid epidemic started to arise in the late 1990s where medical professionals were 
                 prescribing opioid medications to individuals at substantial rates. This was before the Pharmaceutical
                 industry realized that opioids were highly addictive. The misuse of both prescribed and non-prescribed opioids
                 gave rise to overdose deaths at an alarming rate. According to the CDC, since the start of the epidemic, 
                 there have been three waves of opioid-related overdose deaths, where the last two were in 2010 and 2013."
                 ,style="text-align: justify;color:#706f6f")),
        column(width = 4, align = "center",
               p("Data collection and availability have been one of the challenges in understanding the opioid epidemic. 
                 The U.S. Department of Health and Human Services developed a five-point strategy to help communities address
                 these growing issues. One of the points was 'better data 'collection. Several institutions have been recording
                 data on the opioid epidemic; however, it is challenging to run substantial analysis on these measures because
                 different data sources such as hospitalization records and the Prescription Drug Monitoring Program data are
                 stored on various platforms. Since the data is stored on several different sites, the problem of conveniently
                 comparing specific measures arose.",style="text-align: justify;color:#706f6f"))),
      fluidRow(
        h3("Overview of the OPADD Project", align = "center",style="font-weight:bold;"),
        br(),
        height = "450px",
        column(width = 8, align = "center",offset=2,
               p("The Opioid Policy and Data Dashboard (OPADD) was created to solve not having one convenient location where
                 the United States data is compared and evaluated. The dashboard is comprised of seven sources of data and 
                 seven different policy dates and information.  The dashboard focuses on two main functions: Multi-State 
                 Comparison and Single-State Analysis. The Multi-State comparison page allows the user to compare several 
                 states on one metric and see how a policy affected each state in a line graph. The Single-State Analysis 
                 page allows the user to select one state and compare the data of four different metrics at once.  
                 OPADD's goal was to develop a convenient and user-friendly way to understand the opioid epidemic better."
                 ,style="text-align: justify;color:#706f6f"))),
      fluidRow(
        br(),
        br(),
        br(),
      ))),
  tabPanel("Multi-State Comparison", value = "MS",
           tags$head(
             tags$style(HTML(
               "label {font-size:18px;}"))),
           fluidRow(
             column(12, titlePanel(h3("Multi-State Comparison",style=
             'padding-left: 15px')),
             h5("Use this page to compare multiple states at one time using different metrics and see relevant 
             policy information for those selected states. Use the control panel on the left to switch between 
            different metrics to compare and then click states on the map to see more. Use the slider below to
                view different years on the map and press the play button to view the metric over time.",style=
                  'padding-left: 15px; padding-right:15px'),
             br())),
           column(12,
                  sidebarLayout(
                    sidebarPanel(
                      selectInput(inputId="state_measure",
                                  label="Select Measure",
                                  choices=unique(state_opioid_data$Measure),
                                  selected="Crime Incident Related to All Opioid Types") %>% helper(type = "inline",
                                                                                                    title = "What do these variables mean?",
                                                                                                    content = c("<b>Count and Rate of Opioid Overdose Deaths by Natural and Semisynthetic Opioid</b>  refers to the number and rate of overdose deaths related to a category of prescription opioids that includes natural opioid analgesics (e.g. morphine and codeine) and semi-synthetic opioid analgesics (e.g. drugs such as oxycodone, hydrocodone, hydromorphone, and oxymorphone). Source: KFF.org <br>", 
                                                                                                                "<b>Count and Rate Opioid Overdose Deaths by Synthetic Opioids </b> refers to the number and rate of overdose deaths related to a category of opioids including drugs such as tramadol and fentanyl. Synthetic opioids are commonly available by prescription. Fentanyl is legally made as a pharmaceutical drug to treat pain, or illegally made as a non-prescription drug and is increasingly used to intensify the effects (or high) of other drugs, such as heroin.<br>", 
                                                                                                                "<b>Count and Rate of Opioid Overdose Deaths by Methadone</b> refers to the number of overdose deaths related to methadone, a synthetic opioid prescribed to treat moderate to severe pain or to reduce withdrawal symptoms in people addicted to heroin or other narcotic drugs. <br>", 
                                                                                                                "<b>Count and Rate Opioid Overdose Deaths by Heroin </b> refers to the number of overdose deaths related to herion, an illicit (illegally made) opioid synthesized from morphine. <br>", 
                                                                                                                "<b>Count of all Opioid Overdose Deaths </b> refers to the number of deaths caused by all forms of opioids, a class of drugs used commonly to reduce pain. Source: cdc.gov <br>", 
                                                                                                                "<b>Count of all Drug Overdose Deaths</b> refers to the number of overdose deaths caused by any drug.<br>",
                                                                                                                "<b>Opioid Overdose Deaths as Percentage of all Overdose Deaths </b> refers to the percentage of overdose deaths caused by opioids. <br>",
                                                                                                                "<b>Rate of Opioid Overdose Death Per 100,000 (Age Adjusted) </b> refers to opioid overdose death rates per 100,000 peope and adjusted to compensate for populations with vastly different age distributions. <br>",
                                                                                                                "<b>Rate of All Drug Overdose Death Per 100,00 (Age Adjusted)</b> refers to all drug overdose death rates per 100,000 peope and adjusted to compensate for populations with vastly different age distributions. <br>",
                                                                                                                "<b>Count and Rate of Opioid Related Inpatient Hospital Visits </b> refers to the number and rate of which inpatient hospital visits related to opioid use.<br>",
                                                                                                                "<b>Count and Rate of Opioid Related Emergency Department Visits </b> refers to the number and rate of which emergency department hospital visits related to opioid use.<br>",
                                                                                                                "<b>Rate of Opioid Prescription Per 100 Population</b> refers to the rate of opioids being prescribed per 100 people. <br>",
                                                                                                                "<b>Count of Crime Incident Related to Any Drug Type</b> refers to the number of crime incidents related to any drug. <br>",                          
                                                                                                                "<b>Count of Crime Incident Related to Heroin</b> refers to the number of crime incidents related to heroin. <br>",                             
                                                                                                                "<b>Count of Crime Incident Related to Narcotics</b> refers to the number of crime incidents related to narcotics, drugs or other substances that affect mood or behavior and is consumed for nonmedical purposes.<br>",                       
                                                                                                                "<b>Count of Crime Incident Related to All Types of Opioid</b> refers to the number of crime incidents related to opioids. <br>",                    
                                                                                                                "<b>Count of Crime Incident Related to Cocaine</b> refers to the number of crime incidents related to cocaine.<br>",                         
                                                                                                                "<b>Count of Crime Incident Related to Heroin Morphine And Opium</b> refers to the number of crime incidents related to heroin, morphine, and opium. <br>",               
                                                                                                                "<b>Count of Crime Incident Related to Other Opioid</b> refers to the number of crime incidents related to other opioids.")),
                      uiOutput("unit_choice"),
                      sliderInput("dropdown_year", "Select Year",min = 2005, width="100%", max = 2018, value = 2018, step=1, sep = "", animate=TRUE),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      tags$style(HTML(".js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #00006900; border-top: 1px solid #00003900; border-bottom: 1px solid #00003900 ;}")),
                      tags$style(HTML(".js-irs-0 .irs-single { font-size: 12px; font-weight: bold; color: #2C3E50; background: #FFFFFF }")),
                      tags$style(HTML(".js-irs-0 .irs-min, .js-irs-0 .irs-max { font-size: 12px; color: #FFFFFF; background: #2C3E50}")),
                      tags$style(HTML(".js-irs-0 .irs-grid-pol.small { display: none;}")),
                      tags$style(HTML(".js-irs-0 .irs-grid-text { font-size: 10px;}")),
                      h4("Select State(s)"),
                      p("Click on the map on the right panel to select up to 4 states you want to compare.
                        The States you have selected are:"),
                      textOutput("list"),
                      br(),
                      actionButton(
                        inputId = "clearHighlight",
                        label = "Clear selections")),
                    mainPanel(
                      tags$head(
                        tags$style(HTML(".leaflet-container { background: #f2f2f2; }"))
                      ),
                      leafletOutput(
                        outputId = "statemap"),
                      h4(),
                      h3("Compare Trends between States"),
                      p("Click states on the above map to see the selected measure for each state in the below line graph."),
                      tags$head(tags$style("#container * {  display: inline; }")),
                      div(id="container",p('Selected States:'), textOutput("selected_state_note")),
                      div(id="container",p('Selected Measure:'), textOutput("selected_measure_note")),
                      plotlyOutput("plot"),
                      h3("Key Dates of Opioid-Related Policies"),
                      p("The below table summarizes the key dates of opioid-related policies implemented in the selected states."),
                      p("Click on the policy name in any row of the below table to display its timing in the above line graph."),
                      p("Abbreviation: PDMP, Prescription Drug Monitoring Program."),
                      dataTableOutput("policy_table"))
                  )
           )),
  tabPanel("Single-State Analysis", value = "SS",
           fluidRow(
             column(12, titlePanel(textOutput("ss.graph.title")))),
           column(12, offset = 0,
                  sidebarLayout(
                    sidebarPanel(
                      selectInput("dropdown_state_input", selected = "Alabama","Select State", c("Select a State" = "", Policy$State)),
                      pickerInput("single_state_checkbox", "Select Metrics to compare (You may select up to 4)",selected="Opioid Overdose Deaths by Natural and Semisynthetic Opioid",multiple=TRUE,
                                  options = list("max-options" = 4, "max-options-text" = "A maximum of 4 metrics are able to be selected.", header = "Measures"),
                                  choices = list(    `Overdose Deaths` = c(
                                    "Opioid Overdose Deaths by Natural and Semisynthetic Opioid",             
                                    "Opioid Overdose Deaths by Synthetic Opioids",
                                    "Opioid Overdose Deaths by Methadone",                                                        
                                    "Opioid Overdose Deaths by Heroin",                                                          
                                    "All Opioid Overdose Deaths",                                                                    
                                    "All Drug Overdose Deaths",                                                    
                                    "Opioid Overdose Deaths as Percentage of all Overdose Deaths",                   
                                    "Opioid Overdose Death Rate Per 100,000 (Age Adjusted)",
                                    "All Drug Overdose Death Rate Per 100,00 (Age Adjusted)",
                                    "Rate of Opioid Overdose Deaths (Natural & Semisynthetic)",                             
                                    "Rate of Opioid Overdose Deaths (Synthetic Opioids)",                          
                                    "Rate of Opioid Overdose Deaths (Methadone)",                                                   
                                    "Rate of Opioid Overdose Deaths (Herion)"),
                                    `Hospital Use` = c(
                                      "Rate of Opioid Related Inpatient Hospital Visits",                                                                                     
                                      "Rate of Opioid Related Emergency Department Visits",                                                                                  
                                      "Counts of Opioid Related Inpatient Hospital Visits",                                                                           
                                      "Counts of Opioid Related Emergency Department Visits",
                                      "Opioid Prescription Rate Per 100 population"),
                                    `Crime Incidents` = c(
                                      "Number of Crime Incident Related to Any Drug Type",                          
                                      "Number of Crime Incident Related to Heroin",                             
                                      "Number of Crime Incident Related to Narcotics",                       
                                      "Number of Crime Incident Related to All Types of Opioid",                    
                                      "Number of Crime Incident Related to Cocaine",                         
                                      "Number of Crime Incident Related to Heroin Morphine And Opium",               
                                      "Number of Crime Incident Related to Other Opioid")))  %>% helper(type = "inline",
                                                                                                        title = "What do these variables mean?",
                                                                                                        content = c("<b>Count and Rate of Opioid Overdose Deaths by Natural and Semisynthetic Opioid</b>  refers to the number and rate of overdose deaths related to a category of prescription opioids that includes natural opioid analgesics (e.g. morphine and codeine) and semi-synthetic opioid analgesics (e.g. drugs such as oxycodone, hydrocodone, hydromorphone, and oxymorphone). Source: KFF.org <br>", 
                                                                                                                    "<b>Count and Rate Opioid Overdose Deaths by Synthetic Opioids </b> refers to the number and rate of overdose deaths related to a category of opioids including drugs such as tramadol and fentanyl. Synthetic opioids are commonly available by prescription. Fentanyl is legally made as a pharmaceutical drug to treat pain, or illegally made as a non-prescription drug and is increasingly used to intensify the effects (or high) of other drugs, such as heroin.<br>", 
                                                                                                                    "<b>Count and Rate of Opioid Overdose Deaths by Methadone</b> refers to the number of overdose deaths related to methadone, a synthetic opioid prescribed to treat moderate to severe pain or to reduce withdrawal symptoms in people addicted to heroin or other narcotic drugs. <br>", 
                                                                                                                    "<b>Count and Rate Opioid Overdose Deaths by Heroin </b> refers to the number of overdose deaths related to herion, an illicit (illegally made) opioid synthesized from morphine. <br>", 
                                                                                                                    "<b>Count of all Opioid Overdose Deaths </b> refers to the number of deaths caused by all forms of opioids, a class of drugs used commonly to reduce pain. Source: cdc.gov <br>", 
                                                                                                                    "<b>Count of all Drug Overdose Deaths</b> refers to the number of overdose deaths caused by any drug.<br>",
                                                                                                                    "<b>Opioid Overdose Deaths as Percentage of all Overdose Deaths </b> refers to the percentage of overdose deaths caused by opioids. <br>",
                                                                                                                    "<b>Rate of Opioid Overdose Death Per 100,000 (Age Adjusted) </b> refers to opioid overdose death rates per 100,000 peope and adjusted to compensate for populations with vastly different age distributions. <br>",
                                                                                                                    "<b>Rate of All Drug Overdose Death Per 100,00 (Age Adjusted)</b> refers to all drug overdose death rates per 100,000 peope and adjusted to compensate for populations with vastly different age distributions. <br>",
                                                                                                                    "<b>Count and Rate of Opioid Related Inpatient Hospital Visits </b> refers to the number and rate of which inpatient hospital visits related to opioid use.<br>",
                                                                                                                    "<b>Count and Rate of Opioid Related Emergency Department Visits </b> refers to the number and rate of which emergency department hospital visits related to opioid use.<br>",
                                                                                                                    "<b>Rate of Opioid Prescription Per 100 Population</b> refers to the rate of opioids being prescribed per 100 people. <br>",
                                                                                                                    "<b>Count of Crime Incident Related to Any Drug Type</b> refers to the number of crime incidents related to any drug. <br>",                          
                                                                                                                    "<b>Count of Crime Incident Related to Heroin</b> refers to the number of crime incidents related to heroin. <br>",                             
                                                                                                                    "<b>Count of Crime Incident Related to Narcotics</b> refers to the number of crime incidents related to narcotics, drugs or other substances that affect mood or behavior and is consumed for nonmedical purposes.<br>",                       
                                                                                                                    "<b>Count of Crime Incident Related to All Types of Opioid</b> refers to the number of crime incidents related to opioids. <br>",                    
                                                                                                                    "<b>Count of Crime Incident Related to Cocaine</b> refers to the number of crime incidents related to cocaine.<br>",                         
                                                                                                                    "<b>Count of Crime Incident Related to Heroin Morphine And Opium</b> refers to the number of crime incidents related to heroin, morphine, and opium. <br>",               
                                                                                                                    "<b>Count of Crime Incident Related to Other Opioid</b> refers to the number of crime incidents related to other opioids.")),
                      h4("Selected Metrics: "),
                      htmlOutput("ss.selected")
                    ),
                    mainPanel(
                      h5("Use this page to compare multiple different metrics over time for one state and see relevant policy information for the selected state. 
                Use the control panel on the left to switch between states and select different metrics to compare."),
                      plotlyOutput("single.state.plot"),
                      htmlOutput("timeline.title"),
                      h5("The timeline below shows important dates for state-level policies. To see more specific information about the policy and date, click on an event in the timeline and the relevant details will appear below. To zoom in or out use the buttons in the top right, to adjust your view click and drag on the timeline."),
                      timevisOutput("timeline"),
                      htmlOutput("policy.message"),
                      htmlOutput("ss.policy.name"),
                      htmlOutput("ss.policy.date"),
                      htmlOutput("ss.policy.desc"),
                      htmlOutput("ss.date.desc"))
                  )
           )),
  navbarMenu("More",
  tabPanel("Sources",
           box(title = NULL, width = 12,
               h1("OPADD Sources & Citations", align = 'center'),
               p(HTML("The OPADD Team used a variety of sources in the creation of this dashboard. The team has provided a downloadable pdf organized with citations for background information, policy data, and measure specific data. Click the button below to view a full list of these sources.", align = 'center')),
               hr(),
               fluidRow(
                 align = 'center',
                 tags$h2("PDF Download", align = 'center'),
                 tags$a("Download our sources here",href="References.pdf")
               ))),
  tabPanel("About Us",
           value = "b",
           h1("Meet the Team", align = "center"),
           p("Under the advisory of Dr.Chen, a team of seven passonate undergraduates from different majors collaborated to create an online Opioid dashboard for their capstone project. The team worked in groups, Alexander Weaver was the Project Manger, Leah Miller, Andrew Briglia, and John Koons collaborated to implement frontend whereas Benson Wainaina, Zixuan Feng and Kelly Cooper focused on backend. ", align = "center"),
           fluidRow(
             column(width = 4,
                    align = "center",
                    img(src="Ally.png", width=250, height = "250px")),
             column(width = 8, 
                    tags$h3("Alexandra Weaver", align = "center"),
                    p("Ally is a senior in Industrial Engineering currently attending Penn State University. After graduation, she plans to work for Teledyne Brown Engineering as an Operations Research Analyst, making process improvements using discrete event simulation for naval field hospitals. In her free time, she is a dog mom to a very sassy Australian shepherd mix named Harley. "))),
           fluidRow(
             column(width = 4,
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Leah.png", width=250, height = "250px")),
             column(width = 8, 
                    tags$h3("Leah Miller", align = "center"),
                    p("Leah Miller is senior Applied Data Sciences student at Penn State with a minor in Bioethics & Medical Humanities. Upon graduation, she will be working at Eli Lilly as an IT Analyst to continue fulfilling her passion of discovering the intersection of healthcare and technology. In her free time, Leah enjoys doing calligraphy and bullet journaling. "))),
           fluidRow(
             column(width = 4, 
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Jack.png", width=250, height = "250px")),
             column(width = 8, 
                    tags$h3("John Koons", align = "center"),
                    p("John Koons is a senior majoring in Industrial Engineering at Penn State. His experience includes two internships working in manufacturing engineering with a focus on data analysis and process improvement. After graduation, John will begin working for Technomics, Inc. in Arlington, Virginia as a consultant where he will be working on data analytics and cost estimation projects for the United States Government. In his free time John enjoys running and even ran the Philadelphia Marathon during his freshman year at Penn State."))),
           fluidRow(
             column(width = 4, 
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Andrew.png", width=250, height = "300px")),
             column(width = 8,
                    tags$h3("Andrew Briglia", align = "center"),
                    p("Andrew Briglia is an Industrial Engineering student with a minor in Entrepreneurship & Innovation from the Penn State College of Engineering. Andrew’s primary experience comes from process improvement and supply chain management internships in the high tech and pharmaceutical industry where he helped reroute supply chains and create dashboards to track long-term forecasts and KPIs for major product lines. Upon graduation, he will be working at Merck in their Manufacturing Leadership Development Program, pursuing his passion for data analytics and business development. One fun fact about Andrew is that he is starting a Sports Data Management business! Go check it out at"))
           ))),
  tags$footer(HTML('
                    <!-- Footer -->
                           <footer>
                           <div>
                           <img src="https://www.tctmagazine.com/downloads/8935/download/Penn%20State%20Engineering%20logo.png?cb=25e47b9becf674b18fbdfdf0cf264c62"
                           width = "175"
                           height = "40"
                           left = "100" alt="IMG Title" /> 
                           <h3>Penn State IME</h3>
                   </div>
                     <p><small><small><small></small>The OPADD Team has built this dashboard in conjunction with the Penn State College of Industrial Engineering in order to better analyze and asses the severity of the Opioid Epidemic and policies in place to reduce its effect. All information and data in this project was pulled from a variety of sources. Please see the "Sources" page for more information on where we pulled our data from.</small></small></small></p>
                     <p class="copyright">Penn State College of Engineering © 2021</p>
                     </footer>
                           <!-- Footer -->'))
)

server <- function(input, output,session) {
  
  #--------------------------------------------- General Server --------------------------------------------
  
  #Tells Rshiny we have helper buttons
  observe_helpers()
  
  observeEvent(input$Multi.State.Static, {
    updateTabsetPanel(session, "main",
                      selected = "MS")
  })
  
  observeEvent(input$Single.State.Static, {
    updateTabsetPanel(session, "main",
                      selected = "SS")
  })
  
  observeEvent(input$gif1, {
    updateTabsetPanel(session, "main",
                      selected = "SS")
  })
  
  observeEvent(input$gif2, {
    updateTabsetPanel(session, "main",
                      selected = "SS")
  })
  #---------------------------------------------State Comparison Server--------------------------------------------
  
  observeEvent( input$state_measure,{
    df <-  state_opioid_data %>% filter(state_opioid_data$Measure ==input$state_measure)
    output$unit_choice <- renderUI({
      radioButtons(inputId="unit_choice",
                  label="Select Unit",
                  choices=unique(df$Unit))
    })
    
  })

  #filter data function based on the input and return dataframe
  filter_data <- reactive({
    req(input$unit_choice)
    #get the year & measure from main data
    test_data <- state_opioid_data %>% filter(state_opioid_data$Year == input$dropdown_year & state_opioid_data$Measure ==input$state_measure & state_opioid_data$Unit == input$unit_choice) %>% select("Value","name","State")
    #make spatial dataframe & drop unnessescay columns & add our target measure to the df
    spdf <- merge(spdf,test_data,by="name",all.x=FALSE,all.y=FALSE)
    spd <- as_Spatial(st_geometry(spdf), IDs = as.character(1:nrow(spdf))) #make is spatial using sf package
    df <- spdf #back to df & get rid of geometry
    df$geometry <- NULL
    df <- as.data.frame(df)
    spd <- SpatialPolygonsDataFrame(spd, data = df)
    return(spd)
  })
  
  #Foundational heat map Function
  foundational.map <- reactive({
    #call filter
    spd <- filter_data()
    #set palette based on input measure
    pal <- colorNumeric("Blues", domain = spd$Value,na.color = "#808080")
    #start leaflet object (using projections defined in the beginning)
    if (grepl("Rate",input$unit_choice)){
      legend.title="Rate"
    }
    else if (grepl("Count",input$unit_choice)){
      legend.title="Count"
    }
    else{
      legend.title="Percentage"
    }
    map <- leaflet(spd, options = leafletOptions(crs = epsg2163)) %>%
      #set view (allows us to see USA)
      setView(lng= -95.712891,lat=37.09024, zoom=3) %>%
      addLegend(pal = pal, values = spd$Value, opacity = 1, title = legend.title,position = "bottomright") %>% 
      #actually adds the states, layerid refers to the hover info associated with each shape, in our case its state name
      addPolygons(weight = 1,
                  layerId = ~name,
                  color = "#444444",
                  opacity = 1,
                  fillColor = ~pal(spd$Value),
                  fillOpacity = 0.7,
                  smoothFactor = 0.5,
                  label = ~paste(name, spd$Value),
                  labelOptions = labelOptions(direction = "auto"),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = FALSE)) %>%
      addLabelOnlyMarkers(lng = coordinates(spd)[,1], lat = coordinates(spd)[,2], label = spd$i_3166_,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'center', textOnly = TRUE))
    return(map)
    })
  
  #Render heat graph map
  output$statemap <- renderLeaflet({foundational.map()})
  
  #Observe Click Event
  # store the list of clicked polygons in a vector
  click.list <- reactiveValues(ids = vector())
  #use shinys "observe" to see what is clicked, and get the geometry for those lines so we can draw the blue highlights
  observeEvent(input$statemap_shape_click, {
    spd <- filter_data()
    click <- input$statemap_shape_click
    if (isTruthy(is.element(click$id,click.list$ids))){
      clicked <- list(click$id)
      click.list$ids <-click.list$ids[!click.list$ids %in% clicked]
      leafletProxy(mapId = "statemap" ) %>%
        clearGroup("polylines")
    }
    else{
      click.list$ids <- c(click.list$ids, click$id)
    }
    #click.list$ids <- unique(click.list$ids)
    output$list <- renderText(paste(click.list$ids, collapse = ', '))
    output$selected_state_note <- renderText(paste(click.list$ids, collapse = ', '))
    output$selected_measure_note <- renderText(paste(input$state_measure,input$unit_choice))
    lines.of.interest <- spd[which( spd$name %in% click.list$ids), ]
    #filter state function to get data for *clicked* states (we need this for plotting line graph)
    filter_state_data <- reactive({
      #filter data based on input year and then select measure
      data <-  state_opioid_data %>% filter(name %in% click.list$ids & Measure == input$state_measure & Unit == input$unit_choice)
    })
    
    #filter the data for clicked states so we can acce
    st_data <- filter_state_data()
    
    #filter policy data
    filter_policy <- reactive({
      if (length(click.list$ids) == 1) 
      {policy_table <- data.table(Policy)
      data_wide <- data.frame(t(policy_table))
      data_wide <- data_wide %>%row_to_names(row_number = 1)
      row_names <- c(row.names(data_wide))
      policy_data <- data.frame(data_wide[,click.list$ids],row.names=row_names)
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
      if (grepl("Rate",input$unit_choice)){
        axis.title="Rate"
      }
      else if (grepl("Count",input$unit_choice)){
        axis.title="Count"
      }
      else{
        axis.title="Percentage"
      }
      y <- list(title = axis.title)
      plot_ly(st_data, x = ~Year,y = ~Value,color = ~State, type = 'scatter', mode = 'lines', name = ~State, connectgaps = FALSE) %>% 
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
    
    output$policy_table <- DT::renderDataTable(policy_data, server = TRUE, selection='single',options = list(dom = 't'))
    
    output$SelectPolicy <- renderText(paste0(h4("Select a policy from the table to visualize the dates on the line graph.")))
    
    blank.graph <- reactive({
      fig <- plotly_empty(type = "scatter", mode = "markers") %>%
        config(displayModeBar = FALSE) %>%
        layout(title = list(text = "Click states in order to see the line graph.", yref = "paper", y = 0.5))
      return(fig)})
    
    observeEvent(input$policy_table_rows_selected,ignoreNULL = FALSE,{
      if (is.null(input$policy_table_rows_selected) && is.null(click.list$ids)==FALSE){
        output$plot <- renderPlotly({line.graph()})}
      else if(is.null(click.list$ids)) {
        output$plot <- renderPlotly({blank.graph()})}
      else{
        annotations = list()
        shapes = list()
        states = names(policy_data)
        for (i in 1:ncol(policy_data)){
          if (policy_data[input$policy_table_rows_selected,i] != "") {
            date = policy_data[input$policy_table_rows_selected,i]
            year = substr(date, nchar(date) - 3, nchar(date))
            annotation <- list(x = year,
                               y = max(st_data$Value,na.rm=TRUE)-(max(st_data$Value,na.rm=TRUE)/ncol(policy_data))*(i-1),
                               text = paste0(colnames(policy_data)[i]),
                               showarrow = FALSE)
            shape <- list(type='line',
                          x0=year,
                          x1=year,
                          y0=0,
                          y1=max(st_data$Value,na.rm=TRUE),
                          line=list(dash='dot', width=1)) 
            annotations[[i]] <- annotation
            shapes[[i]] <- shape}
          
          annotated.graph <- reactive({
            y <- list(title = "Count")
            plot_ly(st_data, x = ~Year,y = ~Value,
                    color = ~State, type = 'scatter', mode = 'lines',
                    name = ~State, connectgaps = FALSE) %>% 
              layout(paper_bgcolor="#FFFFFF",plot_bgcolor="#FFFFFF",yaxis=y,annotations=annotations, shapes=shapes)
          })
          output$plot <- renderPlotly({annotated.graph()})
        }
    }
      })
    
    
      leafletProxy( mapId = "statemap" ) %>%
        addPolylines(data = lines.of.interest,
                     group='polylines',
                     layerId = lines.of.interest@data$id,
                     color = "#BDFFFF",
                     weight = 5,
                     opacity = 1)
    
  })  # End Click observation 
  
  # Observe clear highlight button & reset everything
  observeEvent( input$clearHighlight, {
    #render blank line graph
    blank.graph <- reactive({
      fig <- plotly_empty(type = "scatter", mode = "markers") %>%
        config(displayModeBar = FALSE) %>%
        layout(title = list(text = "Click states in order to see the line graph.", yref = "paper", y = 0.5))
      return(fig)})
    output$plot <- renderPlotly(blank.graph())
    policy_data <- data.table()
    output$policy_table <- DT::renderDataTable(policy_data,options = list(dom = 't'), server = TRUE, selection='single')
    output$SelectPolicy <- renderText("")
    output$statemap <- leaflet::renderLeaflet({
      click.list$ids <- NULL
      foundational.map()})})
  
  #---------------------------------------------State Comparison Server--------------------------------------------
  
  #---------------------------------------------Single State Server--------------------------------------------
  output$ss.graph.title <- renderText({paste(input$dropdown_state_input, "State Data and Policy Analysis")})
  output$timeline.title <- renderText({paste(h3(input$dropdown_state_input, " State Policy Timeline:"))})
  output$ss.selected <- renderUI({
    HTML(paste(input$single_state_checkbox, '<br/>'))
  })
  
  observeEvent(input$single_state_checkbox, {
    #filter data function based on the input and return dataframe
    filter_single_data <- reactive({
      if (length(input$single_state_checkbox)==0) {
        return(NULL)
      }
      else{
        #get the year & measure from main data
        single_data <- single_state_opioid_data %>% filter(single_state_opioid_data$name == input$dropdown_state_input) %>% select(State,Year,input$single_state_checkbox)
        melted <- single_data %>% pivot_longer(cols = input$single_state_checkbox, names_to="measure",values_to="value")
        return(melted)
      }
    })
    
    single.line.graph <- reactive({
      single_data <- filter_single_data()
      if (length(single_data)==0){
        return(ggplotly(ggplot()))
      }else{
        plot <- ggplotly(ggplot(single_data,aes(x=Year,y=value))+geom_line()+expand_limits(y = 0)+ theme_minimal()+facet_wrap(~measure,scales="free_y",labeller =labeller(measure = label_wrap_gen(35)))+theme(plot.margin = margin(1, 1, 1, 1, "cm"),panel.spacing.y = unit(2, "lines"),strip.background = element_blank(),strip.text.x = element_text(margin = margin(2, 0, 2, 0),size=12)))
        plot <- plot %>% layout(margin=list(t = 75))
        return(plot)
      }
    })
    
    output$single.state.plot <- renderPlotly(single.line.graph())
    
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
    timevis(timeline_react(),showZoom = TRUE, options = list(showCurrentTime = FALSE, zoomKey = 'ctrlKey'))
  })
  
  rv <- reactiveValues(selected = NULL)
  
  observe( {
    rv$selected <- input$timeline_selected
  })
  
  observeEvent(input$dropdown_state_input, {
    rv$selected <- NULL
  })
  
  
  message <- reactive({
    if (is.null(rv$selected)) {
      return(paste(h4("Select a Policy from the Timeline to find out more.")))
    } else{
      return(NULL)
    }
  })
  output$policy.message <- renderUI({HTML(message())})
  
  policy_name <- reactive({
    if (!is.null(rv$selected)) {
      return(paste(h4("Selected Policy: "),policy_info[id==input$timeline_selected,name]))
    } else{
      return(NULL)
    }
  })
  
  policy_date <- reactive({
    if (!is.null(rv$selected)) {
      df = timeline_react()
      return(paste(h4("Took place in: "), format(df[input$timeline_selected,2],"%B %Y")))
    } else{
      return(NULL)
    }
  })
  
  policy_desc <- reactive({
    if (!is.null(rv$selected)) {
      return(paste(h4("About the Policy: "),policy_info[id==input$timeline_selected,policy.desc]))
    } else{
      return(NULL)
    }
  })
  
  date_desc <- reactive({
    if (!is.null(rv$selected)) {
      return(paste(h4("What does this date mean?"), policy_info[id==input$timeline_selected,date.desc]))
      
    } else{
      return(NULL)
    }
  })
  
  output$ss.policy.name <- renderUI({HTML(policy_name())})
  output$ss.policy.date <- renderUI({HTML(policy_date())})
  output$ss.policy.desc <- renderUI({HTML(policy_desc())})
  output$ss.date.desc <- renderUI({HTML(date_desc())})
  

  #---------------------------------------------Single State Server--------------------------------------------
  
}

shinyApp(ui, server)