library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

if (interactive()) {
ui <- navbarPage(

      windowTitle="Home Page",
      selected="a",
      fluid=TRUE,
      tags$style(type="text/css", "body {padding-top: 0px;}"),
      id="mainNavbarPage",
      tags$style(HTML("#toprow {
          background-color: light-blue;
          padding: -30px -30px -30px -30px;
      }")),

  tabPanel(
        div(
        img(src="penn.png",
            height=30, style="padding: 0px 0px;"), "OPADD"),
    value="a",
    fluidPage(
      fluidRow(
        id = "toprow",
        tags$h3("Welcome to the Opioid & Data Dashboard (OPADD)", align = "center"),
        column(width = 6,
            align = "center",
            p("Multi-State Comparison", style = "font-size: large;"),
            icon("balance-scale", "fa-10x"),
            div( style = "padding: 10px 0px 10px 0px;",
                actionBttn(
                  inputId = "Multi.State.Static",
                  label = "START",
                  color = "success"))

               ),
        column(width =6, 
            align = "center",
            p("Single-State Analysis", style = "font-size: large;"),
            icon("search", "fa-10x"),
            div( style = "padding: 10px 0px 10px 0px;",
                 actionBttn(
                   inputId = "Single.State.Static",
                   label = "START",
                   color = "success"))

        )),

    fluidRow(
      style = "padding-top: 50px;",
      h3("Opioid Overview", align = "center"),
      height = "450px",
      column(width = 6, align = "left",
      p("The opioid epidemic started to arise back in the late 1990s where medical professionals were prescribing opioid medication to individuals at a substantial rate. This was before the pharmaceutical industry realized that opioids were highly addictive. The misuse of both prescribed and non-prescribed opioids gave rise to overdose deaths at an alarming rate. According to the CDC, since the start of the epidemic, there have been three waves of the opioid related overdose deaths with the last two being in 2010 and 2013.")),
      column(width = 6, align = "left",
      p("Data collection and availability has been one of the challenges in understanding the opioid epidemic. The US department of Health and Human Services developed a 5-point strategy to help communities address this growing issue. One of these points was ‘better data’ collection. Several institutions have been recording data on the opioid epidemic; however, it is difficult to run substantial analysis on these measures because different data sources such as hospitalization records and PDMP data are stored on different platforms, meaning there is no resource to compare certain measures conveniently."))
    ),
    fluidRow(
            style = "padding-top: 50px;",
            column(6,
                   align = "center",
                   actionLink(
                     "gif1",
                  div(img(src ="Opioid.gif", style = "width: 80%; height: auto;"),
                      ))),
            column(6,
                   align = "center",
                   actionLink(
                     "gif2",
                  div(img(src ="Opioid2.gif", style = "width: 80%; height: auto;"),
                      )))),
    fluidRow(
      style = "padding-top: 50px;",
      h3("OPADD Project Summary", align = "center"),
      height = "450px",
      column(width = 6, align = "left",
             p("The opioid epidemic started to arise back in the late 1990s where medical professionals were prescribing opioid medication to individuals at a substantial rate. This was before the pharmaceutical industry realized that opioids were highly addictive. The misuse of both prescribed and non-prescribed opioids gave rise to overdose deaths at an alarming rate. According to the CDC, since the start of the epidemic, there have been three waves of the opioid related overdose deaths with the last two being in 2010 and 2013.")),
      column(width = 6, align = "left",
             p("Data collection and availability has been one of the challenges in understanding the opioid epidemic. The US department of Health and Human Services developed a 5-point strategy to help communities address this growing issue. One of these points was ‘better data’ collection. Several institutions have been recording data on the opioid epidemic; however, it is difficult to run substantial analysis on these measures because different data sources such as hospitalization records and PDMP data are stored on different platforms, meaning there is no resource to compare certain measures conveniently."))
    ))),
  navbarMenu("More",
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
                    ),

           fluidRow(
             column(width = 4, 
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Kelly.png", width=250, height = "250px")),
             column(width = 8,
                     tags$h3("Kelly Cooper", align = "center"),
                     p("Kelly Cooper is a senior at Penn State University, double majoring in Mathematics and Applied Data Science. Kelly has had internship experiences in UX/UI design, data analytics, as well as digital marketing. She enjoys problem solving and has a passion for innovation. Upon graduation, she will be joining Amazon Operations as a full-time Area Manager. Aside from academics, Kelly is a licensed esthetician who enjoys yoga and playing piano."))),

           fluidRow(
             column(width = 4, 
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Zixuan.png", width=250, height = "250px")),
             column(width = 8, 
                    tags$h3("Zixuan Feng", align = "center"),
                    p("Zixuan Feng is a senior Industrial Engineering student at Penn State. He has broad experience in large-scale data handling with R and implementing the fixed effect model. Since his freshman year, Zixuan has worked as a teaching assistant and teaching intern within and outside the Penn State IME department. He also has three research assistants' experience in Penn State and two industry internship experiences in Petro China.  After his graduation, Zixuan will continue his graduate study in the Penn State IME department to pursue his Ph.D. degree in Industrial Engineering, focusing on operations research."))),
          fluidRow(
             column(width = 4, 
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Benson.png", width=250, height = "250px")),
              column(width = 8,
                     tags$h3("Benson Wainaina", align = "center"),
                     p("Benson Wainaina is a senior currently pursuing a major in Computational Data Science. He has experience in building APIs, data analysis using R and database management using SQL. Through the Nittany AI challenge, hosted annually by Penn State, he utilized his skills to build an API that helps learners access course material. The team, Nyansapo, which he was part of won the competition last year. While not busy staring at code, Benson can be found playing FIFA with friends."))),
           fluidRow(
             column(width = 4, 
                    align = "center",
                    div(style = "padding:5px"),
                    img(src="Dr.Chen.png", width=250, height = "250px")),
             column(width = 8,
                     tags$h3("Qiushi Chen (陈秋实), PhD", align = "center"),
                     p("Prior to joining Penn State, l worked as post-doctoral fellow at Institute for Technology Assessment in Massachusetts General Hospital (MGH-ITA) in 2017. I received my PhD degree in Operations Research in 2016 from the School of Industrial and Systems Engineering, Georgia Institute of Technology. I received my bachelor’s degree in Industrial Engineering from Tsinghua University, China, in 2010.")))
             )))

server <- function(input, output, session) {
  observeEvent(input$Multi.State.Static, {
    updateNavbarPage(session, "mainNavbarPage", "b")})
  observeEvent(input$Single.State.Static, {
    updateNavbarPage(session, "mainNavbarPage", "b")})
  observeEvent(input$gif1, {
    updateNavbarPage(session, "mainNavbarPage", "b")})
  observeEvent(input$gif2, {
    updateNavbarPage(session, "mainNavbarPage", "b")})
  observeEvent(input$pennstatelogo, {

  })
}
shinyApp(ui, server)
}