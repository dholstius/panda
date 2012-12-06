library(shiny)
library(panda)

shinyUI(pageWithSidebar(

  headerPanel("PANDA Log Visualizer"),

  sidebarPanel(
    fileInput("filename", "Input file. May take a minute to load:", multiple = FALSE, accept = ".TXT"),
    selectInput("tz", "Time Zone:",
                    list("Asia/Chongqing" = "Asia/Chongqing", 
                         "UTC" = "UTC")),
    textInput("start", "Starting date (YYYY-MM-DD)"),
    textInput("finish", "Ending date (YYYY-MM-DD)"),
    
selectInput("major_breaks", "Major breaks:",
  list("1 day" = "1 day", 
       "12 hours" = "12 hours",
       "3 hours" = "3 hours",
       "1 hour" = "1 hour")),
       
selectInput("minor_breaks", "Minor breaks:",
    list("1 hour" = "1 hour",
         "15 minutes" = "15 mins"))
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    tabsetPanel(
        tabPanel("History", 
            plotOutput("tsPlot")
        ), 
        tabPanel("Tables", 
            h3("Daily means"),
            tableOutput("dailyMeansTable")
        )
    )
  )
  
))
