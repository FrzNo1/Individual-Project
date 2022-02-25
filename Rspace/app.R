library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)

# Import data
# main_data <- read.table('traffic_violaions.csv', "rt")

main_table <- traffic_violaions.csv
# Tidy the data



# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # App title ----
  titlePanel("Visualization and Analysis of Traffic Violations"),
    
  # Different tabs
  navbarPage("Let's explore",
             
             # tab1: general
             tabPanel(icon("home"),

             ),

             
             # tab2: Analysis of age
             tabPanel("Ages",
                      # General introduction and analysis part
                      fluidRow(column(width=2),
                               column(
                                 h3(p("How ages are related to Traffic Violations",style="color:black;text-align:center")),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      br(),
                      fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                               column(
                                 p("This section analyze the relationship between ages and traffic
                                 violations. It includes two parts: "),
                                 p("The first part shows how 
                                 ages are related to traffic violations in general 
                                 Generally, we find out that "),
                                 p("The second
                                 part shows how ages are related to different categories of traffic
                                 violation. ",style="color:black;text-align:justify"),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # First sector part
                      h4(p(em("Age vs Traffic Violations"),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                      fluidRow(
                        plotOutput(outputId = "tab2_ageVsVio")
                        
                      )
             ),
    
  )
    
  

)


# Define Server

inFile <- traffic_violaions.csv

server <- function(input, output) {
  output$RawData <- renderDataTable(
    datatable({
      inFile
    },
    ))
  
  output$tab2_ageVsVio <- renderPlot({
    
    hist(driver_age)
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
