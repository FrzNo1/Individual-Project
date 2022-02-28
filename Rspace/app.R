library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)

# Import data
main_data <- read.csv('traffic_violaions.csv')
### import the drivers' ages EXCEL



# Tidy the data



# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # App title ----
  titlePanel("Visualization and Analysis of Traffic Violations in the U.S."),
    
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
                                 p("The first part shows one bar chart about how  
                                 ages are related to violations within all violations and another bar
                                 chart shows how ages are related to driving in the U.S. Basicly, we
                                 find out that within all violations, age range of 20-24 constitute the
                                 highest violation percentage, and decrease as age range increases. For
                                 drving in the U.S., age range of 20-59 contitutes majority of 
                                 driving populations.
                                 "),
                                 p("The second part conbines the provious two bar charts to derive the relationship
                                   between ages and traffic violations in general within the U.S.. We can easily see
                                   that "),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # First sector part
                      h4(p(em("Age in Traffic Violations & Driving Pattern in U.S."),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                      fluidRow(
                          column(6, plotOutput(outputId = "tab2_ageVsVio")),
                          column(6, plotOutput(outputId = "tab2_ageVsVio_general"))
                      ),
                      hr(),
                      
                      # Second sector part
                      h4(p(em("Age vs Traffic Violations in the U.S."),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                      
             ),
             
             
             
             # tab3: races
             tabPanel("Races",
                      
             ),
             
             
             
             # tab4: violation type
             tabPanel("Violation Type",
                      # General introduction and analysis part
                      fluidRow(column(width=2),
                               column(
                                 h3(p("Analysis of Violation Types",style="color:black;text-align:center")),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      br(),
                      fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                               column(align="center",
                                 p("This section analyze the violation types within all traffic violations"),
                                 p("Bulabulabula "),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # First graph
                      h4(p(em("Bar & Pie Chart for Types of Traffic Violations"),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                      fluidRow(
                        column(6, plotOutput(outputId = "tab4_vioType_bar"))
                        # coloum(6, plotOutput(outputId = "tab4_vioType_pie"))
                      ),
              
                      hr(),
                      
             ),
    
  )
    
  

)


# Define Server

inFile <- main_data

server <- function(input, output) {
  
  #dealing with data file
  output$RawData <- renderDataTable(
    datatable({
      inFile
    },
    ))
  
  
  # function for tab2:
  # function for tab2_ageVsVio
  output$tab2_ageVsVio <- renderPlot({
    
    hist(inFile$driver_age, is.na = TRUE, breaks = 14,
         xlab = "Drivers' ages",
         main = "Histogram of drivers' ages in traffic violations")
    
  })
  
  # function for tab2_ageVsVio_general
  output$tab2_ageVsVio_general <- renderPlot({
    
      barplot(driver_ages$NUMBER,
              names.arg = driver_ages$AGE,
               xlab = "Drivers' ages",
               ylab = "Frequency",
               main = "Bar chart of drivers' age in the U.S.")
  })
  
  
  
  
  # function for tab4:
  # tidy data for tab4
  tab4_data <- filter(inFile, inFile$violation != "") %>%
    group_by(violation) %>%
     summarise(count = n()) %>%
       na.omit()
  
  # function for tab4_vioType_bar
  output$tab4_vioType_bar <- renderPlot({
    
    barplot(tab4_data$count,
            names.arg = tab4_data$violation,
            xlab = "Types of violations",
            ylab = "Frequency",
            main = "Bar chart of types of violations")
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)





### other stuff
# > trac <- group_by(traffic_violaions, driver_age)
# > summarise(trac, count = n())
