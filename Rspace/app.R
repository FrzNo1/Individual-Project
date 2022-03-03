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
                      #General description part
                      fluidRow(column(tags$img(src="trafficvio.jpg",width="260px",height="200px"),width=4),
                               column(8,
                                 p("balabala", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                                 br()
                               )),
                      hr(),
                      
                      
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
                                   between ages and expected traffic violations in general within the U.S.. Notice that combining 
                                   two data is necessary because even if The absolute value of traffic violations for people under 
                                   the age of 20 is very small, it still account for a lot in proportion since there are not many drivers 
                                   under the age of 20. We can easily see
                                   from the both bar and pie chart that driver ages among 20-30 year olds account for nearly 
                                   33% of total traffic violations. Then as the age group increases, the proportion of traffic 
                                   violations gradually decreases. This is pretty reasonalble due to the fact that driver of 20-30 
                                   year olds are newbies and they drive more impetuously"),
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
                      fluidRow(
                        column(6, plotOutput(outputId = "tab2_ageVsVio_part2_bar")),
                        column(6, plotOutput(outputId = "tab2_ageVsVio_part2_pie"))
                      )
                      
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
                               column(
                                 p("This section analyze the violation types within all traffic violations"),
                                 p("Below are the bar charts and pie charts for different violation types. We
                                   can see that speeding accounts for majority of the total traffic violations,
                                   which is 65%. This is pretty reasonable not only because of wiggle room(you
                                   won't be punished if you speed little), but also because of the speed following rule and the
                                   minor punishment towards speeding. Registration/plates and equipment violations
                                   are less common due to the serious punishment, including vehical impounding. Moving
                                   violation is also less common due to the lack of moniter and punishment."),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # First graph
                      h4(p(em("Bar & Pie Chart for Types of Traffic Violations"),icon("chart-pie",lib = "font-awesome"),style="color:black;text-align:center")),
                      fluidRow(
                        column(6, plotOutput(outputId = "tab4_vioType_bar")),
                        column(6, plotOutput(outputId = "tab4_vioType_pie"))
                      ),
              
                      hr(),
                      
             ),
    
  )
    
  

)


# Define Server

# inFile <- main_data
inFile <- traffic_violaions

server <- function(input, output) {
  
  #dealing with data file
  output$RawData <- renderDataTable(
    datatable({
      inFile
    },
    ))
  
  
  # function for tab2:
  # tidy data for tab2:
    # first section part:
  tab2_data <- inFile %>%
    mutate(MySpecificBins = cut(driver_age, breaks = c(-Inf, 20,25,30,35,40,45,50,55,60,65,70,75,80,85,Inf)))
  tab2_data <- tab2_data %>%
    group_by(MySpecificBins) %>%
      count() %>%
        na.omit()
    # second section part:
  tmp_data <- data.frame(total = c(5859,10677,7579,5436,4864,4547,3921,2933,1813,1017,410,176,70,18,8))
  driver_ages_part4 <- cbind(driver_ages, tmp_data)
  driver_ages_part4$expect_total <- driver_ages_part4$Percentage * driver_ages_part4$total / 100
  driver_ages_last <- cbind(driver_ages, tmp_data)
  driver_ages_last$expect_total <- driver_ages_last$Percentage * driver_ages_last$total / 100
  driver_ages_last <- driver_ages_last[,-4]
  driver_ages_last <- filter(driver_ages_last, driver_ages_last$expect_total > 10)
  
  # Plot for tab2
    # function for tab2_ageVsVio
  output$tab2_ageVsVio <- renderPlot({
    
    barplot(tab2_data$n,
            names.arg = tab2_data$MySpecificBins,
            xlab = "Drivers' ages",
            ylab = "Frequency",
            main = "Bar chart of drivers' ages in traffic violations")
    
  })
  
    # function for tab2_ageVsVio_general
  output$tab2_ageVsVio_general <- renderPlot({
    
      barplot(driver_ages$NUMBER,
              names.arg = driver_ages$AGE,
               xlab = "Drivers' ages",
               ylab = "Frequency",
               main = "Bar chart of drivers' age in the U.S.")
  })
  
    # function for tab2_ageVsVio_part2_bar
  output$tab2_ageVsVio_part2_bar <- renderPlot({
    
    barplot(driver_ages_part4$expect_total,
            names.arg = driver_ages$AGE,
            xlab = "Drivers' ages",
            ylab = "Expected Frequency",
            main = "Bar chart of drivers' ages versus traffic violations in U.S.")
  })
    # function for tab2_ageVsVio_part2_pie
  output$tab2_ageVsVio_part2_pie <- renderPlot({
    
    pie(driver_ages_last$expect_total,
        labels = driver_ages_last$AGE,
        main = "Pie chart of types of violations")
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
  
  #function for tab4_vioType_pie
  output$tab4_vioType_pie <- renderPlot({
    
    pie(tab4_data$count,
        labels = tab4_data$violation,
        xlab = "Types of violations",
        main = "Pie chart of types of violations")
  })
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)


