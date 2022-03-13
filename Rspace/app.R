library(shiny)
library(shinythemes)
library(tidyverse)
library(shinydashboard)
library(datasets)
library(dplyr)
library(ggplot2)
library(tidyr)
library(leaflet)

#  ---------------------------------------- Import data -------------------------------------------------------
traffic_violaions <- read.csv('violaions.csv')
driver_ages <- read.csv('ages.csv')
driver_races <- read.csv('races.csv')
inFile <- traffic_violaions

# -------------------------------------------------------------------------------------------------------------




# ------------------------------------------ Define UI --------------------------------------------------------
ui <- fluidPage(theme = shinytheme("cerulean"),
  
  # App title 
  titlePanel("Visualization and Analysis of Traffic Violations in the U.S."),
    
  # Different tabs
  navbarPage("Let's explore",
            
             # tab1: general------------------------------------------------------------------------------------
             tabPanel(icon("home"),
                      
                      # General description part
                      fluidRow(column(tags$img(src="trafficvio.jpg",width="260px",height="200px"),
                                      width=4, style="color:black;text-align:center"),
                               # Description part
                               column(
                                 p("Traffic Violation is one of the significant topic in the United State. Over 
                                   the century, some people form stereotype related to these issues, such as the 
                                   black people are more likely to involve in traffic violations than other races. 
                                   But is this true? This app visulize and analyze traffic violations with differents 
                                   aspects in the USA, such as races and violation types. It will firstly provide an 
                                   overview of the relationship between traffic violations and various aspects within 
                                   the whole violations. Then, for the part of ages, gender and violation types, 
                                   it combines existed data with driving pattern to generalize how different groups 
                                   related to traffic violation in the USA."),
                                 p("The interactive graph below gives you space to explore various categories within 
                                   the traffic violations. After exploring and having a general overview of these 
                                   categories, clicks parts on the right of the home to keep exploring! These 
                                   sections combines data with car access pattern in U.S. to generalize and 
                                   analyze the specific catagory with traffic violations in the USA. And, of 
                                   course, some results will shock you!"),
                                 width=7,style="background-color:lavender;border-radius: 10px")
                               ),
                      hr(),
                      
                      # The part showing various attributes versus traffic violation
                      h4(p(em("Various Categories versus Traffic Violations in U.S."),
                           icon("chart-pie",lib = "font-awesome"),
                           style="color:black;text-align:center")),
                      br(),
                      sidebarLayout(
                        # Sidebar Part
                        sidebarPanel(
                          selectInput("xaxis",
                                      p("Select the catagory you want to explore:", 
                                        style="color:black; text-align:center"),
                                      choices=c("Gender","Age","Race",
                                                "Violation-Type","Drug-Related")),
                        ),
                        # Main Part
                        mainPanel(
                          fluidRow(
                            column(5, plotOutput(outputId = "tab1_1")),
                            column(6, plotOutput(outputId = "tab1_2"))
                          ))),
             ),

             
             
             # tab2: Analysis of age----------------------------------------------------------------------
             tabPanel("Ages",
                      
                      # General introduction and analysis part
                      fluidRow(column(width=2),
                               column(
                                 h3(p("How Ages are Related to Traffic Violations",
                                      style="color:black;text-align:center")),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      br(),
                      
                      fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                               # Description part
                               column(
                                 p("This section analyze the relationship between ages and traffic
                                 violations. It includes two parts: "),
                                 p("The first part shows one bar chart about how  
                                 ages are related to violations within all violations and another bar
                                 chart shows how ages are related to car access in the U.S. Basicly, we
                                 find out that within all violations, age range of 20-24 constitute the
                                 highest violation value, and decrease as age range increases. For
                                 drving pattern in the USA, age range of 20-59 contitutes majority of 
                                 driving populations.
                                 "),
                                 p("The second part conbines the provious two charts to derive the 
                                   relationship between ages and expected percentage of traffic 
                                   violations in that age range in the USA. Notice that combining two 
                                   data is necessary because even if the absolute value of traffic violations 
                                   for people under the age of 20 is very small, it is still a lot in percentage 
                                   since there are not many drivers under the age of 20. We can easily see 
                                   from the bar chart that driver ages among 15-25 year olds have the highest 
                                   percentage of violation rate. Then as the age group increases, the percentage 
                                   of traffic violations in that range group gradually decreases. This is pretty 
                                   reasonalble due to the fact that driver of 20-30 year olds are newbies and 
                                   they drive more impetuously."),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # First sector part including two plots described above
                      h4(p(em("Age in Traffic Violations & Driving Pattern(ages) in U.S."),
                           icon("chart-pie",lib = "font-awesome"),
                           style="color:black;text-align:center")),
                      fluidRow(
                          column(6, plotOutput(outputId = "tab2_ageVsVio")),
                          column(6, plotOutput(outputId = "tab2_ageVsVio_general"))
                      ),
                      hr(),
                      
                      # Second sector part including one plot described above
                      h4(p(em("Age vs Traffic Violations in the U.S."),
                           icon("chart-pie",lib = "font-awesome"),
                           style="color:black;text-align:center")),
                      fluidRow(
                        column(8, offset = 2, 
                               plotOutput(outputId = "tab2_ageVsVio_part2_bar")))
             ),
             
             
             
             # tab3: races-----------------------------------------------------------------------------------
             tabPanel("Races",
                      # General introduction and analysis part
                      fluidRow(column(width=2),
                               column(
                                 h3(p("How Races are Related to Traffic Violations",
                                      style="color:black;text-align:center")),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      br(),
                      fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                               column(
                                 #description part
                                 p("This section analyze the relationship between races and traffic
                                   violations. It includes two parts: "),
                                 p("The first part shows one bar chart about how races are related to 
                                   violations within all violations and another pie chart shows how 
                                   races are related to car access in the U.S. Basicly, we find out 
                                   that within all violations, white constitute the highest violation 
                                   number, and other races like black, asian and hispanic constitue 
                                   small number of violation."),
                                 p("The second part conbines the provious two charts to derive the 
                                   relationship between races and expected traffic violations in general 
                                   in the USA. Notice that combining two data is also necessary because 
                                   even if the absolute value of traffic violations for white people is 
                                   super high, it still account for not many in proportion since there 
                                   are much more white drivers in the USA. We can easily see from the bar 
                                   chart that black and white drivers are equally likely to violate the 
                                   traffic rules, and the asian and hispanic people are little bit less 
                                   likely than the black and white people. This refreshes our stereotype 
                                   that black people are more likely to have traffic violations. In fact, 
                                   different races share similar traffic violation rate"),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # First sector part including two plots described above
                      h4(p(em("Races in Traffic Violations & Driving Pattern(races) in U.S."),
                           icon("chart-pie",lib = "font-awesome"),
                           style="color:black;text-align:center")),
                      fluidRow(
                        column(6, plotOutput(outputId = "tab3_raceVsVio")),
                        column(6, plotOutput(outputId = "tab3_raceVsVio_general"))
                      ),
                      hr(),
                      
                      # Second sector part including one plot described above
                      h4(p(em("Race vs Traffic Violations in the U.S."),
                           icon("chart-pie",lib = "font-awesome"),
                           style="color:black;text-align:center")),
                      fluidRow(
                        column(6, offset = 3, 
                               plotOutput(outputId = "tab3_raceVsVio_part2_bar")))
             ),
             
             
             
             # tab4: violation type------------------------------------------------------------------------
             tabPanel("Violation Type",
                      # General introduction and analysis part
                      fluidRow(column(width=2),
                               column(
                                 h3(p("Analysis of Violation Types",
                                      style="color:black;text-align:center")),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      br(),
                      fluidRow(column(width=2, icon("hand-point-right","fa-5x"),align="center"),
                               column(
                                 p("This section analyze the violation types within all traffic violations"),
                                 p("Below are the bar chart and pie chart for different violation types. We
                                   can see that speeding accounts for majority of the total traffic violations,
                                   which is 65%. This is pretty reasonable not only because of wiggle room(you
                                   won't be punished if you speed little), but also because of the speed 
                                   following rule and the minor punishment towards speeding. Registration/plates 
                                   and equipment violations are less common due to the serious punishment, 
                                   including vehical impounding. Moving violation is also less common due 
                                   to the lack of moniter and punishment."),
                                 width=8,style="background-color:lavender;border-radius: 10px")),
                      hr(),
                      
                      # Graph part including two plots described above
                      h4(p(em("Bar & Pie Chart for Types of Traffic Violations"),
                           icon("chart-pie",lib = "font-awesome"),
                           style="color:black;text-align:center")),
                      fluidRow(
                        column(6, plotOutput(outputId = "tab4_vioType_bar")),
                        column(6, plotOutput(outputId = "tab4_vioType_pie"))
                      ),
             ),
    
  )
    
  

)



# -------------------------------------------------------------------------------------------------------------

# ------------------------------------------ Define Server ----------------------------------------------------
server <- function(input, output) {
 #dealing with data file
  output$RawData <- renderDataTable(
    datatable({
      inFile
    },
    ))
    
 # function for tab1 ------------------------------------------------------------
  # tidy data for tab1
  tab1_data1 <- filter(inFile, inFile$driver_gender != "") %>%
    group_by(driver_gender) %>%
      summarise(count = n()) %>%
        na.omit()
  tab1_data2 <- group_by(inFile, drugs_related_stop) %>%
    summarise(count = n()) %>%
      na.omit()
  
  # if function for tab1_1 
  output$tab1_1 <- renderPlot({
    if (input$xaxis == "Age") {
      barplot(driver_ages_last$total,
              names.arg = driver_ages_last$AGE,
              xlab = "Drivers' ages",
              ylab = "Frequency",
              main = "Drivers' ages in traffic violations(bar chart)")
    }
    else if (input$xaxis == "Gender") {
      barplot(tab1_data1$count,
              names.arg = c("Female", "Male"),
              xlab = "Drivers' gender",
              ylab = "Frequency",
              main = "Drivers' genders in traffic violations(bar chart)")
    }
    else if (input$xaxis == "Race") {
      barplot(tab3_data$count,
              names.arg = tab3_data$driver_race,
              xlab = "Drivers' races",
              ylab = "Frequency",
              main = "Drivers' races in traffic violations(bar chart)")
    }
    else if (input$xaxis == "Violation-Type") {
      barplot(tab4_data$count,
              names.arg = tab4_data$violation,
              xlab = "Types of violations",
              ylab = "Frequency",
              main = "Types of violations(bar chart)")
    }
    else if (input$xaxis == "Drug-Related") {
      barplot(tab1_data2$count,
              names.arg = c("Not-Related", "Related"),
              xlab = "Drivers' gender",
              ylab = "Frequency",
              main = "Drug related issue(bar chart)")
    }
  })
  
  # if function for tab1_2
  output$tab1_2 <- renderPlot({
    if (input$xaxis == "Age") {
      pie(driver_ages_last$total,
          labels = driver_ages_last$AGE,
          xlab = "Drivers' ages",
          main = "Drivers' ages in traffic violations(pie chart)")
    }
    else if (input$xaxis == "Gender") {
      pie(tab1_data1$count,
          labels = c("Female", "Male"),
          xlab = "Drivers' gender",
          main = "Drivers' genders in traffic violations(pie chart)")
    }
    else if (input$xaxis == "Race") {
      pie(tab3_data$count,
          labels = tab3_data$driver_race,
          xlab = "Drivers' races",
          main = "Drivers' races in traffic violations(pie chart)")
    }
    else if (input$xaxis == "Violation-Type") {
      pie(tab4_data$count,
          labels = tab4_data$violation,
          xlab = "Types of violations",
          main = "Types of violations(pie chart)")
    }
    else if (input$xaxis == "Drug-Related") {
      pie(tab1_data2$count,
          labels = c("Not-Related", "Related"),
          xlab = "Drivers' gender",
          main = "Drug related issue(pie chart)")
    }
  })
  
 # function for tab2 ------------------------------------------------------------
  
  # tidy data for tab2
    # first section part: group the data to different age range
  tab2_data <- inFile %>%
    mutate(MySpecificBins = cut(driver_age, breaks = c(-Inf, 20,25,30,35,40,
                                                       45,50,55,60,65,70,75,80,85,Inf)))
  tab2_data <- tab2_data %>%
    group_by(MySpecificBins) %>%
      count() %>%
        na.omit()
    # second section part: combine two dataset together to derive percentage in ages
  tmp_data <- data.frame(total = c(5859,10677,7579,5436,4864,4547,3921,2933,
                                   1813,1017,410,176,70,18,8))
  driver_ages_part4 <- cbind(driver_ages, tmp_data)
  driver_ages_part4$expect_total <- driver_ages_part4$Percentage * driver_ages_part4$total / 100
  driver_ages_part4$expect_percentage <- driver_ages_part4$total / driver_ages_part4$NUMBER * 100
  
  driver_ages_last <- cbind(driver_ages, tmp_data)
  driver_ages_last$expect_percentage <- driver_ages_last$total / driver_ages_last$NUMBER * 100
  driver_ages_last <- driver_ages_last[,-4]
  driver_ages_last <- filter(driver_ages_last, driver_ages_last$expect_percentage > 0.0015)
  
  
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
               main = "Bar chart of car access according to drivers' age in the U.S.")
  })
  
    # function for tab2_ageVsVio_part2_bar
  output$tab2_ageVsVio_part2_bar <- renderPlot({
    
    barplot(driver_ages_part4$expect_percentage,
            names.arg = driver_ages$AGE,
            xlab = "Drivers' ages",
            ylab = "Expected Percentage",
            main = "Bar chart of drivers' ages versus percentage of traffic violations in U.S.")
  })

  
  
 # function for tab3:------------------------------------------------------------  
  
  # Tidy data for tab3
    # First section part: group the data according to different races
  tab3_data <- filter(inFile, inFile$driver_race != "") %>%
    group_by(driver_race) %>%
      summarise(count = n()) %>%
        na.omit()
  driver_races$total_population <- driver_races$without_car / driver_races$without_percentage * 100
  driver_races$car_access <- driver_races$total_population / (100 - driver_races$without_percentage) * 100
    # Second section part: combine two dataset together to derive percentage in races
  tab3_tmp_data <- data.frame(violation = c(1375, 6594,4099,149,37364))
  driver_races <- cbind(driver_races, tab3_tmp_data)
  driver_races$expected_percentage <- driver_races$violation / driver_races$car_access * 100
  
  # Plot for tab3
    # function for tab3_raceVsVio
  output$tab3_raceVsVio <- renderPlot({
    barplot(tab3_data$count,
            names.arg = tab3_data$driver_race,
            xlab = "Drivers' races",
            ylab = "Frequency",
            main = "Bar chart of drivers' races in traffic violations")
  })
  
    # function for tab3_raceVsVio_general
  output$tab3_raceVsVio_general <- renderPlot({
    pie(driver_races$car_access,
            labels = tab3_data$driver_race,
            xlab = "Drivers' races",
            main = "Pie chart of car access according to drivers' races in the U.S.")
  })
  
  # function for tab3_raceVsVio_part2_bar
  output$tab3_raceVsVio_part2_bar <- renderPlot({
    
    barplot(driver_races$expected_percentage,
            names.arg = driver_races$driver_race,
            xlab = "Drivers' race",
            ylab = "Expected Percentage",
            main = "Bar chart of drivers' race versus percentage of traffic violations in U.S.")
  })
  
  
  
 # function for tab4:-----------------------------------------------------------------
  # Tidy data for tab4
  tab4_data <- filter(inFile, inFile$violation != "") %>%
    group_by(violation) %>%
     summarise(count = n()) %>%
       na.omit()
  
  # Plot for tab4
   # function for tab4_vioType_bar
  output$tab4_vioType_bar <- renderPlot({
    
    barplot(tab4_data$count,
            names.arg = tab4_data$violation,
            xlab = "Types of violations",
            ylab = "Frequency",
            main = "Bar chart of types of violations")
  })
  
   # function for tab4_vioType_pie
  output$tab4_vioType_pie <- renderPlot({
    
    pie(tab4_data$count,
        labels = tab4_data$violation,
        xlab = "Types of violations",
        main = "Pie chart of types of violations")
  })
  
  
  
}

# -------------------------------------------------------------------------------------------------------------

# ------------------------------------------ Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)

# -------------------------------------------------------------------------------------------------------------


