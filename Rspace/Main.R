library(shiny)
library(tidyverse)

# Using "Import Dataset" to import data from 


ggplot(data = traffic_violaions) + 
  stat_count(mapping = aes(x = driver_race), na.rm = TRUE)


