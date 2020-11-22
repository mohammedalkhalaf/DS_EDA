# Mohammed Alkhalaf
# Misk class 
# 15.11.2020
# chicken weight data analysis 

# loading libraries 
library(tidyverse)
library(ggplot2)
library(DT)

#reading the data 
data <- ChickWeight

#exploring the data 
data %>% 
  summary()

# mean weight by diet in all time 
data %>% 
  group_by(Diet) %>% 
  summarise(avg=mean(weight))

# difference in weight over time for each chicken
data %>% 
  filter(Time == 0 | Time == 21) %>% 
  group_by(Chick) %>% 
  summarise(diffrance = weight[Time == 21]-weight[Time == 0],Diet) %>% 
  datatable()

# mean & median difference in weight over time for each diet type
data %>% 
  filter(Time == 0 | Time == 21) %>%
  group_by(Diet) %>% 
  summarise(mean_diffrance = mean(weight[Time == 21]-weight[Time == 0])
            ,median_diffrance = median(weight[Time == 21]-weight[Time == 0])) %>% 
  datatable()

# mean weight for each time
diffrance1 <- data %>% 
  group_by(Time) %>% 
  summarise(avg = mean(weight))

# to check the greatest change time 
diffrance1 %>% 
  mutate(pct_change = (avg/lag(avg) - 1) * 100) %>% 
  datatable()

# mean weight for each time and diet
diffrance2 <- data %>% 
  group_by(Time,Diet) %>% 
  summarise(avg = mean(weight))

# to check the greatest change time for a diet
diffrance2 %>% 
  mutate(pct_change = (avg/lag(avg) - 1) * 100) %>% 
  datatable()

