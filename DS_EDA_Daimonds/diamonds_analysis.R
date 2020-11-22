# Mohammed Alkhalaf
# Misk class 
# Diamonds Analysis 
# 15.11.2020

# libraries 
library(tidyverse)
library(ggplot2)

#diamonds data reading 
jems <- read.csv("C:/Users/USER/Desktop/DS_EDA/DS_EDA_Daimonds/diamonds.csv")

# data summary 
jems %>% 
  summary()

# price average - median
jems %>% 
  summarise(avg= mean(price), median=median(price))

# price avg & median deepening on the clarity type 
jems %>% 
  group_by(clarity) %>% 
  summarise(avg = mean(price), median=median(price))

# price avg & median deepening on the cut type 
jems %>% 
  group_by(cut) %>% 
  summarise(avg = mean(price), median=median(price))

# price avg & median deepening on the color type 
jems %>% 
  group_by(color) %>% 
  summarise(avg = mean(price), median=median(price))

# i wanted to play around and see if i can predict the carat from the size using very basic formula  so i did the following:
# diamonds Weight in carats = Length x Width x Depth x Coefficient (estimated because it depends about the cut shape)
#L/W Ratio 1.00: Coefficient = 0.0060
#L/W Ratio 1.25: Coefficient = 0.0080
#L/W Ratio 1.50: Coefficient = 0.0090
#L/W Ratio 2.00: Coefficient = 0.0100
#L/W Ratio 2.50: Coefficient = 0.0105

jems <- jems %>% 
  mutate(ratio = x / y)

# since there is only about 10 diamonds that they are far from Coefficient 1 we will use the 0.006
jems <- jems %>% 
  mutate(carats = x * y * z  * 0.006)

# comparing my results 
jems <- jems %>% 
  mutate( diff_percenatge= abs(((carat-carats)/carat)*100))

# avg difference 
jems %>% 
  filter(diff_percenatge > 0 & diff_percenatge < 100 ) %>% 
  summarise(avg = mean(diff_percenatge), median= median(diff_percenatge))

jems %>% 
  ggplot(aes( x= carat,y=carats))+ geom_jitter()+
  geom_smooth(method = "lm", se = FALSE)
  
# pretty good :)

# price erorr from the prediction based on the orginal price of the diamond 
jems <- jems %>% 
  mutate(price_unit= price/carat)
         
jems <- jems %>% 
  mutate(new_price= price_unit*carats)

jems <- jems %>% 
  mutate(price_diff= abs(price - new_price))
  
jems %>% 
  filter(diff_percenatge > 0 & diff_percenatge < 100 ) %>% 
  summarise(avg = mean(price_diff), median= median(price_diff))
  

  
            