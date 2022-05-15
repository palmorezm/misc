
# Packages 
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
theme_set(theme_bw())
load("Data/UCDREG.Rdata")
###############################

### ----- Q & A Plots ----- ###

############################### 
UCDREG_Summary <- UCDREG %>% 
  mutate(total_deaths = 
           sum(as.numeric(Deaths), na.rm = T)) %>% 
  group_by(Race, County, Population, Deaths, 
           Crude.Rate, Age.Adjusted.Rate, total_deaths) %>%
  summarise(CR = sum(Deaths, na.rm = T), 
            Percent = (Deaths / total_deaths)*100, 
            CMR = Deaths / Population * 100000) 


UCDREG_Summary %>% 
  filter(County == "Rock County, WI") %>% 
  group_by(Race, CMR, County) %>% 
  summarise(Percent = (Deaths / total_deaths)*100) %>% 
  ggplot(aes(Race, )) + geom_col()
  
  
UCDREG %>% 
  group_by(Race, Hispanic.Origin, Gender, Population) %>% 
  summarise(Sum_Deaths = sum(Deaths, na.rm = T)) %>% 
  ggplot(aes(Race)) + geom_bar()
  
UCDREGSUMMARY <- UCDREG %>% 
  group_by(Race, Hispanic.Origin, Gender, Population) %>% 
  summarise(Sum_Deaths = sum(Deaths, na.rm = T)) 

G1_REG <- UCDREGSUMMARY %>% 
  group_by(Race, Hispanic.Origin, Gender) %>% 
  summarise(Total_Pop = sum(Population, na.rm = T), 
            Total_Deaths = sum(Sum_Deaths, na.rm = T)) 

G1_REG[-1,] %>% 
  mutate(R = Total_Deaths/Total_Pop * 10000) %>% 
  ggplot(aes(Race, R)) + geom_col()
  
  
