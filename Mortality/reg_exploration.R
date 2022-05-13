

# Packages 
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
theme_set(theme_bw())
load("Data/reg_mort.rdata")

###############################

### ----- Q & A Plots ----- ###

############################### 

# What do people of different races in Rock County die from and how many died from that particular cause?
# What proportion of the deaths were from each cause? 
reg_mort <- reg_mort %>% 
  dplyr::select(-X..of.Total.Deaths) %>% 
  rename(ICD.Chapter = UCD...ICD.Chapter, 
         ICD.Chapter.Code = UCD...ICD.Chapter.Code)
unique(reg_mort$Single.Race.6)
unique(REG_MORT$Single.Race.6)
unique(UCDREG_1999$Race)



UCDREG_1999 %>% 
  filter(County == "Rock County, WI") %>% View()
  mutate

  
   
UCD %>% 
  filter(Year == 2020, 
         County == "Rock County, WI") %>% 
  mutate(total_deaths = sum(Deaths)) %>% 
  group_by(ICD.Chapter, Deaths, Crude.Rate, total_deaths) %>% 
  summarise(Percent = (Deaths / total_deaths)*100 ) %>%
  arrange(desc(Percent)) %>%  
  ggplot(aes(reorder(ICD.Chapter, Deaths), Deaths)) + 
  geom_col(fill = "light blue", col = "black") + 
  geom_text(aes(label = paste0(round(Percent, 1), "%"), y = Deaths), 
            hjust = -.25, colour = "black") + 
  coord_flip() + 
  labs(x = "Cause", y = "Deaths", 
       subtitle = "Leading Causes of Death in Rock County")

