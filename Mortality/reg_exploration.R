

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


REG_MORT <- REG_MORT %>% 
  rename(ICD.Chapter = UCD...ICD.Chapter, 
         ICD.Chapter.Code = UCD...ICD.Chapter.Code)

reg_mort %>% 
  filter(Gender != "",
         Year == c(2018, 2019, 2020, 2021, 2022) #,
         #Occurrence.County == "Rock County, WI"
         ) %>% 
  mutate(total_deaths = sum(Deaths)) %>% 
  group_by(ICD.Chapter, Gender, Year, Deaths) %>% 
  summarise(Percent = (Deaths / total_deaths)*100) %>% 
  arrange(desc(Percent)) %>%
  ggplot(aes(Gender, Deaths, fill = Gender)) + 
  geom_col(position = position_dodge()) + 
  facet_wrap(~Year)


# What is the distribution of death counted like by the CDC for gender? 
REG_MORT %>% 
  filter(Gender != "",
         ICD.Chapter == c("Neoplasms", 	
                          "Diseases of the respiratory system"),
         # Occurrence.County == "Rock County, WI"
  ) %>% 
  mutate(total_deaths = sum(Deaths)) %>% 
  group_by(Date, ICD.Chapter, Gender, Deaths) %>% 
  summarise() %>% 
  # group_by(ICD.Chapter, Gender, Date, Deaths) %>% 
  # summarise(Percent = (Deaths / total_deaths)*100) %>% 
  # arrange(desc(Percent)) %>%
  ggplot(aes(Deaths, fill = Gender)) + 
  geom_density(col = "black", alpha = 0.15) + facet_wrap(~ICD.Chapter)


reg_mort %>% 
  filter(Single.Race.6 != "",
         ICD.Chapter == c("Neoplasms", 	
                          "Diseases of the respiratory system", 
                          "Diseases of the circulatory system"),
         # Occurrence.County == "Rock County, WI"
  ) %>% 
  mutate(total_deaths = sum(Deaths)) %>% 
  group_by(ICD.Chapter, Single.Race.6, Deaths) %>% 
  summarise() %>% 
  # group_by(ICD.Chapter, Gender, Date, Deaths) %>% 
  # summarise(Percent = (Deaths / total_deaths)*100) %>% 
  # arrange(desc(Percent)) %>%
  ggplot(aes(Deaths, fill = Single.Race.6)) + 
  geom_histogram(col = "black", alpha = 0.15) + facet_wrap(~ICD.Chapter)


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


reg_mort %>% 
  na.omit() %>% 
  filter(Year == 2020) %>% 
  mutate(total_deaths = sum(Deaths)) %>% 
  group_by(Single.Race.6, ICD.Chapter) %>% 
  summarise(Percent = (Deaths / total_deaths)*100 ) %>%
  arrange(desc(Percent)) %>%  
  ggplot(aes(reorder(Single.Race.6, Percent))) + 
  geom_bar(fill = "light blue", col = "black") + 
  geom_text(aes(label = paste0(round(total_deaths, 1), "%"), y = Percent), 
            hjust = -.25, colour = "black") + 
  coord_flip() + 
  labs(x = "Cause", y = "Deaths", 
       subtitle = "Leading Causes of Death by Race")

