
# Data Exploration
# Topic: Masculinity
# Source: FiveThirtyEight

library(dplyr)
library(tidyr)
library(ggplot2)

# Uses the URL of from FiveThirtyEight's raw content on Github
masculinity <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/raw-responses.csv")
masculinity[is.na(masculinity)] <- "No Answer" # Replace with "No Answer"
sum(is.na(masculinity)) # Should be "0" 


masculinity <- masculinity %>% 
  gather(key, value, -X, -StartDate, -EndDate) 
masculinity %>% 
  group_by(key) %>% 
  summarise(Total = table(value)) %>%
  View()

masculinity %>% 
  group_by(key) %>% 
  summarise(Total = table(value)) %>% 
  ggplot(aes(key, Total)) + 
  geom_point(aes(col = key)) + 
  theme_minimal() + 
  theme(legend.position = "none")  + coord_flip()
