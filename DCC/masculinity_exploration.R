
# Data Exploration
# Topic: Masculinity
# Source: FiveThirtyEight

library(dplyr)
library(tidyr)

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
