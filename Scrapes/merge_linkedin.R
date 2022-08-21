
# Combining Output csv from LinkedIn Scrapes
library(dplyr)

Chicago1 <- read.csv("Chicago1.csv") %>% 
  mutate(Search = "Greater Chicago Area")
Madison1 <- read.csv("Madison1.csv") %>% 
  mutate(Search = "Greater Madison Area")
Milwaukee1 <- read.csv("Milwaukee1.csv") %>% 
  mutate(Search = "Greater Milwaukee")

results <- rbind(Chicago1, Madison1, Milwaukee1)

write.csv(results, "merge1.csv")
