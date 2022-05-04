
# Mortality REG exploration
# R.E.G - Racial, Ethnic, Gender 
# Source: CDC Provisional Mortality Statistics
# See DataRequest.R for details on read in

load('Data/reg_mort.rdata')

REG_MORT %>% 
  filter(Occurrence.County == "Rock County, WI") %>% 
  summarise(Deaths_CountMonthly = sum(Deaths)) # Value is (3497)

reg_mort %>% 
  filter(Occurrence.County == "Rock County, WI") %>% 
  summarise(Deaths_CountYearly = sum(Deaths)) # Value differs (6251)
# The values should be the same


df <- REG_MORT

library(ggplot2)
reg_mort %>% 
  filter(UCD...ICD.Chapter == "Mental and behavioural disorders") %>% 
  ggplot(aes(UCD...ICD.Chapter)) + geom_col(aes(y = Deaths))

