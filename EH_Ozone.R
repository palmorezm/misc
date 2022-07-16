
# 

airquality <- read.socrata(
  "https://data.cdc.gov/resource/cicv-w9dv.json?statefips='55'", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

library(dplyr)

colnames(airquality)
rock <- airquality %>% 
  filter(countyfips == "105")

write.csv(rock, "O3.csv")
