
# 
library(RSocrata)
library(dplyr)

airquality <- read.socrata(
  "https://data.cdc.gov/resource/cicv-w9dv.json?statefips='55'", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

rock <- airquality %>% 
  filter(countyfips == "105")

airquality2 <- read.socrata(
  "https://data.cdc.gov/resource/cicv-w9dv.json?countyfips='105'", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)


airquality3 <- read.socrata(
  "https://data.cdc.gov/resource/cicv-w9dv.json?statefips='55'&countyfips='105'", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

# write.csv(rock, "O3.csv")
