
# NNDSS Weekly Data
# NNDSS - In this Table, provisional cases* of notifiable diseases are displayed for United States, U.S. territories, and Non-U.S. residents.
# Division of Health Informatics and Surveillance (DHIS), Centers for Disease Control and Prevention

# Packages
library(RSocrata)
library(dplyr)

# Access Data
weekly <- read.socrata(
  "https://data.cdc.gov/resource/x9gk-5huc.json", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

