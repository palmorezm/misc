
# rapid prototype
# speed not considered, nor was it run multiple times

weekly <- read.socrata(
  "https://data.cdc.gov/resource/x9gk-5huc.json", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

