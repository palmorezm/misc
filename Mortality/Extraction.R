
# Read in for CDC Wonder 
# 




# Provisional COVID-19 Death Counts in the United States by County
# https://data.cdc.gov/NCHS/Provisional-COVID-19-Death-Counts-in-the-United-St/kn79-hsxy

covid19_deaths <- read.socrata(
  "https://data.cdc.gov/resource/kn79-hsxy.json?$where=state_name=\'WI\'", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

# Provisional COVID-19 Deaths by County, and Race and Hispanic Origin
# https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-County-and-Race-and/k8wy-p9cg

covid19_deaths <- read.socrata(
  "https://data.cdc.gov/resource/k8wy-p9cg.json?$where=state=\'WI\'", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

covid19_deaths %>% 
  filter(county_name == "Rock County") %>% View()

# COVID-19 Case Surveillance Public Use Data with Geography
# https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data-with-Ge/n8mc-b4w4


# Excess Deaths Associated with COVID-19 (State Level)
# https://data.cdc.gov/NCHS/Excess-Deaths-Associated-with-COVID-19/xkkf-xrst




