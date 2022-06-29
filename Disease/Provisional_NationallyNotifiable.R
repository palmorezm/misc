
# NNDSS Weekly Data
# NNDSS - In this Table, provisional cases* of notifiable diseases are displayed for United States, U.S. territories, and Non-U.S. residents.
# Division of Health Informatics and Surveillance (DHIS), Centers for Disease Control and Prevention
# https://data.cdc.gov/NNDSS/NNDSS-Weekly-Data/x9gk-5huc


# Packages
library(RSocrata)
library(tidyverse)

# Access Data
weekly <- read.socrata(
  "https://data.cdc.gov/resource/x9gk-5huc.json", 
  app_token = Sys.getenv("APP_TOKEN"),
  email     = Sys.getenv("PHDS_EMAIL"),
  password  = Sys.getenv("PHDS_PASSWORD")
)

WI <- weekly %>% 
  filter(states == "WISCONSIN")

# Columns of interest
# states, year, week, label, m2, geocode.coordinates
WI$week <- as.integer(WI$week)
WI$label <- as.factor(WI$label)
WI$m2 <- as.numeric(WI$m2)
WI <- subset(WI, select = c("states", "week", "label", "m2", "geocode.coordinates"))

library(leaflet)
library(leaflet.extras)
library(tigris)
library(sf)

WI$STATEFP <- as.character("55")
counties_wi <- tigris::counties() %>% 
  filter(STATEFP == "55")
wisp <- counties_wi %>% 
  left_join(WI, by = "STATEFP")
wisf <- as_Spatial(wisp)
coords <- st_as_sf(wisf@data[[]], coords = c("Longitude", "Latitude"))

unlist(WI$geocode.coordinates[[1]])
vectors <- unlist(WI$geocode.coordinates)
lat <- vectors[c(TRUE, FALSE)]
lng <- vectors[c(FALSE, TRUE)]
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    data = wisf@data,
    lng = lng, 
    lat = lat
  ) # All points are the same - due to marker at state level










