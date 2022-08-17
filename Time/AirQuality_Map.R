
library(leaflet)
library(dplyr)
# source(EH_Ozone.R)

airquality3$lat <- as.numeric(airquality3$latitude)
airquality3$lon <- as.numeric(airquality3$longitude)

leaflet() %>% 
  addTiles() %>% 
  # addProviderTiles(provider = "CartoDB") %>% 
  addCircleMarkers(lng = airquality3$lon, lat = airquality3$lat, 
                   popup = airquality3$ds_o3_pred, radius = 10) %>% 
  addLegend(colors = scale(as.numeric(airquality3$ds_o3_pred), center = TRUE),
            labels = as.numeric(airquality3$ds_o3_pred))
  
