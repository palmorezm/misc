


library(jsonlite)
library(httr)

get1 <- httr::GET(url = "https://api.weather.gov/gridpoints/TOP/31,80/forecast")
get1



get1.data <- fromJSON(rawToChar(get1$content))
data.frame(get1$content)

get1.data$`@context`



# NOAA Climate Data Online

datasets <- httr::GET(url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/datasets")
datasets$content

