

# BEA API Request

# API Key already requested and provided
# A8D60910-9667-4857-B34C-7F7F8427214A

# Required packages
library(jsonlite)
library(httr)

# Make a request
JSON.available <- httr::GET(url = 
  "https://apps.bea.gov/api/data/?&UserID=A8D60910-9667-4857-B34C-7F7F8427214A&method=GETDATASETLIST&ResultFormat=JSON")

# Review results
JSON.available # Status code 200 means request was successful

# Convert data from JSON to raw character string 
# then extract the data from the JSON formated string
data <- fromJSON(rawToChar(JSON.available$content))

# Repeat process on regional personal income request
personalincome.county <- GET("https://apps.bea.gov/api/data/?UserID=A8D60910-9667-4857-B34C-7F7F8427214A&method=GetData&datasetname=Regional&TableName=CAINC1&LineCode=1&Year=2018,2019&GeoFips=COUNTY&ResultFormat=json")
personalincome.county
data <- fromJSON(rawToChar(personalincome.county$content))
df <- data$BEAAPI$Results$Data
df <- data.frame(df)
names(df)

data$BEAAPI$Results$Data

# Check into Real Personal Income by MSA's
realpersonalincome.msa <- GET("https://apps.bea.gov/api/data/?UserID=A8D60910-9667-4857-B34C-7F7F8427214A&method=GetData&datasetname=Regional&TableName=MARPI&LineCode=1&Year=LAST5,2019&GeoFips=MSA&ResultFormat=json")
realpersonalincome.msa
rpi <- fromJSON(rawToChar(realpersonalincome.msa$content))
df.rpi <- rpi$BEAAPI$Results$Data
df.rpi <- data.frame(df.rpi)
names(df.rpi)

df.rpi



# Check into Regional Price Parities by MSA
rpp.last5 <- GET("https://apps.bea.gov/api/data/?UserID=A8D60910-9667-4857-B34C-7F7F8427214A&method=GetData&datasetname=Regional&TableName=MARPP&LineCode=1&Year=LAST5,2019&GeoFips=MSA&ResultFormat=json")
rpp.last5
rpp <- fromJSON(rawToChar(rpp.last5$content))
df.rpp <- rpp$BEAAPI$Results$Data
df.rpp <- data.frame(df.rpi)
names(df.rpi)
df.rpi


# How to get all parameters?
params <- GET("https://apps.bea.gov/api/data/?&UserID=A8D60910-9667-4857-B34C-7F7F8427214A&method=GetParameterValues&datasetname=Regional&ParameterName=TableName&ResultFormat=JSON") 
params
tables <- fromJSON(rawToChar(params$content))
df.tables <- tables$BEAAPI$Results$ParamValue
df <- data.frame(df.tables)

# Check that personal income summary again for per capita income
ipc <- GET("https://apps.bea.gov/api/data/?UserID=A8D60910-9667-4857-B34C-7F7F8427214A&method=GetData&datasetname=Regional&TableName=MARPP&LineCode=1&Year=LAST5,2019&GeoFips=MSA&ResultFormat=json")
ipc
ipc.data <- fromJSON(rawToChar(ipc$content))
ipc.data$BEAAPI$Results$Error
