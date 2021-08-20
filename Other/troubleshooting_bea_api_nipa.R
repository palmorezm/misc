
# Troubleshooting BEA API

# Load packages
library(httr)
library(rjson)

bea_datasets <- httr::GET(url = "https://apps.bea.gov/api/data/?&UserID=A8D60910-9667-4857-B34C-7F7F8427214A-Key&method=GETDATASETLIST&")
bea_datasets
tables <- fromJSON(rawToChar(bea_datasets$content))
df.tables <- tables$BEAAPI$Results$ParamValue
df <- data.frame(df.tables)

bea.datasets <- tables$BEAAPI$Results$Dataset

bea.datasets

data_options <- unlist(tables$BEAAPI$Results$Dataset)

library(stringr)

data.ops <- stringr::str_remove(data_options, "DatasetName")
list(data.ops)

NIPA.bea <- httr::GET(url = "https://apps.bea.gov/api/data/?&UserID=A8D60910-9667-4857-B34C-7F7F8427214A-Key&method=NIPA&")
table <- fromJSON(rawToChar(NIPA.bea$content))
table$BEAAPI$Results$Error
