# How to get Data from NCDC
# Install the packages

library(devtools)
install.packages("rnoaa")

# Obtain API access token from NOAA 
token <- "Copy and paste your token here" 

# Uuse NOAA Package
library(rnoaa)
stations <- ncdc_stations(
  datasetid='GHCND', 
  locationid='FIPS:39113',
  token = token)
data <- ncdc_datasets(datasetid = 'GHCND', 
                      locationid = 'FIPS:US',
                      token = token)
data

# Review the package for full detail
help(package = "rnoaa")
help('datasetid', package = "rnoaa")
??datasetid


# See all available datasets and their descriptions
datasets <- ncdc_datasets(token = token)

# See available categories and their descriptions
data_categories <- ncdc_datacats(token = token)

# Find Daily Summaries for New York, NY for four days in January 
ncdc_2020 <- ncdc(datasetid = "GHCND", 
                  token = token,
                  startdate = '2020-01-01',
                  enddate = '2020-01-31',
                  locationid = 'FIPS:36061')
ncdc_2020$data

# Find daily summaries of New York, NY for 2014
ncdc_2014 <- ncdc(datasetid = "GHCND", 
                  token = token,
                  startdate = '2014-01-01',
                  enddate = '2014-12-31',
                  locationid = 'FIPS:36061')
ncdc_2014$data

# Narrow search by datatype 
ncdc_2014 <- ncdc(datasetid = "GHCND", 
                  token = token,
                  startdate = '2020-01-01',
                  enddate = '2020-12-31',
                  locationid = 'FIPS:36061', 
                  datatypeid = "TMIN")
ncdc_2014$data

# Add datatypes for additional varaibles 
ncdc_2014 <- ncdc(datasetid = "GHCND", 
                  token = token,
                  startdate = '2020-01-01',
                  enddate = '2020-12-31',
                  locationid = 'FIPS:36061', 
                  datatypeid = c("TMIN", "TMAX"))
ncdc_2014$data

