


d2021 <- readxl::read_xlsx("2021.xlsx")
d2022 <- readxl::read_xlsx("2022.xlsx")

# Select columns of interest from both df
# rename columns in each to same name 
# bind the rows of the columns with same name
# change data types

colnames(d2022)
d2022 <- d2022[,c(1:3, 9)] # Select times related to sleep each night
d2021 <- d2021[,c(1:3, 9)] # For both data sets
colnames(d2021) <- colnames(d2022)
colnames(d2021)
df <- rbind(d2021, d2022)
sum(is.na(df))
df[which(is.na(df)),]
as.Date.character(df$Date)
library(lubridate)

df$Bedtime <- lubridate::as_datetime(df$Bedtime)
df$`Wake Time` <- lubridate::as_datetime(df$`Wake Time`)
df$Timeslept <- df$`Wake Time` - df$Bedtime
as.Date(df$Date)
lubridate::as_date(df$Date) # Results in NA 
# can we convert the object from 
class(df$Date) # character as 
head(df$Date) # seemingly sequential numbers
# to a date or date time object? 

# For converting between time formats
# https://support.microsoft.com/en-us/office/time-function-9a5aff99-8f7d-4611-845e-747d0b8d5457


# For converting between data formats
# https://support.microsoft.com/en-us/office/date-function-e36c0c8c-4104-49da-ab83-82328b832349

# Note: (for data formats)
# Excel stores dates as sequential serial numbers so that they can be used in calculations. 
# January 1, 1900 is serial number 1, and January 1, 2008 is serial number 39448 
# because it is 39,447 days after January 1, 1900. You will need to change the 
# number format (Format Cells) in order to display a proper date
lubridate::as_date("1900-01-01")

dt <- lubridate::interval(start = lubridate::as_date("1900-01-01"), end = lubridate::as_date("2022-11-06"))
dt <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2022-12-31"), by = "day")
dt <- data.frame(Date = dt)
dt$Place <- seq(1, length(dt$Date), by = 1)
df$Date[match(df$Date, dt$Place, nomatch = NA)]
df$Date
df$Date[which(df$Date == "44197.0")]

dt$Place
