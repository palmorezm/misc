

setwd("C:/Users/Zachary Palmore/GitHub/misc/Experiments/sleep")
d2021 <- readxl::read_xlsx("2021.xlsx")
d2022 <- readxl::read_xlsx("2022.xlsx")

# Select columns of interest from both df
# rename columns in each to same name 
# bind the rows of the columns with same name
# change data types

d2022 <- d2022[,c(1:3, 9)] # Select times related to sleep each night
d2021 <- d2021[,c(1:3, 9)] # For both data sets
colnames(d2021) <- colnames(d2022)
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

# Need to join with dt reference table the df dates that match place
library(dplyr)
library(ggplot2)
class(dt$Place) # numeric
class(df$Date) # character
df %>% 
  mutate(Place = as.numeric(Date)) %>%  # convert to numeric
  dplyr::inner_join(dt, by = "Place") %>%  # includes all rows in x AND y
  dplyr::select(-Date.x, -Place) %>%
  ggplot(aes(Date.y, seq(1, length(Date.y), 1))) + geom_line()
# Appears there are no missing in the line plot
# There would be 727 obs in this new df if joined
# But let's check the obs of our origional df
nrow(df) # 1204
# These do not match, why? 
df[1,1] # The first numeric date is there 2021-01-02
df[length(df$Date),] # The last date is missing. Are there more?
# After viewing, the last date is 44926.0
df[which(df$Date == "44926.0"),1] # Select it as a character string
# This solves the bulk of entries that were missing but 
727-which(df$Date == "44926.0") # 9 are still missing
seq(as.numeric(df[1,1]), as.numeric(which(df$Date == "44926.0")), 1)
seq(44197, 44926, 1)

df$True_Date <- seq(as.numeric(df[1,1]), by = 1, length.out = as.numeric(which(df$Date == "44926.0")))
length(df$Date) - length(seq(as.numeric(df[1,1]), by = 1, length.out = as.numeric(which(df$Date == "44926.0")))) 
rep(-999, (length(df$Date) - 
          length(seq(as.numeric(df[1,1]), by = 1, 
                                     length.out = as.numeric(which(df$Date == "44926.0")))))
)

df$True_Date <- as.numeric(c(seq(as.numeric(df[1,1]), by = 1, length.out = as.numeric(which(df$Date == "44926.0"))), 
  rep(-999, (length(df$Date) - 
            length(seq(as.numeric(df[1,1]), by = 1, 
                       length.out = as.numeric(which(df$Date == "44926.0")))))
  )
))


class(df$True_Date)
min(df$True_Date)
as.numeric(df$True_Date)
length(df$True_Date)

df %>% 
  rename(Place = True_Date) %>%  # convert to numeric 
  dplyr::inner_join(dt, by = "Place") %>% View() # includes all rows in x AND y
# This functions but is still does not produce 1204 obs
# Is it the conversation from character to numeric or something else? 
class(df$Date)
class(dt$Place)
sum(is.na(length(df$Date)))
sum(is.na(length(as.numeric(df$Date))))

as.numeric(df$Date)
as.numeric(df$True_Date)
as.numeric(dt$Place)

df <- df %>% 
  rename(Place = True_Date) %>%  # convert to numeric
  dplyr::inner_join(dt, by = "Place") %>% # includes all rows in x AND y
  dplyr::select(-Date.x, -Place)
# We ignore trying to answer this question for now. I'm not sure what else to look for. 
df %>% # Does it extend 2021-01 to 2023-XX and contain some information until about 2022-01?
  ggplot(aes(Date.y, seq(1, length(Date.y), 1))) + geom_line() # Yep, it still looks good!


# Converting between time formats 
# https://support.microsoft.com/en-us/office/time-function-9a5aff99-8f7d-4611-845e-747d0b8d5457

dt <- lubridate::interval(start = lubridate::as_date("1900-01-01"), end = lubridate::as_date("2022-11-06"))
dt <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2022-12-31"), by = "day")
dt <- data.frame(Date = dt)
dt$TimeID <- seq(1, length(dt$Date), by = 1)

as.numeric(df$`Time in Bed`)
min(as.numeric(df$`Time in Bed`), na.rm = T)
values <- df$`Time in Bed`[which(df$`Time in Bed` >= 0)]
max(values)
values <- df$`Time in Bed`[which(as.numeric(df$`Time in Bed`) >= 0)]
values <- as.numeric(values)
max(values)
min(values).99988426
time_values <- seq(min(values), max(values), by = 0.00000001) # Thats 99,791,667 obs!
time_values = data.frame(time_values= time_values)
lubridate::interval(start = as_datetime())

format(as.POSIXct(Sys.Date() + 0.8541667), "%H:%M", tz="UTC")
chron::times(0.8541667)
library(chron)
chron::times(values)
df$Date.y <- as.Date(df$Date.y)
df$`Time in Bed` <- as.numeric(df$`Time in Bed`)
df$New_TIB <- format(as.POSIXct(df$Date.y + df$`Time in Bed`), "%H:%M", tz = "UTC")
c(df$Date.y, df$New_TIB)
lubridate::hms(df$New_TIB)
lubridate::dmy_hm(c(df$Date.y, df$New_TIB))
lubridate::dmy_hm(paste(df$Date.y, " ", df$New_TIB))


# Time value fix with chron::times() , check above
# Use this to estimate Time ID




