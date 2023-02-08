
# Packages
library(readxl)
library(lubridate)
library(dplyr)
library(chron)
# Reads in the data (samples)
d2021 <- readxl::read_xlsx("2021.xlsx")
d2022 <- readxl::read_xlsx("2022.xlsx")
d2022 <- d2022[,c(1:3, 9)] # Select times related to sleep each night
d2021 <- d2021[,c(1:3, 9)] # For both data sets
colnames(d2021) <- colnames(d2022) # Rename old names in 2021 to names used in 2022
df <- rbind(d2021, d2022) # Combine the rows / observations of the two years

#####################
# Date Conversation #
#####################
# Convert the dates from Excel serial number to an interpret able interval
# We will do this by creating a reference dataset 
# The goal: Serial numbers will match with an interpretable "true date"
dt <- lubridate::interval(start = lubridate::as_date("1900-01-01"), end = lubridate::as_date("2022-11-06"))
dt <- seq.Date(from = as.Date("1900-01-01"), to = as.Date("2022-12-31"), by = "day")
dt <- data.frame(Date = dt) # Make it a dataframe for easy viewing
dt$IndexA <- seq(1, length(dt$Date), by = 1) # Build a new 
df$Date[which(df$Date == "44197.0")] # Check for a match to a known serial number
df[which(df$Date == "44926.0"),1] # Select last date as a character string

df$TrueDate <- as.numeric(c(seq(as.numeric(df[1,1]), 
                 by = 1, 
                 length.out = 
                   as.numeric(which(df$Date == "44926.0"))), 
             rep(-999, 
                 (length(df$Date) - length(
                   seq(as.numeric(df[1,1]), 
                       by = 1, 
                       length.out = 
                         as.numeric(which(df$Date == "44926.0")))))
             )
)) 

which(df$TrueDate == dt$Date)

df %>% 
  dplyr::select(case_when(Date, contains(TrueDate)))

df$True_Date <- seq(as.numeric(df[1,1]), by = 1, length.out = as.numeric(which(df$Date == "44926.0")))
length(df$Date) - length(seq(as.numeric(df[1,1]), by = 1, length.out = as.numeric(which(df$Date == "44926.0")))) 
rep(-999, (length(df$Date) - 
             length(seq(as.numeric(df[1,1]), by = 1, 
                        length.out = as.numeric(which(df$Date == "44926.0")))))
)


format(as.POSIXct(Sys.Date() + 0.8541667), "%H:%M", tz="UTC")
chron::times(0.8541667)
library(chron)
chron::times(values)

format(as.POSIXct(df$`Time in Bed`), "%H:%M", tz="UTC")


