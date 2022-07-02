


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



