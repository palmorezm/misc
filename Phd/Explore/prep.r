

library(openxlsx)
library(chron)
library(lubridate)

link <- paste0("https://docs.google.com/spreadsheets/",
               "d/e/2PACX-1vR1KCfCDLSP3erqvtZj7SutKAYm",
               "JzDHArloNtBEWq-sJweD7wKBiU0BP_XnEimAXZ",
               "gjxri6-PR06HAx/pub?output=xlsx")

# Read in first iteration
responses <- openxlsx::read.xlsx(link, sheet = 2)[-c(1:2),] # Remove first 2 rows
names(responses) <- c("Program", "Date", "Time", "Interest") # Assign the right field names

# Must know how many sheets to read in and must be sequential
for(i in 3:9){
  tmp <-  openxlsx::read.xlsx(link, sheet = i)[-c(1:2),] # Remove first 2 rows
  names(tmp) <- c("Program", "Date", "Time", "Interest") # Assign the right field names
  responses <- rbind(responses, tmp)
}

# Clean up the data types
responses$Interest <- factor(responses$Interest)
responses$Program <- factor(responses$Program)
responses$Date <- as.Date(as.numeric(stringr::str_remove_all(responses$Date, "[.]0")), origin="1899-12-30")
responses$Time <- chron::times(as.numeric(responses$Time))
responses$DateTime <- lubridate::as_datetime(paste(responses$Date, responses$Time))

# remove all variables that are not useful
rm(list=ls()[which(ls() != "responses")])

