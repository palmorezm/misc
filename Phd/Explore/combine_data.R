
library(readxl)
library(stringr)
library(dplyr)
library(chron)
library(lubridate)

response1 <- read_xlsx("Phd Links.xlsx", sheet = 2)
response2 <- read_xlsx("Phd Links.xlsx", sheet = 3)
response3 <- read_xlsx("Phd Links.xlsx", sheet = 4)
response4 <- read_xlsx("Phd Links.xlsx", sheet = 5)
response5 <- read_xlsx("Phd Links.xlsx", sheet = 6)
response6 <- read_xlsx("Phd Links.xlsx", sheet = 7)
response7 <- read_xlsx("Phd Links.xlsx", sheet = 8)
response8 <- read_xlsx("Phd Links.xlsx", sheet = 9)

print(response1)


tmp <- read_xlsx("Phd Links.xlsx", sheet = 2)[-1,]
rbind(tmp, response2)

# Read in first iteration
responses <- read_xlsx("Phd Links.xlsx", sheet = 2)[-c(1:2),] # Remove first 2 rows
names(responses) <- c("Program", "Date", "Time", "Interest") # Assign the right field names

for(i in 3:8){
  tmp <- read_xlsx("Phd Links.xlsx", sheet = i)[-c(1:2),] # Remove first 2 rows
  names(tmp) <- c("Program", "Date", "Time", "Interest") # Assign the right field names
  responses <- rbind(responses, tmp)
}

# Clean up the data types
responses$Interest <- factor(responses$Interest)
responses$Program <- factor(responses$Program)
responses$Date <- as.Date(as.numeric(str_remove_all(responses$Date, "[.]0")), origin="1899-12-30")
responses$Time <- chron::times(as.numeric(responses$Time))
responses$DateTime <- lubridate::as_datetime(paste(responses$Date, responses$Time))
plot(responses$DateTime, responses$Interest)

count_program_interest <- responses %>% 
  group_by(Program, Interest) %>%
  reframe(Count = n()) 


write.csv(responses, "responses_long.csv")


responses$Interest <- as.numeric(responses$Interest)
responses %>% 
  filter(Interest >= 3) %>% 
  group_by(Program, Interest) %>%
  reframe(Count = n()) 

library(tidyr)
wide <- responses %>% 
  spread(Program, Interest) 

write.csv(wide, "responses_wide.csv")
