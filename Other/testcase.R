library(dplyr)
library(tidyr)

# Real Personal Income (RPI)
# Extraction
rpi <- data.frame(read.csv(
  "https://raw.githubusercontent.com/palmorezm/msds/main/698/Data/rpis_10yr.csv")) 
rpi <- rpi[4:nrow(rpi),]
colnames(rpi) <- rpi[1,]
rpi <- rpi[-1,]
View(rpi)

# Tidying
mrpi <- rpi %>% 
  gather(key, value, -GeoFips, -GeoName, -LineCode, -Description) 
rpi.percapita <- subset(mrpi, LineCode==2)  
rpi.income <- subset(melted, LineCode==1)
mrpi <- rbind(rpi.income, rpi.percapita)
View(mrpi)

# Determine the presence of and deal with missing Values
sum(is.na(mrpi)) # 28 
which(is.na(mrpi))
mrpi[49071,] # Displays the first selected NA
mrpi[which(is.na(mrpi)),] # Displays all NA's
na.omit(mrpi)