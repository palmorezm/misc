
# 
library(dplyr)
MadisonFiles <- list.files(path = "exports/winter/", pattern = "Madison\\d{1+}.csv")
Mad1 <- read.csv(paste0("exports/winter/", MadisonFiles[[1]]), header = TRUE)
read.csv(paste0("exports/winter/", MadisonFiles[[1]]), header = TRUE) %>% 
  mutate(FileName = MadisonFiles[[1]], 
         Date_of_scrape = Sys.Date() - (length(MadisonFiles) - 1))
# Need a better way to get the date to match with the proper day it was run
# Because of the variation in timing, it might need to be entered by hand
MadAll <- read.csv(paste0("exports/winter/", MadisonFiles[[1]]), header = TRUE) %>% 
  mutate(FileName = MadisonFiles[[1]], 
         Date_of_scrape = Sys.Date() - (length(MadisonFiles) - 1))
results <-  read.csv(paste0("exports/winter/", MadisonFiles[[1]]), header = TRUE) %>% 
  mutate(FileName = MadisonFiles[[1]], 
         Date_of_scrape = Sys.Date() - (length(MadisonFiles) - 1))
Mad1 <- MadAll 

for (i in 2:length(MadisonFiles)){
  # Make a loop through the list of files with Madison in the name
  Mad1 <- read.csv(paste0("exports/winter/", MadisonFiles[[i]]), header = TRUE) %>% 
    mutate(FileName = MadisonFiles[[i]], 
           Date_of_scrape = Sys.Date() - (length(MadisonFiles) - i))
  # reassign them to a data frame 
  MadAll <- rbind(MadAll, Mad1)
  # keep the file name so we know where each observation came from 
  # append a the date it was read in to the file
  # add an indicator of progress
}

# Mad Review
length(unique(MadAll$Page)) # 11 Scrapes total
unique(MadAll$Company_Name)
unique(MadAll$Location_Name)

# Repeat for each of the locations
# Ignore the company list (that's for Mel to do a quick scan)

# Consider sorting the files greatest the least
# also consider creating a list for all locations Madison, Milwaukee, Chicago
# Might have to use strings to do this and sort the numbers
numbers <- grep(MadisonFiles, pattern = "\\d") # This sort automatically
numbers <- sort(grep(MadisonFiles, pattern="\\d"))
# The names of the files on day 11 and 12 make it problematic - can we reanme them? It'd be easier



MadisonFiles <- list.files(path = "exports/winter/", pattern = "Madison\\d{1+}.csv")
ChicagoFiles <- list.files(path = "exports/winter/", pattern = "Chicago\\d{1+}.csv")
MilwaukeeFiles <- list.files(path = "exports/winter/", pattern = "Milwaukee\\d{1+}.csv")
AllFiles <- list(MadisonFiles, MilwaukeeFiles, ChicagoFiles)
# Remove the stuff that contains "nextmorning" 
# Keep only the stuff that has the location then a number

results <-  read.csv(paste0("exports/winter/", AllFiles[[1]]), header = TRUE) %>% 
  mutate(FileName = AllFiles[[1]], 
         Date_of_scrape = Sys.Date() - (length(AllFiles) - 1))
resultsAll <- results

for (i in 2:length(AllFiles)){
  # Make a loop through the list of files with Madison in the name
  results <- read.csv(paste0("exports/winter/", MadisonFiles[[i]]), header = TRUE) %>% 
    mutate(FileName = MadisonFiles[[i]], 
           Date_of_scrape = Sys.Date() - (length(MadisonFiles) - i))
  # reassign them to a data frame 
  MadAll <- rbind(MadAll, results)
  # keep the file name so we know where each observation came from 
  # append a the date it was read in to the file
  # add an indicator of progress
}

