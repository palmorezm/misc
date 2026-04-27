
rm(list = ls()) # Clears the variables in the global environment (clean start)
path <- "C:/Users/Zachary Palmore/GitHub/misc/For_Loop/4bryce/spreadsheets/"  # Path to the spreadsheets
files <- list.files(path) # the names of all the files in the folder 
data <- read.csv(paste0(path, files[1])) # combine path to folder with list of file names and run first read in to give the structure of data 

# Run the loop from 0 element to the final element in the list of files 
for (i in 0:length(files)){
  tmp <- read.csv(paste0(path, files[i])) # this had an 1 and is now 'i' 
                                          # to iterate to 0, 1, 2, 3, etc... 
                                          # for length of files to read and 
  data <- rbind(data, tmp) # Gather each file by name
}

# Isolate to the data you want to keep, remove the rest
variables <- ls()
rm(list=c(variables[variables != "data"], "variables"))


