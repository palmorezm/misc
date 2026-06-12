
# read in 366 sheets of enrollment data
# read in 366 sheets of dental claims

# install.packages("dplyr", "lubridate") 
require(dplyr) # install if needed first
require(lubridate)

rm(list = ls()) # Clears the variables in the global environment (clean start)
# path <- "C:/Users/Zachary Palmore/GitHub/misc/For_Loop/4bryce/spreadsheets/"  # Path to the spreadsheets
# on work computer:
path <- "C:/Users/PalmoZM/GitHub/misc/For_Loop/4bryce/data/"
files <- list.files(path) # the names of all the files in the folder 
data <- read.csv(paste0(path, files[1])) # combine path to folder with list of file names 
                                    # and run first read in to give the structure of data 

dental_files <- files[grep("dental", files)]
enrollment_files <- files[grep("enrollment", files)]

dental_claims1 <- read.csv(paste0(path, dental_files[1])) # this had an 1 and is now 'i' 
MA_Dntl_Clm_All_Cnt <- dental_claims1 %>% 
  filter(`Pilot_County_Indicator` == '0') %>%
  filter(`FQHC_Indicator` == "0") %>% 
  filter(`RHC_or_SBS_Indicator` == "0") %>% 
  group_by(Data_Week) %>%
  count() %>%
  rename(MA_Dntl_Clm_All_Cnt = n)



sum(MA_Dntl_Clm_All_Cnt$MA_Dntl_Clm_All_Cnt)

as.Date(MA_Dntl_Clm_All_Cnt$Data_Week)
class(MA_Dntl_Clm_All_Cnt$Data_Week)

lubridate::yday(as.Date(MA_Dntl_Clm_All_Cnt$Data_Week))
(MA_Dntl_Clm_All_Cnt$Data_Week)

dmy(MA_Dntl_Clm_All_Cnt$`Data_Week`) %>% as.Date(`Data_Week`)
class(MA_Dntl_Clm_All_Cnt$Data.Week)

# Logging the run times 
# Define the number of iterations
n_iter <- length(files)

# Pre-allocate the log data frame
log <- data.frame(
  iteration = 1:n_iter,
  start_time = as.POSIXct(rep(NA, n_iter)),
  end_time = as.POSIXct(rep(NA, n_iter)),
  iteration_time_secs = numeric(n_iter)
)

# Run the loop from 0 element to the final element in the list of files 
for (i in 1:length(files)){
  start_time <- Sys.time()
  ptm <- proc.time() 
  # Create a progress indicator
  print(paste(paste0(round(i / length(dental_files)*100, 2),"%"),
              "Working on", i, start_time[[1]], "path:", paste0(path, dental_files[i]), "out of", length(dental_files)))
  tmp <- read.csv(paste0(path, dental_files[i])) # this had an 1 and is now 'i' 
  # to iterate to 0, 1, 2, 3, etc... 
  # for length of files to read and 
  # do some analysis or summarizing
  # write out your plans here:
  # later they can be called with source(your_analysis_script.R)
  # For example, let's sum data by week:
  tmp_sum_by_week <- tmp %>%
    # Convert '01JAN2016' format using %d%b%Y
    mutate(date_obj = as.Date(date, format = "%d%b%Y")) %>%
    # Round down to the nearest week (starts on Sunday by default)
    mutate(week_start = floor_date(date_obj, unit = "week")) %>%
    # Group and sum
    group_by(week_start) %>%
    summarise(total_value = sum(ds_o3_pred))
  sum_by_week <- rbind(sum_by_week, tmp_sum_by_week) # Gather each file by name
  x
  #####
  # additional analysis here using tmp as data in interation
  #####
  
  # For time tracking and comparing interations
  end_time <- Sys.time() # Record time after analysis
  iteration_time <- end_time - start_time # Calculate difference
  # Alternative timing method with CPU usage check
  diff <- proc.time() - ptm # provides user and CPU time
  print(diff) # Difference should be near zero for your operations (check)
  # Print the result for each iteration
  cat("Iteration", i, "took:", iteration_time, "\n")
  
  # Store iteration time data in log
  log$start_time[i] <- start_time
  log$end_time[i]   <- end_time
  log$iteration_time_secs[i] <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Isolate to the data you want to keep, remove the rest
  keep_vars <- c("sum_by_week", "files", "path", "i", "log")
  rm(list= ls()[!ls() %in% keep_vars])
  Sys.sleep(1) # take a 3 second break
}

# Isolate to the data you want to keep, remove the rest
keep_vars <- c("sum_by_week", "files", "path", "i", "log")
rm(list= ls()[!ls() %in% keep_vars])



