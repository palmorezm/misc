
# CDC Wonder Data Request
# Extract Data from txt files provided by: 
# https://wonder.cdc.gov/controller/datarequest/D176;jsessionid=17DD23358407D983242644740882

# This set consists of results grouped by: 
    # 1. County
    # 2. UCD - ICD Chapter
    # 3. Race
    # 4. Hispanic Origin
    # 5. Gender 
# Other Data Notes
# For the state of WI only. Not be reviewed across states. 
# For research and surveillance purposes only. 
# Specifc date set at 01 of each month from data source
# This data is nonprovisional from "Current Final Multiple Cause of Death Data"
# Source: https://wonder.cdc.gov/mcd.html

UCDREG_1999 <- read.delim("C:/Users/Zachary.Palmore/Downloads/REG_ICD_1999.txt")

library(stringr)
library(dplyr)
paths <- Sys.glob("Data/UCD/REG_*.txt")
paths_ucd_years <- str_extract_all(paths, "\\d{4}")
# paths_ucd_base <- str_extract_all(paths, "\\s*UCD_ICD_")
paths_ucd_base <- "Data/UCD/"
paths_ucd_mid <- "REG_ICD_"
paths_file_type <- ".txt"
negative_index_value <- as.numeric(-77)

# df <- data.frame(year = t(paths_ucd_years))
# data.frame(years = paths_ucd_years) 
# df <- ""
# df$year <- paths_ucd_years
# df <- as.data.frame(df)

paths_ucd_years <- paths_ucd_years %>% 
  data.frame() %>% 
  gather() %>% 
  arrange(desc(value)) %>% 
  mutate(file_name = paste0(paths_ucd_mid, value, paths_file_type))

# paste0(paths_ucd_base[[1]], paths_ucd_years$value)

# for (y in length(paths_ucd_years$value)){
#   df$paths <- tibble(path = paste0(paths_ucd_base[[1]], paths_ucd_years$value))
# }


# Extracts for the year 1999 (at the end of the values - 2020 to 1999)
# for (i in length(paths_ucd_years$value)){
#     test <- head(read.delim(file = paste0("Data/UCD/", paths_ucd_years$path[[i]], ".txt"), 
#                         header = T, sep = "\t", quote = "\"", dec = ".", fill = T, 
#                         comment.char = ""), -65) %>%
#                       mutate(Year = paths_ucd_years$value[[i]], 
#                          Month = "ALL", 
#                          Date = as.Date(paste0(paths_ucd_years$value[[i]], "-12-31"))
#                          )
# }

df_all <- head(read.delim(file = paste0(paths_ucd_base, paths_ucd_mid, paths_ucd_years$value[[1]], paths_file_type), 
                          header = T, sep = "\t", quote = "\"", dec = ".", 
                          fill = T, comment.char = ""), negative_index_value) %>%
  mutate(Year = 2020,
         Month = "ALL",
         Date = as.Date("2020-12-31"))


# Create a dataframe for 1 df 
# This gives it the right shape (dims)
# Either denote duplicate information in the notes field OR
# read in the first dataframe from the list to give it the right shape then 
# start at the second item in the list in the loop since the first has already 
# been read in
# df$Notes <- "Duplicate"
# df_all <- df

for (i in 2:length(paths_ucd_years$value)){
  
  # Read-in 
  df <- head(read.delim(file = paste0(paths_ucd_base, paths_ucd_mid, 
                                      paths_ucd_years$value[[i]], paths_file_type), 
                        header = T, sep = "\t", quote = "\"", dec = ".", fill = T, 
                        comment.char = ""), negative_index_value) %>%
    mutate(Year = paths_ucd_years$value[[i]], 
           Month = "ALL", 
           Date = as.Date(paste0(paths_ucd_years$value[[i]], "-12-31")))
  df_all <- rbind(df_all, df)
  
}

UCDREG <- df_all

save(UCDREG,file = "Data/UCDREG.Rdata")
