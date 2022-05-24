
# REG Extraction
# CDC Wonder Data Request
# Extract Data from txt files provided by: 
# https://wonder.cdc.gov/controller/datarequest/D176;jsessionid=17DD23358407D983242644740882

# This set consists of results grouped by: 
    # 1. Occurrence County
    # 2. UCD - ICD Chapter
    # 3. Single Race 6 
    # 4. Hispanic Origin
    # 5. Gender 
# Other Data Notes
# For the state of WI only. Not be reviewed across states. 
# For research and surveillance purposes only. 
# Specifc date set at 01 of each month from data source
# 2021 is provisional - subject to change with updates
# 2022 is incomplete (March most recent month) and is also provisional 



### ----- Packages ----- ### 
library(dplyr)
library(stringr)
library(tidyr)

### ----- Extraction via Globs ----- ###
# Result is stored as REGp

paths_2018 <- Sys.glob("Data/REG/2018/*.txt")
paths_2019 <- Sys.glob("Data/REG/2019/*.txt")
paths_2020 <- Sys.glob("Data/REG/2020/*.txt")
paths_2021 <- Sys.glob("Data/REG/2021/*.txt")
paths_2022 <- Sys.glob("Data/REG/2022/*.txt")

paths <- cbind(paths_2018, paths_2019, paths_2020, paths_2021, paths_2022)

paths_ucd_years <- paths_ucd_years %>% 
  data.frame() %>% 
  gather() %>% 
  arrange(desc(value)) %>% 
  mutate(file_name = paste0(paths_ucd_mid, value, paths_file_type))

df_all <- head(read.delim(file = paste0(paths_ucd_base, paths_ucd_mid, paths_ucd_years$value[[1]], paths_file_type), 
                          header = T, sep = "\t", quote = "\"", dec = ".", 
                          fill = T, comment.char = ""), negative_index_value) %>%
  mutate(Year = 2020,
         Month = "ALL",
         Date = as.Date("2020-12-31"))

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

UCD <- df_all


