
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

### ----- Start Monthly Data ----- ###
# Result is stored as REG_MORT

base_path_2018 <- "Data/REG/2018/"
base_path_2019 <- "Data/REG/2019/"
base_path_2020 <- "Data/REG/2020/"
base_path_2021 <- "Data/REG/2021/"
base_path_2022 <- "Data/REG/2022/"

# paste0(base_path_2018, "REG_2018_JAN.txt")
negative_index_value <- as.integer(-65)

REG_2018_JAN <- head(read.delim(file = paste0(base_path_2018, "REG_2018_JAN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "JAN", 
         Date = as.Date("2018-01-01"))

REG_2018_FEB <- head(read.delim(file = paste0(base_path_2018, "REG_2018_FEB.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "FEB", 
         Date = as.Date("2018-02-01"))

REG_2018_MAR <- head(read.delim(file = paste0(base_path_2018, "REG_2018_MAR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "MAR", 
         Date = as.Date("2018-03-01"))

REG_2018_APR <- head(read.delim(file = paste0(base_path_2018, "REG_2018_APR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "APR", 
         Date = as.Date("2018-04-01"))

REG_2018_MAY <- head(read.delim(file = paste0(base_path_2018, "REG_2018_MAY.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "MAY", 
         Date = as.Date("2018-05-01"))

REG_2018_JUN <- head(read.delim(file = paste0(base_path_2018, "REG_2018_JUN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "JUN", 
         Date = as.Date("2018-06-01"))

REG_2018_JUL <- head(read.delim(file = paste0(base_path_2018, "REG_2018_JUL.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "JUL", 
         Date = as.Date("2018-07-01"))

REG_2018_AUG <- head(read.delim(file = paste0(base_path_2018, "REG_2018_AUG.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "AUG", 
         Date = as.Date("2018-08-01"))

REG_2018_SEP <- head(read.delim(file = paste0(base_path_2018, "REG_2018_SEP.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "SEP", 
         Date = as.Date("2018-09-01"))

REG_2018_OCT <- head(read.delim(file = paste0(base_path_2018, "REG_2018_OCT.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "OCT", 
         Date = as.Date("2018-10-01"))

REG_2018_NOV <- head(read.delim(file = paste0(base_path_2018, "REG_2018_NOV.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "NOV", 
         Date = as.Date("2018-11-01"))

REG_2018_DEC <- head(read.delim(file = paste0(base_path_2018, "REG_2018_DEC.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2018, 
         Month = "DEC", 
         Date = as.Date("2018-12-01"))

REG_2018_MONTHS <- 
  rbind(REG_2018_JAN, REG_2018_FEB, REG_2018_MAR, REG_2018_APR, REG_2018_MAY, REG_2018_JUN,
        REG_2018_JUL, REG_2018_AUG, REG_2018_SEP, REG_2018_OCT, REG_2018_NOV, REG_2018_DEC)
 




 
REG_2019_JAN <- head(read.delim(file = paste0(base_path_2019, "REG_2019_JAN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "JAN", 
         Date = as.Date("2019-01-01"))

REG_2019_FEB <- head(read.delim(file = paste0(base_path_2019, "REG_2019_FEB.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "FEB", 
         Date = as.Date("2019-02-01"))

REG_2019_MAR <- head(read.delim(file = paste0(base_path_2019, "REG_2019_MAR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "MAR", 
         Date = as.Date("2019-03-01"))

REG_2019_APR <- head(read.delim(file = paste0(base_path_2019, "REG_2019_APR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "APR", 
         Date = as.Date("2019-04-01"))

REG_2019_MAY <- head(read.delim(file = paste0(base_path_2019, "REG_2019_MAY.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "MAY", 
         Date = as.Date("2019-05-01"))

REG_2019_JUN <- head(read.delim(file = paste0(base_path_2019, "REG_2019_JUN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "JUN", 
         Date = as.Date("2019-06-01"))

REG_2019_JUL <- head(read.delim(file = paste0(base_path_2019, "REG_2019_JUL.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "JUL", 
         Date = as.Date("2019-07-01"))

REG_2019_AUG <- head(read.delim(file = paste0(base_path_2019, "REG_2019_AUG.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "AUG", 
         Date = as.Date("2019-08-01"))

REG_2019_SEP <- head(read.delim(file = paste0(base_path_2019, "REG_2019_SEP.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "SEP", 
         Date = as.Date("2019-09-01"))

REG_2019_OCT <- head(read.delim(file = paste0(base_path_2019, "REG_2019_OCT.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "OCT", 
         Date = as.Date("2019-10-01"))

REG_2019_NOV <- head(read.delim(file = paste0(base_path_2019, "REG_2019_NOV.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "NOV", 
         Date = as.Date("2019-11-01"))

REG_2019_DEC <- head(read.delim(file = paste0(base_path_2019, "REG_2019_OCT.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2019, 
         Month = "DEC", 
         Date = as.Date("2019-12-01"))

REG_2019_MONTHS <- 
  rbind(REG_2019_JAN, REG_2019_FEB, REG_2019_MAR, REG_2019_APR, REG_2019_MAY, REG_2019_JUN,
        REG_2019_JUL, REG_2019_AUG, REG_2019_SEP, REG_2019_OCT, REG_2019_NOV, REG_2019_DEC)
 


 
REG_2020_JAN <- head(read.delim(file = paste0(base_path_2020, "REG_2020_JAN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "JAN", 
         Date = as.Date("2020-01-01"))

REG_2020_FEB <- head(read.delim(file = paste0(base_path_2020, "REG_2020_FEB.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "FEB", 
         Date = as.Date("2020-02-01"))

REG_2020_MAR <- head(read.delim(file = paste0(base_path_2020, "REG_2020_MAR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "MAR", 
         Date = as.Date("2020-03-01"))

REG_2020_APR <- head(read.delim(file = paste0(base_path_2020, "REG_2020_APR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "APR", 
         Date = as.Date("2020-04-01"))

REG_2020_MAY <- head(read.delim(file = paste0(base_path_2020, "REG_2020_MAY.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "MAY", 
         Date = as.Date("2020-05-01"))

REG_2020_JUN <- head(read.delim(file = paste0(base_path_2020, "REG_2020_JUN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "JUN", 
         Date = as.Date("2020-06-01"))

REG_2020_JUL <- head(read.delim(file = paste0(base_path_2020, "REG_2020_JUL.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "JUL", 
         Date = as.Date("2020-07-01"))

REG_2020_AUG <- head(read.delim(file = paste0(base_path_2020, "REG_2020_AUG.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "AUG", 
         Date = as.Date("2020-08-01"))

REG_2020_SEP <- head(read.delim(file = paste0(base_path_2020, "REG_2020_SEP.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "SEP", 
         Date = as.Date("2020-09-01"))

REG_2020_OCT <- head(read.delim(file = paste0(base_path_2020, "REG_2020_OCT.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "OCT", 
         Date = as.Date("2020-10-01"))

REG_2020_NOV <- head(read.delim(file = paste0(base_path_2020, "REG_2020_NOV.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "NOV", 
         Date = as.Date("2020-11-01"))

REG_2020_DEC <- head(read.delim(file = paste0(base_path_2020, "REG_2020_DEC.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2020, 
         Month = "DEC", 
         Date = as.Date("2020-12-01"))

REG_2020_MONTHS <- 
  rbind(REG_2020_JAN, REG_2020_FEB, REG_2020_MAR, REG_2020_APR, REG_2020_MAY, REG_2020_JUN,
        REG_2020_JUL, REG_2020_AUG, REG_2020_SEP, REG_2020_OCT, REG_2020_NOV, REG_2020_DEC)
 


 
REG_2021_JAN <- head(read.delim(file = paste0(base_path_2021, "REG_2021_JAN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "JAN", 
         Date = as.Date("2021-01-01"))

REG_2021_FEB <- head(read.delim(file = paste0(base_path_2021, "REG_2021_FEB.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "FEB", 
         Date = as.Date("2021-02-01"))

REG_2021_MAR <- head(read.delim(file = paste0(base_path_2021, "REG_2021_MAR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "MAR", 
         Date = as.Date("2021-03-01"))

REG_2021_APR <- head(read.delim(file = paste0(base_path_2021, "REG_2021_APR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "APR", 
         Date = as.Date("2021-04-01"))

REG_2021_MAY <- head(read.delim(file = paste0(base_path_2021, "REG_2021_MAY.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "MAY", 
         Date = as.Date("2021-05-01"))

REG_2021_JUN <- head(read.delim(file = paste0(base_path_2021, "REG_2021_JUN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "JUN", 
         Date = as.Date("2021-06-01"))

REG_2021_JUL <- head(read.delim(file = paste0(base_path_2021, "REG_2021_JUL.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "JUL", 
         Date = as.Date("2021-07-01"))

REG_2021_AUG <- head(read.delim(file = paste0(base_path_2021, "REG_2021_AUG.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "AUG", 
         Date = as.Date("2021-08-01"))

REG_2021_SEP <- head(read.delim(file = paste0(base_path_2021, "REG_2021_SEP.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "SEP", 
         Date = as.Date("2021-09-01"))

REG_2021_OCT <- head(read.delim(file = paste0(base_path_2021, "REG_2021_OCT.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "OCT", 
         Date = as.Date("2021-10-01"))

REG_2021_NOV <- head(read.delim(file = paste0(base_path_2021, "REG_2021_NOV.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "NOV", 
         Date = as.Date("2021-11-01"))

REG_2021_DEC <- head(read.delim(file = paste0(base_path_2021, "REG_2021_DEC.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2021, 
         Month = "DEC", 
         Date = as.Date("2021-12-01"))

REG_2021_MONTHS <- 
  rbind(REG_2021_JAN, REG_2021_FEB, REG_2021_MAR, REG_2021_APR, REG_2021_MAY, REG_2021_JUN,
        REG_2021_JUL, REG_2021_AUG, REG_2021_SEP, REG_2021_OCT, REG_2021_NOV, REG_2021_DEC)
 


 
REG_2022_JAN <- head(read.delim(file = paste0(base_path_2022, "REG_2022_JAN.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2022, 
         Month = "JAN", 
         Date = as.Date("2022-01-01"))

REG_2022_FEB <- head(read.delim(file = paste0(base_path_2022, "REG_2022_FEB.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2022, 
         Month = "FEB", 
         Date = as.Date("2022-02-01"))

REG_2022_MAR <- head(read.delim(file = paste0(base_path_2022, "REG_2022_MAR.txt"), header = T, sep = "\t", 
                                quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
  mutate(Year = 2022, 
         Month = "MAR", 
         Date = as.Date("2022-03-01"))
# 
# REG_2022_APR <- head(read.delim(file = "Data/2022/REG_2022_APR.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "APR", 
#          Date = as.Date("2022-04-01"))
# 
# REG_2022_MAY <- head(read.delim(file = "Data/2022/REG_2022_MAY.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "MAY", 
#          Date = as.Date("2022-05-01"))
# 
# REG_2022_JUN <- head(read.delim(file = "Data/2022/REG_2022_JUN.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "JUN", 
#          Date = as.Date("2022-06-01"))
# 
# REG_2022_JUL <- head(read.delim(file = "Data/2022/REG_2022_JUL.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "JUL", 
#          Date = as.Date("2022-07-01"))
# 
# REG_2022_AUG <- head(read.delim(file = "Data/2022/REG_2022_AUG.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "AUG", 
#          Date = as.Date("2022-08-01"))
# 
# REG_2022_SEP <- head(read.delim(file = "Data/2022/REG_2022_SEP.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "SEP", 
#          Date = as.Date("2022-09-01"))
# 
# REG_2022_OCT <- head(read.delim(file = "Data/2022/REG_2022_OCT.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "OCT", 
#          Date = as.Date("2022-10-01"))
# 
# REG_2022_NOV <- head(read.delim(file = "Data/2022/REG_2022_NOV.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "NOV", 
#          Date = as.Date("2022-11-01"))
# 
# REG_2022_DEC <- head(read.delim(file = "Data/2022/REG_2022_DEC.txt", header = T, sep = "\t", 
#                                 quote = "\"", dec = ".", fill = T, comment.char = ""), -64) %>%
#   mutate(Year = 2022, 
#          Month = "DEC", 
#          Date = as.Date("2022-12-01"))
# 
REG_2022_MONTHS <- 
  rbind(REG_2022_JAN, REG_2022_FEB, REG_2022_MAR#, REG_2022_APR, REG_2022_MAY, REG_2022_JUN, 
        # REG_2022_JUL, REG_2022_AUG, REG_2022_SEP, REG_2022_OCT, REG_2022_NOV, REG_2022_DEC
  )
 
REG_MORT <- rbind(REG_2018_MONTHS, REG_2019_MONTHS, REG_2020_MONTHS, 
                  REG_2021_MONTHS, REG_2022_MONTHS)
 
### ----- End Monthly Data ----- ###
# Result is stored as REG_MORT


############################


### ----- Start Yearly Data ----- ### 
# Result is stored as reg_mort

# REG stands for Race, Ethnicity, Gender
REG2018 <- read.delim(file = "Data/ICD10_REG_2018.txt", 
                      header = T, sep = "\t", quote = "\"", 
                      dec = ".", fill = T, comment.char = "")
REG2019 <- read.delim(file = "Data/ICD10_REG_2019.txt", header = T, 
                      sep = "\t", quote = "\"", 
                      dec = ".", fill = T, comment.char = "")
REG2020 <- read.delim(file = "Data/ICD10_REG_2020.txt", header = T, 
                      sep = "\t", quote = "\"", 
                      dec = ".", fill = T, comment.char = "")
REG2021 <- read.delim(file = "Data/ICD10_REG_2021.txt", header = T, 
                      sep = "\t", quote = "\"", 
                      dec = ".", fill = T, comment.char = "")
REG2022 <- read.delim(file = "Data/ICD10_REG_2022.txt", header = T, 
                      sep = "\t", quote = "\"", 
                      dec = ".", fill = T, comment.char = "")

REG2018$Year <- as.numeric(2018)
REG2019$Year <- as.numeric(2019)
REG2020$Year <- as.numeric(2020)
REG2021$Year <- as.numeric(2021)
REG2022$Year <- as.numeric(2022)

reg_mort <- rbind(REG2018, REG2019, REG2020, REG2021, REG2022) 
# Note that CDC data goes through March in 2022

### ----- End Yearly Data ----- ### 
# Result is stored as reg_mort


### ----- Begin UCD ICD-10 Non-Provisional ----- ###
# Result is stored as UCD

# head(read.delim(file = "Data/UCD/UCD_ICD_1999.txt", header = T, sep = "\t", 
#                 quote = "\"", dec = ".", fill = T, comment.char = ""), negative_index_value)
# 
# head(read.delim(file = "Data/UCD/UCD_ICD_1999.txt", header = T, sep = "\t",
#                 quote = "\"", dec = ".", fill = T, comment.char = ""), -65) %>%
#   mutate(Year = 1999,
#          Month = "ALL",
#          Date = as.Date("2022-12-31"))

paths <- Sys.glob("Data/UCD/UCD_*.txt")
paths_ucd_years <- str_extract_all(paths, "\\d{4}")
# paths_ucd_base <- str_extract_all(paths, "\\s*UCD_ICD_")
paths_ucd_base <- "Data/UCD/"
paths_ucd_mid <- "UCD_ICD_"
paths_file_type <- ".txt"

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

UCD <- df_all

### ----- End UCD ICD-10 Non-Provisional ----- ###
# Result is stored as UCD

save(REG_MORT, reg_mort, UCD, file = "Data/reg_mort.rdata")



