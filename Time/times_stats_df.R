

require(data.table)
library(lubridate)
require(dplyr)
require(ggplot2)
require(tidyr)
require(stringr)
library(ggvenn)
require(ggVennDiagram)
# list.of.packages <- c("ggplot2", "dplyr", "lubridate", "data.table")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# alternative?
# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("yanlinlin82/ggvenn")
# link <- paste0("https://docs.google.com/spreadsheets/",
#        "d/e/2PACX-1vRbI6ljkD0T2Mbf0_o0oTczrPm6d7fSQTiVNM",
#        "-u2MPe56tQ-Ex92rUQbzaT3OPvJQ/pub?output=xlsx")
glink <- paste0("https://docs.google.com/spreadsheets/",
                "d/e/2PACX-1vQrgIzP3_8Fbn7I7kiwKuH8aYPzIRYDXqMj",
                "AIJo9ejpN902Yzv5Gqetbq3QX5DqjOddVT3caxxftuii/pub?",
                "gid=343744476&single=true&output=csv")
times <- read.delim(glink, header = T, sep = ",")
times$Date <- mdy(times$Date)
times$Start <- hms::as_hms(lubridate::parse_date_time(times$Start, "%I:%M %p"))
times$End <- hms::as_hms(lubridate::parse_date_time(times$End, "%I:%M %p"))
times$project_duration <- (times$End - times$Start)
work_duration_day <- 8 * 60 * 60 # h * m * s = s
work_duration_week <- work_duration_day * 5 # 5 days per work week (excluding holidays)
# parse_date_time(times$Start, "%I:%M %p") # Convert to datetime UTC
# class(hms::as_hms(lubridate::parse_date_time(times$Start, "%I:%M %p")))
# seq(as_date(min(times$Date)), as_date(max(times$Date)), by="day") # Includes weekends, holidays
# unique(times$Date) # Should be fewer than in the sequence
# seq(as_date(min(times$Date)), as_date(max(times$Date)), by="week")
# data.frame(Date = seq(as_date(min(times$Date)), as_date(max(times$Date)), by="day"))
# seq(as_date(min(times$Date)), as_date(max(times$Date)), by = "day")
#######################################################################
# What to do about missing days or days when no project time is entered? 
# Need an estimator or simulation with the expected normal work days
# From the start date when data was first entered (3/21) create a sequence of dates every 7 days
work_week_startdate <- seq(
  as_date(min(times$Date)), as_date(max(times$Date)), by = "7 days"
) 
# Create a df with the first work week
df_all <- data.frame(Date = seq(work_week_startdate[[1]], 
                                work_week_startdate[[1]] + 4, by = "day"))
# add 5 days for every work week startdate based on start of data collection 
# with times which occurred on a Monday 
for (i in 2:length(work_week_startdate)){
  df <- data.frame(Date = seq(work_week_startdate[[i]], 
                              work_week_startdate[[i]] + 4, by = "day"))
  df_all <- rbind(df_all, df)
}

df_all <- df_all %>% 
  mutate(time_day = as.integer(work_duration_day)) 
# Not needed with join via data.table? 
# df_merge <- times %>%
#   left_join(df_all, by = Date)

# Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
setDT(times)
setDT(df_all)
df <- times[df_all, on = "Date"] # Merge to DF
df$project_duration[which(is.na(df$project_duration))] <- 0 # NA is equivalent to zero s

df.stats <- df %>% 
  group_by(day = floor_date(Date, "day")) %>% 
  summarise(prj_time = sum(as.numeric(project_duration)), 
            non_prj_time = as.numeric(work_duration_day - prj_time),
            tot_time = (prj_time + non_prj_time)) %>% 
  summarise(avg = mean(prj_time/3600),
            med = median(prj_time/3600))