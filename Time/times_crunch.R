
# Times reevaluation of existing methods
# Contains time spent on projects for Rock

# Challenge: There are no methods for tracking data science projects or time at RCPHD. Management is interested
# learning what the data scientist is doing, when, and gaining an overall better understanding of how much time
# the data scientist spends on various projects. At this time (6/14/2022) they are also interested in learning
# what projects the data scientist is choosing to work on (since no specifics have been given). It is also 
# important that the process be automatic as much as possible. 

# Solution: Time has been getting tracked in a Google sheet since 3/21/2022. It contains various categories that 
# are helpful for answering questions the data scientist or management may have including start and end times 
# for each date and project. 

# This Script:
# In this script a new method of crunching the time values is attempted. In it
# the time is calculated by each job category and/or anaconda survey areas.

# Packages
require(pacman)
pacman::p_install(c("dplyr", "lubridate", "data.table", "ggplot2"))

# Source
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
work_duration_week <- work_duration_day * 5 
work_week_startdate <- seq(
  as_date(min(times$Date)), as_date(max(times$Date)), by = "7 days"
) 
df_all <- data.frame(Date = seq(work_week_startdate[[1]], 
                                work_week_startdate[[1]] + 4, by = "day"))
for (i in 2:length(work_week_startdate)){
  df <- data.frame(Date = seq(work_week_startdate[[i]], 
                              work_week_startdate[[i]] + 4, by = "day"))
  df_all <- rbind(df_all, df)
}

df_all <- df_all %>% 
  mutate(time_day = as.integer(work_duration_day)) 
setDT(times)
setDT(df_all)
df <- times[df_all, on = "Date"] # Merge to DF
df$project_duration[which(is.na(df$project_duration))] <- 0 # NA is equivalent to zero s
df[70,] # NA because it is not in the dataset
df$Primary_Venn_Class <- toupper(df$Primary_Venn_Class)
df %>% 
  group_by(Job_Category) %>% 
  summarise(sum(project_duration))
# df %>% 
#   group_by(DataScience_Anaconda_Survey_TimeSpentOn_Area) %>% 
#   summarise(t = sum(project_duration)) %>%
#   na.exclude() %>% 
#   ggplot(aes(reorder(DataScience_Anaconda_Survey_TimeSpentOn_Area, t))) + 
#   geom_bar(stat = "identity", aes(y = t/360, col = DataScience_Anaconda_Survey_TimeSpentOn_Area), 
#            fill = "white") + theme(legend.position = "none")\
df %>% 
  group_by(Primary_Venn_Class) %>% 
  summarise(t = sum(project_duration), 
            hours = as.numeric(t)/360)

# How much time have I had leftover each day? 
# How would this line chart look with 0 as the minimum time (ignore OT)? 
df %>% 
  group_by(Project) %>% 
  summarise(prj_time = sum(as.numeric(project_duration)), 
            # non_prj_time = as.numeric(work_duration_day - prj_time),
            # tot_time = (prj_time + non_prj_time), # = work_duration_day
            # project_time = work_duration_day - non_prj_time 
  ) %>% # prj_time and project_time should overlay exactly on plot 
  filter(prj_time >= 0) %>% 
  gather(key, value, -Project) %>% 
  ggplot(aes(Project, (value/3600))) + 
  geom_col(position = "stack", fill = "light blue", col = "black", alpha = 0.75) + 
  geom_hline(yintercept = df.stats$med, lty = "dashed") + coord_flip() +
  labs(x = "Day", y = "Hours", title = "Title", subtitle = "subtitle") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), 
                          plot.subtitle = element_text(hjust = 0.5))





