
# Times read in and data prep 
# Contains time spent on projects for Rock

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
#times$Date <- mdy(times$Date)
#sort(as_datetime(times$Date))
# times$Date
# as_date(times$Date)
# as.Date(times$Date)
# class(times$Date)
# which(is.na(mdy(times$Date)))
# times$Date[510]
# times$Date <- as_date(times$Date)
#min(times$Date)
#min(as.Date(times$Date))
#sort(times$Date)[[1]]
#times$Date[[length(times$Date)]]
sort(times$Date)[[1]]
sort(mdy(times$Date), na.rm = T)[[1]]
seq((sort(mdy(times$Date), na.rm = T)[[1]]), 
    (times$Date[[length(times$Date)]]), by = "7 days")
work_week_startdate <- seq(
  (as_date(sort(times$Date)[[1]])), 
  (times$Date[[length(times$Date)]]), 
    by = "7 days")
times$Date[[length(times$Date)]]
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

# How much time have I had leftover each day? 
df %>% 
  group_by(day = floor_date(Date, "day")) %>% 
  summarise(prj_time = sum(as.numeric(project_duration)), 
            # non_prj_time = as.numeric(work_duration_day - prj_time),
            # tot_time = (prj_time + non_prj_time), # = work_duration_day
            # project_time = work_duration_day - non_prj_time 
            ) %>% # prj_time and project_time should overlay exactly on plot 
  filter(prj_time >= 0) %>% 
  gather(key, value, -day) %>% 
  ggplot(aes(day, (value/3600))) + 
  geom_col(position = "stack", fill = "light blue", col = "black", alpha = 0.75) + 
  geom_hline(yintercept = df.stats$med, lty = "dashed") + 
  geom_hline(yintercept = 8, lty = "solid") + 
  labs(x = "Day", y = "Hours", title = "Title", subtitle = "subtitle") + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5), 
                     plot.subtitle = element_text(hjust = 0.5))

# How would this line chart look with 0 as the minimum time (ignore OT)? 


# Locate overtime locations (where prj_time > work_duration_day)
df %>% 
  group_by(day = floor_date(Date, "day")) %>% 
  summarise(prj_time = sum(project_duration), 
            non_prj_time = work_duration_day - prj_time) %>%
  filter(non_prj_time < 0) %>% View() 



# What classifications do I spend most of my time on? 
factors <- df %>% 
  dplyr::select(Primary_Venn_Class, Venn_Class_Heirarchy, 
                DataScience_Anaconda_Survey_TimeSpentOn_Area, 
                Job_Category)
factors <- data.frame(lapply(factors, as.factor))

# Compare this to updated Primary_Venn_Class using Venn_Class_Heirarchy 
factors %>% 
  transmute(Primary_Venn_Class = toupper(Primary_Venn_Class), 
            Venn_Class_Heirarchy = toupper(Venn_Class_Heirarchy), 
            DataScience_Anaconda_Survey_TimeSpentOn_Area = toupper(DataScience_Anaconda_Survey_TimeSpentOn_Area), 
            Job_Category = toupper(Job_Category)) %>% 
  na.omit() %>%
  ggplot(aes(Primary_Venn_Class)) + geom_bar(aes(fill = Primary_Venn_Class))

factors <- factors %>% 
  transmute(Primary_Venn_Class = toupper(Primary_Venn_Class), 
            Venn_Class_Heirarchy = toupper(Venn_Class_Heirarchy), 
            DataScience_Anaconda_Survey_TimeSpentOn_Area = toupper(DataScience_Anaconda_Survey_TimeSpentOn_Area), 
            Job_Category = toupper(Job_Category)) %>% na.omit() 

# How much of each Secondary_Venn_Class accounts for each Primary_Venn_Class?
factors %>% 
  group_by(Primary_Venn_Class) %>%
  summarise(CS = sum(str_count(Venn_Class_Heirarchy, "CS")), 
            BK = sum(str_count(Venn_Class_Heirarchy, "BK")),
            MT = sum(str_count(Venn_Class_Heirarchy, "MT")),
  ) %>% 
  gather(key, value, -Primary_Venn_Class) %>% 
  ggplot(aes(key, value, col = Primary_Venn_Class)) + 
  geom_point(alpha = 0.95) + 
  geom_col(aes(fill = Primary_Venn_Class)) + theme_minimal()

# Based on Common Data Science Groupings
# What areas do I spend the most and least time?
factors %>% 
  group_by(DataScience_Anaconda_Survey_TimeSpentOn_Area) %>% 
  summarise(Freq = table(DataScience_Anaconda_Survey_TimeSpentOn_Area)) %>% 
  ggplot(aes(DataScience_Anaconda_Survey_TimeSpentOn_Area, Freq, 
             fill = DataScience_Anaconda_Survey_TimeSpentOn_Area)) + 
  geom_col() + coord_flip()

# Which Primary_Venn_Class are the most diverse and how do their proportions compare?
factors %>% 
  group_by(Primary_Venn_Class) %>%
  summarise(CS = sum(str_count(Venn_Class_Heirarchy, "CS")), 
            BK = sum(str_count(Venn_Class_Heirarchy, "BK")),
            MT = sum(str_count(Venn_Class_Heirarchy, "MT")),
            CSMT = sum(str_count(Venn_Class_Heirarchy, "CSMT"),
                       str_count(Venn_Class_Heirarchy, "MTCS")), 
            BKMT = sum(str_count(Venn_Class_Heirarchy, "BKMT"),
                       str_count(Venn_Class_Heirarchy, "MTBK")),
            CSBK = sum(str_count(Venn_Class_Heirarchy, "BKCS"),
                       str_count(Venn_Class_Heirarchy, "CSBK")),
            DS = sum(str_count(str_length(Venn_Class_Heirarchy), "6"))
  ) %>% 
  gather(key, value, -Primary_Venn_Class) %>% 
  ggplot(aes(Primary_Venn_Class, value, fill = key)) + geom_col(col = "black")

# Summarize the matrix of factors and find ratios between the classes 
factors_matrixsummary <- factors %>% 
  group_by(Primary_Venn_Class) %>%
  summarise(CS = sum(str_count(Venn_Class_Heirarchy, "CS")), 
            BK = sum(str_count(Venn_Class_Heirarchy, "BK")),
            MT = sum(str_count(Venn_Class_Heirarchy, "MT")),
            CSMT = sum(str_count(Venn_Class_Heirarchy, "CSMT"),
                       str_count(Venn_Class_Heirarchy, "MTCS")), 
            BKMT = sum(str_count(Venn_Class_Heirarchy, "BKMT"),
                       str_count(Venn_Class_Heirarchy, "MTBK")),
            CSBK = sum(str_count(Venn_Class_Heirarchy, "BKCS"),
                       str_count(Venn_Class_Heirarchy, "CSBK")),
            DS = sum(str_count(str_length(Venn_Class_Heirarchy), "6"))
  ) %>% rowwise() %>% 
  mutate(total = sum(CS, BK, MT, CSMT, BKMT, CSBK, DS)) 


# Compare total here to count of geom_bar() in raw primary_venn_class
factors_matrixsummary %>%
  ggplot(aes(Primary_Venn_Class, total, fill = Primary_Venn_Class)) + geom_col()


# Determine the ratios between the factors that can be used for simulation 
factors_matrixsummary %>%
  gather(key, value, -Primary_Venn_Class) %>% 
  filter(key != "total", key != "BK") %>% 
  mutate(total = sum(value),
         Ratio = value/ total) %>% View()

# How much of each primary class in percent of total class values? 
factors_matrixsummary %>% 
  gather(key, value, -Primary_Venn_Class) %>% 
  filter(key == "total") %>% 
  mutate(total = sum(value), 
         percent = (value / total) * 100) 

# Example VennDiagram
library(ggVennDiagram)
set.seed(20220519)
genes <- paste("gene",1:1000,sep="")
x <- list(
  A = sample(genes,300), 
  B = sample(genes,525), 
  C = sample(genes,440),
  D = sample(genes,350)
)
ggVennDiagram(x[1:3], label_alpha = 0)



### ----- Testing Start ----- ###

# Unique (Primary) Classes
factors_matrixsummary[[3]][[1]] # BK | BK
factors_matrixsummary[[2]][[2]] # CS | CS
factors_matrixsummary[[4]][[3]] # MT | MT

# Area 1 (BK and MT)
factors_matrixsummary[[4]][[1]] # BK | MT = 11
factors_matrixsummary[[6]][[1]] # BK | BKMT = 9
factors_matrixsummary[[3]][[3]] # MT | BK = 14
factors_matrixsummary[[6]][[3]] # MT | BKMT = 6

# Area 2 (BK and CS)
factors_matrixsummary[[3]][[2]] # CS | BK = 48
factors_matrixsummary[[7]][[2]] # CS | CSBK = 31
factors_matrixsummary[[2]][[1]] # BK | CS = 14
factors_matrixsummary[[7]][[1]] # BK | CSBK = 12

# Area 3 (CS and MT)
factors_matrixsummary[[4]][[2]] # CS | MT = 42 
factors_matrixsummary[[5]][[2]] # CS | CSMT = 36
factors_matrixsummary[[2]][[3]] # MT | CS = 17
factors_matrixsummary[[5]][[3]] # MT | CSMT = 14

# Area 4 (DS)
factors_matrixsummary[[5]][[1]] # BK | CSMT = 4
factors_matrixsummary[[6]][[2]] # CS | BKMT = 23
factors_matrixsummary[[7]][[3]] # MT | CSBK = 11
factors_matrixsummary[[8]][[1]] # BK | DS = 4
factors_matrixsummary[[8]][[2]] # CS | DS = 23
factors_matrixsummary[[8]][[3]] # MT | DS = 11

### ----- Testing End ----- ###

# Need 3 classes with overlap (duplication) in each while preserving ratios
# BK Primary Class (Areas 1, 2, 4, and 5)
BK_Primary_Class <- c(rep(1, (factors_matrixsummary[[6]][[3]] + 
  factors_matrixsummary[[3]][[3]] + 
  factors_matrixsummary[[6]][[1]] + 
  factors_matrixsummary[[4]][[1]])), # Area 1
rep(2, (factors_matrixsummary[[3]][[2]]  + 
  factors_matrixsummary[[7]][[2]] + 
  factors_matrixsummary[[2]][[1]] + 
  factors_matrixsummary[[7]][[1]])), # Area 2 
rep(4, (factors_matrixsummary[[5]][[1]] + 
  factors_matrixsummary[[6]][[2]] +
  factors_matrixsummary[[7]][[3]] + 
  factors_matrixsummary[[8]][[1]] +
  factors_matrixsummary[[8]][[2]] +
  factors_matrixsummary[[8]][[3]])), # Area 4 
rep(5, factors_matrixsummary[[3]][[1]]) # BK only
)

# CS Primary Class (Areas 1, 3, 4, and 6) 

CS_Primary_Class <- c(rep(1, (factors_matrixsummary[[6]][[3]] + 
          factors_matrixsummary[[3]][[3]] + 
          factors_matrixsummary[[6]][[1]] + 
          factors_matrixsummary[[4]][[1]])), # Area 1
  rep(3, (factors_matrixsummary[[4]][[2]] +
          factors_matrixsummary[[5]][[2]] +
          factors_matrixsummary[[2]][[3]] +
          factors_matrixsummary[[5]][[3]])), # Area 3
  rep(4, (factors_matrixsummary[[5]][[1]] + 
          factors_matrixsummary[[6]][[2]] +
          factors_matrixsummary[[7]][[3]] + 
          factors_matrixsummary[[8]][[1]] +
          factors_matrixsummary[[8]][[2]] +
          factors_matrixsummary[[8]][[3]])), # Area 4
  rep(6, factors_matrixsummary[[2]][[2]]) # CS only
)
# MT Primary Class (Areas 2, 3, 4, and 7) 

MT_Primary_Class <- c(rep(2, (factors_matrixsummary[[3]][[2]]  + 
          factors_matrixsummary[[7]][[2]] + 
          factors_matrixsummary[[2]][[1]] + 
          factors_matrixsummary[[7]][[1]])), # Area 2 
  rep(3, (factors_matrixsummary[[4]][[2]] +
          factors_matrixsummary[[5]][[2]] +
          factors_matrixsummary[[2]][[3]] +
          factors_matrixsummary[[5]][[3]])), # Area 3
  rep(4, (factors_matrixsummary[[5]][[1]] + 
          factors_matrixsummary[[6]][[2]] +
          factors_matrixsummary[[7]][[3]] + 
          factors_matrixsummary[[8]][[1]] +
          factors_matrixsummary[[8]][[2]] +
          factors_matrixsummary[[8]][[3]])), # Area 4
  rep(7, factors_matrixsummary[[4]][[3]]) # MT only
)

z <- list(
  BK_Primary_Class, 
  CS_Primary_Class, 
  MT_Primary_Class
)

ggVennDiagram(z, label_alpha = 0)

y = list(
  E = c(1,2,3,8,9,10),
  D = c(1, 2, 4, 5, 6, 11, 12, 13),
  S = c(1, 2, 2, 3, 4, 5, 6, 7)
)

ggVennDiagram(y, label_alpha = 50)

z <- list(
  BK = c(1, 2, 4, 5),
  CS = c(1, 3, 4, 6),
  MT = c(2, 3, 4, 7)
)

ggVennDiagram(z, label_alpha = 0)

library(RVenn)
RVenn::Venn(z)
RVenn::Venn(w)


# 

Area1 <- rep(paste0("BKCS", 1:(factors_matrixsummary[[6]][[3]] + # Specific to BKCS 
                      factors_matrixsummary[[3]][[3]] +  
                      factors_matrixsummary[[6]][[1]] + 
                      factors_matrixsummary[[4]][[1]])), 
    (factors_matrixsummary[[6]][[3]] + 
     factors_matrixsummary[[3]][[3]] + 
     factors_matrixsummary[[6]][[1]] + 
     factors_matrixsummary[[4]][[1]])) # Area 1 
Area2 <- rep(paste0("BKMT", 1:(factors_matrixsummary[[3]][[2]]  + # Specific to BKMT
                        factors_matrixsummary[[7]][[2]] + 
                        factors_matrixsummary[[2]][[1]] + 
                        factors_matrixsummary[[7]][[1]])), 
    (factors_matrixsummary[[3]][[2]]  + 
     factors_matrixsummary[[7]][[2]] + 
     factors_matrixsummary[[2]][[1]] + 
     factors_matrixsummary[[7]][[1]])) # Area 2 
Area3 <- rep(paste0("CSMT", 1:(factors_matrixsummary[[4]][[2]] + # Specific to  CSMT
                        factors_matrixsummary[[5]][[2]] +
                        factors_matrixsummary[[2]][[3]] +
                        factors_matrixsummary[[5]][[3]])), 
    (factors_matrixsummary[[4]][[2]] +
     factors_matrixsummary[[5]][[2]] +
     factors_matrixsummary[[2]][[3]] +
     factors_matrixsummary[[5]][[3]])) # Area 3
Area4 <- rep(paste0("DS", 1:(factors_matrixsummary[[5]][[1]] + # Specific to DS
                      factors_matrixsummary[[6]][[2]] +
                      factors_matrixsummary[[7]][[3]] + 
                      factors_matrixsummary[[8]][[1]] +
                      factors_matrixsummary[[8]][[2]] +
                      factors_matrixsummary[[8]][[3]])), 
    (factors_matrixsummary[[5]][[1]] + 
     factors_matrixsummary[[6]][[2]] +
     factors_matrixsummary[[7]][[3]] + 
     factors_matrixsummary[[8]][[1]] +
     factors_matrixsummary[[8]][[2]] +
     factors_matrixsummary[[8]][[3]])) # Area 4 
Area5 <- rep(paste0("BK", 1:factors_matrixsummary[[3]][[1]]), factors_matrixsummary[[3]][[1]]) # Area 5 (BK only)
Area6 <- rep(paste0("CS", 1:factors_matrixsummary[[2]][[2]]), factors_matrixsummary[[2]][[2]]) # Area 6 (CS only)
Area7 <- rep(paste0("MT", 1:factors_matrixsummary[[4]][[3]]), factors_matrixsummary[[4]][[3]]) # Area 7 (MT only)
w <- list(
  BK = c(Area1, Area2, Area4, Area5),
  CS = c(Area1, Area3, Area4, Area6),
  MT = c(Area2, Area3, Area4, Area7)
)

library(ggVennDiagram)
library(ggplot2)

ggVennDiagram(w, 
              category.names = c("Domain", "Science", "Math"), 
              show_intersect = F, 
              set_color = "black", 
              set_size = 4, 
              label = "both", 
              label_alpha = 0.00, 
              label_geom = "label", 
              label_color = "black", 
              label_size = 4, 
              label_percent_digit = 2, 
              label_txtWidth = 30,
              edge_lty = "solid", 
              edge_size = 1) + 
  guides(fill = guide_legend(title = "Title", )) + 
  scale_color_manual(values = c("white", "white", "white")) + 
  scale_fill_gradient(low = "#c2fffc", high = "#00c4bb") + 
  labs(title = "Project Class Diagram", 
       subtitle = "What project areas do I spend time on?", 
       caption = Sys.Date()) +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = 0.5)) 
# scale_color_manual(values=wes_palette(n=3, name="GrandBudapest2")) + 
# scale_fill_distiller(palette = "Blues", direction = 1)  

?guides()


