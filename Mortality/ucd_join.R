
# UCD Cleaning Helper Script 
# Source
load("Data/reg_mort.rdata")

tmp_all <- data.frame(Date = rep(unique(UCD$Date)[[1]], length(unique(UCD$County.Code))))
tmp <- data.frame(Date = rep(unique(UCD$Date)[[2]], length(unique(UCD$County.Code))))
for (i in 2:length(unique(UCD$County.Code))){
  # Create list with appended dates and form tmp df
  tmp <- data.frame(Date = rep(unique(UCD$Date)[[i]], length(unique(UCD$County.Code))))
  tmp_all <- rbind(tmp_all, tmp)
}
tmp2_all <- data.frame(County.Code = rep(unique(UCD$County.Code), length(unique(UCD$Date))), 
                       County = rep(unique(UCD$County), length(unique(UCD$Date))))
df <- cbind(tmp_all, tmp2_all)
tmp3_all <- UCD %>% left_join(df, by = "Date") 

tmp3_all %>% 
  group_by(Date, Year, County.x, ICD.Chapter) %>% 
  summarise(sum(Deaths)) %>% View()

tmp4_all <- merge(UCD, df)
tmp_all <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter)[[1]], length(unique(UCD$County.Code))))
tmp <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter)[[2]], length(unique(UCD$County.Code))))
for (i in 2:length(unique(UCD$County.Code))){
  # Create list with appended dates and form tmp df
  tmp <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter)[[i]], length(unique(UCD$County.Code))))
  tmp_all <- rbind(tmp_all, tmp)
}
unique(tmp_all$ICD.Chapter) == unique(UCD$ICD.Chapter) # All True 
tmp4_all <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter), length(df$County.Code)))
tmp3_all <- merge(tmp4_all, df)


tmp5_all <- tmp4_all %>% 
  left_join(UCD, by = "ICD.Chapter")

tmp5_all <- merge(df, tmp4_all)

rep(unique(UCD$ICD.Chapter)[[1]], length(unique(UCD$County.Code)))

tmp4_all$ICD.Chapter
rep(unique(UCD$Date, ))

tmp_all <- data.frame(Date = rep(unique(UCD$Date)[[1]], length(unique(UCD$County.Code))))
tmp <- data.frame(Date = rep(unique(UCD$Date)[[2]], length(unique(UCD$County.Code))))
for (i in 2:length(unique(UCD$County.Code))){
  # Create list with appended dates and form tmp df
  tmp <- data.frame(Date = rep(unique(UCD$Date)[[i]], length(unique(UCD$County.Code))))
  tmp_all <- rbind(tmp_all, tmp)
}

obs <- tmp4_all
num_counties <- length(unique(UCD$County))
num_icd <- length(unique(UCD$ICD.Chapter))
num_dates <- length(unique(UCD$Date))

# Binding Date to ICD.Chapter with 28908 obs 
tmp6_all <- data.frame(Date = rep(unique(UCD$Date)[[1]], (num_counties * num_icd)))
tmp <- data.frame(Date = rep(unique(UCD$Date)[[2]], (num_counties * num_icd)))

tmp <- rbind(tmp6_all, tmp)

for (i in 2:length(unique(UCD$Date))){
  tmp <- data.frame(Date = rep(unique(UCD$Date)[[i]], 1314))
  tmp6_all <- rbind(tmp6_all, tmp)
}
obs <- cbind(obs, tmp6_all)

# Binding County to Date and ICD.Chapter with 28908 obs 
tmp7_all <- data.frame(County = rep(unique(UCD$County)[[1]], (num_icd)))
tmp <- data.frame(County = rep(unique(UCD$County)[[2]], num_icd))
for (i in 2:length(unique(UCD$County))){
  tmp <- data.frame(County = rep(unique(UCD$County)[[i]], (num_icd)))
  tmp7_all <- rbind(tmp7_all, tmp)
}
tmp8_all <- data.frame(County = rep(tmp7_all$County, length(unique(UCD$Date))))
obs <- cbind(obs, tmp7_all)

?left_join()

?merge()

tmp9_all <- merge(obs, UCD, all = TRUE)
sum(is.na(tmp9_all$Deaths))
tmp9_all$Deaths[is.na(tmp9_all$Deaths)] <- 0

UCDc <- tmp9_all
UCDc$ICD.Chapter[which(is.na(UCDc$ICD.Chapter))]
UCDc[UCDc$ICD.Chapter[UCDc$ICD.Chapter == ""],]
UCDc <- UCDc %>% 
  filter(ICD.Chapter != "")
UCDc <- UCDc %>% 
  filter(County != "") 

# save(UCDc, file = "Data/UCDcleaned.rdata")
library(plotly)
ggplotly(
  tmp9_all %>% 
  filter(County == "Rock County, WI") %>% 
  ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
  geom_line() + 
  facet_wrap(~ICD.Chapter, scales = "free") + 
  theme(legend.position = "none")
  )




# for (i in 1:length(unique(UCD$Date))){
#   for (j in 1:length(unique(UCD$County.Code))){
#     print(unique(UCD$County.Code[[j]]))
#     print(unique(UCD$Date[[i]]))
#   }
# }