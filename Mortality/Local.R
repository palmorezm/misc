
# Local Data After Classification

# Packages
pkgs <- c("dplyr", "data.table")
npkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(npkgs)) install.packages(npkgs)
library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
# Loading
path <- "C:/Users/Zachary.Palmore/GitHub/rock/Mortality/Data/Classified_DeathData2021.xlsx"
df <- readxl::read_xlsx(path)

# Cleaning
# Change variable names
colnames(df) <- c("DecedentName", "Gender", "DOB", "DOD", "AOD", "POD", 
                  "FacilityName", "DecedentAddress", "DecedentCity", 
                  "UsualOccupation", "Ethnicity", "Race", "Education", 
                  "Autopsy", "PregStatus", "TobaccoUse", 
                  "AlcoholUse", "MOD", "ImmediateCOD", "Consequence1", 
                  "Consequence2", "Consequence3", "OtherSignificant", 
                  "Multiple", "SingleCOD", "Consider", 
                  "Include", "Notes", "Links", "Empty")
# Convert to proper data type
df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
tmp <- df %>% 
  select(DecedentName, DecedentCity, DecedentAddress, 
         Consequence1, Consequence2, Consequence3, 
         FacilityName, ImmediateCOD, OtherSignificant, 
         Notes, Links)
tmp <- as.data.frame(lapply(df %>% 
         select(DecedentName, DecedentCity, DecedentAddress, 
                Consequence1, Consequence2, Consequence3, 
                FacilityName, ImmediateCOD, OtherSignificant, 
                Notes, Links), 
       as.character))
df <- cbind(df %>% 
              select(-DecedentName, -DecedentCity, -DecedentAddress, 
                     -Consequence1, -Consequence2, -Consequence3, 
                     -FacilityName, -ImmediateCOD, -OtherSignificant, 
                     -Notes, -Links), tmp)
df$DOB <- as.Date.character(df$DOB, format = "%m/%d/%Y")
df$DOD <- as.Date.character(df$DOD, format = "%m/%d/%Y")
df$AOD <- stringr::str_extract_all(as.character(df$AOD), "\\d(.*?) ")
df$AOD <- as.numeric(str_remove(df$AOD, " "))
# See Census for population estimates
# https://www.census.gov/quickfacts/fact/table/rockcountywisconsin/AGE135220
pop2021 <- as.numeric(164381)
Race2021 <- data.frame(matrix(c("WHITE", 90.3,
                       "BLACK", 5.3, 
                       "AMERICAN INDIAN", 0.6, 
                       "ASIAN", 1.3, 
                       "NATIVE HI / PACIFIC ISLANDER", 0.1, 
                       "MIXED", 2.5), ncol = 2, byrow = T)) %>% 
  rename(Race = X1, 
         Percent = X2) %>% 
  mutate(Pop = pop2021, 
         RacePop = as.numeric(Percent)/100 * Pop)
Eth2021 <- data.frame(matrix(c("NOT HISPANIC", 82.3, 
                               "HISPANIC", 9.1), 
                  ncol = 2, byrow = T)) %>% 
  rename(Ethnicity = X1, 
         Percent = X2) %>% 
  mutate(Pop = pop2021, 
         EthPop = as.numeric(Percent)/100 * Pop)
Gender2021 <- data.frame(matrix(c("MALE", 49.3,
                                  "FEMALE", 50.7), ncol = 2, byrow = T)) %>%   
  rename(Gender = X1, 
         Percent = X2) %>% 
  mutate(Pop = pop2021, 
         GenderPop = as.numeric(Percent)/100 * Pop)
df$DOB <- as.numeric(2021 - df$AOD) # Since everyone has a AOD we use it to calculate DOB 
df$Race[which(is.na(df$Race))] <- "UNKNOWN"
tmp <- df %>% 
  group_by(Race) %>% 
  summarise(Mean_AOD = mean(AOD), 
            Med_AOD = median(AOD), 
            N = n())
setDT(tmp)
setDT(Race2021)
Race2021 <- tmp[Race2021, on = "Race"] %>% 
  mutate(CrudeRate = N / RacePop)

df$Ethnicity[which(is.na(df$Ethnicity))] <- "UNKNOWN"
tmp <- df %>% 
  group_by(Ethnicity) %>% 
  summarise(Mean_AOD = mean(AOD), 
            Med_AOD = median(AOD), 
            N = n())
setDT(tmp)
setDT(Eth2021)
Eth2021 <- tmp[Eth2021, on = "Ethnicity"] %>% 
  mutate(CrudeRate = N / EthPop)
df$Gender[which(is.na(df$Gender))] <- "UNKNOWN"
tmp <- df %>% 
  group_by(Gender) %>% 
  summarise(Mean_AOD = mean(AOD), 
            Med_AOD = median(AOD), 
            N = n())
setDT(tmp)
setDT(Gender2021)
Gender2021 <- tmp[Gender2021, on = "Gender"] %>% 
  mutate(CrudeRate = N / GenderPop)

# save(Eth2021, Race2021, Gender2021, file = "C:/Users/Zachary.Palmore/GitHub/rock/Mortality/Data/EthRaceGen2021.rdata")

# Race
df %>% 
  ggplot(aes(AOD)) + 
  geom_histogram(binwidth = 1, fill = "light grey", col = "grey", alpha = 0.5) + 
  geom_vline(xintercept = mean(df$AOD), lty = "dashed") +
  geom_vline(xintercept = Race2021$Med_AOD[1], col = "mediumblue") + 
  geom_vline(xintercept = Race2021$Med_AOD[2], col = "seagreen") + 
  geom_vline(xintercept = Race2021$Med_AOD[3], col = "red") + 
  geom_vline(xintercept = Race2021$Med_AOD[4], col = "orange") +
  labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
       x = "Age", 
       y = "Number of Individuals") + 
  annotate('text', x = 15, y = 40, 
           label = paste0("~mu==", Race2021$Med_AOD[2], "~(Black)"), parse = TRUE, size=5, 
           col = "orange") +
  annotate('text', x = 15, y = 50, 
           label = paste0("~mu==", Race2021$Med_AOD[1], "~(White)"), parse = TRUE, size=5, 
           col = "mediumblue") +
  annotate('text', x = 15, y = 30, 
           label = paste0("~mu==", Race2021$Med_AOD[4], "~(Asian)"), parse = TRUE, size=5, 
           col = "seagreen") +
  annotate('text', x = 21, y = 20, 
           label = paste0("~mu==", Race2021$Med_AOD[3], "~(AmericanIndian)"), parse = TRUE, size=5, 
           col = "red") +
  theme_minimal() + 
  theme(plot.subtitle = element_text(hjust = 0.5), 
        legend.text = element_text())

# Ethnicity
df %>% 
  ggplot(aes(AOD)) + 
  geom_histogram(binwidth = 1, fill = "light grey", col = "grey", alpha = 0.5) + 
  geom_vline(xintercept = mean(df$AOD), lty = "dashed") +
  geom_vline(xintercept = Eth2021$Med_AOD[1], col = "mediumblue") + 
  geom_vline(xintercept = Eth2021$Med_AOD[2], col = "orange") +
  labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
       x = "Age", 
       y = "Number of Individuals") + 
  annotate('text', x = 15, y = 40, 
           label = paste0("~mu==", Eth2021$Med_AOD[2], "~(Hispanic)"), parse = TRUE, size=5, 
           col = "orange") +
  annotate('text', x = 15, y = 50, 
           label = paste0("~mu==", Eth2021$Med_AOD[1], "~(Not~Hispanic)"), parse = TRUE, size=5, 
           col = "mediumblue") +
  theme_minimal() + 
  theme(plot.subtitle = element_text(hjust = 0.5), 
        legend.text = element_text())

# Gender
df %>% 
  ggplot(aes(AOD)) + 
  geom_histogram(binwidth = 1, fill = "light grey", col = "grey", alpha = 0.5) + 
  geom_vline(xintercept = median(df$AOD), lty = "dashed") +
  geom_vline(xintercept = Gender2021$Med_AOD[1], col = "orange") + 
  geom_vline(xintercept = Gender2021$Med_AOD[2], col = "mediumblue") +
  labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
       x = "Age", 
       y = "Number of Individuals") + 
  annotate('text', x = 15, y = 40, 
           label = paste0("~mu==", Gender2021$Med_AOD[2], "~(Male)"), parse = TRUE, size=5, 
           col = "mediumblue") +
  annotate('text', x = 15, y = 50, 
           label = paste0("~mu==", Gender2021$Med_AOD[1], "~(Female)"), parse = TRUE, size=5, 
           col = "orange") +
  theme_minimal() + 
  theme(plot.subtitle = element_text(hjust = 0.5), 
        legend.text = element_text())


