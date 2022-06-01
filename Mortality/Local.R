
# Local Data After Classification

# Packages
pkgs <- c("dplyr", "data.table")
npkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(npkgs)) install.packages(npkgs)

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

df %>% 
  ggplot(aes(AOD)) + 
  geom_histogram(binwidth = 1, fill = "light blue", col = "grey", alpha = 0.5) + 
  geom_vline(xintercept = mean(df$AOD), lty = "dashed") +
  geom_vline(xintercept = Race2021$Med_AOD[1]) + 
  geom_vline(xintercept = Race2021$Med_AOD[2]) + 
  geom_vline(xintercept = Race2021$Med_AOD[3]) + 
  geom_vline(xintercept = Race2021$Med_AOD[4]) +
  labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
       x = "Age", 
       y = "Number of Individuals") + 
  theme(plot.subtitle = element_text(hjust = 0.5))



