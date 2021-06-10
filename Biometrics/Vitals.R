# Biometrics Test


vitals <- read.csv("https://raw.githubusercontent.com/palmorezm/misc/master/Biometrics/Vitals1.csv")
View(vitals)

library(dplyr)
head(vitals)

df <- vitals %>% 
  mutate(AVGBPS = (BPS1 + BPS2 + BPS3)/ 3,
         AVGBPD = (BPD1 + BPD2 + BPD3)/ 3,
         AVGHR = (HR1 + HR2 + HR3) / 3)  %>% 
  dplyr::select(Date,
                Weight, 
                AVGBPS, 
                AVGBPD, 
                AVGHR)
plot(df)
summary(df)

df.mod <- lm(Weight~AVGBPS + AVGBPD + AVGHR, df)
summary(df.mod)
