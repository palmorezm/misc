# Biometrics Test

library(dplyr)
library(kableExtra)

vitals <- read.csv("https://raw.githubusercontent.com/palmorezm/misc/master/Biometrics/Vitals1.csv")

vitals2 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTcFzJXYjKvyhUI3my6VTaqRfWG0-pHldCRvf3nndHPIVMh-C1BqzGvB8P9p-GZZ63fbXdS4i0O8a5C/pub?output=csv")


df <- vitals %>% 
  mutate(AVGBPS = (BPS1 + BPS2 + BPS3)/ 3,
         AVGBPD = (BPD1 + BPD2 + BPD3)/ 3,
         AVGHR = (HR1 + HR2 + HR3) / 3)  %>% 
  dplyr::select(Date,
                Weight, 
                AVGBPS, 
                AVGBPD, 
                AVGHR)
df.plt <- plot(df)
df.summary <- summary(df)

df.mod <- lm(Weight~AVGBPS + AVGBPD + AVGHR, df)
summary(df.mod)


df2 <- vitals2 %>% 
  mutate(AVGBPS = (BPS1 + BPS2 + BPS3)/ 3,
         AVGBPD = (BPD1 + BPD2 + BPD3)/ 3,
         AVGHR = (HR1 + HR2 + HR3) / 3)  %>% 
  dplyr::select(Date,
                Weight, 
                AVGBPS, 
                AVGBPD, 
                AVGHR)

df2.plt <- plot(df2)
df.summary <- summary(df2)

df2.mod <- lm(Weight~AVGBPS + AVGBPD + AVGHR, df2)
summary(df2.mod)

plot(df)
plot(df2)
