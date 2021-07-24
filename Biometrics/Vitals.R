# Biometrics Test

library(dplyr)
library(ggplot2)
library(tidyr)

vitals <- read.csv("https://raw.githubusercontent.com/palmorezm/misc/master/Biometrics/Vitals1.csv")

vitals2 <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTcFzJXYjKvyhUI3my6VTaqRfWG0-pHldCRvf3nndHPIVMh-C1BqzGvB8P9p-GZZ63fbXdS4i0O8a5C/pub?output=csv")

vitals <- vitals2

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

df[is.na(df)] <- 0


df.excludeszero <- df %>% 
  data.frame() %>% 
  gather(key, value) %>% 
  filter(key == "AVGBPS") %>%
  mutate(Value = as.numeric(value),
         BPS.avg = (round(Value, 1))) %>% 
  filter(BPS.avg >1) 
  
df.excludeszero %>% 
  filter(Value > 100) %>% 
  ggplot(aes(Value, fill = key)) + 
  geom_histogram(stat = "count", binwidth = 5) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


