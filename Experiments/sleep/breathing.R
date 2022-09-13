
# Can I predict when I wake up based only on historical data?
library(lubridate)
library(dplyr)
link <- paste0("https://docs.google.com/spreadsheets/d/e/",
               "2PACX-1vQp7qw5PXoLR10Cvtn2wKm6TIThr6rYwx1Y",
               "muhMKPG5eKn9d1ws7pi39Pz1gTaAF3BDv0XcxPDi237",
               "_/pub?output=tsv")
df <- read.delim(link)
df$Date <- mdy(df$Date)
df$Wake.Time <- parse_time(df$Wake.Time)
df <- df %>% 
  dplyr::select(Date, Wake.Time) %>% 
  na.omit()

df %>% 
  ggplot(aes(x = Date, y = Wake.Time)) +
  geom_point() 

library(forecast)  # for `auto.arima`
fit_model <- function(x, ...) {
  # suggested by Matt Dancho:
  x %>%
    analysis() %>%
    # Since the first day changes over resamples, adjust it
    # based on the first date value in the data frame 
    tk_ts(start = .$date[[1]] %>% as.yearmon(), 
          frequency = 12, 
          silent = TRUE) %>%
    auto.arima(...)
}

