
# DATA 608
# Shiny Final

# Shiny App Data Requirements

# This could be more concise

# packages
library(tidyverse)
library(ggpubr)
library(kableExtra)
library(rjson)
library(tigris)
library(plotly)
library(shiny)
library(geojsonsf)
library(sf)
library(DT)
theme_set(theme_minimal())

### MSA HAI Estimates ###

# Read in Data 
# Revision! Compiled Data Location Changed
# df <- read.csv("https://raw.githubusercontent.com/palmorezm/msds/main/Research%20Project/Data/compiled.csv")
df <- read.csv("https://raw.githubusercontent.com/palmorezm/msds/main/Research%20Projects/Data/compiled.csv")
# Change Data Types to Numeric
df[4:length(df)] <- sapply(df[4:length(df)], as.numeric)
df <- df %>% 
  dplyr::select(-X)
# Calculate HAI Estimates
df.fin <- df %>% 
  mutate(IR = 0.035, 
         PMT = MEDVAL * 0.8 * (IR / 12)/(1 - (1/(1 + IR/12)^360)), 
         QINC = PMT * 4 * 12,
         HAI = (MEDINC / QINC) * 100) %>% 
  mutate(ADJALL = (MEDINC - ((RPPALL / 100)* MEDINC)),
         AINCALL = MEDINC + ADJALL, 
         # Real Wage HAI:
         HAIRW = (AINCALL / QINC)*100) %>% 
  mutate(ADJRNT = (MEDINC - ((RPPRENT / 100)* MEDINC)), 
         AINCRNT = MEDINC + ADJRNT, 
         # Rent Adjusted HAI:
         HAIRNT = (AINCRNT / QINC)*100) %>% 
  mutate(ADJIPD = (AINCRNT + AINCALL / 2)*(IPD/100),
         # IPD Projected HAI:
         HAIIPD = (ADJIPD / QINC)*100) %>% 
  mutate(PMTRAW = MEDVAL * 0.99 * (IR / 12)/(1 - (1/(1 + IR/12)^360)), 
         QINCRAW = PMTRAW * 4 * 12,
         # Raw HAI at 20% Down Payment:
         HAIRAW = (MEDINC / QINCRAW) * 100) %>% 
  mutate(HHSIZE = 2.53, 
         DEBTMV = (20000/HHSIZE),
         DEBTED = 9664*HHSIZE, 
         DEBTIL = 9609*HHSIZE,
         DEBTCC = 3500*HHSIZE, 
         DEBTOC = 10000*HHSIZE, 
         DEBTS = DEBTMV + DEBTED + DEBTIL + DEBTCC + DEBTOC, 
         AQINC30 =  PMT * (100/30) * 12,
         AINCDBT = ((AINCRNT + AINCALL / 2) - DEBTS), 
         # HAI Adjusted for Average American Household Debts
         HAIDBT = AINCDBT / AQINC30) %>% 
  mutate(PMT3DP = MEDVAL * 0.97 * (IR / 12)/(1 - (1/(1 + IR/12)^360)),
         AQINC60 = PMT * (100/60)*12,
         # Lenient Lending Practices HAI 
         #(60% of monthly income on mortgage is acceptable with 3% DP)
         HAILEN = (AINCALL / AQINC60)*100) 

### County Income & Population Data ###
counties <- rjson::fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
df <- read.csv("https://raw.githubusercontent.com/palmorezm/msds/main/Knowledge%20and%20Visual%20Analytics/Final/Data/CAINC1_AllCounties_1969_2019.csv")

df_decades <- df %>%
  gather(year, value, -GeoFIPS, -GeoName, -Region, 
         -TableName, -LineCode, -IndustryClassification,
         -Description, -Unit)  %>% # Keep descriptors for each County
  slice(-c(1:3)) %>% # Remove US Totals/Averages
  # filter(LineCode == 3) %>% # Subset to per capita personal income by county
  mutate(Value = as.numeric(value)) %>% # Convert all values to numeric type
  dplyr::select(-value) %>% # Remove non-numeric value column
  mutate_all(~replace(., is.na(.), 0)) %>% # Replace missing with 0
  filter(Value > 0) %>% # Remove missing
  mutate(Year = as.numeric(str_remove(year, "^X"))) %>% 
  dplyr::select(-year) %>% 
  mutate(Statistic = case_when(
    endsWith(Description, "(thousands of dollars)") ~ "Personal Income",
    endsWith(Description, "2/") ~ "Income Per Capita", 
    endsWith(Description, "1/") ~ "Population"
  ), 
  Years = as.Date(paste(Year, 12, 31, sep = "-"))) %>% 
  filter(Year %in% c(1969, 1979, 1989, 1999, 2009, 2019)) %>% 
  dplyr::select(-Description, -TableName, -IndustryClassification)

df_decades <- df_decades %>% 
  slice( -(str_which(df$GeoFIPS, pattern = "\\d{2}(000)")) ) %>% # extract counties by GeoFIPS string code
  mutate(GeoFIPs = str_remove_all(GeoFIPS, '\\\"'), 
         GeoFips = str_remove(GeoFIPs, "\\s")) %>% 
  dplyr::select(-GeoFIPS, -GeoFIPs)


df <- df %>%
  gather(year, value, -GeoFIPS, -GeoName, -Region, 
         -TableName, -LineCode, -IndustryClassification,
         -Description, -Unit)  %>% # Keep descriptors for each County
  slice(-c(1:3)) %>% # Remove US Totals/Averages
  # filter(LineCode == 3) %>% # Subset to per capita personal income by county
  mutate(Value = as.numeric(value)) %>% # Convert all values to numeric type
  dplyr::select(-value) %>% # Remove non-numeric value column
  mutate_all(~replace(., is.na(.), 0)) %>% # Replace missing with 0
  filter(Value > 0) %>% # Remove missing
  mutate(Year = as.numeric(str_remove(year, "^X"))) %>% 
  dplyr::select(-year) %>% 
  mutate(Statistic = case_when(
    endsWith(Description, "(thousands of dollars)") ~ "Personal Income",
    endsWith(Description, "2/") ~ "Income Per Capita", 
    endsWith(Description, "1/") ~ "Population"
  ), 
  Years = as.Date(paste(Year, 12, 31, sep = "-"))) %>% 
  # filter(Year %in% c(1969, 1979, 1989, 1999, 2009, 2019)) %>% 
  dplyr::select(-Description, -TableName, -IndustryClassification) 

df <- df %>% 
  slice( -(str_which(df$GeoFIPS, pattern = "\\d{2}(000)")) ) %>% # extract counties by GeoFIPS string code
  mutate(GeoFIPs = str_remove_all(GeoFIPS, '\\\"'), 
         GeoFips = str_remove(GeoFIPs, "\\s"), # Remove quotations and whitespace from county GeoFIPS codes
  ) %>% 
  dplyr::select(-GeoFIPS, -GeoFIPs) 


df_0919 <- df %>%
  filter(Year > 2008) %>% 
  filter(LineCode == 3)

df.tbl <- df %>% 
  group_by(Statistic, Year, Years) %>% 
  summarise(min = min(Value), 
            med = median(Value), 
            max = max(Value))


# Extract HAIs < 100
ltdf <- df.fin %>% 
  gather(key, value, -GeoFips, -GeoName, 
         -year, -MEDINC, 
         -MEDVAL, -MOE, 
         -UMEDVAL, -LMEDVAL, 
         -POP, -PERINC, -AINCALL) %>%
  filter(key == c("HAI", "HAIRW", 
                  "HAIRNT", 
                  "HAIIPD", 
                  "HAIRAW", 
                  "HAILEN", 
                  "HAIDBT")) %>% 
  subset( value < 100) 
# Extract HAIs >= 100
gtdf <- df.fin %>% 
  gather(key, value, -GeoFips, -GeoName, 
         -year, -MEDINC, 
         -MEDVAL, -MOE, 
         -UMEDVAL, -LMEDVAL, 
         -POP, -PERINC, -AINCALL) %>%
  filter(key == c("HAI", "HAIRW", 
                  "HAIRNT", 
                  "HAIIPD", 
                  "HAIRAW", 
                  "HAILEN", 
                  "HAIDBT")) %>% 
  subset( value >= 100) 
# Summarize by group the "less affordable" population and other statistics 
lttbl <- ltdf %>%
  group_by(key, year) %>% 
  na.exclude() %>%
  summarize(Set = 'LT100', 
            N = n(), 
            MMEDHAI = median(value), 
            AAVGHAI = mean(value),
            SMEDINC = sum(MEDINC),
            SMEDVAL = sum(MEDVAL),
            SUMEDVAL = sum(UMEDVAL), 
            SLMEDVAL = sum(LMEDVAL), 
            SPERINC = sum(PERINC), 
            SREALWAGE = sum(AINCALL), 
            TPOP = sum(POP), 
            TPOPN = (TPOP/N), 
            PMEDINC = (SMEDINC/N),
            PMEDHAI = (MMEDHAI/N),
            PAVGHAI = (AAVGHAI/N) )
# Summarize by group the "more affordable" population and other statistics 
gttbl <- gtdf %>%
  group_by(key, year) %>% 
  na.exclude() %>%
  summarize(Set = 'GT100', 
            N = n(), 
            MMEDHAI = median(value), 
            AAVGHAI = mean(value),
            SMEDINC = sum(MEDINC),
            SMEDVAL = sum(MEDVAL),
            SUMEDVAL = sum(UMEDVAL), 
            SLMEDVAL = sum(LMEDVAL), 
            SPERINC = sum(PERINC), 
            SREALWAGE = sum(AINCALL), 
            TPOP = sum(POP), 
            TPOPN = (TPOP/N), 
            PMEDINC = (SMEDINC/N),
            PMEDHAI = (MMEDHAI/N),
            PAVGHAI = (AAVGHAI/N) )
# Real Income and HAI's
# create a title list for HAI
HAINAMES <- list("Normal" = "HAI", "Debt Adjusted"="HAIDBT", "Real Wage"="HAIRW", "Rent Adjustment"="HAIRNT", 
                 "Inflation Adjusted"="HAIIPD", "Raw Values"="HAIRAW", "Lenient Lending"="HAILEN")
hainames <- c("HAI" = "Normal", 
              "HAIDBT" = "Debt Adjusted", 
              "HAIRW"="Real Wage", 
              "HAIRNT"="Rent Adjusted", 
              "HAIIPD"="Inflation Adjusted", 
              "HAIRAW"="Raw Values", 
              "HAILEN"="Lenient Lending")
HAItitle <- function(variable,value){
  return(HAINAMES[value])
}

# Check Row Observations Match Data in Glb Env.
# sum(gttbl$N) 
# sum(lttbl$N)
d <- rbind(lttbl, gttbl) 
# Create shifting function to move index values of column in d upwards by n 
shift <- function(d, n){
  c(d[-(seq(n))], rep(NA, n))
}
# Summarize with filtering
data <- d %>% 
  dplyr::select(key, year, Set, N, TPOP, TPOPN, SMEDVAL, SUMEDVAL, PMEDINC, PMEDHAI, PAVGHAI) %>% 
  filter(Set == 'LT100' & year == 2010 | year == 2019) %>% 
  filter(Set != 'GT100') %>% 
  mutate(SKEW = PAVGHAI - PMEDHAI, 
         MOE = ((SUMEDVAL - SMEDVAL)/N), 
         C2019 = ifelse(year == 2019, TPOP, NA), 
         C2010 = ifelse(year == 2010, TPOP, NA))%>% 
  dplyr::select(-PAVGHAI, -Set, -TPOPN, -SMEDVAL, -SUMEDVAL)

data$C2010 <- shift(data$C2010, 1)
data <- data %>% 
  mutate(CHANGE = C2019-C2010) %>% 
  dplyr::select(-C2010, -C2019) %>%
  mutate(CHANGE = replace_na(CHANGE, 0))
data <- as.data.frame(data) 
percent_pop_MSA <- 0.83
censuspop2019 <- 328300000

# HAI Scatterplot Comparisson Plots
HAINAMES <- list("Normal" = "HAI", "Real Wage"="HAIRW", "Rent Adjustment"="HAIRNT", 
                 "Inflation Adjusted"="HAIIPD", "Raw Values"="HAIRAW", "Lenient Lending"="HAILEN")
hainames <- c("HAI" = "Normal", 
              "HAIRW"="Real Wage", 
              "HAIRNT"="Rent Adjusted", 
              "HAIIPD"="Inflation Adjusted", 
              "HAIRAW"="Raw Values", 
              "HAILEN"="Lenient Lending")
HAItitle <- function(variable,value){
  return(HAINAMES[value])
}
# Main 6 HAIs labeled Scatterplot Trends
main6 <- df.fin %>%
  gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
         -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) %>%
  filter(key == c("HAI", "HAIRW", "HAIRNT", 
                  "HAIIPD", "HAIRAW", "HAILEN")) %>% 
  filter(AINCALL <100000) %>% 
  ggplot(aes(value, (AINCALL/1000))) + 
  geom_point(aes(x = value, alpha = .01, col = key)) + 
  geom_vline(xintercept = 100, lty = "dotdash", col = "black") + 
  geom_smooth(aes(x = value), 
              method="lm", col = "grey30", fill = "light grey",
              se = T, na.rm = T) + 
  labs(x = "Home Affordability Value", y = "Income ($1000)", 
       subtitle = "Method Comparison to National Benchmark") +
  facet_wrap(key ~ ., scales = "free_x", shrink = F, labeller = labeller(key = hainames)) + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(hjust = 0.5), 
        panel.grid = element_blank())
# Outstanding Debts HAI ScatterPlot Trends
dbts1 <- df.fin %>%
  gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
         -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) %>%
  filter(key == c("HAIDBT")) %>% 
  filter(AINCALL <100000) %>% 
  ggplot(aes(value, (AINCALL/1000))) + 
  geom_point(aes(x = value, alpha = .01, col = key)) + 
  geom_vline(xintercept = 100, lty = "dotdash", col = "black") + 
  geom_smooth(aes(x = value), 
              method="loess", col = "grey30", fill = "light grey",
              se = T, na.rm = T) + 
  labs(x = "Home Affordability Value", y = "Income ($1000)", 
       subtitle = "Debt Adjusted") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = .5)) 
dbts2 <- df.fin %>%
  gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
         -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) %>%
  filter(key == c("HAIDBT")) %>% 
  filter(AINCALL <100000) %>% 
  ggplot(aes(value, (AINCALL/1000))) + 
  geom_point(aes(x = value, alpha = .01, col = key)) +
  geom_smooth(aes(x = value), 
              method="loess", col = "grey30", fill = "light grey",
              se = T, na.rm = T) + 
  labs(x = "Home Affordability Value", y = element_blank(), 
       subtitle = "Debt Adjusted Centered") +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = .5), 
        panel.grid.minor.y = element_blank())

# Megaplot
MEGANAMES <- list("Affordable" = "GT100", "Unaffordable"="LT100")
meganames <- c("GT100" = "Affordable", 
               "LT100"="Unaffordable")
megatitles <- function(variable,value){
  return(MEGANAMES[value])
}
# Megaplots 
PMEDINC.median <- median(d$PMEDINC)
megaplot1 <- d %>% 
  sample_n(715, replace = T) %>% 
  filter(key != "HAIDBT") %>%
  group_by(key, Set) %>%
  arrange(desc(Set)) %>% 
  ggplot(aes(x = MMEDHAI, col = key)) + geom_point(aes(y = PMEDINC, col = key, size = 4)) + 
  geom_hline(yintercept = PMEDINC.median, lty = "dotted") + 
  geom_point(aes(y = PMEDINC)) +
  facet_wrap(~Set, scales = "free_x", labeller = labeller(Set = meganames)) +
  geom_smooth(aes(y = PMEDINC), 
              method="loess",
              col = "grey30", 
              fill = "light grey", 
              se = T, na.rm = T, 
              lty = "solid") + 
  geom_smooth(aes(y = PMEDINC), 
              method="lm", alpha = 0.25,
              col = "grey70", 
              fill = "light blue", 
              se = F, na.rm = T, 
              lty = "dashed") + 
  labs(x = "Median HAI Value", y = "Median Income") + 
  theme(legend.position = "none", plot.title = element_blank())
megaplot2 <- d %>% 
  sample_n(715, replace = T) %>% 
  filter(key != "HAIDBT") %>%
  group_by(key, Set) %>% 
  ggplot(aes(x = MMEDHAI, col = key)) + geom_point(aes(y = PMEDINC, col = key, size = 4)) + 
  geom_hline(yintercept = PMEDINC.median, lty = "dotted")  +
  facet_wrap(~key, scales = "free_x", labeller = labeller(key = hainames)) +
  geom_smooth(aes(y = PMEDINC), 
              method="loess",
              col = "grey30", 
              fill = "light grey", 
              se = F, na.rm = T, 
              lty = "solid") + 
  geom_smooth(aes(y = PMEDINC), 
              method="lm", alpha = 0.25,
              col = "grey30", 
              fill = "light blue", 
              se = T, na.rm = T, 
              lty = "dashed") + 
  labs(x = element_blank(), y = "Median Income", subtitle = "Patterns in MSA by HAI Method") + 
  theme(legend.position = "none", 
        plot.subtitle = element_text(hjust=0.5))

df_finkey <- df.fin %>% 
  gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
         -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) 

df_types <- df_finkey %>% 
  spread(key, value) %>% 
  gather(key, value, -GeoFips, -GeoName, -year) %>% 
  mutate(Type = case_when(
    endsWith(key, "MEDINC") ~ "Basics",
    endsWith(key, "MEDVAL") ~ "Basics",
    endsWith(key, "MOE") ~ "Basics",
    endsWith(key, "UMEDVAL") ~ "Basics",
    endsWith(key, "LMEDVAL") ~ "Basics",
    endsWith(key, "PERINC") ~ "Basics",
    endsWith(key, "POP") ~ "Basics",
    endsWith(key, "ADJALL") ~ "Cost Adjustments",
    endsWith(key, "ADJIPD") ~ "Cost Adjustments",
    endsWith(key, "ADJRNT") ~ "Cost Adjustments",
    endsWith(key, "AINCALL") ~ "Income Adjustments",
    endsWith(key, "AINCDBT") ~ "Income Adjustments",
    endsWith(key, "AINCRNT") ~ "Income Adjustments",
    endsWith(key, "AQINC30") ~ "Income Adjustments",
    endsWith(key, "AQINC60") ~ "Income Adjustments", 
    endsWith(key, "DEBTCC") ~ "Debts and Obligations",
    endsWith(key, "DEBTED") ~ "Debts and Obligations",
    endsWith(key, "DEBTIL") ~ "Debts and Obligations",
    endsWith(key, "DEBTMV") ~ "Debts and Obligations",
    endsWith(key, "DEBTOC") ~ "Debts and Obligations",
    endsWith(key, "DEBTS") ~ "Debts and Obligations",
    endsWith(key, "HHSIZE") ~ "Debts and Obligations",
    endsWith(key, "IR") ~ "Debts and Obligations",
    endsWith(key, "HAI") ~ "Home Affordability Indices",
    endsWith(key, "HAIIPD") ~ "Home Affordability Indices",
    endsWith(key, "HAILEN") ~ "Home Affordability Indices",
    endsWith(key, "HAIRAW") ~ "Home Affordability Indices",
    endsWith(key, "HAIRNT") ~ "Home Affordability Indices",
    endsWith(key, "HAIRW") ~ "Home Affordability Indices",
    endsWith(key, "HAIDBT") ~ "Home Affordability Indices",
    endsWith(key, "RPPALL") ~ "Regional Price Parities",
    endsWith(key, "RPPGOODS") ~ "Regional Price Parities",
    endsWith(key, "RPPRENT") ~ "Regional Price Parities",
    endsWith(key, "RPPSOTH") ~ "Regional Price Parities",
    endsWith(key, "IPD") ~ "Implicit Price Deflator",
    endsWith(key, "PMT") ~ "Monthly Payments",
    endsWith(key, "PMT3DP") ~ "Monthly Payments",
    endsWith(key, "PMTRAW") ~ "Monthly Payments",
    endsWith(key, "QINC") ~ "Qualifying Income",
    endsWith(key, "QINCRAW") ~ "Qualifying Income",
  )) 

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white'))


dmap <- df_decades %>% 
  filter(Statistic == "Income Per Capita") %>% 
  mutate(mpay = (Value*.28)/12, 
         lmpay = (Value*.16)/12,
         dif = mpay - lmpay) %>% 
  mutate(RegionName = case_when(
    Region == 1 ~ "New England", 
    Region == 2 ~ "Mid-Atlantic",
    Region == 3 ~ "Midwest",
    Region == 4 ~ "Great Plains",
    Region == 5 ~ "South",
    Region == 6 ~ "Southwest",
    Region == 7 ~ "Rocky Mountain",
    Region == 8 ~ "West",
  )) 

# Alter to fit map
dmap$hover <- with(dmap, 
                   paste(GeoName, "<br>",
                         "Region:", RegionName, "<br>",
                         '<br>', "Income Per Capita:", paste0("$", format(Value, nsmall = 0)), "<br>",
                         "Max. Monthly Payment:", paste0("$", format(mpay, nsmall = 2, digits = 2)), "<br>",
                         "Min. Monthly Payment:", paste0("$", format(lmpay, nsmall = 2, digits = 2)), "<br>",
                         "Affordability Range", paste0("$", format(dif, nsmall = 2, digits = 2)))) 


colors <- data.frame(list(c("Bluered", "Cividis", "Earth", "Electric", "Greens", "Greys", "Hot", "Picnic", "RdBu", "Viridis")))
colnames(colors) <- "col"

ui <- fluidPage(
  tabsetPanel(
  tabPanel("Map", fluid = TRUE,
           titlePanel("Measuring Housing Affordability"),
           h4("An app to observe and assess measurements of affordability in the United States"),
           h5("Specifically we try to determine:"), 
           h5("     - Where is housing most affordable and why?"),
           h5("     - How has housing affordability changed?"),
           h5("     - Using standard NAR HAI, what is considered affordable?"),
           h5("     - If changes in HAI methods could better represent 
              existing challenges, what might they look like?"),
           h5("     - Explain major macroeconomic forces that effect affordability at scale"),
           hr(), 
           
  fluidRow(
    column(12,
      selectizeInput(
        inputId = "mapyear", 
        label = "Census Year:", 
        choices = unique(dmap$Year), 
        selected = 2019, 
        multiple = F), 
      selectizeInput(
        inputId = "mapcolor", 
        label = "Choose Color:", 
        choices = unique(colors$col), 
        selected = "Cividis", 
        multiple = F),
      plotlyOutput(outputId = "map", 
                   height = "1000px", 
                   width = "1600px")
      ), 
    h4("Income and Affordability"),
    h5("For this mapping tab we identify the geographic distribution of the primary driver of 
     the home affordability index (HAI), income. At its core, the HAI is represented as
     ratio of Median Household Income to a Qualifying Income per location. This Home 
     Affordability Index equation incorporates the home value within the 
     qualifying income. It is based on the idea that to afford housing one only 
     needs to have a house price and enough income to qualify for a mortgage 
     at a specific interest rate (3.5%). Here, we use the rule of 28% to estimate the maximum 
     proportion of monthly income an individual should spend on housing given 
     financial standards. For a minimum we found that the lowest quantile of homeowners
     financed their mortgage with a median of no less than 16% of their monthly income. 
     This is applied to estimate minimum payments by county and the difference between the
     two is that county's affordability range."),
    h5("With this map we can select the last year in each decade from 1969 to 2019. 
      Changing the map's colors can improve visibility of certain areas of interest. 
      For example, for isolating high and low values a Red-Blue might be of use while 
      Electric or Viridis with high lower value but still highlight higher values. 
      Income values per county through each year are displayed on the same scale $0 - $100,000.
      This begins to show how much has changed with income and monthly housing payments since
      1969. The state of Virginia contains multiple counties that are aggregated to form any area
      similar to a CBSA 
      and as such are not displayed on a county-level map."),
    
    hr(), 
    
    helpText("Data Source: U.S. Census Bureau at https://data.census.gov/cedsci/"),
    helpText("Note: Data has a maximum margin of error (MOE) of $17,381 and 
                    minimum of $447 for median home values and are estimated with 
                    ACS 5-Year results. Estimates of HAI are based on the 
                    observed median in real personal income 
                    of individuals alongside the median estimate of individuals'
                    perceived home values")
    )), # End tab 1 
    # Start tab 2 
    tabPanel("Methods", fluid = TRUE,
             sidebarLayout(
               sidebarPanel( # Create sidebar panel for Tab 2
                 selectizeInput(
                   inputId = "df_finkey_key", 
                   label = "Choose a Set of Affordability Methods", 
                   choices = c("HAI", "HAIRW", "HAIRNT", 
                               "HAIIPD", "HAIRAW", "HAILEN"), 
                   selected = c("HAI", "HAIRW", "HAIRNT", 
                                "HAIIPD", "HAIRAW", "HAILEN"), 
                   multiple = T), 
                 selectizeInput(
                   inputId = "method6type", 
                   label = "Choose the Statistical Data Category", 
                   choices = unique(df_types$Type), 
                   selected = "Basics", 
                   multiple = T),  
                 radioButtons("method_geom_function", "Visual Type",
                              c("Boxplot" = "boxplot",
                                "Density Plot" = "density",
                                "Histogram" = "hist",
                                "Bar Chart" = "bar")),
                 h4("Summary"), 
                 h5("There are 7 methods each developed to solve a particular
                    issue within the American housing market. They include
                    new measurements with adjustments for cost of living (HAIRW), rent (HAIRNT),
                    inflation (HAIIPD), raw home values (HAIRAW), outstanding debts (HAIDBT), 
                    theoretical 
                    lenient lending practices (HAILEN), and our control method (HAI) established
                    by the National Association of Realtors Home Affordability 
                    Index. Through these new measurements we gain an understanding
                    of how these adjustments might better respresent existing challenges 
                    for potential homebuyers while identifying what is considered 
                    affordable. Everyone has a different definition of what 
                    affordable housing means to them. The scope of our approach is purposefully
                    broad and based on a narrow definition of affordability so that measurements of 
                    deviations from 'normal' can be assessed.
                    We analyze metropolitan statistical areas (MSA) using the
                    most recent decade of available median home value estimates from the 
                    U.S. Census Bureau. These MSA areas made up roughly 83% of 
                    the U.S. population in 2019 which contained 3.28 million people."),
               ), # Close sidebar panel for tab 2
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("Distributions", plotOutput(outputId = "method1"),
                                      h4("Examining the Distributions by Method"),
                                      h5("Here we observe differences in the distributions of 
                                         the selected set of affordability methods. By default, we see all 
                                         of the methods except the one that includes debts. 
                                         The black horizontal line marks the point at which 
                                         the HAI value becomes affordable
                                         for MSA to the right of line. Those left of the line are 
                                         less than affordable. Be aware of the axes as they are 
                                         not all on the same scale.")), 
                             tabPanel("Comparison", plotOutput(outputId = "method2"),
                                      h4("Comparison to National Benchmark"), 
                                      h5("This scatterplot compares income relative to
                                         other selections from the set of methods. In all cases, 
                                         we draw attention to the debt-focused method. The vertical 
                                         dot-dashed line indicates the point at which housing 
                                         could be afforded. Values to the right of this line are in more affordable
                                         MSA while values to the left of it are less than affordable.
                                         No MSA is considered affordable when the average American's consumer 
                                         debts (which excludes mortgages) are included in the calculation. This
                                         occurs regardless of income since the closest MSA hover around 0-2 on a scale 
                                         in which 100+ indicates reasonable affordability.")), 
                             tabPanel("Patterns", plotOutput(outputId = "method3"),
                                      h4("Relationships between Method Features"),
                                      h5("This plot shows median HAI values for all MSA 
                                         in all years of the study (2009 through 2019).  
                                         The HAI values are plotted by method and LOESS 
                                         regression and linear regression trends are shown in
                                         black and blue respectively. The two bottom plots display
                                         the trends when grouped by affordability. The horizontal dotted line 
                                         is the median of all method adjusted median incomes sampled for this study."),
                                      h5("Notice the differences 
                                         in affordable and unaffordable. One has a slight increase while the
                                         other slightly decreases. The methods also appear to follow different
                                         relationships between income and affordability. For example our
                                         normal, inflation, and real-wage (or cost of living) adjustments show
                                         income decreases to a specific apex, then increases at the same rate. 
                                         Meanwhile the methods for lenient lending practices, raw home values, 
                                         and rent adjustments imply income increases until it is affordable (100), 
                                         decreases for a time thereafter, then increases again once the MSA is 
                                         affordable enough. These plots also highlight the natural grouping of MSA into 
                                         affordable and unaffordable categories. We can interpret this to mean, expensive 
                                         areas make it more difficult to increase earnings well-enough to afford comparably 
                                         priced housing in less expensive MSA.")),
                             tabPanel("Income & Affordability", plotOutput(outputId = "method4"),
                                      h4("The Primary Driver of Affordability"),
                                      h5("These line charts offer a near perspective on the patterns of income for 
                                         affordable and unaffordable MSA. We fit LOESS trends for each plot. 
                                         The horizontal dotted line marks the median of all method adjusted median incomes
                                         sampled in this study.
                                         In this manner, with income shown year-over-year,
                                         it is clear affordable MSA increase income at a greater rate than less affordable
                                         MSA. Additionally, we notice that many unaffordable MSA still experience low median 
                                         incomes at or only slightly above the median incomes of affordable MSA. By default,
                                         all methods except debts are included. The debts method is an outlier of extreme proportions and 
                                         while it may be realistic for many, it does not measure well for all. Reliable 
                                         data on debts is difficult to gather for each MSA in each year of the study and as such, 
                                         the method is removed.")), 
                             tabPanel("HAI & Affordability", plotOutput(outputId = "method4_5"),
                                      h4("Affordability over Time"),
                                      h5("Here we review how the HAI value changes by year in this study. 
                                         By default, all methods except debts are shown. Try changing the 
                                         methods selected and observe the different trends. It should become 
                                         clear that affordable MSA appear to get more affordable with time while 
                                         less affordable MSA stay roughly the same or at a slight decrease in affordability.
                                         However, our rent adjusted method (which measures the rate of 'rent as a service' found 
                                         in regional price parities per MSA from the BEA) exemplifies how much affordability would change 
                                         if we adjusted income relative to rent or rent was adjusted relative to income. This is a common 
                                         problem in larger more heavily populated MSA and they also make up a disproportionate amount 
                                         of unaffordable MSA by all HAI methods.")), 
                             tabPanel("HAI Caterpillars", plotOutput(outputId = "method5"),
                                      h4("Mind the Gap by Method"),
                                      h5("Among the affordable and unaffordable groups inherent to the way 
                                         MSA are distributed in the U.S. we find the normal NAR HAI does not show 
                                         underlying macroeconomic forces effecting income. In all methods except for inflation
                                         adjustments and lenient lending practices there is a point above and below the affordability
                                         benchmark (100). For years where every MSA was considered affordable, points are missing in
                                         inflation in 2017 and theoretical lenient lending practices for several years. A linear trend
                                         is fit to the data. We notice little to no change in the raw home values method, normal, 
                                         and inflation adjusted HAI methods but slight increases for affordability to the remaining methods.
                                         Importantly, the distances between the affordable and unaffordable groups changes by method. Those 
                                         closest together show a smaller affordability gap and for those farther apart a larger gap.")), 
                             tabPanel("Statistics", plotOutput(outputId = "method6"),
                                      h4("Additional Statistics"),
                                      h5("For this tab we include all 43 statistics used to calculate the incomes and
                                         HAI methods. These statistics have been categorized to make choosing the Statistical 
                                         Data Category easier. As usual, you have the option to view any number of them in 4 different
                                         visual types. This can display adjustments made to cost, income, debts, regional price parities, 
                                         qualifying incomes, and home affordability indcies directly. Do note that debts are a single average
                                         value applied to income (specifically subtracted from income) and the implicit regional price deflator
                                         is our closest approximation of experienced inflation. It is updated at regular annual intervals by
                                         the Bureau of Economic Analysis (BEA)."))
                 ) # End Child Tab Panel of Main Panel 
               ) # End Main Panel 
             ) # End Sidebar layout for tab 2
    ), # End Tab 2
    # Start Tab 3
    tabPanel("Population", fluid = TRUE,
             sidebarLayout(
               sidebarPanel( 
                 selectizeInput(
                   inputId = "barcharthaikey", 
                   label = "Select Methods for Total Population Estimates", 
                   choices = unique(d$key), 
                   selected = "HAI", 
                   multiple = T),
                 selectizeInput(
                   inputId = "barcharthaiset", 
                   label = "Select Affordability Set for Total Population", 
                   choices = unique(d$Set), 
                   selected = "LT100", 
                   multiple = F), 
                 selectizeInput(
                   inputId = "barcharthaikey2", 
                   label = "Select Proportion of Population Method", 
                   choices = unique(d$key), 
                   selected = "HAI", 
                   multiple = F), 
                 h4("Summary"),
                 h5("In this tab we review how many people are placed into
                    affordable and unaffordable areas based on the method of
                    measurement. Ideally, the best measurement would reduce
                    the amount of people in unaffordable MSA to zero (or near
                    zero) while maintaining stability. This analysis excludes
                    sustainability evaluations of these methods but such evaluations
                    are integral to ensure access to safe, adequate, long-lasting 
                    housing in a way that is not detrimental to the environment.
                    These should be reexamined in future studies.")
               ), # close Sidebar Panel for Tab 3 
               mainPanel(fluidRow( # Create main panel Tab 3
                 column(4, plotlyOutput("barcharthai")),
                 column(8, plotOutput("barproportionmsabymethod")),
                 h4("Populations by Method"),
                 h5("On the y-axis we have the total population in millions of people
                 for the selected method(s) of interest in the Total Population Estimates 
                 box. It is also important to select the affordability set for the total 
                 population. For the bar chart on the left it is represented as 
                 less than 100 (LT100) or greater than
                 100 (GT100) for comparison to other method charts. In the stacked bar chart 
                 on the right we see the proportion of the populations that are in affordable 
                 and unaffordable MSA. For this proportion chart, we only show one method 
                 at a time and it is controlled by the selection of the proportion of the 
                 population method."), 
                 h5("You will notice that while debts are included in this visual, none of
                    the population will be shown on the chart on the left because there is
                    no MSA that can afford to pay for both their consumer debts and obligations
                    as well as housing. Meanwhile, some methods perform better than our normal (HAI)
                    method at reducing the total population in the category LT100. There is natural 
                    variation in the data set and many other factors to consider when evaluating if 
                    a method performs better than the rest. We focus only on the total population here
                    and notice that if we lended leniently or adjusted income by inflation we would 
                    achieve the best results. Now, realistically, these lenient lending practices are 
                    what is thought to have caused the 2008 great recession so they are not necessarily
                    the best by other standards. Additionally, projecting inflation is also not likely 
                    to be very accurate when adjusting a houshold's income. It would be similar to trying to
                    hit a moving target and might also produce unintended consequences of furthering inflation. 
                    We consider another perspective."), 
                 h5("For the realists among us, adjustments to qualifying or median individual income is more likely
                    with a rent (HAIRNT) and real-wage (HAIRW) methods. Examples of these adjustments already exist
                    and appear to paint a more valid picture when assessing affordability in MSA when compared to 
                    the normal method (HAI). This is visualized best when the 3 methods are selected from the total
                    population estimates and the affordability set is LT100. In the HAIRNT method we include the price
                    of rent in income which incorporates values that more than 44 million people in the U.S. pay monthly. 
                    Of those renters, 45% pay more than 30% of their monthly income to rent. 
                    This prevalence is so high that financial institutions have updated their recommendations
                    for renters while mortgage recommendations remain the same. However, both have gotten more strict
                    in practices. This HAIRNT method, applies best in heavily populated or dense MSA which typically have 
                    higher rents. The alternative for less populated, sparse MSA is the HAIRW. This method adjusts income by the cost
                    of living using a market basket of goods and services from the Bureau of Labor Statistics and the Bureau of Economic 
                    Analysis. Since it is typical that the prices of goods and services are lower in less populated MSA, this
                    gives a boost to the affordability of locations outside of locations that HAIRNT performs best in. 
                    ")
                ) # Close mainPanel fluidRow
               ) # Close main panel 
             ) # End Tab 3 Sidebar Layout 
    ), # End Tab 3
    # Start Tab 4
    tabPanel("Income", fluid = TRUE,
             sidebarLayout(
               sidebarPanel( # Create Sidebar Panel
                 selectizeInput(
                   inputId = "Statisticdftbl", 
                   label = "Select Statistic", 
                   choices = unique(df.tbl$Statistic), 
                   selected = "Income Per Capita", 
                   multiple = F), 
                 h4("Summary"),
                 h5("Income is the primary variable with which an individual 
                    can hope to improve their affordability of housing. However, 
                    we know simply increasing our income is easier said than done.
                    It is better to know how much income should increase to be in 
                    an affordable position regardless of MSA. It is also reasonable 
                    to wonder how many people are present in an area and assess your 
                    income relative to the median and minium income levels of those 
                    in the same area. The closest local level in this analysis is the MSA but
                    this contains 83% of the U.S. population and serves as a good indicator of
                    overall income trends in the country. We break down the statistics by boxplot
                    and scatterplot and let you assess how income has changed since 2009."), 
                 h5("By default, we start with Income per capita in each MSA and see a stretched 
                    maximum box in green and condensed minimum blue box. If we select either 
                    personal income or population, we notice the other median and minimums seem flat.
                    To see the other median and minimum for these categories the max and/or median traces
                    must be hidden.")
               ), # close Sidebar Panel for Tab 5
               mainPanel(fluidRow( # Create main panel Tab 5
                 column(4, plotlyOutput("boxplotincome")),
                 column(8, plotlyOutput("plotincome2")),
                 h4("Income and Affordability"),
                 h5("Within an HAI, income is the primary statistic by which
                    an individual can improve the circumstances surrounding 
                    their own affordability challenges. However, there are 
                    macroeconomic forces that show wealth inequality increasing 
                    across the entire United States, which lowers individuals' 
                    chances of affording homes. Of course, there will still be
                    individuals who can afford more expensive homes because
                    the wealth still exists to afford them. It is just concentrated
                    among fewer individuals. But, since there are still individuals 
                    available to purchase less affordable homes, prices will 
                    continue to increase at a steady rate."), 
                 h5("This can be seen directly as the growing distance between minimum 
                    and maximum values over time. To visualize, when selecting the statistic, 
                    choose either income per capita or personal income. Deselect 
                    traces one at a time to isolate the minimum, median, and maximum
                    trends. Notice the differences in the boxplot height, medians, 
                    and quartiles. The maximum value is stretching upwards while 
                    the minimum box appears compressed. The scatterplot on the
                    right shows just how far the maximum income is from the minimum. 
                    Our median exists much closer to the minimum in all years but 
                    especially in the last decade compared to that of 1970. For 
                    personal income and population, we have no choice but to isolate
                    each trace. Otherwise, the values appear flat when in fact, 
                    they are not!"),
                 h5("The bottom line, this spreading of income between the highest earners
                    and lowest earners hurts individuals' chances of affording homes.
                    Quite simply, it is due to a widening of the home affordability 
                    gap by decreasing the number of people with enough income to 
                    afford the median home value. Referring to manipulations in 
                    the standard National Association of Realtors equation, we 
                    know that if enough people in the U.S. were to increase their 
                    income, it would take time for median home values to catch up 
                    and more homes would be affordable. However, what we notice is 
                    the opposite. Based solely on historical earnings, if people 
                    are not increasing their income then fewer people will be able
                    to afford housing.")
                 
               ) # Close mainPanel fluid Row
               ) # Close main panel 
             ) # End Tab 4 Sidebar Layout 
    ),
    # Start tab 5
    tabPanel("Tables", fluid = TRUE,
             sidebarLayout(
               sidebarPanel( # Create sidebar panel for Tab 5
                 selectizeInput(
                   inputId = "haitablestab", 
                   label = "Select Method", 
                   choices = unique(ltdf$key), 
                   selected = "HAI", 
                   multiple = F), 
                 selectizeInput(
                   inputId = "dffingeoname", 
                   label = "Select MSA", 
                   choices = unique(df.fin$GeoName), 
                   selected = "New York-Newark-Jersey City, NY-NJ-PA (Metropolitan Statistical Area)", 
                   multiple = T),
                 h4("Summary"),
                 h5("This section includes organized tables for referencing. The limiting 
                    factor in this study was our earliest year 2009-2010 when several MSA were
                    missing values. This dropped our total MSA count to 539. Those listed in the 
                    All tab show every HAI value and corresponding statistic for each year in the
                    study (2009 through 2019). Those listed as unaffordable contain values determined by the
                    normal HAI method to be less than affordable (or less than 100) and those that are listed as affordable
                    were determined by the same method but were greater than or equal to 100. Filtering
                    is allowed and selections can be made for HAI method and specific MSA.")
               ), # Close sidebar panel for tab 5 
               mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("All", dataTableOutput("alltable")),
                             tabPanel("Unaffordable", dataTableOutput("ltdftable")),
                             tabPanel("Affordable", dataTableOutput("gtdftable"))
                            
                 )
               )
             )
      )
    )
  )

# Define server functions
server <- function(input, output){

  output$map <- renderPlotly({
    dmap %>% 
      filter(Year == input$mapyear) %>%
      plot_ly(.) %>% 
      add_trace(
        type="choropleth",
        geojson=counties,
        locations= ~GeoFips,
        z= ~Value,
        colorscale= input$mapcolor,
        zmin=0,
        zmax=100000,
        text = ~hover,
        marker=list(line=list(
          width=0))) %>% 
      colorbar(title = "Salary") %>% 
      layout(title = "U.S. Median Income Per Capita") %>% 
      layout(geo = g) 
    })
  
  # Method Tab
  output$method1 <- renderPlot({
    
    method_function <- switch(input$method_geom_function,
                              boxplot = geom_boxplot,
                              density = geom_density,
                              hist = geom_histogram, 
                              bar = geom_bar)
    
    df_finkey %>% 
      filter(key == input$df_finkey_key) %>% 
      ggplot(aes(value, col = key)) + 
      method_function(alpha = .05) + 
      geom_vline(xintercept = 100) + 
      labs(subtitle = "Method Distributions", x = "Selected Statistic", y = "Count") + 
      theme(plot.subtitle = element_text(hjust = 0.5)) + 
      facet_wrap(~key, scales = "free_x", labeller = labeller(key = hainames)) +
      theme(legend.position = "none", 
            panel.grid = element_blank()) 
  })
  
  output$method2 <- renderPlot({
    # HAI Scatterplot Comparisson Plots
    HAINAMES <- list("Normal" = "HAI", "Real Wage"="HAIRW", "Rent Adjustment"="HAIRNT", 
                     "Inflation Adjusted"="HAIIPD", "Raw Values"="HAIRAW", "Lenient Lending"="HAILEN")
    hainames <- c("HAI" = "Normal", 
                  "HAIRW"="Real Wage", 
                  "HAIRNT"="Rent Adjusted", 
                  "HAIIPD"="Inflation Adjusted", 
                  "HAIRAW"="Raw Values", 
                  "HAILEN"="Lenient Lending")
    HAItitle <- function(variable,value){
      return(HAINAMES[value])
    }
    # Main 6 HAIs labeled Scatterplot Trends
    main6 <- df.fin %>%
      gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
             -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) %>%
      filter(key == input$df_finkey_key) %>% 
      filter(AINCALL <100000) %>% 
      ggplot(aes(value, (AINCALL/1000))) + 
      geom_point(aes(x = value, alpha = .01, col = key)) + 
      geom_vline(xintercept = 100, lty = "dotdash", col = "black") + 
      geom_smooth(aes(x = value), 
                  method="lm", col = "grey30", fill = "light grey",
                  se = T, na.rm = T) + 
      labs(x = "Home Affordability Value", y = "Income ($1000)", 
           subtitle = "Method Comparison to National Benchmark") +
      facet_wrap(key ~ ., scales = "free_x", shrink = F, labeller = labeller(key = hainames)) + 
      theme(legend.position = "none", 
            plot.subtitle = element_text(hjust = 0.5), 
            panel.grid = element_blank())
    # Outstanding Debts HAI ScatterPlot Trends
    dbts1 <- df.fin %>%
      gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
             -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) %>%
      filter(key == c("HAIDBT")) %>% 
      filter(AINCALL <100000) %>% 
      ggplot(aes(value, (AINCALL/1000))) + 
      geom_point(aes(x = value, alpha = .01, col = key)) + 
      geom_vline(xintercept = 100, lty = "dotdash", col = "black") + 
      geom_smooth(aes(x = value), 
                  method="loess", col = "grey30", fill = "light grey",
                  se = T, na.rm = T) + 
      labs(x = "Home Affordability Value", y = "Income ($1000)", 
           subtitle = "Debt Adjusted") +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = .5)) 
    dbts2 <- df.fin %>%
      gather(key, value, -GeoFips, -GeoName, -year, -MEDINC, 
             -MEDVAL, -MOE, -UMEDVAL, -LMEDVAL, -POP, -PERINC, -AINCALL) %>%
      filter(key == c("HAIDBT")) %>% 
      filter(AINCALL <100000) %>% 
      ggplot(aes(value, (AINCALL/1000))) + 
      geom_point(aes(x = value, alpha = .01, col = key)) +
      geom_smooth(aes(x = value), 
                  method="loess", col = "grey30", fill = "light grey",
                  se = T, na.rm = T) + 
      labs(x = "Home Affordability Value", y = element_blank(), 
           subtitle = "Debt Adjusted Centered") +
      theme(legend.position = "none", 
            plot.title = element_text(hjust = .5), 
            panel.grid.minor.y = element_blank())
    # Arrange plots 
    ggarrange(main6,
              ggarrange(dbts1, dbts2, 
                        ncol=2, labels = c(" ", " ")),
              nrow = 2, labels = " ")
  })
  
  output$method3 <- renderPlot({
    MEGANAMES <- list("Affordable" = "GT100", "Unaffordable"="LT100")
    meganames <- c("GT100" = "Affordable", 
                   "LT100"="Unaffordable")
    megatitles <- function(variable,value){
      return(MEGANAMES[value])
    }
    # Megaplots 
    PMEDINC.median <- median(d$PMEDINC)
    megaplot1 <- d %>% 
      sample_n(715, replace = T) %>% 
      filter(key == input$df_finkey_key) %>%
      group_by(key, Set) %>%
      arrange(desc(Set)) %>% 
      ggplot(aes(x = MMEDHAI, col = key)) + geom_point(aes(y = PMEDINC, col = key, size = 4)) + 
      geom_hline(yintercept = PMEDINC.median, lty = "dotted") + 
      geom_point(aes(y = PMEDINC)) +
      facet_wrap(~Set, scales = "free_x", labeller = labeller(Set = meganames)) +
      geom_smooth(aes(y = PMEDINC), 
                  method="loess",
                  col = "grey30", 
                  fill = "light grey", 
                  se = T, na.rm = T, 
                  lty = "solid") + 
      geom_smooth(aes(y = PMEDINC), 
                  method="lm", alpha = 0.25,
                  col = "blue", 
                  fill = "light blue", 
                  se = F, na.rm = T, 
                  lty = "dashed") + 
      labs(x = "Median HAI Value", y = "Median Income") + 
      theme(legend.position = "none", plot.title = element_blank())
    megaplot2 <- d %>% 
      sample_n(715, replace = T) %>% 
      filter(key == input$df_finkey_key) %>%
      group_by(key, Set) %>% 
      ggplot(aes(x = MMEDHAI, col = key)) + geom_point(aes(y = PMEDINC, col = key, size = 4)) + 
      geom_hline(yintercept = PMEDINC.median, lty = "dotted")  +
      facet_wrap(~key, scales = "free", labeller = labeller(key = hainames)) +
      geom_smooth(aes(y = PMEDINC), 
                  method="loess",
                  col = "grey30", 
                  fill = "light grey", 
                  se = F, na.rm = T, 
                  lty = "solid") + 
      geom_smooth(aes(y = PMEDINC), 
                  method="lm", alpha = 0.25,
                  col = "blue", 
                  fill = "light blue", 
                  se = T, na.rm = T, 
                  lty = "dashed") + 
      labs(x = element_blank(), y = "Median Income", subtitle = "Patterns in MSA by HAI Method") + 
      theme(legend.position = "none", 
            plot.subtitle = element_text(hjust=0.5))
    ggarrange(megaplot2, megaplot1, nrow=2)
  })
  
  output$method4 <- renderPlot({
    d %>% 
      sample_n(715, replace = T) %>%
      filter(key == input$df_finkey_key) %>% 
      group_by(key, Set) %>% 
      ggplot(aes(x = year, col = key)) + geom_point(aes(y = PMEDINC, col = key)) + 
      geom_hline(yintercept = PMEDINC.median, lty = "dotted") + 
      geom_line(aes(y = PMEDINC)) +
      facet_wrap(~Set, scales = "free", labeller = labeller(Set = meganames)) +
      geom_smooth(aes(y = PMEDINC), 
                  method="loess",
                  col = "grey30", 
                  fill = "light grey", 
                  se = T, na.rm = T, 
                  lty = "solid") + 
      labs(x = element_blank(), y = "Median Income", subtitle = "Method Comparison for all MSA per Year") +
      theme(legend.position = "none", 
            plot.subtitle = element_text(hjust = 0.5),
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank())
  })
  
  output$method4_5 <- renderPlot({
    d %>% 
      sample_n(715, replace = T) %>%
      filter(key == input$df_finkey_key) %>%
      group_by(key, Set) %>% 
      ggplot(aes(x = year, col = key)) + geom_point(aes(y = MMEDHAI, col = key)) + 
      geom_hline(yintercept = 100,  lty = "dotted") + 
      geom_line(aes(y = MMEDHAI)) +
      facet_wrap(~Set, scales = "fixed", labeller = labeller(Set = meganames)) +
      geom_smooth(aes(y = MMEDHAI), 
                  method="lm",
                  col = "grey30", 
                  fill = "light grey", 
                  se = T, na.rm = T, 
                  lty = "solid") + 
      labs(x = element_blank(), y = "Median HAI", subtitle = "Method Comparison for all MSA per Year") +
      theme(legend.position = "none", 
            plot.subtitle = element_text(hjust = 0.5),
            panel.grid.minor.x = element_blank(), 
            panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank()) 
  })
  
  output$method5 <- renderPlot({
    d %>% 
      sample_n(715, replace = T) %>%
      filter(key == input$df_finkey_key) %>%
      group_by(key, Set) %>% 
      ggplot(aes(x = year, col = key)) + geom_point(aes(y = MMEDHAI, col = key)) + 
      geom_hline(yintercept = 100, lty = "dotted")  +
      facet_wrap(~key, scales = "fixed", labeller = labeller(key = hainames)) +
      geom_smooth(aes(y = MMEDHAI), 
                  method="lm",
                  col = "grey30", 
                  fill = "light blue", 
                  se = T, na.rm = T, 
                  lty = "solid") + 
      theme(legend.position = "none", 
            plot.subtitle = element_text(hjust = 0.5)) + 
      labs(x = element_blank(), y = "Median HAI", 
           subtitle = "Method Comparisson for all MSA by Year (Above and Below National Benchmark = 100)")
  })
  
  output$method6 <- renderPlot({
    
    method_function <- switch(input$method_geom_function,
           boxplot = geom_boxplot,
           density = geom_density,
           hist = geom_histogram, 
           bar = geom_bar)
    
    df_types %>% 
      filter(Type == input$method6type) %>% 
      ggplot(aes(value, col = key, alpha = 0.05)) + 
      method_function() + 
      labs(subtitle = "Distribution", x = "Selected Statistic", y = "Count") + 
      theme(plot.subtitle = element_text(hjust = 0.5)) + 
      facet_wrap(~key, scales = "free") +
      theme(legend.position = "none", 
            panel.grid = element_blank())
  })
  
  
  # Income Tab
  output$boxplotincome <- renderPlotly({
    df.tbl %>% 
      filter(Statistic == input$Statisticdftbl) %>% 
      plot_ly(., y = ~min, name = 'min', type = "box", boxpoints = "all", jitter = 0.75,
              pointpos = 0, 
              marker = list(opacity = 0.5, color = 'rgb(17, 157, 255)', line = list(width = 2))) %>% 
      add_trace(y = ~med, name = 'med', boxpoints = "all", jitter = 0.3,
                pointpos = 0, 
                marker = list(opacity = 0.5, color = 'rgb(17, 157, 255)', line = list(width = 2))) %>% 
      add_trace(y = ~max, name = 'max', boxpoints = "all", jitter = 0.3,
                pointpos = 0, 
                marker = list(opacity = 0.5, color = 'rgb(17, 157, 255)', line = list(width = 2))) %>% 
      layout(yaxis = list(title = "Selected Statistic"))
  })
  
  output$plotincome2 <- renderPlotly({
    df.tbl %>% 
      filter(Statistic == input$Statisticdftbl) %>% 
      plot_ly(., x = ~Years, y = ~min, name = "min", type = "scatter", mode = "lines+markers") %>% 
      add_trace(y = ~med, name = 'med', mode = 'lines+markers') %>% 
      add_trace(y = ~max, name = 'max', mode = 'lines+markers') %>% 
      layout(xaxis = list(title = "Year"),
             yaxis = list(title = "Selected Statistic"))
  })
  
  
  # Population Tab
  output$barproportionmsabymethod <- renderPlot({
    d %>% 
      filter(key == input$barcharthaikey2) %>% 
      ggplot(aes(year, (TPOP/1000000), fill=Set)) + 
      geom_col(col = "grey38", alpha= 0.25) + 
      # scale_x_discrete(limit = c(2010, 2015, 2019)) + 
      labs(x = "Year (2010 - 2019)", y = "Population (Millions)", subtitle = "Proportion of MSA Population by Method") + 
      theme(axis.text.x = element_blank(), 
            # axis.ticks = element_line(size = .5), 
            plot.subtitle = element_text(hjust = 0.5)) +
      facet_wrap(~key, scales = "fixed", labeller = labeller(key = hainames)) + 
      scale_fill_discrete(limits = c("LT100", "GT100"), labels = c("Unaffordable", "Affordable") )
  })
  
  output$barcharthai <- renderPlotly({
    d %>% 
      filter(Set == input$barcharthaiset) %>%
      filter(key %in% input$barcharthaikey) %>% 
      plot_ly(., x = ~year, y = ~(TPOP/1000000), type = "bar", name = ~key, opacity = 0.75) %>% 
      layout(yaxis = list(title = "Population (Millions)"))
  })  
  
  # Tables Tab
  output$ltdftable <- renderDataTable({
    ltdf %>% 
      dplyr::select(GeoName, key, year, MEDINC, MEDVAL, MOE, POP, AINCALL, value) %>%
      filter(key == input$haitablestab) 
  }, filter = 'top',
  rownames = T)
  
  output$gtdftable <- renderDataTable({
    gtdf %>% 
      dplyr::select(GeoName, key, year, MEDINC, MEDVAL, MOE, POP, AINCALL, value) %>%
      filter(key == input$haitablestab) 
  }, filter = 'top',
  rownames = T)
  
  output$alltable <- renderDataTable({
    df.fin %>% 
      filter(GeoName == input$dffingeoname)
  }, filter = "top", 
  rownames = F)
  
} # Close server

shinyApp(ui, server)

