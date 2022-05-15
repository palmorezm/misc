
# Packages 
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
theme_set(theme_bw())
load("Data/reg_mort.rdata")
###############################

### ----- Q & A Plots ----- ###

############################### 

# What do people in Rock County die from and how many died from that particular cause?
# What proportion of the deaths were from each cause? 
UCD %>% 
  filter(Year == 2020, 
         County == "Rock County, WI") %>% 
  mutate(total_deaths = sum(Deaths)) %>% 
  group_by(ICD.Chapter, Deaths, Crude.Rate, total_deaths) %>% 
  summarise(Percent = (Deaths / total_deaths)*100 ) %>%
  arrange(desc(Percent)) %>%  
  ggplot(aes(reorder(ICD.Chapter, Deaths), Deaths)) + 
  geom_col(fill = "light blue", col = "black") + 
  geom_text(aes(label = paste0(round(Percent, 1), "%"), y = Deaths), 
            hjust = -.25, colour = "black") + 
  coord_flip() + 
  labs(x = "Cause", y = "Deaths", 
       subtitle = "Leading Causes of Death in Rock County")
# plotly::ggplotly(Rock_COD_plot)

# Unknown question - what is the distribution of each varaible?
UCD %>% 
  filter(County == "Rock County, WI", # select multiple for boxplot?  
         # ICD.Chapter == "Neoplasms" # select multiple for classified causes of death?
         ) %>% 
  group_by(ICD.Chapter) %>% 
  arrange(desc(Deaths)) %>% 
  ggplot(aes(x = Deaths)) + geom_density(aes(col = County)) # select different functions? Density/histogram/boxplot


# Have more or less people died of [x] over time in Rock County? 
UCD %>% 
  filter(County == c("Rock County, WI"), 
         Year >= 1999, Year <= 2020, 
         ICD.Chapter == c("Neoplasms", "Diseases of the circulatory system")) %>% 
  ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
  geom_line() + geom_point()

# Compared to the rest of WI, does the crude mortality rate seem normal in Rock County? 
boxfunci <- function(d) {
  stats <- boxplot.stats(d)
  data.frame(ymin = stats$conf[1], ymax = stats$conf[2], y = stats$stats[3])
}

UCD %>%
  filter(Year == 2019, ) %>% 
  group_by(County, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000) %>% 
  ggplot(aes(reorder(ICD.Chapter, Crude_Mortality_Rate), Crude_Mortality_Rate)) + 
  geom_point(aes(fill = ICD.Chapter), alpha = 0.10) +
  geom_boxplot(aes(fill = ICD.Chapter, alpha = 0.5, col = ICD.Chapter), 
               notch = TRUE, notchwidth = 0.5) + 
  stat_summary(fun.data = boxfunci, geom = "crossbar", 
               colour = NA, fill = "light grey", width = 0.8, alpha = 0.45) + 
  coord_flip() + theme(legend.position = "none")

###############################

### ----- UCD Mapping ----- ###

############################### 
library(tigris)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(dplyr)
WI_counties <- tigris::counties(state = "WI")
UCD <- UCD %>% 
  rename(GEOID = County.Code)
UCD$GEOID <- as.character(UCD$GEOID)

UCD_Crude <- UCD %>% 
  group_by(County, GEOID, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths, na.rm = T), 
            Population = sum(Population, na.rm = T), 
            Crude_Mortality_Rate = as.numeric(Crude_Mortality / Population * 100000))
pal_UCD_max <- as.numeric(max(UCD_Crude$Crude_Mortality_Rate, na.rm = T))
pal_UCD_min <- as.numeric(min(UCD_Crude$Crude_Mortality_Rate, na.rm = T))
pal_UCD <- colorNumeric(c("RdYlGn"), pal_UCD_min:pal_UCD_max)

UCDsf <- WI_counties %>% 
  left_join(UCD_Crude, by = "GEOID")
UCDsf <- sf::as_Spatial(UCDsf)

UCDsf %>%
  leaflet() %>%
  addTiles(group = "OSM") %>% #group = "OSM"
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>%
  addSearchOSM() %>% 
  addReverseSearchOSM() %>%
  addPolygons(weight = 1, color = ~pal_UCD(Crude_Mortality_Rate),
              label = ~paste(NAME, "</br>",
                             "Total Deaths:", signif(Crude_Mortality, digits = 1)),
              highlight = highlightOptions(weight = 1, color = "black",
                                           bringToFront = TRUE)) %>% 
  addLegend(, position = "bottomright", pal = pal2) %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Endangered", "Threatened", "Not Listed")) 
# setView(lat = 42.94033923, lng = -75.05859375, zoom = 4)

