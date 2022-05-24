
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
  geom_col(fill = "sky blue", col = "black") + 
  geom_text(aes(label = paste0(round(Percent, 1), "%"), y = Deaths), 
            hjust = -.25, colour = "black") + 
  coord_flip() + 
  labs(x = "Cause", y = "Deaths", 
       subtitle = "Leading Causes of Death in Rock County")
# plotly::ggplotly(Rock_COD_UCD_plot)

# Unknown question - what is the distribution of each varaible?
UCD %>% 
  filter(County == "Rock County, WI", # select multiple for boxplot?  
         # ICD.Chapter == "Neoplasms" # select multiple for classified causes of death?
         ) %>% 
  group_by(ICD.Chapter) %>% 
  arrange(desc(Deaths)) %>% 
  ggplot(aes(x = Deaths)) + geom_density(aes(col = County)) + 
  facet_wrap(~ICD.Chapter, scales = "free")# select different functions? Density/histogram/boxplot

# Have more or less people died of [x] over time in Rock County? 
UCD %>% 
  filter(County == c("Rock County, WI"), 
         Year >= 1999, Year <= 2020, 
         ICD.Chapter == c("Neoplasms", "Diseases of the circulatory system")) %>% 
  ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
  geom_line() + geom_point()


# Can we model and predict if more or less people will die of [x] over time? 
UCD %>% 
  filter(County == c("Rock County, WI"), 
         Year >= 1999, Year <= 2020, 
         ICD.Chapter == c("Neoplasms", "Diseases of the circulatory system")) %>% 
  ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
  geom_point() + geom_smooth()

?geom_smooth()

# Compared to the rest of WI, does the crude mortality rate seem normal in Rock County? 
boxfunci <- function(d) {
  stats <- boxplot.stats(d)
  data.frame(ymin = stats$conf[1], ymax = stats$conf[2], y = stats$stats[3])
}

UCDboxplot <- UCD %>%
  filter(Year == 2019, ) %>% 
  group_by(County, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000) %>% 
  ggplot(aes(reorder(ICD.Chapter, Crude_Mortality_Rate), Crude_Mortality_Rate)) + 
  # geom_point(aes(fill = ICD.Chapter), alpha = 0.10) +
  geom_boxplot(aes(fill = ICD.Chapter, alpha = 0.5, col = ICD.Chapter), 
               notch = TRUE, notchwidth = 0.5) + 
  stat_summary(fun.data = boxfunci, geom = "crossbar", 
               colour = NA, fill = "light grey", width = 0.8, alpha = 0.45) + 
  labs(y = "Crude Mortality Rate",
       x = "ICD-10 Chapter Cause of Death"
       ) +
  coord_flip() + theme(legend.position = "none", 
                       axis.title.y = element_blank())
ggplotly(UCDboxplot)


    UCD %>% 
      filter(Year == 2019) %>% 
      group_by(County, ICD.Chapter) %>% 
      summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000) %>%
      ggplot(aes(Crude_Mortality_Rate)) + 
      # geom_point(aes(fill = ICD.Chapter), alpha = 0.10) +
      geom_bar(aes(fill = ICD.Chapter)) + 
      labs(y = "Crude Mortality Rate",
           x = "ICD-10 Chapter Cause of Death") +
      theme(legend.position = "none", 
            axis.title.y = element_blank()) + 
      facet_wrap(~ICD.Chapter, scales = "free")


###############################

### ----- UCD Mapping ----- ###

############################### 
library(tigris)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(dplyr)

### -----  ----- ### 

#  Map Filter Testing

### -----  ----- ###
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

UCD_sub <- UCD_Crude %>% 
  filter(ICD.Chapter == "Neoplasms")

UCD_sub_wi <- WI_counties %>% 
  left_join(UCD_sub, by = "GEOID")
UCD_sub_sf <- sf::as_Spatial(UCD_sub_wi)

pal_UCD_mx <- as.numeric(max(UCD_sub_wi$Crude_Mortality_Rate, na.rm = T))
pal_UCD_mn <- as.numeric(min(UCD_sub_wi$Crude_Mortality_Rate, na.rm = T))
pal_UCD <- colorNumeric(c("RdYlGn"), pal_UCD_mn:pal_UCD_mx, reverse = T)

names(providers)
?leaflet::addTiles()

UCD_sub_sf %>%
  leaflet() %>%
  addTiles(layerId = "OSM", group = "OSM", 
           options = providerTileOptions(opacity = 0.5)) %>% 
  addProviderTiles(layerId = "CARTO", "CartoDB", group = "CARTO", 
                   options = providerTileOptions(opacity = 0.99)) %>% 
  addProviderTiles(layerId = "ESRI", "Esri", group = "ESRI") %>%
  addSearchOSM() %>% 
  # addReverseSearchOSM() %>%
  addPolygons(weight = 2, color = ~pal_UCD(Crude_Mortality_Rate),
              label = ~paste0(NAME,
                              ", ", signif(Crude_Mortality_Rate, digits = 4)),
              highlight = highlightOptions(weight = 1, color = "Black",
                                           bringToFront = TRUE)) %>% 
  addLegend(UCD_sub_wi$Crude_Mortality_Rate, position = "bottomright", pal = pal_UCD) %>%
  addLayersControl(baseGroups = c("CARTO", "OSM", "ESRI")) 
# setView(lat = 42.94033923, lng = -75.05859375, zoom = 4)


save(WI_counties, UCD_Crude, file = "Data/UCD_monprovisional.rdata")

