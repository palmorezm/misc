
# APP for UCD 

# Packages
library(tigris)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
theme_set(theme_bw())

# Data
load("Data/reg_mort.rdata")
# load() Mapping data?

ui <- navbarPage(
  "4D Rock County",   
  fluidRow(
    column(12, 
           title = "Mortality",
           h4("An app to observe and assess measurements of Mortality in Rock County"),
                      h5("Specifically we try to determine:"),
                      helpText("Data Source: U.S. CDC at https://data.cdc.gov/"),
                      hr()
  )),
  tabPanel("Map", 
           fluidRow(
             column(c(2, 2, 8), 
                    Title = "Selection Options", 
                    h4("Map Directions")), 
             selectizeInput(
               inputId = "tab1_ucdicd",
               label = "ICD-10 Chapter",
               choices = unique(UCD$ICD.Chapter),
               selected = UCD$ICD.Chapter[[1]], 
               multiple = F),
             leafletOutput("Leaflet_Map", height = 850))),
  tabPanel("Trends", sidebarPanel(
                                  selectizeInput(
                                    inputId = "tab2_ucdcounty",
                                    label = "County",
                                    choices = unique(UCD$County),
                                    selected = "Rock County, WI", 
                                    multiple = F),
                                  selectizeInput(
                                    inputId = "tab2_ucdyear", 
                                    label = "Census Year:", 
                                    choices = unique(UCD$Year), 
                                    selected = 2019, 
                                    multiple = F), 
                                  selectizeInput(
                                    inputId = "tab2_ucdicd",
                                    label = "ICD-10 Chapter",
                                    choices = unique(UCD$ICD.Chapter),
                                    selected = UCD$ICD.Chapter[[1]], 
                                    multiple = F), 
                                  h4("Lorem ipsum dolor sit amet, ea nam aeterno regione, cu qui quaeque civibus gloriatur. Ut qui nobis causae omittam, mea malis nulla dolore ea. Mel graece essent no. Summo civibus dolores ex mei. At his indoctum torquatos reprimique, duo ea labores commune adipiscing. Fugit prodesset ei duo.")
  ),
  mainPanel("Main", 
            plotOutput(outputId = "UCD_COD_Column_All"))),
  tabPanel("Pattern", "three"),
  navbarMenu("Sources", 
             tabPanel("Local", "Rock"),
             tabPanel("State", "DHS"),
             tabPanel("Federal", "CDC")
  )
)

# Define server functions
server <- function(input, output){
  
  output$UCD_COD_Column_All <- renderPlot({
    UCD %>% 
    filter(Year == input$tab2_ucdyear, 
           County == input$tab2_ucdcounty) %>% 
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
  })
  
  output$Leaflet_Map <- renderLeaflet({
    # Requires UCD_Crude, WI_counties
    UCD_sub <- UCD_Crude %>% 
      filter(ICD.Chapter == input$tab1_ucdicd)
    
    UCD_sub_wi <- WI_counties %>% 
      left_join(UCD_sub, by = "GEOID")
    
    UCD_sub_sf <- sf::as_Spatial(UCD_sub_wi)
    
    pal_UCD_mx <- as.numeric(max(UCD_sub_wi$Crude_Mortality_Rate, na.rm = T))
    pal_UCD_mn <- as.numeric(min(UCD_sub_wi$Crude_Mortality_Rate, na.rm = T))
    pal_UCD <- colorNumeric(c("RdYlGn"), pal_UCD_mn:pal_UCD_mx, reverse = T)
    
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
    
  })
  
  
} # Close server

shinyApp(ui, server)
