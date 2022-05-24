
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
library(markdown)
library(shinythemes)
library(thematic)
library(shiny)
theme_set(theme_classic())

# Data
load("Data/reg_mort.rdata")
load("Data/UCD_nonprovisional.rdata") # Mapping data?

ui <- navbarPage(
  "4D Rock County",   
  theme = shinytheme("cosmo"),
 # header = "Header Section for all Tabs in Navbar",
   tabPanel("Map",
            # fluidPage(
            #   fluidRow(
            #     column(c(12)),
            fluidRow(
              column(c(2, 2, 8), 
                     Title = "Selection Options"),
            tags$style(type = "text/css", "html, body {width:100%; height:100%}"),
            leafletOutput("Leaflet_Map", height = 850),
            absolutePanel(
              top = 125,
              left = 120,
              width = 300,
              draggable = TRUE,
              wellPanel(
                HTML(markdownToHTML(fragment.only=TRUE, text=c(
                  "This is an absolutePanel that uses `bottom` and `right` attributes.

It also has `draggable = TRUE`, so you can drag it to move it around the page.

The slight transparency is due to `style = 'opacity: 0.92'`.

You can put anything in absolutePanel, including inputs and outputs:"
                ))),
             selectizeInput(
               inputId = "tab1_ucdicd",
               label = "ICD-10 Chapter",
               choices = unique(UCD$ICD.Chapter),
               selected = UCD$ICD.Chapter[[1]], 
               multiple = F) 
              ), 
          style = "opacity: 0.85")
             # ))
            )
), # End tab panel 1
  tabPanel("Leading Causes", 
      sidebarPanel(
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
    checkboxGroupInput(
      inputId = "tab2_ucdicd",
      label = "ICD-10 Chapter",
      choices = unique(UCD$ICD.Chapter)[1:17],
      selected = UCD$ICD.Chapter[[1]]), 
    h4("Lorem ipsum dolor sit amet, ea nam aeterno regione, cu qui quaeque civibus gloriatur. Ut qui nobis causae omittam, mea malis nulla dolore ea. Mel graece essent no. Summo civibus dolores ex mei. At his indoctum torquatos reprimique, duo ea labores commune adipiscing. Fugit prodesset ei duo.")
  ),
  mainPanel("Main", 
            plotOutput(outputId = "UCD_COD_Column_All"), 
            plotOutput(outputId = "UCD_COD_Line_All")) 
  ),
  tabPanel("Distributions", 
           radioButtons("method_geom_function", "Visual Type",
                        c("Boxplot" = "boxplot",
                          "Density Plot" = "density",
                          "Histogram" = "hist",
                          "Bar Chart" = "bar")),
           plotlyOutput(outputId = "UCD_MethodFunction_Plotly", height = 900)
           ),
  navbarMenu("Sources", 
        # Contains tables of the data with a download button? 
             tabPanel("Local", "Rock"),
             tabPanel("State", "DHS"),
             tabPanel("Federal", "CDC")
    )
  )



# Define server functions
server <- function(input, output){
  thematic::thematic_shiny()
  
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
           subtitle = paste("Leading Causes of Death in", input$tab2_ucdcounty))
  })
  
  # Have more or less people died of [x] over time in Rock County? 
  output$UCD_COD_Line_All <- renderPlot({
    UCD %>% 
      filter(County == c(input$tab2_ucdcounty), 
             Year >= 1999, Year <= 2020, 
             ICD.Chapter == c(input$tab2_ucdicd)) %>% 
      ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
      geom_line() + geom_point()
  })
    
  
  output$UCD_MethodFunction_Plotly <- renderPlotly({
    
    # Compared to the rest of WI, does the crude mortality rate seem normal in Rock County? 
    boxfunci <- function(d) {
      stats <- boxplot.stats(d)
      data.frame(ymin = stats$conf[1], ymax = stats$conf[2], y = stats$stats[3])
    }
    
    method_function <- switch(input$method_geom_function,
                              boxplot = geom_boxplot,
                              density = geom_density,
                              hist = geom_histogram, 
                              bar = geom_bar)
    ggplotly(
      UCD %>%
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
                           axis.title.y = element_blank()))
  })
  
  
} # Close server

shinyApp(ui, server)
