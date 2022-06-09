
# APP for UCD 

# Packages
library(tigris)
library(sf)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(stringr)
library(plotly)
library(tidyr)
library(markdown)
library(shinythemes)
library(thematic)
library(shiny)
theme_set(theme_classic())


tmp_all <- data.frame(Date = rep(unique(UCD$Date)[[1]], length(unique(UCD$County.Code))))
tmp <- data.frame(Date = rep(unique(UCD$Date)[[2]], length(unique(UCD$County.Code))))
for (i in 2:length(unique(UCD$County.Code))){
  # Create list with appended dates and form tmp df
  tmp <- data.frame(Date = rep(unique(UCD$Date)[[i]], length(unique(UCD$County.Code))))
  tmp_all <- rbind(tmp_all, tmp)
}
tmp2_all <- data.frame(County.Code = rep(unique(UCD$County.Code), length(unique(UCD$Date))), 
                       County = rep(unique(UCD$County), length(unique(UCD$Date))))
df <- cbind(tmp_all, tmp2_all)
tmp3_all <- UCD %>% left_join(df, by = "Date") 
tmp4_all <- merge(UCD, df)
tmp_all <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter)[[1]], length(unique(UCD$County.Code))))
tmp <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter)[[2]], length(unique(UCD$County.Code))))
for (i in 2:length(unique(UCD$County.Code))){
  # Create list with appended dates and form tmp df
  tmp <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter)[[i]], length(unique(UCD$County.Code))))
  tmp_all <- rbind(tmp_all, tmp)
}
unique(tmp_all$ICD.Chapter) == unique(UCD$ICD.Chapter) # All True 
tmp4_all <- data.frame(ICD.Chapter = rep(unique(UCD$ICD.Chapter), length(df$County.Code)))
merge(tmp4_all, df)

# for (i in 1:length(unique(UCD$Date))){
#   for (j in 1:length(unique(UCD$County.Code))){
#     print(unique(UCD$County.Code[[j]]))
#     print(unique(UCD$Date[[i]]))
#   }
# }



# Data
load("Data/reg_mort.rdata")
load("Data/UCD_nonprovisional.rdata") # Mapping data?
load("C:/Users/Zachary.Palmore/GitHub/rock/Mortality/Data/D2_Mapping.rdata")
load("C:/Users/Zachary.Palmore/GitHub/rock/Mortality/Data/EthRaceGen2021.rdata")
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
# base <- "C:/Users/Zachary.Palmore/GitHub/rock/Mortality/"
# df <- read.csv(paste0(base,"Data/DeathData2021.csv"))
# df <- df[1:22]
# colnames(df) <- c("Gender", "DOB", "DOD", "AOD", "POD", 
#                   "FacilityName", "HomeAddress", "Area", 
#                   "Occupation", "Ethnicity", "Race", "Education", 
#                   "Autopsy", "Pregnancy", "TobaccoUse", 
#                   "AlcoholUse", "MOD", "COD", "Consequence1", 
#                   "Consequence2", "Consequence3", "Other")
# df <- as.data.frame(unclass(df), stringsAsFactors = TRUE)
# df$DOB <- as.Date.character(df$DOB, format = "%m/%d/%Y")
# df$DOD <- as.Date.character(df$DOD, format = "%m/%d/%Y")
# df$AOD <- stringr::str_extract_all(as.character(df$AOD), "\\d(.*?) ")
# df$AOD <- as.numeric(str_remove(df$AOD, " "))
# df$HomeAddress <- as.character(df$HomeAddress)
# df$Consequence1 <- as.character(df$Consequence1)
# df$Consequence2 <- as.character(df$Consequence2)
# df$Consequence3 <- as.character(df$Consequence3)
# df$Other <- as.character(df$Other)
# df$FullAddress <- paste0(df$HomeAddress,", ", df$Area, ", ", "WI")
# geo1 <- read.csv(paste0(base, "Data/geos1.csv"))
# geo2 <- read.csv(paste0(base, "Data/geos2.csv"))
# geo3 <- read.csv(paste0(base, "Data/geos3.csv"))
# geo4 <- read.csv(paste0(base, "Data/geos4.csv"))
# geos <- rbind(geo1, geo2, geo3, geo4)
# geos <- geos[1:3]
# colnames(geos) <- c("FullAddress", "lat", "lon")
# d2 <- geos %>% 
#   left_join(df, by = "FullAddress")
boxfunci <- function(d) {
  stats <- boxplot.stats(d)
  data.frame(ymin = stats$conf[1], ymax = stats$conf[2], y = stats$stats[3])
}



ui <- navbarPage(
  "Rock County Mortality Dashboard",   
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
            leafletOutput("Leaflet_Map", height = 800),
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
            plotlyOutput(outputId = "UCD_COD_Column_All"), 
            hr(),
            h5("Some words may go here"),
            hr(),
            plotlyOutput(outputId = "UCD_COD_Line_All")) 
  ),
  tabPanel("Distributions", 
           # radioButtons("method_geom_function", "Visual Type",
           #              c("Boxplot" = "boxplot",
           #                "Density Plot" = "density",
           #                "Histogram" = "hist",
           #                "Bar Chart" = "bar")),
           plotlyOutput(outputId = "UCD_MethodFunction_Plotly", height = 900)
           ),
tabPanel("Disparities", 
         h3("Disclaimer"),
         h4("The information presented here is not representative 
            of the county at large"),
         selectizeInput(
           inputId = "tab4_AODhist",
           label = "Select Group",
           choices = c("Gender", "Race", "Ethnicity"),
           selected = "Gender",
           multiple = F),
         hr(),
         plotlyOutput("hist_AOD", height = 600)),
  navbarMenu("Sources", 
        # Contains tables of the data with a download button? 
             tabPanel("Local", "Rock",
                      tableOutput("summary_table_iris")),
             tabPanel("State", "DHS", 
                      dataTableOutput("summary_datatable_iris")),
             tabPanel("Federal", "CDC & Census Bureau")
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
  
  output$UCD_COD_Column_All <- renderPlotly({
    UCD_COD_Column_All_Plotly <- UCD %>% 
      filter(Year == input$tab2_ucdyear, 
             County == input$tab2_ucdcounty) %>% 
      mutate(total_deaths = sum(Deaths)) %>% 
      group_by(ICD.Chapter, Deaths, Crude.Rate, total_deaths) %>% 
      summarise(Percent = (Deaths / total_deaths)*100 ) %>%
      arrange(desc(Percent)) %>%  
      ggplot(aes(reorder(ICD.Chapter, Deaths), Deaths)) + 
      geom_col(fill = "orange", col = "white") + 
      geom_text(aes(label = paste0(round(Percent, 1), "%"), y = Deaths), 
                hjust = 1.50, colour = "black") + 
      coord_flip() + 
      labs(x = "", y = "Deaths", 
           subtitle = paste("Leading Causes of Death in", input$tab2_ucdcounty))
    ggplotly(UCD_COD_Column_All_Plotly)
  })
  
  # Have more or less people died of [x] over time in Rock County? 
  output$UCD_COD_Line_All <- renderPlotly({
    UCD_COD_Line_All_Plotly <- UCD %>% 
      filter(County == c(input$tab2_ucdcounty), 
             Year >= 1999, Year <= 2020, 
             ICD.Chapter == c(input$tab2_ucdicd)) %>% 
      ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
      geom_line() + geom_point()
    ggplotly(UCD_COD_Line_All_Plotly)
  })
    
  
  output$UCD_MethodFunction_Plotly <- renderPlotly({
    
    # Compared to the rest of WI, does the crude mortality rate seem normal in Rock County? 
    
    # method_function <- switch(input$method_geom_function,
    #                           boxplot = geom_boxplot,
    #                           density = geom_density,
    #                           hist = geom_histogram, 
    #                           bar = geom_bar)
      tab3_UCD_distribution_box <- UCD %>%
      filter(Year == 2019) %>% 
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
      ggplotly(tab3_UCD_distribution_box)
  })
  
  output$hist_AOD <- renderPlotly({
    
   disparities_df <- switch(
      input$tab4_AODhist, 
      'Race' = df %>% 
        ggplot(aes(AOD)) + 
        geom_histogram(binwidth = 1, fill = "light grey", col = "grey", alpha = 0.5) + 
        geom_vline(xintercept = mean(df$AOD), lty = "dashed") +
        geom_vline(xintercept = Race2021$Med_AOD[1], col = "mediumblue") + 
        geom_vline(xintercept = Race2021$Med_AOD[2], col = "seagreen") + 
        geom_vline(xintercept = Race2021$Med_AOD[3], col = "red") + 
        geom_vline(xintercept = Race2021$Med_AOD[4], col = "orange") +
        labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
             x = "Age", 
             y = "Number of Individuals") + 
        annotate('text', x = 15, y = 40, 
                 label = paste0("~mu==", Race2021$Med_AOD[2], "~(Black)"), parse = TRUE, size=5, 
                 col = "orange") +
        annotate('text', x = 15, y = 50, 
                 label = paste0("~mu==", Race2021$Med_AOD[1], "~(White)"), parse = TRUE, size=5, 
                 col = "mediumblue") +
        annotate('text', x = 15, y = 30, 
                 label = paste0("~mu==", Race2021$Med_AOD[4], "~(Asian)"), parse = TRUE, size=5, 
                 col = "seagreen") +
        annotate('text', x = 15, y = 20, 
                 label = paste0("~mu==", Race2021$Med_AOD[3], "~(AmericanIndian)"), parse = TRUE, size=5, 
                 col = "red") +
        theme_minimal() + 
        theme(plot.subtitle = element_text(hjust = 0.5), 
              legend.text = element_text()), 
      'Ethnicity' = df %>% 
        ggplot(aes(AOD)) + 
        geom_histogram(binwidth = 1, fill = "light grey", col = "grey", alpha = 0.5) + 
        geom_vline(xintercept = mean(df$AOD), lty = "dashed") +
        geom_vline(xintercept = Eth2021$Med_AOD[1], col = "mediumblue") + 
        geom_vline(xintercept = Eth2021$Med_AOD[2], col = "orange") +
        labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
             x = "Age", 
             y = "Number of Individuals") + 
        annotate('text', x = 15, y = 40, 
                 label = paste0("~mu==", Eth2021$Med_AOD[2], "~(Hispanic)"), parse = TRUE, size=5, 
                 col = "orange") +
        annotate('text', x = 15, y = 50, 
                 label = paste0("~mu==", Eth2021$Med_AOD[1], "~(Not~Hispanic)"), parse = TRUE, size=5, 
                 col = "mediumblue") +
        theme_minimal() + 
        theme(plot.subtitle = element_text(hjust = 0.5), 
              legend.text = element_text()),
      'Gender' = df %>% 
        ggplot(aes(AOD)) + 
        geom_histogram(binwidth = 1, fill = "light grey", col = "grey", alpha = 0.5) + 
        geom_vline(xintercept = mean(df$AOD), lty = "dashed") +
        geom_vline(xintercept = Gender2021$Med_AOD[1], col = "orange") + 
        geom_vline(xintercept = Gender2021$Med_AOD[2], col = "mediumblue") +
        labs(subtitle = "Post-Extraction Distribution of Individuals\' Age at Death", 
             x = "Age", 
             y = "Number of Individuals") + 
        annotate('text', x = 15, y = 40, 
                 label = paste0("~mu==", Gender2021$Med_AOD[2], "~(Female)"), parse = TRUE, size=5, 
                 col = "mediumblue") +
        annotate('text', x = 15, y = 50, 
                 label = paste0("~mu==", Gender2021$Med_AOD[1], "~(Male)"), parse = TRUE, size=5, 
                 col = "orange") +
        theme_minimal() + 
        theme(plot.subtitle = element_text(hjust = 0.5), 
              legend.text = element_text()) 
      ) 
    
    ggplotly(disparities_df)
    
  })
  
  output$summary_table_iris <-  renderTable(
    iris
  )
  
  output$summary_datatable_iris <-  renderDataTable({
    iris
  })
  
  
} # Close server

shinyApp(ui, server)
