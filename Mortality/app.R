
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
library(tidyverse)
library(stats)
theme_set(theme_classic())

# Data
load("Data/reg_mort.rdata")
load("Data/UCD_nonprovisional.rdata") # Mapping data?
load("C:/Users/Zachary.Palmore/GitHub/rock/Mortality/Data/D2_Mapping.rdata")
load("C:/Users/Zachary.Palmore/GitHub/rock/Mortality/Data/EthRaceGen2021.rdata")
load("Data/UCDcleaned.rdata")
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

vline <- function(x = 0, color = "green", dash = "dot") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash=dash)
  )
}
hline <- function(y = 0, color = "black", dash = "dot") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash=dash)
  )
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
              width = 450,
              draggable = TRUE,
              wellPanel(
                HTML(markdownToHTML(fragment.only=TRUE, text=c(
                  "Welcome!

This is the Mortality Surveillance Dashboard of Rock County.  

For the best experience, navigate the tabs at the top of the screen. 

**Leading Causes**: Details leading causes of death <br>
**Distributions**: Statistics and distributions of causes <br>
**Disparities**: Racial, ethnic, and gender mortality review  <br>
**Sources**: Methodology and data used in dashboard  <br>

On this tab:

Select an ICD-10 Chapter from the list to see how Rock County compares to the rest of Wisconsin."
                ))),
             selectizeInput(
               inputId = "tab1_ucdicd",
               label = "ICD-10 Chapter",
               choices = unique(UCD$ICD.Chapter),
               selected = UCD$ICD.Chapter[[1]], 
               multiple = F), 
HTML(markdownToHTML(fragment.only=TRUE, text=c(
  "Numbers provided are shown as crude rates which are relative to the population size of each county."
))), 
hr(), 
HTML(markdownToHTML(fragment.only=TRUE, text=c(
  "*Disclaimer*
  
  *Due to the relative nature of crude rates, in some cases these numbers may not be representative or reliable*
  "
)))
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
                  # h3("Select Statistics for Table"),
                  # selectizeInput(
                  #   inputId = "tab3_icd_choice_table",
                  #   label = "ICD-10 Chapter",
                  #   choices = unique(UCDc$ICD.Chapter),
                  #   selected = unique(UCDc$ICD.Chapter)[[1]],
                  #   multiple = F),
                  # selectizeInput(
                  #   inputId = "tab3_county_choice_table",
                  #   label = "ICD-10 Chapter",
                  #   choices = unique(UCD$County),
                  #   selected = "Rock County, WI",
                  #   multiple = F),
           
           dataTableOutput("summary_datatable_boxplot"),
           sidebarPanel(
             selectizeInput(
               inputId = "tab3_plotchoice",
               label = "Plot Type",
               choices = c("Box", "Column", "Delta", "Points", "Violin"),
               selected = "Box",
               multiple = F),
             checkboxGroupInput(
             inputId = "tab3_ucdicd",
             label = "ICD-10 Chapter",
             choices = unique(UCD$ICD.Chapter)[1:17],
             selected = c("Diseases of the circulatory system",
                          "Neoplasms", 
                          "Diseases of the respiratory system", 
                          "Diseases of the nervous system" 
                          ))),
           # radioButtons("method_geom_function", "Visual Type",
           #              c("Boxplot" = "boxplot",
           #                "Density Plot" = "density",
           #                "Histogram" = "hist",
           #                "Bar Chart" = "bar")),
           mainPanel(
             plotlyOutput(outputId = "UCD_MethodFunction_Plotly", height = 650)
             # tableOutput("summary_table_iris")
             )
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
             tabPanel("Local", "Rock"),
                      # tableOutput("summary_table_iris")),
             tabPanel("State", "DHS"),
                      # dataTableOutput("summary_datatable_iris")),
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
    UCD_Column_All_Data <- UCD %>% 
      filter(Year == input$tab2_ucdyear, 
             County == input$tab2_ucdcounty) %>% 
      mutate(total_deaths = sum(Deaths)) %>% 
      group_by(ICD.Chapter, Deaths, Crude.Rate, total_deaths) %>% 
      summarise(Percent = (Deaths / total_deaths)*100 ) %>%
      arrange(desc(Percent)) 
    
    UCD_Column_All_Data$ICD.Chapter <- 
      factor(UCD_Column_All_Data$ICD.Chapter, 
             levels = unique(UCD_Column_All_Data$ICD.Chapter)[order(UCD_Column_All_Data$Deaths, 
                                                                    decreasing = F)])
    ucd_column_all_fig <- plot_ly(UCD_Column_All_Data, 
                                  x = ~Deaths, 
                                  y = ~ICD.Chapter, 
                                  color = ~ICD.Chapter,
                                  opacity = 0.50,
                                  type = "bar", 
                                  orientation = 'h')
    
    ucd_column_all_fig  %>% 
      layout(
        title = "Leading Causes of Death by County and Year", 
        # Hover for More Information about Each Cause
        plot_bgcolor='#ffff',
        yaxis = list(# title = '', 
          showticklabels = F, 
          visible = T,
          zerolinecolor = '#ffff', 
          zerolinewidth = 1, 
          gridcolor = 'ffff'), 
        showlegend = F) %>% 
      add_annotations(
        text = " ",
        x = 0.5,
        y = 1.05,
        yref = "paper",
        xref = "paper",
        xanchor = "middle",
        yanchor = "top",
        showarrow = FALSE,
        font = list(size = 15)
      )
  })
  
  # Have more or less people died of [x] over time in Rock County? 
  output$UCD_COD_Line_All <- renderPlotly({
    
    UCDc_Line_All_Data <- UCDc[UCDc$ICD.Chapter %in% c(input$tab2_ucdicd),]
    UCDc_Line_All_Data <- UCDc_Line_All_Data %>% 
      filter(County == input$tab2_ucdcounty)
    
    
    plotly_lineplot <- plot_ly(type = 'scatter', mode = 'lines+markers')
    for(i in 1:length(unique(UCDc_Line_All_Data$ICD.Chapter))){
      plotly_lineplot <- plotly_lineplot %>%
        add_trace(x = subset(UCDc_Line_All_Data, 
                             ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])$Date, 
                  y = subset(UCDc_Line_All_Data, 
                             ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])$Deaths, 
                  name = subset(UCDc_Line_All_Data,
                                ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])[1, "ICD.Chapter"],
                  fill = 'none')
    }
    plotly_lineplot 
    
    
  })
    
  # plot_function_tab3 <- reactive({
  #   switch(input$tab3_plotchoice,
  #          Box = "box",
  #          Column = "bar",
  #          Points = "scattergl",
  #          Violin = "violin",
  #          Delta = "waterfall")
  # })
  
  
  output$UCD_MethodFunction_Plotly <- renderPlotly({
    
    # Compared to the rest of WI, does the crude mortality rate seem normal in Rock County? 
    
    # method_function <- switch(input$method_geom_function,
    #                           boxplot = geom_boxplot,
    #                           density = geom_density,
    #                           hist = geom_histogram, 
    #                           bar = geom_bar)
      # tab3_UCD_distribution_box <- UCD %>%
      # filter(Year == 2019) %>% 
      # group_by(County, ICD.Chapter) %>% 
      # summarise(Crude_Mortality = sum(Deaths), 
      #           Population = sum(Population), 
      #           Crude_Mortality_Rate = Crude_Mortality / Population * 100000) %>% 
      # ggplot(aes(reorder(ICD.Chapter, Crude_Mortality_Rate), Crude_Mortality_Rate)) + 
      # # geom_point(aes(fill = ICD.Chapter), alpha = 0.10) +
      # geom_boxplot(aes(fill = ICD.Chapter, alpha = 0.5, col = ICD.Chapter), 
      #              notch = TRUE, notchwidth = 0.5) + 
      # stat_summary(fun.data = boxfunci, geom = "crossbar", 
      #              colour = NA, fill = "light grey", width = 0.8, alpha = 0.45) + 
      # labs(y = "Crude Mortality Rate",
      #      x = "ICD-10 Chapter Cause of Death"
      # ) +
      # coord_flip() + theme(legend.position = "none", 
      #                      axis.title.y = element_blank())
      # ggplotly(tab3_UCD_distribution_box)
      UCD_Boxplot_All_Data2 <- UCD %>%
        group_by(County, ICD.Chapter) %>% 
        summarise(Crude_Mortality = sum(Deaths), 
                  Population = sum(Population), 
                  Crude_Mortality_Rate = Crude_Mortality / Population * 100000)
      UCD_Boxplot_All_Data2_sub <- UCD_Boxplot_All_Data2[UCD_Boxplot_All_Data2$ICD.Chapter %in% c(input$tab3_ucdicd),]
      UCD_Boxplot_All_Data2_sub <- UCD_Boxplot_All_Data2_sub %>% 
        arrange(Crude_Mortality_Rate)
      
      plot_function_tab3 <- switch(input$tab3_plotchoice,
             Box = "box",
             Column = "bar",
             Points = "scattergl",
             Violin = "violin",
             Delta = "waterfall")
      
      plotly_boxplot <- plot_ly(type = plot_function_tab3, boxpoints="suspectedoutliers")
      # add one trace for each "sale" column
      for(i in 1:length(unique(UCD_Boxplot_All_Data2_sub$ICD.Chapter))){
        plotly_boxplot <- plotly_boxplot %>%
          add_trace(x = subset(UCD_Boxplot_All_Data2_sub, 
                               ICD.Chapter == unique(UCD_Boxplot_All_Data2_sub$ICD.Chapter)[i])$Crude_Mortality_Rate, 
                    name= subset(UCD_Boxplot_All_Data2_sub,
                                 ICD.Chapter == unique(UCD_Boxplot_All_Data2_sub$ICD.Chapter)[i])[1, "ICD.Chapter"]
          )
      }
      plotly_boxplot
  })
  
  output$summary_datatable_boxplot <-  renderDataTable({
    # UCD_Boxplot_All_Data2_sub_table <- UCD_Boxplot_All_Data2_sub %>% 
    #   filter(ICD.Chapter == c(input$tab3_icd_choice_table)) 
    # # County == c("Rock County, WI"))
    summary(UCD[c(4, 6, 7:11, 14)])
  })
  
  output$hist_AOD <- renderPlotly({
    
    plotly_hist <- plot_ly(type = 'histogram')
    plotly_hist <- plotly_hist %>% 
      add_trace(x = df$AOD, bingroup=1, name = "AOD, Count", 
                marker = list(color = "lightgrey")) # "rgba(150, 150, 150, 0.7)"
    
    disparities_df<- switch(
      input$tab4_AODhist, 
      'Race' = plotly_hist %>% 
        layout(
          shapes=list(vline(mean(df$AOD), color = "black", dash = "dot"),
                      vline(Race2021$Med_AOD[[1]], color = "mediumblue", dash = "solid"),
                      vline(Race2021$Med_AOD[[2]], color = "seagreen", dash = "solid"),
                      vline(Race2021$Med_AOD[[3]], color = "red", dash = "solid"),
                      vline(Race2021$Med_AOD[[4]], color = "orange", dash = "solid")
                      # Too few to reasonably display 
                      # vline(Race2021$Med_AOD[[5]], color = "orange", dash = "solid"),
                      # vline(Race2021$Med_AOD[[6]], color = "orange", dash = "solid")
          ),
          barmode="relative",
          bargap=0.1, 
          title = "Disparities in Age of Death", 
          # Age of Death for Racial, Ethnic, and Gender Groups Differ Across All Causes
          # Hover for More Information about Each Cause
          plot_bgcolor='#ffff',
          yaxis = list(title = 'Number of Individuals', 
                       showticklabels = T, 
                       visible = T,
                       zerolinecolor = '#ffff', 
                       zerolinewidth = 1, 
                       gridcolor = 'ffff'), 
          showlegend = F, 
          xaxis = list(title = 'Age', 
                       showticklabels = T, 
                       visible = T,
                       zerolinecolor = '#ffff', 
                       zerolinewidth = 1, 
                       gridcolor = 'ffff'), 
          showlegend = F) %>% 
        add_text(showlegend = T, 
                 x = 30, y = 95,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Race2021$Med_AOD[[1]], "(White)")
                 ),
                 textposition = "left center") %>% 
        add_text(showlegend = T, 
                 x = 30, y = 90,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Race2021$Med_AOD[[2]], "(Black)")
                 ),
                 textposition = "left center") %>% 
        add_text(showlegend = T, 
                 x = 30, y = 85,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Race2021$Med_AOD[[3]], "(American Indian)")
                 ),
                 textposition = "left center") %>% 
        add_text(showlegend = T, 
                 x = 30, y = 80,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Race2021$Med_AOD[[4]], "(Asian)")
                 ),
                 textposition = "left center") %>%
        # add_text(showlegend = T, 
        #          x = 25, y = 75,
        #          size = I(14),
        #          text = c(
        #            paste(
        #              "\U003BC", "=", Race2021$Med_AOD[[5]], "(Native Hawaiian & PI)")
        #          ),
        #          textposition = "left center") %>% 
        # add_text(showlegend = T, 
        #          x = 25, y = 70,
        #          size = I(14),
      #          text = c(
      #            paste(
      #              "\U003BC", "=", Race2021$Med_AOD[[6]], "(Mixed)")
      #          ),
      #          textposition = "left center")
      add_annotations(x = Race2021$Med_AOD[[1]],
                      y = 40,
                      text = "White",
                      xref = "x",
                      yref = "y",
                      showarrow = TRUE,
                      arrowhead = 4,
                      arrowsize = .5,
                      ax = 30,
                      ay = 50) %>%
        add_annotations(x = Race2021$Med_AOD[[2]],
                        y = 80,
                        text = "Black",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = -25,
                        ay = 40) %>% 
        add_annotations(x = Race2021$Med_AOD[[3]],
                        y = 40,
                        text = "American Indian",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = -25,
                        ay = 40) %>% 
        add_annotations(x = Race2021$Med_AOD[[4]],
                        y = 60,
                        text = "Asian",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = -25,
                        ay = 40),
        
      'Ethnicity' = plotly_hist %>% 
        layout(
          shapes=list(vline(mean(df$AOD), color = "black", dash = "dot"),
                      vline(Eth2021$Med_AOD[[1]], color = "orange", dash = "solid"),
                      vline(Eth2021$Med_AOD[[2]], color = "blue", dash = "solid")),
          barmode="relative",
          bargap=0.1, 
          title = "Disparities in Age of Death", 
          # Age of Death for Racial, Ethnic, and Gender Groups Differ Across All Causes
          # Hover for More Information about Each Cause
          plot_bgcolor='#ffff',
          yaxis = list(title = 'Number of Individuals', 
                       showticklabels = T, 
                       visible = T,
                       zerolinecolor = '#ffff', 
                       zerolinewidth = 1, 
                       gridcolor = 'ffff'), 
          showlegend = F, 
          xaxis = list(title = 'Age', 
                       showticklabels = T, 
                       visible = T,
                       zerolinecolor = '#ffff', 
                       zerolinewidth = 1, 
                       gridcolor = 'ffff'), 
          showlegend = F) %>% 
        add_text(showlegend = T, 
                 x = 25, y = 95,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Eth2021$Med_AOD[[1]], "(Non-hispanic)")
                 ),
                 textposition = "left center") %>% 
        add_text(showlegend = T, 
                 x = 25, y = 90,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Eth2021$Med_AOD[[2]], "(Hispanic)")
                 ),
                 textposition = "left center") %>% 
        add_annotations(x = Eth2021$Med_AOD[[1]],
                        y = 30,
                        text = "Non-hispanic",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = 30,
                        ay = 50) %>%
        add_annotations(x = Eth2021$Med_AOD[[2]],
                        y = 40,
                        text = "Hispanic",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = -30,
                        ay = 40),
        
      'Gender' = 
        plotly_hist %>% 
        layout(
          shapes=list(vline(mean(df$AOD), color = "black", dash = "dot"),
                      vline(Gender2021$Med_AOD[[1]], color = "orange", dash = "solid"),
                      vline(Gender2021$Med_AOD[[2]], color = "blue", dash = "solid")),
          barmode="relative",
          bargap=0.1, 
          title = "Disparities in Age of Death", 
          # Age of Death for Racial, Ethnic, and Gender Groups Differ Across All Causes
          # Hover for More Information about Each Cause
          plot_bgcolor='#ffff',
          yaxis = list(title = 'Number of Individuals', 
                       showticklabels = T, 
                       visible = T,
                       zerolinecolor = '#ffff', 
                       zerolinewidth = 1, 
                       gridcolor = 'ffff'), 
          showlegend = F, 
          xaxis = list(title = 'Age', 
                       showticklabels = T, 
                       visible = T,
                       zerolinecolor = '#ffff', 
                       zerolinewidth = 1, 
                       gridcolor = 'ffff'), 
          showlegend = F) %>% 
        add_text(showlegend = T, 
                 x = 25, y = 95,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Gender2021$Med_AOD[[1]], "(Male)")
                 ),
                 textposition = "left center") %>% 
        add_text(showlegend = T, 
                 x = 25, y = 90,
                 size = I(14),
                 text = c(
                   paste(
                     "\U003BC", "=", Gender2021$Med_AOD[[2]], "(Female)")
                 ),
                 textposition = "left center") %>% 
        add_annotations(x = Gender2021$Med_AOD[[2]],
                        y = 40,
                        text = "Female",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = 30,
                        ay = 40) %>%
        add_annotations(x = Gender2021$Med_AOD[[1]],
                        y = 40,
                        text = "Male",
                        xref = "x",
                        yref = "y",
                        showarrow = TRUE,
                        arrowhead = 4,
                        arrowsize = .5,
                        ax = -25,
                        ay = 40) 
      ) # end switch
    
  })
  
  output$summary_table_iris <-  renderTable(
    iris
  )
  
  
} # Close server

shinyApp(ui, server)
