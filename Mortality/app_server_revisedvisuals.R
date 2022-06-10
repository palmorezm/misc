
# Column Chart

library(plotly)
load("Data/reg_mort.rdata")


###########################

# Chart 1 - Column Chart -Leading Causes

########################### 

# Original Plot
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

# Plotly Converted Plot
# UCD Column Chart for Leading Causes
UCD_Column_All_Data <- UCD %>% 
  filter(Year == 2019, 
         County == "Rock County, WI") %>% 
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

###########################

# Chart 2 - Line Graph -Leading Causes

########################### 

# Original Line Graph
output$UCD_COD_Line_All <- renderPlotly({
  UCD_COD_Line_All_Plotly <- UCD %>% 
    filter(County == c(input$tab2_ucdcounty), 
           Year >= 1999, Year <= 2020, 
           ICD.Chapter == c(input$tab2_ucdicd)) %>% 
    ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
    geom_line() + geom_point()
  ggplotly(UCD_COD_Line_All_Plotly)
})

# Plotly Converted Plot
# UCD Line Graph for Leading Causes
UCD_Line_All_Data<- UCD %>% 
  filter(County == "Rock County, WI", 
         Year >= 1999, Year <= 2020, 
         ICD.Chapter == c("Neoplasms")) 


  
  
