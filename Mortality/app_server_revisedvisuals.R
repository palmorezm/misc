
# Column Chart

library(plotly)
library(tidyverse)
library(stats)
load("Data/reg_mort.rdata")
load("Data/UCDcleaned.rdata")

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


UCD %>% 
  filter(County == c("Rock County, WI"), 
         Year >= 1999, Year <= 2020, 
         # ICD.Chapter == c("Diseases of the circulatory system"), 
         ICD.Chapter == c("Neoplasms")) %>% 
  ggplot(aes(Date, Deaths, col = ICD.Chapter)) + 
  geom_line() + geom_point()

# Plotly Converted Plot
# UCD Line Graph for Leading Causes
# Alternative method for grobbing, filter, display
UCDc_Line_All_Data <- UCDc[UCDc$ICD.Chapter %in% c("Neoplasms", 
                             "Diseases of the circulatory system", 
                             "Diseases of the respiratory system"),]
UCDc_Line_All_Data <- UCDc_Line_All_Data %>% 
  filter(County == "Rock County, WI")

length(unique(UCDc_Line_All_Data$ICD.Chapter))

# Assigns subset ucdc line data to global environment
for (i in 1:length(unique(UCDc_Line_All_Data$ICD.Chapter))){
  assign(paste0("UCDc_Line_All_Data", i), 
         subset(UCDc_Line_All_Data, 
                ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i]))
}

# Assigns fig(i) to global environment
for (i in 1:length(unique(UCDc_Line_All_Data$ICD.Chapter))){
  assign(paste0("fig", i), 
         plot_ly(UCDc_Line_All_Data, 
                 type = "scatter", mode = "lines+markers") %>% 
           add_trace(y = ~subset(UCDc_Line_All_Data, 
                          ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])$Deaths,
                     name = subset(UCDc_Line_All_Data,
                            ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])[1, "ICD.Chapter"], 
                     mode = "lines+markers", type = "scatter"))
}


# Plot traces 1:length(unique(UCDc$ICD.Chapters)) 
plotly_lineplot <- plot_ly(type = 'scatter', mode = 'lines+markers')
# add one trace for each "sale" column
for(i in 1:length(unique(UCDc_Line_All_Data$ICD.Chapter))){
  plotly_lineplot <- plotly_lineplot %>%
    add_trace(x = subset(UCDc_Line_All_Data, 
                         ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])$Date, 
              y = subset(UCDc_Line_All_Data, 
                         ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])$Deaths, 
              name = subset(UCDc_Line_All_Data,
                            ICD.Chapter == unique(UCDc_Line_All_Data$ICD.Chapter)[i])[1, "ICD.Chapter"],
              fill = 'none')
  # fillcolor = fill_colors[i])
}
plotly_lineplot 

###########################

# Chart 3 - Boxplot - Distributions

########################### 

# Helper Function for Boxplot
boxfunci <- function(d) {
  stats <- boxplot.stats(d)
  data.frame(ymin = stats$conf[1], ymax = stats$conf[2], y = stats$stats[3])
}


# Original Boxplot
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


# Plotly Boxplot 
UCD_Boxplot_All_Data <- UCD %>% 
  filter(Year == 2019) %>% 
  group_by(County, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000)
UCDc_Boxplot_All_Data <- UCDc %>% # Identical to UCD_Boxplot_All_Data
  filter(Year == 2019) %>% 
  group_by(County, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000) 

UCDc_Boxplot_All_Data <- UCDc %>% 
  group_by(County, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000)

UCD_Boxplot_All_Data2 <- UCD %>%
  group_by(County, ICD.Chapter) %>% 
  summarise(Crude_Mortality = sum(Deaths), 
            Population = sum(Population), 
            Crude_Mortality_Rate = Crude_Mortality / Population * 100000)
UCD_Boxplot_All_Data2_sub <- UCD_Boxplot_All_Data2[UCD_Boxplot_All_Data2$ICD.Chapter %in% c("Neoplasms", 
                                                   "Diseases of the circulatory system", 
                                                   "Diseases of the respiratory system"),]

plotly_boxplot <- plot_ly(type = 'box', quartilemethod = "linear")
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

# ggplotly(UCDc %>%
#   filter(Year == 2019) %>% 
#   group_by(County, ICD.Chapter) %>% 
#   summarise(Crude_Mortality = sum(Deaths), 
#             Population = sum(Population), 
#             Crude_Mortality_Rate = Crude_Mortality / Population * 100000) %>% 
#   ggplot(aes(reorder(ICD.Chapter, Crude_Mortality_Rate), Crude_Mortality_Rate)) + 
#   # geom_point(aes(fill = ICD.Chapter), alpha = 0.10) +
#   geom_boxplot(aes(fill = ICD.Chapter, alpha = 0.5, col = ICD.Chapter), 
#                notch = TRUE, notchwidth = 0.5) + 
#   stat_summary(fun.data = boxfunci, geom = "crossbar", 
#                colour = NA, fill = "light grey", width = 0.8, alpha = 0.45) + 
#   labs(y = "Crude Mortality Rate",
#        x = "ICD-10 Chapter Cause of Death"
#   ) +
#   coord_flip() + theme(legend.position = "none", 
#                        axis.title.y = element_blank()))
# 
# plot_ly(UCDc_Boxplot_All_Data, x = ~UCDc_Boxplot_All_Data$Crude_Mortality_Rate,
#         type = "box", quartilemethod="linear", 
#         name=unique(UCDc_Boxplot_All_Data$ICD.Chapter[1])) %>% 
#   add_trace(x = UCDc_Boxplot_All_Data$Crude_Mortality_Rate, 
#             quartilemethod="inclusive", 
#             name=unique(UCDc_Boxplot_All_Data$ICD.Chapter[2])) %>% 
#   add_trace(x = UCDc_Boxplot_All_Data$Crude_Mortality_Rate, 
#             quartilemethod="exclusive", 
#             name=unique(UCDc_Boxplot_All_Data$ICD.Chapter[3]))
# 
# plot_ly(x = UCDc_Boxplot_All_Data$Crude_Mortality_Rate, 
#         type = "box", 
#         q1=list(quantile(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)[[2]]), 
#         median=boxplot.stats(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)$stats[[3]],
#         q3=list(quantile(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)[[4]]), 
#         lowerfence=list(boxplot.stats(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)$stats[[1]]),
#         upperfence=list(boxplot.stats(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)$stats[[5]]), 
#         mean=mean(UCDc_Boxplot_All_Data$Crude_Mortality_Rate), 
#         sd=list(sd(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)),
#         notchspan=list(median(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)*.13), 
#         boxpoints = 'suspectedoutliers')
#         # sd=list(0.2, 0.4, 0.6), notchspan=list(0.2, 0.4, 0.6))
#         # jitter = 0.3, pointpos = -1.5, boxpoints = "all"),
#         
# mean(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)
# quantile(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)[[4]]
# boxplot.stats(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)
# sd(UCDc_Boxplot_All_Data$Crude_Mortality_Rate)
# 
# plotly_boxplot <- plot_ly(type = 'box')
# # add one trace for each "sale" column
# for(i in 1:length(unique(UCDc_Boxplot_All_Data$ICD.Chapter))){
#   plotly_boxplot <- plotly_boxplot %>%
#     add_trace(x = UCDc_Boxplot_All_Data$Crude_Mortality_Rate, 
#               quartilemethod="linear", 
#               name=unique(UCDc_Boxplot_All_Data$ICD.Chapter[i]))
# }
# plotly_boxplot


###########################

# Chart 4 - Histogram - Disparities

########################### 

# Original Histogram
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
        legend.text = element_text())




plotly_hist <- plot_ly(type = 'histogram')
# add one trace for each "sale" column
for(i in 1:length(unique(UCD_Boxplot_All_Data2_sub$ICD.Chapter))){
  plotly_hist <- plotly_hist %>%
    add_trace(x = subset(UCD_Boxplot_All_Data2_sub, 
                         ICD.Chapter == unique(UCD_Boxplot_All_Data2_sub$ICD.Chapter)[i])$Crude_Mortality_Rate, 
              name= subset(UCD_Boxplot_All_Data2_sub,
                           ICD.Chapter == unique(UCD_Boxplot_All_Data2_sub$ICD.Chapter)[i])[1, "ICD.Chapter"]
    )
}
plotly_hist

UCDc %>% 






