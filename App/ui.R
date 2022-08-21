
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)
library(dplyr)
library(stringr)
library(shinythemes)

fluidPage(
  theme = shinytheme("flatly"),
  # Application title
  titlePanel("Word Cloud Generator"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a Question:",
                  choices = Qs),
      selectInput("color", label = "Choose a color:", 
                  choices = c("Blues", "Greens", "Reds",
                              "Oranges", "YlGn", "RdYlBu", "Dark2", 
                              "Accent"), 
                  selected = "Accent"),
      actionButton("update", "Update Word Cloud"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 1),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 500,  value = 10),
      hr(),
      plotOutput("plot_bar", height = 400)
    ),
    
    # Show Word Cloud
    mainPanel(
      plotOutput("plot", height = 900),
    )
  )
)