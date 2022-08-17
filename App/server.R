
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)
library(dplyr)
library(stringr)
library(shinythemes)

function(input, output, session) {
  
    terms <- reactive({
      input$update
      isolate({
        withProgress({
          setProgress(message = "Processing...")
          getTermMatrix(input$selection)
        })
      })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    output$plot <- renderPlot({
      v <- terms()
      wordcloud_rep(names(v), v, scale=c(8,2),
                    min.freq = input$freq, max.words=input$max,
                    colors=brewer.pal(8, input$color))
    })
    output$plot_bar <- renderPlot({
      getWordFreq(input$selection)
    })
  }