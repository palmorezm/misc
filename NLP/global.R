
library(tm)
library(wordcloud)
library(memoise)
library(ggplot2)
library(dplyr)
library(stringr)
library(shinythemes)

# The function
getWordFreq <- function(Q)({
  path <- paste0("https://docs.google.com/spreadsheets/d/e/",  
                 "2PACX-1vQYdcn-sC_a0GvWYmwIEHNUGwWpcnPYlA-", 
                 "RacDqmXKl0dxhFvigLm537HL8EkxOqwIKNC7_uD6_okD3/", 
                 "pub?gid=970557300&single=true&output=csv")
  df <- read.delim(file = path, header = T, sep = ",", quote = "\"", 
                   dec = ".", fill = TRUE, comment.char = ""
  )
  colnames(df) <- c("Timestamp", "Question1", "Question2", "Empty") 
  txt <- df %>% dplyr::select(Q)
  txt <- str_replace_all(txt,"[^[:graph:]]", " ") 
  docs <- Corpus(VectorSource(txt))
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  docs <- tm_map(docs, toSpace, "/")
  docs <- tm_map(docs, toSpace, "@")
  docs <- tm_map(docs, toSpace, "\\|")
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removeWords, c("and", "the", "don"))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  # Text stemming
  # docs <- tm_map(docs, stemDocument)
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  d$freq2 <- (d$freq^2)
  head(d, 10) %>% 
    ggplot(aes(freq)) + 
    geom_col(aes(y = reorder(word, freq)), fill = "light blue", alpha = 0.90) +
    labs(x = "Frequency", y = "Word", 
         title = "10 Most Frequent Words", 
         subtitle = "Extracted from Data Survey Responses", 
         caption = "Anonymized from source and stored only temporarily as cache and cookies") + coord_flip() + 
    theme_minimal() + theme(panel.grid.major.y = element_blank(),
                            panel.grid.major.x = element_blank(), 
                            panel.grid = element_blank(), 
                            plot.title = element_text(hjust = 0.5), 
                            plot.subtitle = element_text(hjust = 0.5), 
                            plot.caption = element_text(hjust = 0.5))
})


# The list of valid options
Qs <<- list("When you think about data, what comes to mind?" = "Question1",
            "What is your biggest challenge in using data to inform your work?" = "Question2")
# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function(Q) {
  # Careful not to let just any name slip in here; a
  # malicious user could manipulate this value.
  if (!(Q %in% Qs))
    stop("Bad Request - See Documentation")
  path <- paste0("https://docs.google.com/spreadsheets/d/e/", 
                 "2PACX-1vQYdcn-sC_a0GvWYmwIEHNUGwWpcnPYlA-", 
                 "RacDqmXKl0dxhFvigLm537HL8EkxOqwIKNC7_uD6_okD3/", 
                 "pub?gid=970557300&single=true&output=csv")
  df <- read.delim(file = path, header = T, sep = ",", quote = "\"", 
                   dec = ".", fill = TRUE, comment.char = "", 
                   encoding = "UTF-8")
  colnames(df) <- c("Timestamp", "Question1", "Question2", "Empty") 
  txt <- df %>% 
    dplyr::select(Q)
  txt <- str_replace_all(txt,"[^[:graph:]]", " ") 
  myCorpus = Corpus(VectorSource(txt))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("SMART"), "the", "and", "but", "don", ","))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})
