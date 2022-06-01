
# Text Mining Wordle Generator
# Packages 
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

path <- paste0("https://docs.google.com/spreadsheets/d/e/", 
               "2PACX-1vQYdcn-sC_a0GvWYmwIEHNUGwWpcnPYlA-", 
               "RacDqmXKl0dxhFvigLm537HL8EkxOqwIKNC7_uD6_okD3/", 
               "pub?gid=970557300&single=true&output=csv")
df <- read.delim(file = path, header = T, sep = ",", quote = "\"", 
               dec = ".", fill = TRUE, comment.char = ""
)


# COD 
docs <- Corpus(VectorSource(df[3]))
inspect(docs) # Display Strings
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
inspect(docs)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("and", "the")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)
inspect(docs) # Is it clean? 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d$freq2 <- d$freq^2

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

headd <- head(d, 10)
headd %>% 
  ggplot(aes(freq)) + 
  geom_col(aes(y = reorder(word, freq)), fill = "light blue", alpha = 0.75)

# https://shiny.rstudio.com/gallery/word-cloud.html

  


