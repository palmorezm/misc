
# Scrape Indeed for Jobs Data

# Base link for Indeed Search
# https://www.indeed.com/jobs?q=

# Packages
require(rvest)
require(readr)
require(tidyverse)
require(DT)

# --------------------- Sample Scrape --------------------- # 

#Specifying the url for desired website to be scraped
url <- 'https://www.indeed.com/jobs?q=data+science&l='

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
job_data_html <- html_nodes(webpage,'.clickcard , .jobtitle') 
# The characters '.text-primary' came from selector gadget

#Converting the ranking data to text
sample_job_data <- html_text(job_data_html)

#Let's have a look at the rankings
head(sample_job_data)

# Scrub-a-dub
sample <- read_delim(sample_job_data, "\t", quote = "\\\"", 
                   escape_double = FALSE, col_names = FALSE, 
                   na = "empty", trim_ws = TRUE)
# All clean? 
head(sample)

# -------------------------- End -------------------------- #





# -------------------- Search Filters --------------------- #

# Example:
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote&explvl=entry_level&fromage=7

# Job title 
# https://www.indeed.com/jobs?q= #*decision+scientist*&l #location is left blank to search everywhere

# Location
# https://www.indeed.com/jobs?q=decision+scientist&l= #*Remote* 

# Experience level 
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote& # explvl= #*entry_level*

# Date posted
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote&explvl=entry_level& #fromage= #*7* # Within last 7 days

# -------------------------- End -------------------------- #





# ------------------- Text Data Scrapes ------------------- #

# Title: Decision Scientist

listings <- data.frame(title=character(),
                       company=character(), 
                       location=character(), 
                       summary=character(), 
                       link=character(), 
                       description = character(),
                       stringsAsFactors=FALSE) 

for (i in seq(0, 10, 2)){
  url_ds <- paste0('https://www.indeed.com/jobs?q=Data+Science&l=',i)
  var <- read_html(url_ds)
  title <-  var %>% 
    html_nodes('#resultsCol .jobtitle') %>%
    html_text() %>%
    str_extract("(\\w+.+)+") 
  summary <- var %>%
    html_nodes('#resultsCol .summary') %>%
    html_text() %>%
    str_extract(".+") 
  link <- var %>%
    html_nodes('#resultsCol .jobtitle .turnstileLink, #resultsCol a.jobtitle') %>%
    html_attr('href') 
  link <- paste0("https://www.indeed.com",link)
  location <- var %>%
    html_nodes('#resultsCol .location') %>%
    html_text() %>%
    str_extract("(\\w+.)+,.[A-Z]{2}") 
  company <- var %>% 
    html_nodes('#resultsCol .company') %>%
    html_text() %>%
    str_extract("(\\w+).+")  
  
  listings <- rbind(listings, as.data.frame(cbind(title,
                                                  company,
                                                  location,
                                                  summary,
                                                  link)))
}


# Data Scientist 

# Data Analyst

# Operations Analyst 

# Operations Manager 

# [ Insert title ] ... Scrape

