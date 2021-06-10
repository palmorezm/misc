
# ---------------- Objective ---------------- #

# What selectors are available to extract html information from? 
# Where is the information stored in the html variable? 
# How does that information get read into the environment?
# Determine what needs immediate extraction and steps required
# Shorten munging steps to most useful in analysis

# --------------- End Objective ------------- #



# ------------------ Setup ------------------ # 

# Scrape Test 

# Packages
require(rvest)
require(readr)
require(tidyverse)

# Create empty data frame 
df <- data.frame(title = character(),
                 company = character())

# Read html
url_ds <- paste0('https://www.indeed.com/jobs?q=Data+Science&l=')
var <- read_html(url_ds)

# ------------------ End setup ------------------ # 



# ------------- Evaluate selectors -------------- # 

var %>% 
  html_nodes('.jobtitle') %>%
  html_text() %>%
  str_extract("(\\w+.+)+") 
var %>% 
  html_nodes('.company') %>%
  html_text() %>%
  str_extract("(\\w+).+")
var %>%
  html_nodes('.summary') %>%
  html_text() %>%
  str_extract(".+") 
var %>%
  html_nodes('.location') %>%
  html_text() %>%
  str_extract("(\\w+.)+,.[A-Z]{2}")
var %>% 
  html_nodes('.date-a11y') %>% 
  html_text() 
var %>% 
  html_nodes('.turnstileLink') %>% 
  html_text()
var %>% 
  html_nodes('.pagination-list :nth-child(1)') %>%
  html_text()
var %>% 
  html_nodes('.pagination-list') %>%
  html_text()
var %>% 
  html_nodes('.vjs-highlight') %>% 
  html_text()
var %>% 
  html_nodes('.clickcard')
var %>% 
  html_nodes('li') %>% 
  html_text()
var %>% 
  html_nodes('//*[@id="sja0"]') 
var %>% 
  html_nodes('document.querySelector("#sja0")') %>% 
  html_text()

# ------------- End evaluate selectors -------------- # 




