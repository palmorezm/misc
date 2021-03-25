# Scrape Test 


# Packages
require(rvest)
require(readr)
require(tidyverse)

# Create empty data frame 
df <- data.frame(title = character(),
                 company = character())

# Scrape location
url_ds <- paste0('https://www.indeed.com/jobs?q=Data+Science&l=')
var <- read_html(url_ds)

# Extract data
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
