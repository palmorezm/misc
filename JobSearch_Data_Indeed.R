
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


# ------------------- Single Scrape Test ------------------ #

# Create fillable data frame
page1 <- data.frame(title=character(),
                       employer=character(), 
                       short_description=character(), 
                       location=character(), 
                       date=character(), 
                       link=character(),
                       id=character(),
                       stringsAsFactors=FALSE) 

# Title only - "Data+Science"
url <- paste0("https://www.indeed.com/jobs?q=Data+Science&l=")
# Web scrape function
var <- read_html(url)

# Collect job titles 
# Searches do not match exact title specified
title <- var %>% 
  html_nodes('.jobtitle') %>%
  html_text() %>%
  str_extract("(\\w+.+)+") 

# Collect company/employer information
# May be repeats 
employer <- var %>% 
  html_nodes('.company') %>%
  html_text() %>%
  str_extract("(\\w+).+")

# Collect summary from scroll bar used to browse jobs 
# Contains the first few sentences from the job description or
# Particular requirements, specifications, or pertinent information 
short_description <- var %>%
  html_nodes('.summary') %>%
  html_text() %>%
  str_extract(".+") 

# Collect listed job location where applicable
# Most remote listed as NA 
location <- var %>%
  html_nodes('.location') %>%
  html_text() %>%
  str_extract("(\\w+.)+,.[A-Z]{2}")

# Collect posting date of the job
# Listed as characters due to "just posted" and "active" categories
date <- var %>% 
  html_nodes('.date-a11y') %>% 
  html_text() 

# Collect links to full job descriptions and further details
# Given aggregation methods this could be used in conjunction with site name
link <- var %>% 
  html_nodes('a.jobtitle.turnstileLink') %>%
  str_extract("href=.* onmouse") %>%
  str_remove_all("href=") %>% 
  str_remove_all("onmouse") %>% 
  str_remove_all("\"")

# Collect 'id' as listed with links
# Thought to be unique character set to each posting 
id <- var %>%
  html_nodes('a.jobtitle.turnstileLink') %>%
  str_extract_all("id=*.* href") %>% 
  str_remove_all(" href") %>% 
  str_remove_all("id=") %>% 
  str_remove_all("\"")

# Bring it all together
page1 <- rbind(listings, as.data.frame(cbind(title,
                                                employer,
                                                short_description,
                                                location,
                                                date,
                                                link,
                                                id)))

# Display results for first page in console
# A single page should hold 15 listings
print(page1)

# ------------------- Text Data Scrapes ------------------- #

# Title: "Data Science"

listings <- data.frame(title=character(),
                       employer=character(), 
                       short_description=character(), 
                       location=character(), 
                       date=character(), 
                       link=character(),
                       id=character(),
                       stringsAsFactors=FALSE) 

for (i in seq(0,10,2)){

# Title only - "Data+Science"
  url <- paste0("https://www.indeed.com/jobs?q=Data+Science&l=", i)
# Web scrape function
  var <- read_html(url)
  
# Collect job titles 
# Searches do not match exact title specified
        title <- var %>% 
          html_nodes('.jobtitle') %>%
          html_text() %>%
          str_extract("(\\w+.+)+") 
        
# Collect company/employer information
# May be repeats 
        employer <- var %>% 
          html_nodes('.company') %>%
          html_text() %>%
          str_extract("(\\w+).+")

# Collect summary from scroll bar used to browse jobs 
# Contains the first few sentences from the job description or
# Particular requirements, specifications, or pertinent information 
        short_description <- var %>%
          html_nodes('.summary') %>%
          html_text() %>%
          str_extract(".+") 

# Collect listed job location where applicable
# Most remote listed as NA 
        location <- var %>%
          html_nodes('.location') %>%
          html_text() %>%
          str_extract("(\\w+.)+,.[A-Z]{2}")
        
# Collect posting date of the job
# Listed as characters due to "just posted" and "active" categories
        date <- var %>% 
          html_nodes('.date-a11y') %>% 
          html_text() 

# Collect links to full job descriptions and further details
# Given aggregation methods this could be used in conjunction with site name
        link <- var %>% 
          html_nodes('a.jobtitle.turnstileLink') %>%
          str_extract("href=.* onmouse") %>%
          str_remove_all("href=") %>% 
          str_remove_all("onmouse") %>% 
          str_remove_all("\"")

# Collect 'id' as listed with links
# Thought to be unique character set to each posting 
        id <- var %>%
          html_nodes('a.jobtitle.turnstileLink') %>%
          str_extract_all("id=*.* href") %>% 
          str_remove_all(" href") %>% 
          str_remove_all("id=") %>% 
          str_remove_all("\"")

      # Bring it all together
listings <- rbind(listings, as.data.frame(cbind(title,
                                                employer,
                                                short_description,
                                                location,
                                                date,
                                                link,
                                                id)))

}

# ------------------ End Text Data Scrapes ----------------- #


# Data Scientist 

# Data Analyst

# Operations Analyst 

# Operations Manager 

# [ Insert title ] ... Scrape

