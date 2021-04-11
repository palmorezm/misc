# Created April 2, 2021 
# Indeed Jobs Scrape
# Method: Combination Function
# Search Title: Data Science


# Packages 
require(rvest)
require(readr)
require(tidyverse)
require(purrr)


# ------------------- Full Scrape ------------------- #


jobs <- data.frame(title=character(),
                   employer=character(), 
                   short_description=character(), 
                   location=character(), 
                   date=character(), 
                   link=character(),
                   id=character(),
                   stringsAsFactors=FALSE) 

# Title only - "Data+Science"
url <- "https://www.indeed.com/jobs?q=Data+Science&l="

jobs <- map_df(1:1000, function(i) {
  
  # Progress indication
  cat(".")
  
  # Collect by pages
  pg <- read_html(sprintf(url, i))
  
  # Collect job titles 
  # Searches do not match exact title specified
  title <-  pg %>%
    html_nodes('.jobtitle') %>%
    html_text() %>%
    str_extract("(\\w+.+)+")
  
  # Collect company/employer information
  # May be repeats 
  employer <- pg %>% 
    html_nodes('.company') %>%
    html_text() %>%
    str_extract("(\\w+).+")
  
  # Collect salary/income range
  # Has not been cleaned
  salary <- pg %>% 
    html_nodes('.salaryText') %>% 
    html_text() 
  
  # Collect summary from scroll bar used to browse jobs 
  # Contains the first few sentences from the job description or
  # Particular requirements, specifications, or pertinent information 
  short_description <- pg %>%
    html_nodes('.summary') %>%
    html_text() %>%
    str_extract(".+") 
  
  # Collect listed job location where applicable
  # Most remote listed as NA 
  location <- html_text(html_nodes(pg, '.location'))
  
  # Collect posting date of the job
  # Listed as characters due to "just posted" and "active" categories
  date <- pg %>% 
    html_nodes('.date-a11y') %>% 
    html_text() 
  
  # Collect links to full job descriptions and further details
  # Given aggregation methods this could be used in conjunction with site name
  link <- pg %>% 
    html_nodes('a.jobtitle.turnstileLink') %>%
    str_extract("href=.* onmouse") %>%
    str_remove_all("href=") %>% 
    str_remove_all("onmouse") %>% 
    str_remove_all("\"")
  
  # Collect 'id' as listed with links
  # Thought to be unique character set to each posting 
  id <- pg %>%
    html_nodes('a.jobtitle.turnstileLink') %>%
    str_extract_all("id=*.* href") %>% 
    str_remove_all(" href") %>% 
    str_remove_all("id=") %>% 
    str_remove_all("\"")
  
  # Add a page number for the scrape
  page <- (i + 1)
  
  Sys.sleep(1)
  
  as.data.frame(cbind(title, 
                      employer, 
                      short_description, 
                      location, 
                      date, 
                      link=paste0("indeed.com",link), 
                      id,
                      page=(page-1)
                      ))

})



# write.csv(jobs, file="C:/data/combination_scrape_test15_sixthfullrun.csv")

