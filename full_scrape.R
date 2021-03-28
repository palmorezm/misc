

# Full Scrape 

# Packages
require(rvest)
require(readr)
require(tidyverse)

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
  url <- paste0("https://www.indeed.com/jobs?q=Data+Science&l=")
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

