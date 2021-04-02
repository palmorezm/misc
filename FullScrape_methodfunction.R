
# April 1, 2021
# Indeed Jobs Scrape
# Search Title: Data Science

# Packages 
require(rvest)
require(purrr)
require(readr)


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

job <- map_df(1:10, function(i) {
  
  # Progress indication
  cat(".")

  # Collect by pages
  pg <- read_html(sprintf(url, i))
  
  data.frame(
    # job title
    title=html_text(html_nodes('.jobtitle')), 
    # employer name
    employer=html_text(html_nodes('.company')), 
    # short description or summary
    short_description=html_text(html_nodes('.summary')),
    # job location
    location=html_text(html_nodes('.location')), 
    # posting date
    date=html_text(html_nodes('.date-a11y'))#,
    # hyperlink from indeed.com/'link'
    #link=html_text(html_nodes('a.jobtitle.turnstileLink'))
    )
})
