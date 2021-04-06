
# April 1, 2021 
# Indeed Jobs Scrape
# Method: Pagination Function
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

jobs <- map_df(1:10, function(i) {
  
  # Progress indication
  cat(".")

  # Collect by pages
  pg <- read_html(sprintf(url, i))
  
  data.frame(
    # job title
    title=html_text(html_nodes(pg, '.jobtitle')), 
    # employer name
    employer=html_text(html_nodes(pg, '.company')), 
    # short description or summary
    short_description=html_text(html_nodes(pg, '.summary')),
    # job location
    location=html_text(html_nodes(pg, '.location')), 
    # posting date
    date=html_text(html_nodes(pg, '.date-a11y')),
    # hyperlink from indeed.com/'link'
    # includes job ids
    link=html_text(html_nodes(pg, 'a.jobtitle.turnstileLink'))
    )
})

glimpse(job)

# To export
# Contains location and date 
# write.csv(job, file="C:/data/jobs_april022021.csv")
# Data should be cleaned before data frame


