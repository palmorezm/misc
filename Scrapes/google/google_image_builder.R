


require(rvest)
require(purrr)

# Use Beer and Wine Ratings Scrape to extract images
# of locations from locations data frame

url_base <- "https://www.google.com/imghp?hl=en&authuser=0&ogbl"

images <- locations_df(1:39, function(i) {
  
  # simple but effective progress indicator
  cat(".")
  
  pg <- read_html(sprintf(url_base, i))
  
  data.frame(wine=html_text(html_nodes(pg, ".review-listing .title")),
             excerpt=html_text(html_nodes(pg, "div.excerpt")),
             rating=gsub(" Points", "", html_text(html_nodes(pg, "span.rating"))),
             appellation=html_text(html_nodes(pg, "span.appellation")),
             price=gsub("\\$", "", html_text(html_nodes(pg, "span.price"))),
             stringsAsFactors=FALSE)
  
})

dplyr::glimpse(wines)

# -------------------- Example 2: Beer Reviews -------------------- # 

url <- "https://www.ratebeer.com/beer-ratings/0/2/"

beer <- map_df(1:10, function(i) {
  
  cat(".")
  
  pg <- read_html(sprintf(url, i))
  
  data.frame(beer_name=html_text(html_nodes(pg, "#onetrust-accept-btn-handler , td > a")), 
             beer_rating=html_text(html_nodes(pg, ".uas")),
             beer_location=html_text(html_nodes(pg, ".small")), 
             beer_brewery=html_text(html_nodes(pg, ".table div a")))
}) 

glimpse(beer)


# -------------------- Thoughts: Beer Reviews -------------------- #

# Successful replication of the wine scrape

# Results
# Although messy, 300 beer reviews were captured with 5 observations each
# None were missing or NA (which is interesting but each observation on the site seemed to follow this pattern)
# This was tested with:

sum(is.na(beer))