
setwd("C:/Users/Zachary.Palmore/GitHub/misc/Scrapes")
require(rvest)
require(purrr)
require(dplyr)

# Scrape Once
url_base <- "https://www.google.com/"
tack_search <- paste0("search?q=rock+county+public+health+department",
                      "&rlz=1C1GCEB_enUS992US992&oq=Rock+County+Public+Health+Dep",
                      "&aqs=chrome.0.0i355i512j46i175i199i512j69i57j0i22i30l2j69i64",
                      ".4456j0j7&sourceid=chrome&",
                      "ie=UTF-8#lrd=0x88061738a35ed54b:0xc4d59b7528223cb3,1,,,")
tack_map <- paste0("maps/place/Rock+County+Health+Department/", 
                   "@42.7236348,-89.0335531,17z/",
                   "data=!4m7!3m6!1s0x88061738a35ed54b:0xc4d59b7528223cb3!8m2!",
                   "3d42.7236309!4d-89.0313651!9m1!1b1")
url <- paste0(url_base, tack_search)
pg <- read_html(url)
pg %>% data.frame(
  Person_Name = html_text(html_nodes(pg, ".d4r55"))
)
html_text(html_nodes(pg, ".d4r55"))
# .rsqaWe , .hCCjke , .RfnDt span , .wiI7pd , .d4r55
html_text(html_nodes(pg, ".rsqaWe"))

html_nodes(pg, ".hCCjke")
html_text(html_nodes(pg, ".review-dialog"))

# Maybe use python?
# https://medium.com/@isguzarsezgin/scraping-google-reviews-with-selenium-python-23135ffcc331




