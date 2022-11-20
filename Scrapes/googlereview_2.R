
# Scrape 2 from Link that Nick Sent
setwd("C:/Users/Zachary.Palmore/GitHub/misc/Scrapes")
require(rvest)
require(purrr)
require(dplyr)

url <- paste0("https://www.google.com/travel/hotels/",
               "entity/CgoIqJbZ6P-Ala9aEAE/",
               "reviews?q=holiday%20inn%20janesville%20wi",
               "&g2lb=2502548%2C2503771%2C2503781%2C2504096%",
               "2C4258168%2C4270442%2C4284970%2C4291517%2C4306835",
               "%2C4597339%2C4718358%2C4723331%2C4757164%2C4801235",
               "%2C4814050%2C4816977%2C4826689%2C4849799%2C4852066",
               "%2C4861688%2C4865301%2C4871746%2C26483161",
               "&hl=en-US&gl=us&ssta=1",
               "&ts=CAESABpJCisSJzIlMHg4",
               "ODA2MTA4ODViYjQ2MTJmOjB4NWE1ZTU0MDdmZD",
               "E2NGIyOBoAEhoSFAoHCOYPEAsYEhIHCOYPEAsYExg",
               "BMgIQACoJCgU6A1VTRBoA&qs=CAE4AkILCShLFv0HVF5",
               "aGAFCCwkoSxb9B1ReWhgB&ictx=1",
               "&sa=X&utm_campaign=sharing",
               "&utm_medium=link&utm_source=htls",
               "&ved=0CAAQ5JsGahcKEwiQuIu0qt36AhUAAAAAHQAAAAAQBA")
# url <- paste0(url_base, tack_search)
pg <- read_html(url)
pg %>% data.frame(
  Person_Name = html_text(html_nodes(pg, ".d4r55"))
)
.K7oBsc span , #reviews .CQYfx , .QB2Jof
html_text(html_nodes(pg, ".CQYfx")) # Works
html_text(html_nodes(pg, ".K7oBsc"))
