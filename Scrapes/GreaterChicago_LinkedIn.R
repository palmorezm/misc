
# Components of the URL
# Location: "Greater Chicago Area"
# Keywords: "Data"

setwd("C:/Users/Zachary Palmore/GitHub/misc/Scrapes")

# Layout of URL on LinkedIn
# https://www.linkedin.com/jobs/search?
#   keywords=Data%20Science&location=Wisconsin%2C%20United%20
#   States&locationId=&geoId=104454774&
#     f_TPR=&
#     f_SB2=3&
#     f_JT=F&
#     f_WT=2%2C3&
#     f_E=2&
#     position=1&
#     pageNum=0


require(rvest)
require(purrr)
require(dplyr)

# LinkedIn Pattern Goes Like This
#   1-25 Jobs shown for page 0
#   1-25 Jobs shown for page 1
#   1-25 Jobs shown for page 2

# Scrape Once:
url_base_chicago <- "https://www.linkedin.com/jobs/search?keywords=Data&location=Greater%20Chicago%20Area&geoId=90000014&trk=public_jobs_jobs-search-bar_search-submit&position=1"
url_page_number <- paste0("&pageNum=", 0)
url_read_html <- paste0(url_base_chicago, url_page_number)
pg <- read_html(url_read_html)
df_all <- data.frame(
  Page = as.numeric(999),
  Position_Name=gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__subtitle"))),
  Company_Name=gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__title"))), 
  Location_Name=gsub("\\W", "", html_text(html_nodes(pg, ".job-search-card__location"))),
  stringsAsFactors=FALSE)

# Scrape Page 0 through X (You Specify)
for (i in 0:9){
  start_time <- Sys.time() 
  cat(".")
  Sys.sleep(runif(1, min = 65, max = 129))
  url_page_number <- paste0("&pageNum=", i)
  url_read_html <- paste0(url_base_chicago, url_page_number)
  pg <- read_html(url_read_html)
  results <- data.frame(
    Page = as.numeric(i),
    Position_Name = gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__subtitle"))),
    Company_Name = gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__title"))), 
    Location_Name = gsub("\\W", "", html_text(html_nodes(pg, ".job-search-card__location"))),
    stringsAsFactors = FALSE)
  df_all <- rbind(df_all, results)
  end_time <- Sys.time()
  time_needed <- end_time - start_time
  print(paste("Step", i, "was finished after", time_needed, "seconds."))
}

# 8/22/2022
# Minimum 5, Max 19 - resulted in stop on this scrape at step 1
# Changed min to 35 and max to 79. 

writeLines(unique(df_all$Company_Name), "companies_chicago5.csv", sep = ",")

write.csv(df_all, "Chicago5.csv")
