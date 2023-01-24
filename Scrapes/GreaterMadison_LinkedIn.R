
# Components of the URL
# Location: "Greater Madison Area"
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
url_base_madison <- "https://www.linkedin.com/jobs/search?keywords=Data&location=Greater%20Madison%20Area&geoId=90000472&trk=public_jobs_jobs-search-bar_search-submit&position=1"
url_page_number <- paste0("&pageNum=", 0)
url_read_html <- paste0(url_base_madison, url_page_number)
pg <- read_html(url_read_html)
df_all <- data.frame(
  Page = as.numeric(999),
  Company_Name=gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__subtitle"))),
  Position_Name=gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__title"))), 
  Location_Name=gsub("\\W", "", html_text(html_nodes(pg, ".job-search-card__location"))),
  stringsAsFactors=FALSE)

# Scrape Page 0 through X (You Specify)
for (i in 0:9){
  start_time <- Sys.time() 
  cat(".")
  Sys.sleep(runif(1, min = 35, max = 99))
  url_page_number <- paste0("&pageNum=", i)
  url_read_html <- paste0(url_base_madison, url_page_number)
  pg <- read_html(url_read_html)
  results <- data.frame(
    Page = as.numeric(i),
    Company_Name = gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__subtitle"))),
    Position_Name = gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__title"))), 
    Location_Name = gsub("\\W", "", html_text(html_nodes(pg, ".job-search-card__location"))),
    stringsAsFactors = FALSE)
  df_all <- rbind(df_all, results)
  end_time <- Sys.time()
  time_needed <- end_time - start_time
  print(paste("Step", i, "was finished after", time_needed, "seconds."))
}

writeLines(unique(df_all$Company_Name), "companies_madison2.csv", sep = ",")

write.csv(df_all, "Madison2.csv")

