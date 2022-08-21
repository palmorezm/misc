


https://www.linkedin.com/jobs/search?
  keywords=Data%20Science&location=Wisconsin%2C%20United%20
  States&locationId=&geoId=104454774&
    f_TPR=&
    f_SB2=3&
    f_JT=F&
    f_WT=2%2C3&
    f_E=2&
    position=1&
    pageNum=0
  

require(rvest)
require(purrr)
  
url_base <- "https://www.linkedin.com/jobs/search?keywords=DataScience"

# .base-search-card__title , Job Title
# .base-search-card__subtitle , Company Name
# .job-search-card__location, Job Location
# 

pg <- read_html(url_base)

node <- html_nodes(pg, ".base-search-card__subtitle")

html_text(node, ".base-search-card__subtitle")

r

url_base_specificlocation <- "https://www.linkedin.com/jobs/search?keywords=Data&location=Greater%20Madison%20Area&geoId=90000472&trk=public_jobs_jobs-search-bar_search-submit&position=1&pageNum=0"

pg_location <- read_html(url_base_specificlocation)
node_location <- html_nodes(pg_location, ".base-search-card__subtitle")
node_title <- html_nodes(pg_location, ".base-search-card__title")
node_positionlocation <- html_nodes(pg_location, ".job-search-card__location")
company_name <- html_text(node_location)
position_name <- html_text(node_title)
location_name <- html_text(node_positionlocation)

company_name

companies <- gsub("\\W", "", company_name)
positions <- gsub("\\W", "", position_name)
locations <- gsub("\\W", "", location_name)

gsub("\\W", "", html_text(html_nodes(pg_location, ".job-search-card__location")))

# write.csv(data.frame(companies, positions, locations), "sample1.csv")

url_base <- "https://www.linkedin.com/jobs/search?keywords=Data&location=Greater%20Madison%20Area&geoId=90000472&trk=public_jobs_jobs-search-bar_search-submit&position=1&pageNum=0"
url_base_madison <- "https://www.linkedin.com/jobs/search?keywords=Data&location=Greater%20Madison%20Area&geoId=90000472&trk=public_jobs_jobs-search-bar_search-submit&position=1&pageNum=0"
url_base_madison_pg3 <- "https://www.linkedin.com/jobs/search?keywords=Data&location=Greater%sMadison%sArea&geoId=90000472&trk=public_jobs_jobs-search-bar_search-submit&position=1&pageNum=3"

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
  Position_Name=gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__subtitle"))),
  Company_Name=gsub("\\W", "", html_text(html_nodes(pg, ".base-search-card__title"))), 
  Location_Name=gsub("\\W", "", html_text(html_nodes(pg, ".job-search-card__location"))),
  stringsAsFactors=FALSE)

# Scrape Page 0 through X (You Specify)
for (i in 0:9){
  start_time <- Sys.time() 
  cat(".")
  Sys.sleep(runif(1, min = 5, max = 19))
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

writeLines(unique(df_all$Company_Name), "companies.csv", sep = ",")

write.csv(df_all, "sample2.csv")

unique(df_all$Company_Name)

# Alternative Method
# Using Function

# site <- map_df(1:3, function(i) {
#   
#   # simple but effective progress indicator
#   start_time <- Sys.time() 
#   
#   cat(".")
# 
#   Sys.sleep(5)
#   
#   pg <- read_html(url_base_madison, i)
#   
#   data.frame(Position_Name=gsub("\\W", "", html_text(html_nodes(pg_location, ".base-search-card__subtitle"))),
#              Company_Name=gsub("\\W", "", html_text(html_nodes(pg_location, ".base-search-card__title"))), 
#              Location_Name=gsub("\\W", "", html_text(html_nodes(pg_location, ".job-search-card__location"))),
#              stringsAsFactors=FALSE)
#   
#   end_time <- Sys.time()
#   
#   time_needed <- end_time - start_time
#   
#   print(paste("Step", i, "was finished after", time_needed, "seconds."))
#   
# })
