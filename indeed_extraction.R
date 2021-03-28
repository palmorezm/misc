# Packages
require(rvest)
require(readr)
require(tidyverse)

# ------- Test Prep ------ # 

# Create empty data frame 
df <- data.frame(title = character(),
                 company = character())

# Create a list of job titles for searching 
search_titles <- as.character(c("Data+Science", 
                                "Data+Analyst", 
                                "Decision+Scientist", 
                                "Operations+Manager", 
                                "Statistician", 
                                "Business+Analyst", 
                                ))

search_titles[[1]]
# ------- End Test Prep ------ # 



# ----------------------- Satisfactory Txt Variables (Labeled) ----------------------- # 

url_ds <- paste0('https://www.indeed.com/jobs?q=Data+Science&l=')
var <- read_html(url_ds)

title <- var %>% 
  html_nodes('.jobtitle') %>%
  html_text() %>%
  str_extract("(\\w+.+)+") 
employer <- var %>% 
  html_nodes('.company') %>%
  html_text() %>%
  str_extract("(\\w+).+")
description <- var %>%
  html_nodes('.summary') %>%
  html_text() %>%
  str_extract(".+") 
location <- var %>%
  html_nodes('.location') %>%
  html_text() %>%
  str_extract("(\\w+.)+,.[A-Z]{2}")
date <- var %>% 
  html_nodes('.date-a11y') %>% 
  html_text() 

# ------------------- End Satisfactory Txt Variables (Labeled) ---------------------- # 



# ------------------------ Locate Additional Txt of Interest ------------------------ # 

# Full xpath =             //*[@id="sja0"]
# target(s)
# <a target="_blank" 
#         id="sja0" 
# href="https://www.indeed.com/pagead/clk?mo=r&amp;ad=-6NYlbfkN0BT3klw8UxTRJmsnyYjJc17eo_UMjSAChSDVUQt3YpKLemtm-VKOdCbgz_IZJPidYhpSudWHWyQf6XVwqKsX3yX-EtStIgPSeZkcw5okt97dbSqD9y6Hjw8nSqlBNls5ItiWhQfxMZSL95qWSmB2mlI28u__gjkZnfibCGNz5l5MEmP7AF8Yi6XJIg4SwchmiCEHCWvRJnJpd3WPfFjqe3z4Fnf7zE_CgMnrF97pzMjH1IWjtZo3mX9bV95r0H2UpZrI8vpgin9WVWsAdr6V1bUKNrZomiyGpPhCyilF6IKetIOKjXu10mBnKABagAwmA2noQx8PYa6_qQRQBRNcu5bpT6WflK_Mv2z1epeG2543pMzjq4Z1EheTftGrR3dN2K0GzmJlW6KOr3TELU5HczKRXmCNjSdIlFUr5a6BZIRWlImWYYR5Z14&amp;p=0&amp;fvj=1&amp;vjs=3&amp;tk=1f1lgr113u3mu801&amp;jsa=1203&amp;sal=1" onmousedown="sjomd('sja0'); clk('sja0'); rclk(this,jobmap[0],1);" onclick="setRefineByCookie([]); sjoc('sja0', 1); convCtr('SJ'); rclk(this,jobmap[0],true,1);" rel="noopener nofollow" title="Data Analyst Intern" class="jobtitle turnstileLink " data-tn-element="jobTitle">
# <b>Data</b> Analyst Intern</a>

# ---------------------- End Locate Additional Txt of Interest ---------------------- # 



# ----------------------- Extract Pieces of Target(s) ------------------------------- # 

# Extract the entire html for jobtitle and associated link          
var %>% 
  html_nodes('a.jobtitle.turnstileLink')

# Store as html temporarily for testing 
html <- var %>% 
  html_nodes('a.jobtitle.turnstileLink') 

# Extract where there is exactly "href=" in each line
html %>% 
  str_extract("href=")

# Extract any characters a-z in the line
html %>% 
  str_extract_all('"/[a-z]')
# failed to return
# html is not of data type character

# convert to data type character 
# extract any printed character
as.character(html) %>% 
  str_extract_all("href=\"[:print:]")

# re-test on html with printed character
html %>% 
  str_extract_all("href=\"[:print:].")

# To isolate better restrategize
# Try another function in the tidyverse
gsub(".*href=", "", html)
gsub(".*href=", "", html)
gsub(".* onmouse", "", html)

# extract href through the elimination of its surroundings
html %>%
  str_extract_all("") %>%
  str_remove_all("mouse.*") %>% 
  str_remove_all(".* href")

# extract id by isolating the unique values
  # that follow the '=' in front of 'id' 
html %>% 
  str_extract_all("id=*.* href") %>% 
  str_remove_all(" href") %>% 
  str_remove_all("id=") %>% 
  str_remove_all("\"")
  

# ----------------------- Locate Txt of Interest ----------------------- #

# Use base var html to extract
# var %>% 
  # html_nodes('a.jobtitle.turnstileLink') %>%
  # str_remove_all(pattern = "mouse.*")

  
# Extract 'href' links to individual job posts 

# Target 1 - link of aggregated page
# Stored as 'link' and functions as such
link <- var %>% 
  html_nodes('a.jobtitle.turnstileLink') %>%
  # Extract "href=" and " onmouse" with everything in-between
  str_extract("href=.* onmouse") %>%
  # Remove useless characters
  str_remove_all("href=") %>% 
  str_remove_all("onmouse") %>% 
  str_remove_all("\"")


# Target 2 - job identifier
# Stored as 'code' by string of characters
id <- var %>%
  html_nodes('a.jobtitle.turnstileLink') %>%
  # Extract unique 'id' assigned to each post 
  str_extract_all("id=*.* href") %>% 
  str_remove_all(" href") %>% 
  str_remove_all("id=") %>% 
  str_remove_all("\"")


# ---------------------------- Run on Targets -------------------------- #







