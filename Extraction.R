# Packages
require(rvest)
require(readr)
require(tidyverse)

# Create empty data frame 
df <- data.frame(title = character(),
                 company = character())

# Create a list of job titles for searching 
search_titles <- as.character(c("Data+Science", 
                                "Data+Analyst"))

search_titles[[1]]



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
var %>% 
  html_nodes('')
             //*[@id="sja0"]
             <a target="_blank" id="sja0" href="https://www.indeed.com/pagead/clk?mo=r&amp;ad=-6NYlbfkN0BT3klw8UxTRJmsnyYjJc17eo_UMjSAChSDVUQt3YpKLemtm-VKOdCbgz_IZJPidYhpSudWHWyQf6XVwqKsX3yX-EtStIgPSeZkcw5okt97dbSqD9y6Hjw8nSqlBNls5ItiWhQfxMZSL95qWSmB2mlI28u__gjkZnfibCGNz5l5MEmP7AF8Yi6XJIg4SwchmiCEHCWvRJnJpd3WPfFjqe3z4Fnf7zE_CgMnrF97pzMjH1IWjtZo3mX9bV95r0H2UpZrI8vpgin9WVWsAdr6V1bUKNrZomiyGpPhCyilF6IKetIOKjXu10mBnKABagAwmA2noQx8PYa6_qQRQBRNcu5bpT6WflK_Mv2z1epeG2543pMzjq4Z1EheTftGrR3dN2K0GzmJlW6KOr3TELU5HczKRXmCNjSdIlFUr5a6BZIRWlImWYYR5Z14&amp;p=0&amp;fvj=1&amp;vjs=3&amp;tk=1f1lgr113u3mu801&amp;jsa=1203&amp;sal=1" onmousedown="sjomd('sja0'); clk('sja0'); rclk(this,jobmap[0],1);" onclick="setRefineByCookie([]); sjoc('sja0', 1); convCtr('SJ'); rclk(this,jobmap[0],true,1);" rel="noopener nofollow" title="Data Analyst Intern" class="jobtitle turnstileLink " data-tn-element="jobTitle">
               <b>Data</b> Analyst Intern</a>
            
var %>% 
  html_nodes('a.jobtitle.turnstileLink')

html <- var %>% 
  html_nodes('a.jobtitle.turnstileLink') 

html %>% 
  str_extract("href=")

html %>% 
  str_extract_all('"/[a-z]')

as.character(html) %>% 
  str_extract_all("href=\"[:print:]")

html %>% 
  str_extract_all("href=\"[:print:].")

gsub(".*href=", "", html)

gsub(".*href=", "", html)
gsub(".* onmouse", "", html)

# extract href
html %>%
  str_extract_all("")
  str_remove_all("mouse.*") %>% 
  str_remove_all(".* href")

# extract id
html %>% 
  str_extract_all("id=*.* href") %>% 
  str_remove_all(" href") %>% 
  str_remove_all("id=") %>% 
  str_remove_all("\"")
  
  