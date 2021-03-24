
# March 24, 2021
# Scrape Indeed for Jobs Data

# Link for Indeed Search
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote

# Packages
require(rvest)
require(readr)
require(tidyverse)
require(DT)


# Sample Scrape
# 
#Specifying the url for desired website to be scraped
url <- 'https://www.indeed.com/jobs?q=Data%20Science&l&vjk=a04c89acee43bb71'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
job_data_html <- html_nodes(webpage,'.clickcard , .jobtitle') 
# The characters '.text-primary' came from selector gadget

#Converting the ranking data to text
job_data <- html_text(job_data_html)

#Let's have a look at the rankings
head(job_data)


# Search filters

# Example:
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote&explvl=entry_level&fromage=7

# Job title 
# https://www.indeed.com/jobs?q= #*decision+scientist*&l #location is left blank to search everywhere

# Location
# https://www.indeed.com/jobs?q=decision+scientist&l= #*Remote* 

# Experience level 
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote& # explvl= #*entry_level*

# Date posted
# https://www.indeed.com/jobs?q=decision+scientist&l=Remote&explvl=entry_level& #fromage= #*7* # Within last 7 days


# HTML Text Data Scrapes

# Decision Scientist Scrape

# Data Scientist Scrape

# Data Analyst Scrape

# Operations Analyst Scrape

# Operations Manager Scrape 

# [ Insert title ] ... Scrape

