
# Data Cleaning
# Topic: Masculinity
# Source: FiveThirtyEight

# Import the results from remote 
masculinity_results <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/masculinity-survey.csv")
head(masculinity_results) # Shows no missing values 

# Notes
# The proportions are derrived from the sample size 
# n, which came from 1,615 adult men

# Import the raw data after downloading from remote 
# Found here: https://github.com/fivethirtyeight/data/blob/master/masculinity-survey/raw-responses.csv
# Two methods shown: "Download then Upload" and "Direct from Remote"

# Method: Download and Upload 
masculinity <- read.csv("C:/data/masculinity.csv")
head(masculinity) # Review to ensure it is what you intended
# Should dispaly a "tidy" data with each individual's responses
sum(is.na(masculinity)) # Sum how many values are missing
which(is.na(masculinity)) # Extract the index value of each missing value
masculinity[58141,] # Check the first
masculinity[58142,] # And Second 
masculinity[58144,] # And Third for patterns
# It appears some have all values missing and others were just skipped in the survey
# Most categories include a section for "No Answer" 
# Rather than omit all results we create another category to capture this data
# then we replace all the NA values with the category in the 'masculinity_results' 
masculinity[is.na(masculinity)] <- "No Answer" # Replace with "No Answer"
sum(is.na(masculinity)) # Check for missing values after replacement

# Method: Direct from Remote
# Uses the URL of from FiveThirtyEight's raw content on Github
masculinity <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/masculinity-survey/raw-responses.csv")
head(masculinity) # Review to ensure it matches expectations
# This should contain the same as above prior to cleaning
# Repeat the cleaning steps (namely just the replacement and check for missing)
masculinity[is.na(masculinity)] <- "No Answer" # Replace with "No Answer"
sum(is.na(masculinity)) # Any value >0 is an issue that should be reviewed further
# Otherwise we continue as the data is now tidy

# Export as csv
write.csv(masculinity, file = "C:/data/masculinity_cleaned.csv")
# Check file path for file


