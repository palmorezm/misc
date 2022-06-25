

readxl::read_xlsx()

data2021 <- readxl::read_xlsx("C:/Users/Owner/Documents/GitHub/misc/Experiments/2021.xls")
d2021 <- readxl::read_xlsx("2021.xlsx")
d2022 <- readxl::read_xlsx("2022.xlsx")

# Select columns of interest from both df
# rename columns in each to same name 
# bind the rows of the columns with same name
# change data types
