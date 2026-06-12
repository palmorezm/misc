# create similar data to enrollment
# Characteristics of enrollment are
# 1.250658 million for each enrollment spreadsheet 
# 366 spreadsheets
# 4 columns in each spreadsheet including:
  # ID
    # 400-600
  # DOB
    # DD[3 letter month]YYYY
  # Disability Indicator (Yes/No) 1/0
    # 0 or 1
  # Enrollment Date
    # DD[3 letter month]YYYY
# About 5,001,875 total entries per csv

# enrollment <- matrix(c(
#   c1 = "400", 
#   c2 = "12JUL1988",
#   c3 = "1",
#   c4 = "04JUL2018"),  
#   ncol = 4, 
# )
# enrollment <- data.frame(enrollment)
# names(enrollment) <- c("ID", "DOB", "Accepted", "Date")
# enrollment$ID <- runif(n = n, min=400, max=600)
# Convert boundaries to numeric integers

n = 100 # 1250658
start_num <- as.numeric(as.Date("2017-01-01"))
end_num   <- as.numeric(as.Date("2024-12-31"))
dob_start <- as.numeric(as.Date("1920-01-01"))
dob_end   <- as.numeric(as.Date("1985-01-01"))

enrollment <- data.frame(
  ID = runif(n = n, min=400, max=600), 
  DOB = toupper(
    format(
      as.Date(
        sample(
          dob_start:dob_end, 
          size = n, 
          replace = TRUE), 
        origin = "1970-01-01"), 
      "%d%b%Y")
    ),
  Status = sample(x = 0:1, size = n, replace = T), 
  Date = toupper(
    format(
      as.Date(
        sample(
          start_num:end_num, 
          size = n, 
          replace = TRUE), 
        origin = "1970-01-01"), 
      "%d%b%Y")
  )
)

spreadsheets = 366

for(i in 1:spreadsheets){
  enrollment <- data.frame(
    ID = runif(n = n, min=400, max=600), 
    DOB = toupper(
      format(
        as.Date(
          sample(
            dob_start:dob_end, 
            size = n, 
            replace = TRUE), 
          origin = "1970-01-01"), 
        "%d%b%Y")
    ),
    Status = sample(x = 0:1, size = n, replace = T), 
    Date = toupper(
      format(
        as.Date(
          sample(
            start_num:end_num, 
            size = n, 
            replace = TRUE), 
          origin = "1970-01-01"), 
        "%d%b%Y")
    )
  )
  write.csv(enrollment, file = paste0("data/enrollment_", i, ".csv"))
}


