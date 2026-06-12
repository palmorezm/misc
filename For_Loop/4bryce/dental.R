# create similar data to dental claims
# Characteristics are:
# 1.250658 million for each dental claims spreadsheet 
# 366 spreadsheets
# 7 used columns in each spreadsheet including:
  # Recipient ID
    # 10-digit numeric code
  # First Service Date
    # Date object formated two digit day, short month name, four digit year
  # Rendering Provider NPI
    # a unique, permanent 10-digit identification number 
    # assigned to healthcare providers by the Centers for 
    # Medicare & Medicaid Services (CMS)
  # FQHC Indicator
    # Only 0/1 where "0" is not and "1" is a Federal Qualified Health Center
  # RHC or SBS Indicator
    # Only 0/1 where "0" is not and "1" is a Rural Health Clinic or School-Based Service
  # Pilot County Indicator
    # Only 0/1 where "0" is not and "1" is a Pilot County 
  # Data Week
    # YYYY-WW: e.g., 2026-24 (Year and week of the year).

n = 100 # 1250658
start_service <- as.numeric(as.Date("2017-01-01"))
end_service   <- as.numeric(as.Date("2024-12-31"))
start_week <- as.numeric(as.Date("2017-01-01"))
end_week   <- as.numeric(as.Date("2024-12-31"))

dental <- data.frame(
  Recipient_ID = (runif(10, min=0.0000000001, max=0.1000000000 )*100000000000), 
  First_Service_Date = toupper(
    format(
      as.Date(
        sample(
          start_service:end_service, 
          size = n, 
          replace = TRUE), 
        origin = "1970-01-01"), 
      "%d%b%Y")
  ),
  Rendering_Provider_NPI = (runif(10, min=0.0000000001, max=0.1000000000 )*100000000000),
  FQHC_Indicator = sample(x = 0:1, size = n, replace = T), 
  RHC_or_SBS_Indicator = sample(x = 0:1, size = n, replace = T), 
  Pilot_County_Indicator = sample(x = 0:1, size = n, replace = T), 
  Data_Week = toupper(
    format(
      as.Date(
        sample(
          start_week:end_week, 
          size = n, 
          replace = TRUE), 
        origin = "1970-01-01"), 
      "%Y-%V")
  )
)

spreadsheets = 366

for(i in 1:spreadsheets){
  dental <- data.frame(
    Recipient_ID = (runif(10, min=0.0000000001, max=0.1000000000 )*100000000000), 
    First_Service_Date = toupper(
      format(
        as.Date(
          sample(
            start_service:end_service, 
            size = n, 
            replace = TRUE), 
          origin = "1970-01-01"), 
        "%d%b%Y")
    ),
    Rendering_Provider_NPI = (runif(10, min=0.0000000001, max=0.1000000000 )*100000000000),
    FQHC_Indicator = sample(x = 0:1, size = n, replace = T), 
    RHC_or_SBS_Indicator = sample(x = 0:1, size = n, replace = T), 
    Pilot_County_Indicator = sample(x = 0:1, size = n, replace = T), 
    Data_Week = toupper(
      format(
        as.Date(
          sample(
            start_week:end_week, 
            size = n, 
            replace = TRUE), 
          origin = "1970-01-01"), 
        "%Y-%V")
    )
  )
  write.csv(dental, file = paste0("data/dental_claims", i, ".csv"))
}


