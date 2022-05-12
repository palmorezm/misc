
# APP for UCD 

load("Data/reg_mort.rdata")

ui <- fluidPage(
  fluidRow(
    column(12,
           titlePanel("Mortality Review"),
           h4("An app to observe and assess measurements of Mortality in Rock County"),
           h5("Specifically we try to determine:"), 
           helpText("Data Source: U.S. CDC at https://data.cdc.gov/"),
           hr(),
           selectizeInput(
             inputId = "mapyear", 
             label = "Census Year:", 
             choices = unique(UCD$Year), 
             selected = 2019, 
             multiple = F), 
           selectizeInput(
             inputId = "ucdicd",
             label = "ICD-10 Chapter",
             choices = unique(UCD$ICD.Chapter),
             selected = UCD$ICD.Chapter[[1]], 
             multiple = F)
    )) # End tab 1 
)

# Define server functions
server <- function(input, output){
  
  
  
  
} # Close server

shinyApp(ui, server)
