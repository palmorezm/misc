
# Shiny Mortality REG exploration

df <- REG_MORT

ui <- fluidPage(
             fluidRow(
               column(12,
                      titlePanel("Mortality Review"),
                      h4("An app to observe and assess measurements of affordability in the United States"),
                      h5("Specifically we try to determine:"), 
                      helpText("Data Source: U.S. CDC at https://data.cdc.gov/"),
                      hr(),
                      selectizeInput(
                        inputId = "mapyear", 
                        label = "Census Year:", 
                        choices = unique(df$Year), 
                        selected = 2019, 
                        multiple = F), 
                      selectizeInput(
                        inputId = "ucdicd",
                        label = "ICD-10 Code",
                        choices = unique(df$UCD...ICD.Chapter.Code),
                        selected = df$UCD...ICD.Chapter.Code[[1]], 
                        multiple = F)
             )) # End tab 1 
  )

# Define server functions
server <- function(input, output){
  
  
  
} # Close server

shinyApp(ui, server)