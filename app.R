#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

#create function to import telemetry .dat files from each station google drive links and save to environment
importdata <- function(station,file_id) {
  #create google drive download link
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

  # Read and process
  dat <- read.csv(url, skip = 1) %>%
    #remove top row
    slice(-c(1:2)) %>%
    #PST datetime
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")) %>%
    #make everything else numeric
    mutate(across(c(3, 5:20), ~as.numeric(.))) %>%
    #select key columns
    dplyr::select(TIMESTAMP, FchlugL_Med, ODOsat_med, pH_Med, Turbidity_Med) %>%
    #NA out some pre-review but incorrect data
    mutate(FchlugL_Med = ifelse(FchlugL_Med>1000,NA,FchlugL_Med),
           FchlugL_Med = ifelse(FchlugL_Med<0,NA,FchlugL_Med),
           pH_Med = ifelse(pH_Med<7,NA,pH_Med),
           ODOsat_med = ifelse(ODOsat_med<50,NA,ODOsat_med)
    )%>%
    #rename columns
    set_names(c("Datetime", "Chl-a (µg/L)", "DO (% saturation)", "pH", "Turbidity (FNU)"))
  
  # Assign to global environment
  assign(station, dat, envir = .GlobalEnv)
}
#import each station
importdata("SLM","1NRGzJTKUOb7MxyrFKvxwnaKX9sH_1Iij")
importdata("SHL","1omJnHmqR4hi9tCvZKSkON0-UuqDI2OgW")
importdata("SMB","10wL7cMNOli4lvygvcNw-y5gpESms31Tc")  # Example: "smbdata"

ui <- fluidPage(
  titlePanel("Mooring telemetry data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select Dataset:", choices = c("SLM", "SHL", "SMB")),
      selectInput("y", "Y-axis:", choices = c("Chl-a (µg/L)", "DO (% saturation)", "pH", "Turbidity (FNU)")),
      sliderInput("daterange", "Select Date Range:",
                     min = min(as_date(SMB$Datetime),na.rm = TRUE),  # optional range limits
                     max = max(as_date(SMB$Datetime),na.rm = TRUE),
                     value = c(max(as_date(SMB$Datetime),na.rm = TRUE) - 14, max(as_date(SMB$Datetime),na.rm = TRUE)),
                     timeFormat = "%Y-%m-%d")
    ),
    mainPanel(
      plotlyOutput("dataPlot",height="900px")
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    switch(input$site,
           "SLM" = SLM,
           "SHL" = SHL,
           "SMB" = SMB)
  })
  
  output$dataPlot <- renderPlotly({
    data <- selected_data()
    
    start_date <- as.Date(input$daterange[1], origin = "1970-01-01")
    end_date <- as.Date(input$daterange[2], origin = "1970-01-01")
    
    filtered <- data %>%
      filter(Datetime >= start_date &
               Datetime <= end_date)
    
    p <- ggplot(filtered, aes(x = Datetime, y = .data[[input$y]])) +
      geom_line(color = "steelblue") +
      labs(title = paste("Plot of", input$y, "over Time at", input$site),
           x = "Time", y = input$y) +
      theme_minimal()
    
    ggplotly(p)
  })
}


shinyApp(ui = ui, server = server)