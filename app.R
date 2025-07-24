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
library(googledrive)

importdata <- function(station,file_id) {
  
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

  # Read and process
  dat <- read.csv(url, skip = 1) %>%
    slice(-c(1:2)) %>%
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")) %>%
    mutate(across(c(3, 5:20), ~as.numeric(.))) %>%
    dplyr::select(TIMESTAMP, FchlugL_Med, ODOsat_med, pH_Med, Turbidity_Med) %>%
    mutate(FchlugL_Med = ifelse(FchlugL_Med>1000,NA,FchlugL_Med),
           FchlugL_Med = ifelse(FchlugL_Med<0,NA,FchlugL_Med),
           pH_Med = ifelse(pH_Med<7,NA,pH_Med),
           ODOsat_med = ifelse(ODOsat_med<50,NA,ODOsat_med)
    )%>%
    set_names(c("Datetime", "Chl-a (µg/L)", "DO (% saturation)", "pH", "Turbidity (FNU)"))
  
  # Assign to global environment
  assign(station, dat, envir = .GlobalEnv)
}

importdata("SLM","1NRGzJTKUOb7MxyrFKvxwnaKX9sH_1Iij")
importdata("SHL","1omJnHmqR4hi9tCvZKSkON0-UuqDI2OgW")
importdata("SMB","10wL7cMNOli4lvygvcNw-y5gpESms31Tc")  # Example: "smbdata"

ui <- fluidPage(
  titlePanel("Mooring telemetry data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("site", "Select Dataset:", choices = c("SLM", "SHL", "SMB")),
      selectInput("y", "Y-axis:", choices = c("Chl-a (µg/L)", "DO (% saturation)", "pH", "Turbidity (FNU)")),
      dateRangeInput("daterange", "Select Date Range:",
                     start = Sys.Date() - 14,
                     end = Sys.Date(),
                     min = Sys.Date() - 365,  # optional range limits
                     max = Sys.Date(),
                     format = "yyyy-mm-dd")
    ),
    mainPanel(
      plotlyOutput("dataPlot",height="900px")
    )
  )
)

server <- function(input, output) {
  selected_data <- reactive({
    switch(input$site,
           "SLM" = slmdata,
           "SHL" = shldata,
           "SMB" = smbdata)
  })
  
  output$dataPlot <- renderPlotly({
    data <- selected_data()
    
    filtered <- data %>%
      filter(Datetime >= as.POSIXct(input$daterange[1]) &
               Datetime <= as.POSIXct(input$daterange[2]))
    
    p <- ggplot(filtered, aes(x = Datetime, y = .data[[input$y]])) +
      geom_line(color = "steelblue") +
      labs(title = paste("Plot of", input$y, "over Time at", input$site),
           x = "Time", y = input$y) +
      theme_minimal()
    
    ggplotly(p)
  })
}


shinyApp(ui = ui, server = server)