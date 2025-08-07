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
library(zoo)
library(sf)
library(leaflet)

coords<-read.csv("MooredSensor_Coordinates.csv")%>%
  filter(Abbrev %in% c("SHL","SMB","SLM"))%>%
  st_as_sf(coords= c(x="Longitude",y="Latitude"),crs=4269)


#create function to import telemetry .dat files from each station google drive links and save to environment
importdata <- function(station, file_id) {
  # create google drive download link
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
  
  # Read and process
  dat <- read.csv(url, skip = 1) %>%
    # remove top row
    slice(-c(1:2)) %>%
    # PST datetime
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "America/Los_Angeles")) %>%
    # make everything else numeric
    mutate(across(c(3, 5:20), ~as.numeric(.)))
  
  # Base column selection
  base_cols <- c("TIMESTAMP", "FchlugL_Med","WTemp_Med","ODOsat_med", "pH_Med", "Turbidity_Med", "Sal_Med", "RelativeFDOM_Med")
  
  # Add nitrate column if SHL
  if (station == "SHL") {
    base_cols <- c(base_cols, "SunaNitrateUM_AVG_Calc")
  }
  
  dat <- dat %>%
    # Select key columns (conditional)
    dplyr::select(all_of(base_cols)) %>%
    # NA out some pre-review but incorrect data
    mutate(
      FchlugL_Med = ifelse(FchlugL_Med > 1000 | FchlugL_Med < 0, NA, FchlugL_Med),
      pH_Med = ifelse(pH_Med < 7, NA, pH_Med),
      ODOsat_med = ifelse(ODOsat_med < 50, NA, ODOsat_med),
      RelativeFDOM_Med = ifelse(between(RelativeFDOM_Med,0,50), RelativeFDOM_Med, NA),
      Sal_Med= ifelse(between(Sal_Med,5,35), Sal_Med, NA)
    )%>%
    mutate(
      roll_mean = rollapply(WTemp_Med, width = 30, FUN = mean, fill = NA, align = "center", na.rm = TRUE),
      roll_sd   = rollapply(WTemp_Med, width = 30, FUN = sd, fill = NA, align = "center", na.rm = TRUE),
      WTemp_Med = ifelse(
        abs(WTemp_Med - roll_mean) > 1.5 * roll_sd,
        NA, WTemp_Med
      )
    ) %>%
    select(-roll_mean, -roll_sd)%>%
    mutate(
      roll_mean = rollapply(pH_Med, width = 20, FUN = mean, fill = NA, align = "center", na.rm = TRUE),
      roll_sd   = rollapply(pH_Med, width = 20, FUN = sd, fill = NA, align = "center", na.rm = TRUE),
      pH_Med = ifelse(
        abs(pH_Med - roll_mean) > 2 * roll_sd,
        NA, pH_Med
      )
    )%>%
    select(-roll_mean, -roll_sd)%>%
    mutate(WTemp_Med = ifelse(WTemp_Med<18 & month(TIMESTAMP)>7,NA,WTemp_Med))
  
  # Rename columns
  new_names <- c("Datetime", "Chl-a (µg/L)","Temperature (°C)", "DO (% saturation)", "pH", "Turbidity (FNU)", "Salinity (PSU)", "fDOM (RFU)")
  if (station == "SHL") {
    new_names <- c(new_names, "Nitrate + nitrate (µMol/L)")
  }
  
  dat <- set_names(dat, new_names)
  
  # Assign to global environment
  assign(station, dat, envir = .GlobalEnv)
}

#import each station
importdata("SLM","1NRGzJTKUOb7MxyrFKvxwnaKX9sH_1Iij")
importdata("SHL","1omJnHmqR4hi9tCvZKSkON0-UuqDI2OgW")
importdata("SMB","10wL7cMNOli4lvygvcNw-y5gpESms31Tc")

SLM<-SLM%>%
  mutate("Nitrate + nitrate (µMol/L)" = NA)%>%
  mutate(across(
    .cols = -Datetime,  # exclude timestamp
    .fns = ~ ifelse(between(as_date(Datetime), as_date("2025-05-08"), as_date("2025-05-16")), NA, .)
  ))

SMB<-SMB%>%
  mutate("Nitrate + nitrate (µMol/L)" = NA)

SHL<-SHL%>%
  mutate(`Nitrate + nitrate (µMol/L)` = as.numeric(`Nitrate + nitrate (µMol/L)`))

startdate<-min(as_date(SMB$Datetime),na.rm = TRUE)
enddate<-max(as_date(SMB$Datetime),na.rm = TRUE)+1

ui <- fluidPage(
  tags$head(  # Insert JavaScript for clipboard support
    tags$script(HTML("
      Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
        var copyText = document.getElementById(message.id);
        copyText.select();
        document.execCommand('copy');
      });
    "))
  ),
  tags$head(
    tags$style(HTML("
    html, body, .container-fluid {
      height: 100%;
      margin: 0;
      padding: 0;
      overflow: hidden;
    }
    .plot-container {
      height: calc(100vh - 120px); /* Adjust offset for header and controls */
    }
  "))
  ),
  
  titlePanel("Mooring telemetry data"),
  sidebarLayout(
    sidebarPanel(
      "Click a station to change to that dataset",
      leafletOutput("siteMap", height = 400),
      selectInput("site", "Select Dataset:", choices = c("SLM", "SHL", "SMB")),
      selectInput("y", "Y-axis:", choices =c("Chl-a (µg/L)", "DO (% saturation)","Temperature (°C)", "pH", "Turbidity (FNU)", "Salinity (PSU)","Nitrate + nitrate (µMol/L)")),
      sliderInput("daterange", "Select Date Range:",
                  min = startdate,  # optional range limits
                  max = enddate,
                  value = c(enddate - 14, enddate),
                  timeFormat = "%Y-%m-%d"),
      actionButton("bookmarkBtn", "Bookmark Current View")
    ),
    mainPanel(
      div(class = "plot-container",
          plotlyOutput("dataPlot", height = "100%")
      )
    )
    
))

server <- function(input, output,session) {
  selected_data <- reactive({
    switch(input$site,
           "SLM" = SLM,
           "SHL" = SHL,
           "SMB" = SMB)
  })
  
  output$siteMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -122.23, lat = 37.64, zoom = 10) %>%
      addCircleMarkers(data = coords,
                       radius = 8,
                       color = "blue",
                       fillOpacity = 0.6,
                       label = ~Abbrev,
                       layerId = ~Abbrev,
                       labelOptions = labelOptions(
                         noHide = TRUE,          # Keep labels always visible
                         direction = "center",     # Position label intelligently
                         textOnly = TRUE,        # Don't show speech bubble around text
                         style = list("font-size" = "8px", "font-weight" = "bold","color"="white")
                       ))
  })
  
  observeEvent(input$siteMap_marker_click, {
    site_clicked <- input$siteMap_marker_click$id
    updateSelectInput(session, "site", selected = site_clicked)
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
  
  # Bookmark logic
  observeEvent(input$bookmarkBtn, {
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    showModal(modalDialog(
      title = "Bookmark Created",
      tagList(
        p("Use the button below to copy this URL:"),
        textInput("bookmarkURL", NULL, value = url, width = "100%"),
        actionButton("copyURL", "📋 Copy URL")
      ),
      footer = NULL,
      easyClose = TRUE
    ))
    
    # Inject JavaScript for clipboard functionality
    observeEvent(input$copyURL, {
      session$sendCustomMessage(type = "copyToClipboard", message = list(id = "bookmarkURL"))
    })
    
  })
}


shinyApp(ui = ui, server = server, enableBookmarking = "url")