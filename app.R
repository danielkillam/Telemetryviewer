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
  filter(Abbrev %in% c("SHL","SMB","SLM","ALA"))%>%
  st_as_sf(coords= c(x="Longitude",y="Latitude"),crs=4269)


#cache google drive downloads locally so repeated app restarts don't re-download every station's data
cache_dir <- "cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

get_cached_path <- function(url, cache_name, max_age_mins = 15) {
  cache_path <- file.path(cache_dir, cache_name)
  is_stale <- !file.exists(cache_path) ||
    difftime(Sys.time(), file.info(cache_path)$mtime, units = "mins") > max_age_mins

  if (is_stale) {
    downloaded <- tryCatch({
      download.file(url, cache_path, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) {
      message("Download failed for ", cache_name, ", using cached copy if available: ", conditionMessage(e))
      FALSE
    })
    if (!downloaded && !file.exists(cache_path)) {
      stop("No cached data available for ", cache_name, " and download failed.")
    }
  }
  cache_path
}

#create function to import telemetry .dat files from each station google drive links and save to environment
importdata <- function(station, file_id) {
  # create google drive download link
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
  cache_path <- get_cached_path(url, paste0(station, ".csv"))

  # Read and process
  dat <- read.csv(cache_path, skip = 1) %>%
    # remove top row
    slice(-c(1:2)) %>%
    # PST datetime
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")) %>%
    # make everything else numeric
    mutate(across(c(3, 5:20), ~as.numeric(.)),
           Depth = NA)
  
  # Base column selection
  base_cols <- c("TIMESTAMP", "FchlugL_Med","WTemp_Med","ODOsat_med", "pH_Med", "Turbidity_Med", "Sal_Med", "RelativeFDOM_Med","Depth_Med")
  
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
  new_names <- c("Datetime", "Chl-a (µg/L)","Temperature (°C)", "DO (% saturation)", "pH", "Turbidity (FNU)", "Salinity (PSU)", "fDOM (RFU)","Depth (m)")
  if (station == "SHL") {
    new_names <- c(new_names, "Nitrate + nitrate (µMol/L)")
  }
  
  dat <- set_names(dat, new_names)
  
  # Assign to global environment
  assign(station, dat, envir = .GlobalEnv)
}

importdata_ala <- function(station, file_id) {
  # create google drive download link
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)
  cache_path <- get_cached_path(url, paste0(station, "_ala.csv"))

  # Read and process
  dat <- read.csv(cache_path, skip = 1) %>%
    # remove top row
    slice(-c(1:2)) %>%
    # PST datetime
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")) %>%
    # make everything else numeric
    mutate(across(c(2:28), ~as.numeric(.)))
  
  # Base column selection
  base_cols <- c("TIMESTAMP", "Chl_ugL","Temp","DO_pct", "pH", "Turb_FNU", "Sal", "fDOM_RFU","Depth")
  
  #names of other parameters
  match_cols <- c("TIMESTAMP", "FchlugL_Med","WTemp_Med","ODOsat_med", "pH_Med", "Turbidity_Med", "Sal_Med", "RelativeFDOM_Med","Depth")
  
  dat <- dat %>%
    # Select key columns (conditional)
    dplyr::select(all_of(base_cols)) %>%
    set_names(match_cols)%>%
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
  new_names <- c("Datetime", "Chl-a (µg/L)","Temperature (°C)", "DO (% saturation)", "pH", "Turbidity (FNU)", "Salinity (PSU)", "fDOM (RFU)","Depth (m)")
  if (station == "SHL") {
    new_names <- c(new_names, "Nitrate + nitrate (µMol/L)")
  }
  
  dat <- set_names(dat, new_names)
  
  # Assign to global environment
  assign(station, dat, envir = .GlobalEnv)
}

#import each station
#importdata("SLM","1NRGzJTKUOb7MxyrFKvxwnaKX9sH_1Iij")
importdata("SLM","1vRG41vvds1uF-sSSZ7rCEv7J6H39h1Fh")
importdata("SHL","1omJnHmqR4hi9tCvZKSkON0-UuqDI2OgW")
importdata("SMB","10wL7cMNOli4lvygvcNw-y5gpESms31Tc")
#importdata_ala("ALA","1xVmdjB9RtvTviaaWGQNfGGDixQ1dO1bq")
importdata_ala("ALA","1Es6apaa7P19oZ0odi21xo_ugRDxcDaa1")
SLM<-SLM%>%
  mutate("Nitrate + nitrate (µMol/L)" = NA)%>%
  mutate(across(
    .cols = -Datetime,  # exclude timestamp
    .fns = ~ ifelse(between(as_date(Datetime), as_date("2025-05-08"), as_date("2025-05-16")), NA, .)
  ))

SMB<-SMB%>%
  mutate("Nitrate + nitrate (µMol/L)" = NA)

#import and tack on old SHL data
importolddata <- function(station, path) {
  dat <- read.csv(path, skip = 1) %>%
    # remove top row
    slice(-c(1:2)) %>%
    # PST datetime
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")) %>%
    # make everything else numeric
    mutate(across(c(3, 5:20), ~as.numeric(.)),
           Depth = NA)
  
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
    mutate(WTemp_Med = ifelse(WTemp_Med<18 & month(TIMESTAMP)>7,NA,WTemp_Med),
           Depth_Med=NA)
  
  # Rename columns
  new_names <- c("Datetime", "Chl-a (µg/L)","Temperature (°C)", "DO (% saturation)", "pH", "Turbidity (FNU)", "Salinity (PSU)", "fDOM (RFU)","Depth (m)")
  if (station == "SHL") {
    new_names <- c(new_names, "Nitrate + nitrate (µMol/L)")
  }
  
  dat <- set_names(dat, new_names)%>%
    mutate(`Depth (m)`=NA)
  
  # Assign to global environment
  assign(paste0(station,"_old"), dat, envir = .GlobalEnv)
}


importolddata("SHL","SHL2_NWIS_Data.dat.backup")

SHL<-SHL%>%
  bind_rows(SHL_old)%>%
  dplyr::distinct(Datetime,.keep_all = TRUE)%>%
  arrange(Datetime)%>%
  mutate(`Nitrate + nitrate (µMol/L)` = as.numeric(`Nitrate + nitrate (µMol/L)`))

importolddata("SMB","SMB2_NWIS_Data.dat.backup")
SMB<-SMB%>%
  bind_rows(SMB_old)%>%
  dplyr::distinct(Datetime,.keep_all = TRUE)%>%
  arrange(Datetime)

SLM<-SLM%>%
  mutate(`Depth (m)`=as.numeric(`Depth (m)`))

importolddata_ala <- function(station, file_id) {
  # Read and process
  dat <- read.csv(file_id, skip = 1) %>%
    # remove top row
    slice(-c(1:2)) %>%
    # PST datetime
    mutate(TIMESTAMP = as_datetime(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT+8")) %>%
    # make everything else numeric
    mutate(across(c(2:28), ~as.numeric(.)))
  
  # Column names vary between datalogger program versions
  do_col  <- intersect(c("DO_pct", "DO_prct"), names(dat))[1]
  sal_col <- intersect(c("Sal", "Sal_psu"), names(dat))[1]

  # Base column selection
  base_cols <- c("TIMESTAMP", "Chl_ugL","Temp",do_col, "pH", "Turb_FNU", sal_col, "fDOM_RFU","Depth")

  #names of other parameters
  match_cols <- c("TIMESTAMP", "FchlugL_Med","WTemp_Med","ODOsat_med", "pH_Med", "Turbidity_Med", "Sal_Med", "RelativeFDOM_Med","Depth")
  
  dat <- dat %>%
    # Select key columns (conditional)
    dplyr::select(all_of(base_cols)) %>%
    set_names(match_cols)%>%
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
  new_names <- c("Datetime", "Chl-a (µg/L)","Temperature (°C)", "DO (% saturation)", "pH", "Turbidity (FNU)", "Salinity (PSU)", "fDOM (RFU)","Depth (m)")
  if (station == "SHL") {
    new_names <- c(new_names, "Nitrate + nitrate (µMol/L)")
  }
  
  dat <- set_names(dat, new_names)
  
  # Assign to global environment
  assign(paste0(station,"_old"), dat, envir = .GlobalEnv)
}

importolddata_ala("ALA","ALA_20260120_20260324.dat")
ALA_old1<-ALA_old
importolddata_ala("ALA","ALA_20260325_20260604.dat")
ALA_old2<-ALA_old
importolddata_ala("ALA","ALA_20260120_20260324(1).dat")
ALA_old3<-ALA_old
importolddata_ala("ALA","Copy of ALA2_ExoDirect1Data.dat")
ALA_old4<-ALA_old

ALA_old<-bind_rows(ALA_old1,ALA_old2,ALA_old3,ALA_old4)

ALA<-ALA%>%
  bind_rows(ALA_old)%>%
  dplyr::distinct(Datetime,.keep_all = TRUE)%>%
  arrange(Datetime)%>%
  mutate("Nitrate + nitrate (µMol/L)" = NA)

startdate<-min(as_date(SHL$Datetime),na.rm = TRUE)
enddate<-max(as_date(SHL$Datetime),na.rm = TRUE)+1

ui <- function(request) {
  fluidPage(
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
  
  #titlePanel("Mooring telemetry data"),
  sidebarLayout(
    sidebarPanel(
      "Click a station to change to that dataset",
      leafletOutput("siteMap", height = 200),
      selectInput("site", "Select Dataset:", choices = c("SLM", "SHL", "SMB","ALA"), selected = "SMB"),
      selectInput("y", "Y-axis:", choices =c("Chl-a (µg/L)", "DO (% saturation)","Temperature (°C)", "pH", "Turbidity (FNU)", "Salinity (PSU)","Nitrate + nitrate (µMol/L)","Depth (m)")),
      sliderInput("daterange", "Select Date Range:",
                  min = startdate,  # optional range limits
                  max = enddate,
                  value = c(enddate - 14, enddate),
                  timeFormat = "%Y-%m-%d"),
      selectInput("y2", "Second Y-axis (optional):",
                  choices = c("None", "Chl-a (µg/L)", "DO (% saturation)",
                              "Temperature (°C)", "pH", "Turbidity (FNU)",
                              "Salinity (PSU)", "Nitrate + nitrate (µMol/L)","Depth (m)"),
                  selected = "None"),
      # --- Side-by-Side Y-Axis Minimums ---
      fluidRow(
        column(6, numericInput("ymin", "Min (Primary):", value = NA)),
        column(6, numericInput("y2min", "Min (Secondary):", value = NA))
      ),
      
      # --- Side-by-Side Y-Axis Maximums ---
      fluidRow(
        column(6, numericInput("ymax", "Max (Primary):", value = NA)),
        column(6, numericInput("y2max", "Max (Secondary):", value = NA))
      ),
      
      br(), # Adds a little bit of vertical padding
      actionButton("bookmarkBtn", "Bookmark Current View")
    ),
    mainPanel(
      div(class = "plot-container",
          plotlyOutput("dataPlot", height = "100%")
      )
    )

  ))
}

server <- function(input, output,session) {
  selected_data <- reactive({
    switch(input$site,
           "SLM" = SLM,
           "SHL" = SHL,
           "SMB" = SMB,
           "ALA" = ALA)
  })
  
  output$siteMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -122.23, lat = 37.64, zoom = 9) %>%
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
    end_date   <- as.Date(input$daterange[2], origin = "1970-01-01")
    
    filtered <- data %>%
      filter(Datetime >= start_date & Datetime <= end_date)
    
    # --- Primary Plot (p1) ---
    p1 <- ggplot(filtered, aes(x = Datetime, y = .data[[input$y]])) +
      geom_line(color = "steelblue") +
      labs(x = element_blank(), y = input$y) +
      theme_minimal()
    
    if (!is.na(input$ymin) || !is.na(input$ymax)) {
      p1 <- p1 + coord_cartesian(ylim = c(input$ymin, input$ymax))
    }
    
    # --- Secondary Plot (p2) ---
    if (input$y2 != "None") {
      p2 <- ggplot(filtered, aes(x = Datetime, y = .data[[input$y2]])) +
        geom_line(color = "darkred") +
        labs(x = element_blank(), y = input$y2) +
        theme_minimal()
      
      # Apply limits to the secondary plot
      if (!is.na(input$y2min) || !is.na(input$y2max)) {
        p2 <- p2 + coord_cartesian(ylim = c(input$y2min, input$y2max))
      }
      
      # Convert both to plotly
      p1_plotly <- ggplotly(p1)
      p2_plotly <- ggplotly(p2)
      
      # Stack vertically
      # Note: Use shareY = FALSE to ensure they keep their independent manual scales
      subplot(p1_plotly, p2_plotly, nrows = 2, shareX = TRUE, titleY = TRUE)
      
    } else {
      ggplotly(p1)
    }
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