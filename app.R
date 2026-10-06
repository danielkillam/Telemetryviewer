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


#create function to import telemetry .dat files from each station google drive links and save to environment
importdata <- function(station, file_id) {
  # create google drive download link
  url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

  # Read and process
  dat <- read.csv(url, skip = 1) %>%
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

  # Read and process
  dat <- read.csv(url, skip = 1) %>%
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

#import and tack on old SHL data
#(function defs only here; invocations live in load_all_stations() below)
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

#runs the full station data pipeline, always pulling the latest data from Google Drive
load_all_stations <- function() {
  #import each station
  importdata("SLM","1vRG41vvds1uF-sSSZ7rCEv7J6H39h1Fh")
  importdata("SHL","1omJnHmqR4hi9tCvZKSkON0-UuqDI2OgW")
  importdata("SMB","10wL7cMNOli4lvygvcNw-y5gpESms31Tc")
  importdata_ala("ALA","1Es6apaa7P19oZ0odi21xo_ugRDxcDaa1")


  SLM<<-SLM%>%
    mutate("Nitrate + nitrate (µMol/L)" = NA)%>%
    mutate(across(
      .cols = -Datetime,  # exclude timestamp
      .fns = ~ ifelse(between(as_date(Datetime), as_date("2025-05-08"), as_date("2025-05-16")), NA, .)
    ))

  SMB<<-SMB%>%
    mutate("Nitrate + nitrate (µMol/L)" = NA)

  importolddata("SHL","SHL2_NWIS_Data.dat.backup")

  SHL<<-SHL%>%
    bind_rows(SHL_old)%>%
    dplyr::distinct(Datetime,.keep_all = TRUE)%>%
    arrange(Datetime)%>%
    mutate(`Nitrate + nitrate (µMol/L)` = as.numeric(`Nitrate + nitrate (µMol/L)`))

  importolddata("SMB","SMB2_NWIS_Data.dat.backup")
  SMB<<-SMB%>%
    bind_rows(SMB_old)%>%
    dplyr::distinct(Datetime,.keep_all = TRUE)%>%
    arrange(Datetime)

  SLM<<-SLM%>%
    mutate(`Depth (m)`=as.numeric(`Depth (m)`))

  importolddata_ala("ALA","Patched_Data/ALA_20260120_20260324.dat")
  ALA_old1<-ALA_old
  importolddata_ala("ALA","Patched_Data/ALA_20260325_20260604.dat")
  ALA_old2<-ALA_old
  importolddata_ala("ALA","Patched_Data/ALA_20260120_20260324(1).dat")
  ALA_old3<-ALA_old
  importolddata_ala("ALA","Patched_Data/Copy of ALA2_ExoDirect1Data.dat")
  ALA_old4<-ALA_old
  importolddata_ala("ALA","Patched_Data/ALA_patch_20260406_20260720.csv")
  ALA_old5<-ALA_old
  importolddata_ala("ALA","Patched_Data/ALA_patch_20260724_20260728.csv")
  ALA_old6<-ALA_old
  ALA_old<-bind_rows(ALA_old1,ALA_old2,ALA_old3,ALA_old4,ALA_old5,ALA_old6)

  ALA<<-ALA%>%
    bind_rows(ALA_old)%>%
    dplyr::distinct(Datetime,.keep_all = TRUE)%>%
    arrange(Datetime)%>%
    mutate("Nitrate + nitrate (µMol/L)" = NA)
}

#cache of the fully processed station data, so startup skips the download + cleanup pipeline
#(rsconnect bundles this file on deploy even though it's gitignored)
cache_file <- "cache/station_data.rds"

save_cache <- function() {
  #write to a temp file then rename it into place, so a session starting mid-save
  #never reads a half-written cache
  tmp_file <- paste0(cache_file, ".tmp")
  tryCatch({
    dir.create(dirname(cache_file), showWarnings = FALSE)
    saveRDS(list(SLM = SLM, SHL = SHL, SMB = SMB, ALA = ALA), tmp_file)
    if (!file.rename(tmp_file, cache_file)) stop("rename failed")
  }, error = function(e) {
    unlink(tmp_file)
    message("Could not write cache: ", conditionMessage(e))
  })
}

#returns TRUE if cached data was loaded into the global environment
load_cache <- function() {
  cached <- tryCatch(readRDS(cache_file), error = function(e) NULL)
  if (is.null(cached)) return(FALSE)
  list2env(cached, envir = .GlobalEnv)
  TRUE
}

#date limits for the date picker, recomputed whenever the data changes
set_date_bounds <- function() {
  all_dates <- c(SLM$Datetime, SHL$Datetime, SMB$Datetime, ALA$Datetime)
  startdate <<- min(as_date(all_dates), na.rm = TRUE)
  enddate   <<- max(as_date(all_dates), na.rm = TRUE) + 1
  data_through <<- max(all_dates, na.rm = TRUE)
}

#use the cache if there is one; otherwise build from scratch and cache it
if (!load_cache()) {
  load_all_stations()
  save_cache()
}
set_date_bounds()

#shared across sessions so every open view redraws when anyone updates the data
data_version <- reactiveVal(0)

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
      fluidRow(style = "margin-top: 8px; margin-bottom: 8px;",
        column(7, textOutput("dataAge")),
        column(5, actionButton("refreshData", "Update Data", class = "btn-sm", width = "100%"))
      ),
      selectInput("site", "Select Dataset:", choices = c("SLM", "SHL", "SMB","ALA"), selected = "SMB"),
      selectInput("y", "Y-axis:", choices =c("Chl-a (µg/L)", "DO (% saturation)","Temperature (°C)", "pH", "Turbidity (FNU)", "Salinity (PSU)","Nitrate + nitrate (µMol/L)","Depth (m)")),
      dateRangeInput("daterange", "Select Date Range:",
                     start = enddate - 14,
                     end = enddate,
                     min = startdate,
                     max = enddate,
                     format = "yyyy-mm-dd"),
      # quick presets, each ending at the most recent data
      div(style = "margin-top: -10px; margin-bottom: 15px;",
          actionButton("preset_7",   "7d",  class = "btn-xs"),
          actionButton("preset_14",  "14d", class = "btn-xs"),
          actionButton("preset_30",  "30d", class = "btn-xs"),
          actionButton("preset_90",  "90d", class = "btn-xs"),
          actionButton("preset_365", "1y",  class = "btn-xs"),
          actionButton("preset_all", "All", class = "btn-xs")),
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
    data_version()  # redraw after a data update
    switch(input$site,
           "SLM" = SLM,
           "SHL" = SHL,
           "SMB" = SMB,
           "ALA" = ALA)
  })

  output$siteMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldGrayCanvas') %>% 
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

  # date range presets
  for (n in c(7, 14, 30, 90, 365)) {
    local({
      days <- n
      observeEvent(input[[paste0("preset_", days)]], {
        updateDateRangeInput(session, "daterange", start = enddate - days, end = enddate)
      })
    })
  }

  # "All" covers the selected site's full record
  observeEvent(input$preset_all, {
    updateDateRangeInput(session, "daterange",
                         start = min(as_date(selected_data()$Datetime), na.rm = TRUE),
                         end = enddate)
  })

  output$dataAge <- renderText({
    data_version()
    paste("Data through", format(data_through, "%Y-%m-%d %H:%M"))
  })

  # pull the latest data from Google Drive, then refresh the cache
  observeEvent(input$refreshData, {
    ok <- withProgress(message = "Downloading latest data...", value = 0.5, {
      tryCatch({
        load_all_stations()
        TRUE
      }, error = function(e) {
        showNotification(paste("Update failed:", conditionMessage(e)), type = "error", duration = 10)
        FALSE
      })
    })

    if (!ok) {
      # a partial update can leave stations half-processed, so fall back to the cached copy
      load_cache()
      return()
    }

    save_cache()
    set_date_bounds()
    # move this view's end date forward to include the new data
    updateDateRangeInput(session, "daterange", end = enddate, max = enddate)
    data_version(data_version() + 1)
  })

  # other open sessions get the new date limit too
  observeEvent(data_version(), {
    updateDateRangeInput(session, "daterange", max = enddate)
  }, ignoreInit = TRUE)

  # keep button clicks out of bookmarks so they don't replay when a bookmark is opened
  setBookmarkExclude(c(paste0("preset_", c(7, 14, 30, 90, 365)), "preset_all", "refreshData"))

  
  output$dataPlot <- renderPlotly({
    data <- selected_data()
    
    req(input$daterange[1], input$daterange[2])
    start_date <- input$daterange[1]
    end_date   <- input$daterange[2]

    # inclusive of the whole end day
    filtered <- data %>%
      filter(between(as_date(Datetime), start_date, end_date))
    
    # --- Primary Plot (p1) ---
    p1 <- ggplot(filtered, aes(x = Datetime, y = .data[[input$y]])) +
      geom_line(color = "steelblue") +
      labs(x = NULL, y = input$y) +
      theme_minimal()
    
    if (!is.na(input$ymin) || !is.na(input$ymax)) {
      p1 <- p1 + coord_cartesian(ylim = c(input$ymin, input$ymax))
    }
    
    # --- Secondary Plot (p2) ---
    if (input$y2 != "None") {
      p2 <- ggplot(filtered, aes(x = Datetime, y = .data[[input$y2]])) +
        geom_line(color = "darkred") +
        labs(x = NULL, y = input$y2) +
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