#### Libraries ###
library(shiny)
library(shinyalert)
library(tidyverse)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(gridExtra)
source("../../utils/spatial.R")
options(shiny.maxRequestSize=10*1024^2)

#### Data ####
# tracking data
# tracking_dat <- readRDS("data-raw/Masi/xovis/2021-10-25.rds")
# test <- slice(tracking_dat, 1:1000)
# saveRDS(test, file = "test.rds")

# building
building <- terra::vect(paste0("../../data-raw/", "Masi", "/building/clinic-vector.gpkg"))
building_sf <- sf::st_as_sf(building, crs = NA)
st_crs(building_sf) <- NA
building_df <- fortify(building_sf)
building_df$col_group <- c(NA, "Clinic", "Clinic", "Clinic", NA, "Exit", "Seating area", "Entrance reception",
                           "Seating area", "Seating area", "Seating area", "Entrance treatment room", "Entrance treatment room",
                           "Exit", "Exit", NA)

# building plot
building_pl <- ggplot(building_df) +
  geom_sf(mapping = aes(fill = col_group), linewidth = .5, alpha = 0.2) +
  annotate("text", x = -13500, y = -1500, label = "Waiting room", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = -14000, y = -2000, label = "Seating area", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = -5000, y = -1000, label = "Seating area", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = 200, y = -1000, label = "Seating area", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = 2700, y = -1000, label = "Seating area", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = -11500, y = -6500, label = "Entrance / Main exit", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = -6000, y = -4700, label = "Reception", hjust = 0, size = 10 / cm(1), angle = 90, vjust = 0) +
  annotate("text", x = -4000, y = -7000, label = "Reception\nroom", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = -2500, y = -250, label = "Passage", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = 3000, y = -3500, label = "TB room", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = 3000, y = -7000, label = "Sputum\nroom", hjust = 0, size = 10 / cm(1), vjust = 0) +
  annotate("text", x = -1500, y = 900, label = "Care room", hjust = 0, size = 10 / cm(1), angle = 90, vjust = 0) +
  annotate("text", x = 1300, y = 900, label = "Care room", hjust = 0, size = 10 / cm(1), angle = 90, vjust = 0) +
  annotate("text", x = -12000, y = 900, label = "Emergency exit", hjust = 0, size = 10 / cm(1), angle = 90, vjust = 0) +
  annotate("text", x = -7000, y = 900, label = "Exit", hjust = 0, size = 10 / cm(1), angle = 90, vjust = 0) +
  scale_fill_discrete(na.translate = F) +
  scale_x_continuous(breaks = seq(-20000, 20000, 1000)) +
  scale_y_continuous(breaks = seq(-20000, 20000, 1000)) +
  guides(fill = "none")
building_pl

#### Functions ####

standing_height <- function(x, min_height = 1500) {
  quantile(x[x>1500], .95)
}

filter_oids <- function(df, oid, max_timediff, max_distance) {
  
  # current id
  df_i <- df %>%
    filter(patient_id == oid) %>%
    slice(n())
  
  # other potential ids
  df_other <- df %>%
    filter(is.na(tracking_end)) %>%
    group_by(patient_id) %>%
    filter(first(time) > df_i$time[1]) %>%
    ungroup()
  
  # other ids filtered for maximum timediff and distance
  df_other_feat <- df_other %>%
    group_by(patient_id) %>%
    slice(1) %>%
    ungroup() %>%
    merge(df_i, suffixes = c("_other", "_i"), by = NULL) %>%
    mutate(timediff = as.numeric(difftime(time_other, time_i, units = "secs")),
           distance = convert_dist(euclidean(x_other, x_i, y_other, y_i))) %>%
    filter(timediff <= max_timediff,
           distance <= max_distance)
  
  # paths of other ids
  df_other <- df_other %>%
    filter(patient_id %in% df_other_feat$obs_id_other) %>%
    left_join(df_other_feat %>% 
                rename(patient_id = patient_id_other) %>%
                dplyr::select(patient_id, timediff, distance), 
              by = "patient_id")
  
  return(df_other)
}

plot_other_oids <- function(pl_oid, df_i, df_other) {
  
  df_i_f <- df_i %>%
    slice(1)
  df_i_l <- df_i %>%
    slice(n())
  
  df_other_f <- df_other %>%
    group_by(patient_id) %>%
    slice(1)
  df_other_l <- df_other %>%
    group_by(patient_id) %>%
    slice(n())
  
  pointsize <- 4
  
  pl_oid +
    geom_path(data = df_i, mapping = aes(x = x, y = y), color = "black") +
    geom_point(data = df_i_f, mapping = aes(x = x, y = y), shape = 1, color = "black", size = pointsize) +
    geom_point(data = df_i_l, mapping = aes(x = x, y = y), shape = 13, color = "black", size = pointsize) +
    geom_path(data = df_other, mapping = aes(x = x, y = y, color = factor(patient_id))) +
    geom_point(data = df_other_f, mapping = aes(x = x, y = y, color = factor(patient_id)), shape = 1, fill = "white", size = pointsize) +
    geom_point(data = df_other_l, mapping = aes(x = x, y = y, color = factor(patient_id)), shape = 13, fill = "white", size = pointsize) +
    theme(legend.position = "bottom", legend.title = element_blank())
  
}

default_time <- 15
default_dist <- 5

#### Shiny UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Load file"),
      textOutput("date", container = span),
      br(),
      uiOutput("totalIDs"),
      br(),
      uiOutput("totEntranceIDs"),
      uiOutput("data"),
      uiOutput("currentTime"),
      uiOutput("currentDuration"),
      br(),
      sliderInput("timeInput", "Time", min = 0, max = 900, value = default_time, step = 15, post = "s"),
      sliderInput("distanceInput", "Distance", min = 0, max = 5, value = default_dist, step = 0.25, post = "m"),
      numericInput("linkID", "Link patient with ID", value = -1, min = 0, max = 100000, step = 1),
      actionButton("linkTrack", "Link track"),
      actionButton("unlinkTrack", "Unlink previous track"),
      br(),
      uiOutput("noLinks"),
      br(),
      selectInput("terminalInput", "Stop", choices = c("ID lost", "ID exited")),
      actionButton("endTrack", "End track"),
      br(),
      uiOutput("cleanTracks"),
      br(),
      h5("Save file to:"),
      uiOutput("saveto")
      ),
    mainPanel(plotOutput("clinic", inline = T),
              br(),
              tableOutput("displayedIDs"))
  )
)



#### Shiny Sever ####
server <- function(input, output, session) {
  
  # load data
  values <- reactiveValues(dat = NULL)
  observeEvent(input$fileInput, {
    # get file
    file <- input$fileInput
    if (is.null(file)) {return(NULL)}
    else {
      # read data
      values$dat <- readRDS(file$datapath) 
      # values$dat <- values$dat %>%
      #   group_by(obs_id) %>%
      #   mutate(duration = as.numeric(difftime(last(time), first(time), units = "secs")),
      #          distance = convert_dist( euclidean(first(x), last(x), first(y), last(y)) ),
      #          first_entered = first(is_entrance),
      #          has_exited = any(is_exit)) %>%
      #   ungroup() %>%
      #   filter(!(duration <= 3 & distance <= 1 & !first_entered & !has_exited) ) %>%
      #   dplyr::select(-distance, -duration)
      if (is.null(values$dat$tracking_end)) {
        values$dat$tracking_end <- NA
      }
      if (is.null(values$dat$patient_id)) {
        values$dat$patient_id <- values$dat$obs_id_new
      }
      
      # get entered IDs
      values$entered_ids <- values$dat %>%
        filter(is.na(tracking_end)) %>%
        group_by(patient_id) %>%
        summarize(entered = first(is_entrance)) %>%
        ungroup() %>%
        filter(entered) %>%
        dplyr::select(patient_id) %>%
        unlist() %>%
        unique()
      
      # number of clean tracks
      output$cleanTracks <- reactive({
        clTracks <- values$dat %>%
        filter(!is.na(tracking_end),
               tracking_end == "ID exited") %>%
        group_by(patient_id) %>%
        slice(1) %>%
        ungroup() %>%
        nrow() %>%
        as.character()
        paste("Clean tracks:", clTracks)
      })
      
      # total number of patient IDs
      output$totalIDs <- reactive({
        paste("Total no. of IDs:", as.character(n_distinct(values$dat$patient_id)))
      })
      
      # entrance IDs 
      output$totEntranceIDs <- reactive({
        paste("Remaining entered IDs:", as.character(length(values$entered_ids)))
      })
    }
    
    
  })
  
  # get date
  output$date <- reactive({
    req(input$fileInput)
    current_date <- as.character(as.Date(values$dat$time[1]))
    paste("Date:", current_date)
  })
  
  # create directory to save file
  save_dir <- reactive({
    req(input$fileInput)
    wd <- getwd()
    sd <- dirname(dirname(wd))
    sd <- paste(sd, "data-clean", "Masi", "patient-tracking-data", as.character(as.Date(values$dat$time[1])), sep = "/")
    return(paste0(sd, "/linked-patient-id-data.rds"))
  })
  output$saveto <- reactive({
    req(input$fileInput)
    save_dir()
  })
  
  # create input selection of entered IDs
  output$data <- renderUI({
    req(input$fileInput)
    selectizeInput("idInput", "Patient ID", choices = values$entered_ids, selected = min(values$entered_ids))
  })
  
  # get currently selected id
  current_id <- reactive({
    req(input$idInput)
    as.integer(input$idInput)
  })
  
  observeEvent(input$idInput, {
    updateSliderInput(inputId = "timeInput", value = default_time)
    updateSliderInput(inputId = "distanceInput", value = default_dist)
  })
  
  # get currently selected date
  current_time <- reactive({
    as.numeric(input$timeInput)
  })
  
  # get currently selected distance
  current_distance <- reactive({
    as.numeric(input$distanceInput)
  })
  
  # current ID
  dat_i <- reactive({
    req(input$idInput)
    dat_i <- filter(values$dat, patient_id == current_id())
  })
  
  
  # current track duration
  output$currentDuration <- reactive({
    req(input$idInput)
    dur <- values$dat %>% 
      filter(patient_id == current_id()) %>%
      summarize(duration = format(round(as.numeric(difftime(last(time), first(time), units = "mins")), 1), nsmall = 1)) %>%
      dplyr::select(duration) %>%
      unlist() %>%
      as.character()
    paste("Track duration:", dur, " min")
  })
    
  # possible IDs to link
  dat_other <- reactive({
    dat_other <- filter_oids(values$dat, current_id(), current_time(), current_distance())
  })
  
  # current number of links
  output$noLinks <- reactive({
    nL <- (n_distinct(values$dat$obs_id[values$dat$patient_id == current_id()]) - 1) %>% as.character()
    paste("No. of links:", nL)
  })
  
  # make new linkage
  observeEvent(input$linkTrack, {
    lid <- input$linkID
    displayed_ids <- dat_other()$`patient_id`
    name_linkage_success <- paste("ID", current_id(), "linked to", lid, ".")
    name_linkage_error <- paste("ID", lid, "was not on display. No linkage made. Try another ID!")
    if (is.null(displayed_ids)) {
      shinyalert("Error", name_linkage_error, type = "error", timer = 3000)
    } else if (!(lid %in% displayed_ids)) {
      shinyalert("Error", name_linkage_error, type = "error", timer = 3000)
    } else {
      shinyalert("Success", name_linkage_success, type = "success", timer = 1000)
      values$entered_ids <- values$entered_ids[values$entered_ids != lid]
      values$dat <- mutate(values$dat, patient_id = ifelse(patient_id == lid, current_id(), patient_id))
      base::saveRDS(object = values$dat, file = save_dir())
      updateSelectizeInput(inputId = "idInput", choices = values$entered_ids, selected = current_id())
      output$totEntranceIDs <- reactive({
        paste("Remaining entered IDs:", as.character(length(values$entered_ids)))
      })
    }
  })
  
  observeEvent(input$unlinkTrack, {
    last_id <- values$dat %>%
      filter(patient_id == current_id()) %>%
      slice(n()) %>%
      dplyr::select(obs_id_new) %>%
      unlist()
    values$dat <- mutate(values$dat, patient_id = ifelse(obs_id_new == last_id, last_id, patient_id))
    unlink_info <- paste("Unlinking ID", last_id, "from", current_id()) 
    shinyalert("Info", unlink_info, type = "info", timer = 1000)
    base::saveRDS(object = values$dat, file = save_dir())
    updateSliderInput(inputId = "timeInput", value = default_time)
    updateSliderInput(inputId = "distanceInput", value = default_dist)
    output$noLinks <- reactive({
      nL <- (n_distinct(values$dat$obs_id_new[values$dat$patient_id == current_id()]) - 1) %>% as.character()
      paste("No. of links:", nL)
    })
  })
  
  # end track of ID
  observeEvent(input$endTrack, {
    req(input$terminalInput)
    values$dat$tracking_end[values$dat$patient_id==current_id()] <- input$terminalInput
    base::saveRDS(object = values$dat, file = save_dir())
    name_end <- paste("Ending tracking of ID", current_id(), "because", input$terminalInput)
    shinyalert("Done.", name_end, type = "info", timer = 1000)
    values$entered_ids <- values$entered_ids[values$entered_ids != current_id()]
    updateSelectizeInput(inputId = "idInput", choices = values$entered_ids, selected = values$entered_ids[values$entered_ids>current_id()][1])
    output$totEntranceIDs <- reactive({
      paste("Remaining entered IDs:", as.character(length(values$entered_ids)))
    })
    updateSliderInput(inputId = "timeInput", value = default_time)
    updateSliderInput(inputId = "distanceInput", value = default_dist)
    output$cleanTracks <- reactive({
      clTracks <- values$dat %>%
      filter(!is.na(tracking_end),
             tracking_end == "ID exited") %>%
      group_by(patient_id) %>%
      slice(1) %>%
      ungroup() %>%
      nrow() %>%
      as.character()
      paste("Clean tracks:", clTracks)
    })
  })
  
  # current time
  output$currentTime <- reactive({
    req(input$idInput)
    ct <- format(tail(values$dat$time[values$dat$patient_id==current_id()], 1), "%H:%M:%S")
    paste("Current time:", ct)
  })
    
  # make and update plot
  output$clinic <- renderPlot({
    if (is.null(values$dat)) {
      building_pl 
    } else {
      plot_other_oids(building_pl, dat_i(), dat_other())
    } 
  }, height = 750, width = 1000)
  
  # make and update table
  output$displayedIDs <- renderTable({
    if (!is.null(values$dat)) {
      
      datCurrentID <- dat_i() %>%
        group_by(patient_id) %>%
        summarize(last_height = last(height),
                  max_height = standing_height(height),
                  duration = as.numeric(difftime(last(time), first(time), units = "mins"))) %>%
        ungroup() 
      
      datDisplayedIDs <- dat_other() %>%
        group_by(patient_id) %>%
        summarize(first_height = first(height),
                  max_height = standing_height(height),
                  duration = as.numeric(difftime(last(time), first(time), units = "mins")),
                  timediff = first(timediff),
                  distance = first(distance)) %>%
        ungroup() %>%
        mutate(current_id = datCurrentID$patient_id,
               current_id_last_height = datCurrentID$last_height,
               current_id_max_height = datCurrentID$max_height,
               current_id_duration = datCurrentID$duration) %>%
        mutate(across(c(current_id_last_height, current_id_max_height, first_height, max_height), ~ format(round(.x / 10), nsmall = 0)),
               timediff = format(timediff, nsmall = 0)) %>%
        dplyr::select(current_id, patient_id, current_id_last_height, first_height, current_id_max_height, max_height, duration, timediff, distance) %>%
        set_names("Pat. ID (P)", "Obs. ID (O)", "P: Last H", "O: First H", "P: Stand H", "O: Stand H", "O: Duration", "Time", "Dist.")
      
      return(datDisplayedIDs)
    }
  })
}





#### Shiny App ####
shinyApp(ui = ui, server = server)