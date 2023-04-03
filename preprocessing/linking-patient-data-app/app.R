#### Libraries ###
library(shiny)
library(shinyalert)
library(shinyWidgets)
library(tidyverse)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(gridExtra)
library(ggrepel)
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
  if (all(x < min_height)) {
    return(max(x))
  } else {
    quantile(x[x>=min_height], .9)
  }
}

filter_oids <- function(df, oid, max_timediff, max_distance, max_heightdiff, nextIDs = T, exclude = NULL) {
  
  if (nextIDs) {
    # current id
    df_i <- df %>%
      filter(patient_id == oid) %>%
      slice(n()) %>%
      dplyr::select(x, y, time, height)
    
    # other potential ids
    df_other <- df %>%
      filter(is.na(tracking_end)) %>%
      group_by(patient_id) %>%
      filter(first(time) > df_i$time) %>%
      ungroup()
    
    # other ids filtered for maximum timediff and distance
    df_other_feat <- df_other %>%
      group_by(patient_id) %>%
      slice(1) %>%
      ungroup()
    
  } else {
    
    # current id
    df_i <- df %>%
      filter(patient_id == oid) %>%
      slice(1) %>%
      dplyr::select(x, y, time, height)
    
    # other potential ids
    df_other <- df %>%
      filter(is.na(tracking_end),
             patient_id != exclude) %>%
      group_by(patient_id) %>%
      filter(last(time) < df_i$time) %>%
      ungroup()
    
    # other ids filtered for maximum timediff and distance
    df_other_feat <- df_other %>%
      group_by(patient_id) %>%
      slice(n()) %>%
      ungroup()
  }
  
  
  df_other_feat <- df_other_feat %>%
    base::merge(df_i, suffixes = c("", "_i"), by = NULL) %>%
    mutate(timediff = abs(as.numeric(difftime(time, time_i, units = "secs"))), 
           distance = convert_dist(euclidean(x, x_i, y, y_i)),
           heightdiff = abs(height - height_i) / 10) %>%
    filter(timediff <= max_timediff,
           distance <= max_distance,
           heightdiff <= max_heightdiff)
  
  # paths of other ids
  df_other <- df_other %>%
    filter(patient_id %in% df_other_feat$obs_id) %>%
    left_join(dplyr::select(df_other_feat, patient_id, timediff, distance), by = "patient_id")
  
  return(df_other)
}

plot_other_oids <- function(pl_oid, df_i, df_other, df_rest) {
  
  # graph parameters
  pointsize <- 4
  
  # df_i first and last track
  df_i_f <- df_i %>%
    slice(1)
  df_i_l <- df_i %>%
    slice(n())
  
  # df_i plot
  pl <- pl_oid +
    geom_path(data = df_i, mapping = aes(x = x, y = y), color = "black") +
    geom_point(data = df_i_f, mapping = aes(x = x, y = y), shape = 1, color = "black", size = pointsize) +
    geom_point(data = df_i_l, mapping = aes(x = x, y = y), shape = 13, color = "black", size = pointsize)
  
  
  # add df_other
  if (nrow(df_other) > 0) {
    df_other_f <- df_other %>%
      group_by(patient_id) %>%
      slice(1)
    df_other_l <- df_other %>%
      group_by(patient_id) %>%
      slice(n())
    
    pl <- pl +
      geom_path(data = df_other, mapping = aes(x = x, y = y, color = factor(patient_id))) +
      geom_point(data = df_other_f, mapping = aes(x = x, y = y, color = factor(patient_id)), shape = 1, fill = "white", size = pointsize) +
      geom_point(data = df_other_l, mapping = aes(x = x, y = y, color = factor(patient_id)), shape = 13, fill = "white", size = pointsize) +
      theme(legend.position = "bottom", legend.title = element_blank())
  }
  
  # add df_rest
  if (nrow(df_rest) > 0) {
    df_rest_f <- df_rest %>%
      group_by(patient_id) %>%
      slice(1)
    df_rest_l <- df_rest %>%
      group_by(patient_id) %>%
      slice(n())
    
    pl  <- pl +
      geom_path(data = df_rest, mapping = aes(x = x, y = y, group = factor(patient_id)), alpha = .1) +
      geom_text_repel(data = df_rest_l, mapping = aes(x = x, y = y, group = factor(patient_id), label = patient_id), alpha = .25, size = 10 / cm(1)) +
      geom_point(data = df_rest_f, mapping = aes(x = x, y = y, group = factor(patient_id)), shape = 1, fill = "white", size = pointsize, alpha = .2) +
      geom_point(data = df_rest_l, mapping = aes(x = x, y = y, group = factor(patient_id)), shape = 13, fill = "white", size = pointsize, alpha = .2)
  }
  
  return(pl)
}

default_time <- 30
default_dist <- 5
default_height <- 10

#### Shiny UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Load file"),
      h5("Overview"),
      textOutput("date", container = span),
      uiOutput("totalIDs"),
      uiOutput("cleanTracks"),
      uiOutput("totEntranceIDs"),
      br(),
      uiOutput("data"),
      uiOutput("currentTime"),
      uiOutput("currentDuration"),
      uiOutput("noLinks"),
      br(),
      actionButton("nextEnteredID", "Next entrance ID"),
      br(),
      br(),
      radioButtons("quickInput", "Quick filter", choices = list("Moving" = 1, "Sitting" = 2), inline = T),
      sliderTextInput("timeInput", "Time", from_min = 10, to_max = 900, selected = default_time, 
                      choices = c(10, 20, 30, 60, 120, 180, 300, 600, 900), grid = T, post = "sec"),
      sliderTextInput("distanceInput", "Distance", from_min = 0.5, to_max = 5, selected = default_dist, 
                      choices = c(0.5, 1, 3, 5), grid = T, post = "m"),
      sliderTextInput("heightInput", "Height", from_min = 10, to_max = 50, selected = default_height, 
                      choices = c(10, 20, 50), grid = T, post = "cm"),
      selectizeInput("altIDs", "Show alternatives for possible link to ID", choices = NULL, options = list(maxItems = 1)),
      selectizeInput("posIDs", "Possible link IDs", choices = NULL, options = list(maxItems = 1)),
      actionButton("linkTrack", "Link track"),
      actionButton("unlinkTrack", "Unlink previous track"),
      br(),
      br(),
      selectInput("terminalInput", "Stop linking", choices = c("ID lost", "ID exited")),
      actionButton("endTrack", "End track"),
      h5("Save file to"),
      uiOutput("saveto")
      ),
    mainPanel(plotOutput("clinic", inline = T),
              br(),
              tableOutput("displayedIDs"))
  )
)



#### Shiny Sever ####
server <- function(input, output, session) {
  
  #### Load data ####
  values <- reactiveValues(dat = NULL)
  observeEvent(input$fileInput, {
    # get file
    file <- input$fileInput
    if (is.null(file)) {return(NULL)}
    else {
      # read data
      values$dat <- readRDS(file$datapath) 
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
      
      # get all minus entered IDs 
      values$all_ids <- values$dat %>%
        filter(is.na(tracking_end)) %>%
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
        paste("Total number of patient IDs:", as.character(n_distinct(values$dat$patient_id)))
      })
      
      # entrance IDs 
      output$totEntranceIDs <- reactive({
        paste("Unfinished IDs starting at entrance:", as.character(length(values$entered_ids)))
      })
    }
    
    
  })
  
  #### Data info ####
  
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
    if (!dir.exists(sd)) {
      dir.create(sd)
    }
    return(paste0(sd, "/linked-patient-id-data.rds"))
  })
  
  # display directory where file is stored
  output$saveto <- reactive({
    req(input$fileInput)
    save_dir()
  })
  
  
  #### Current ID ####
  
  # create input selection of IDs
  output$data <- renderUI({
    req(input$fileInput)
    selectizeInput("idInput", "Patient ID", choices = values$all_ids, selected = min(values$entered_ids), multiple = F, options = list(maxItems = 1))
  })
  
  # current id
  current_id <- reactive({
    req(input$idInput)
    as.integer(input$idInput)
  })
  
  # current data 
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
  
  # current time
  output$currentTime <- reactive({
    req(input$idInput)
    ct <- format(tail(values$dat$time[values$dat$patient_id==current_id()], 1), "%H:%M:%S")
    paste("Current time:", ct)
  })
    
  #### Update Inputs ####
  # select next entered ID
  observeEvent(input$nextEnteredID, {
    updateSelectizeInput(inputId = "idInput", selected = values$entered_ids[values$entered_ids>current_id()][1])
  })
  
  # update inputs based on id
  observeEvent(input$idInput, {
    updateRadioButtons(session, "quickInput", selected = 1)
    updateSliderTextInput(session, inputId = "timeInput", selected = default_time)
    updateSliderTextInput(session, inputId = "distanceInput", selected = default_dist)
    updateSliderTextInput(session, inputId = "heightInput", selected = default_height)
  })
  
  # get currently selected date
  current_time <- reactive({
    as.numeric(input$timeInput)
  })
  
  # get currently selected distance
  current_distance <- reactive({
    as.numeric(input$distanceInput)
  })
  
  # get currently selected height
  current_height <- reactive({
    as.numeric(input$heightInput)
  })
  
  #### Quick filter ####
  observeEvent(input$quickInput, {
    if (input$quickInput == 1) {
      updateSliderTextInput(session, inputId = "timeInput", selected = default_time)
      updateSliderTextInput(session, inputId = "distanceInput", selected = default_dist)
      updateSliderTextInput(session, inputId = "heightInput", selected = default_height)
    } else {
      updateSliderTextInput(session, inputId = "timeInput", selected = 300)
      updateSliderTextInput(session, inputId = "distanceInput", selected = 0.5)
      updateSliderTextInput(session, inputId = "heightInput", selected = 50)
    }
  })
  
  #### Possible links ####
  # data of possible links
  dat_other <- reactive({
    other_df <- filter_oids(values$dat, current_id(), current_time(), current_distance(), current_height())
    other_ids <- unique(other_df$patient_id)
    updateSelectizeInput(inputId = "posIDs", choices = other_ids)
    updateSelectizeInput(inputId = "altIDs", choices = other_ids)
    return(other_df)
  })
  
  # data for alternatives to possible links
  dat_alt <- reactive({
    req(input$idInput)
    req(input$altIDs)
    filter_oids(values$dat, input$altIDs, current_time(), current_distance(), current_height(), nextIDs = F, exclude = current_id())
  })
  
  #### New link ####
  # make link
  observeEvent(input$linkTrack, {
    lid <- input$posIDs
    name_linkage_success <- paste("ID", current_id(), "linked to", lid, ".")
    shinyalert("Success", name_linkage_success, type = "success", timer = 1000)
    values$entered_ids <- values$entered_ids[values$entered_ids != lid]
    values$all_ids <- values$all_ids[values$all_ids != lid]
    values$dat <- mutate(values$dat, patient_id = ifelse(patient_id == lid, current_id(), patient_id))
    base::saveRDS(object = values$dat, file = save_dir())
    updateSelectizeInput(inputId = "idInput", choices = values$all_ids, selected = current_id())
    output$totEntranceIDs <- reactive({
      paste("Unfinished IDs starting at entrance:", as.character(length(values$entered_ids)))
    })
    output$totIDs <- reactive({
      paste("Total number of patient IDs:", as.character(length(values$all_ids)))
    })
  })
  
  # current number of links
  output$noLinks <- reactive({
    nL <- (n_distinct(values$dat$obs_id[values$dat$patient_id == current_id()]) - 1) %>% as.character()
    paste("No. of IDs linked:", nL)
  })
  
  #### Remove link ####
  observeEvent(input$unlinkTrack, {
    last_id <- values$dat %>%
      filter(patient_id == current_id()) %>%
      slice(n()) %>%
      dplyr::select(obs_id_new) %>%
      unlist()
    if (values$dat$is_entrance[values$dat$obs_id_new==last_id][1]) {
      values$entered_ids <- c(values$entered_ids, last_id)
    }
    values$all_ids <- c(values$all_ids, last_id)
    updateSelectizeInput(inputId = "idInput", choices = values$all_ids, selected = current_id())
    values$dat <- mutate(values$dat, patient_id = ifelse(obs_id_new == last_id, last_id, patient_id))
    unlink_info <- paste("Unlinking ID", last_id, "from", current_id()) 
    shinyalert("Info", unlink_info, type = "info", timer = 1000)
    base::saveRDS(object = values$dat, file = save_dir())
    output$noLinks <- reactive({
      nL <- (n_distinct(values$dat$obs_id_new[values$dat$patient_id == current_id()]) - 1) %>% as.character()
      paste("No. of IDs linked:", nL)
    })
  })
  
  
  #### End track ####
  observeEvent(input$endTrack, {
    req(input$terminalInput)
    values$dat$tracking_end[values$dat$patient_id==current_id()] <- input$terminalInput
    base::saveRDS(object = values$dat, file = save_dir())
    name_end <- paste("Ending tracking of ID", current_id(), "because", input$terminalInput)
    shinyalert("Done.", name_end, type = "info", timer = 1000)
    values$entered_ids <- values$entered_ids[values$entered_ids != current_id()]
    values$all_ids <- values$all_ids[values$all_ids != current_id()]
    updateSelectizeInput(inputId = "idInput", choices = values$all_ids, selected = values$all_ids[values$all_ids>current_id()][1])
    output$totEntranceIDs <- reactive({ paste("Unfinished IDs starting at entrance:", as.character(length(values$entered_ids))) })
    output$totIDs <- reactive({ paste("Total number of patient IDs:", as.character(length(values$all_ids))) })
    updateRadioButtons(session, "quickInput", selected = 1)
    updateSliderTextInput(session, inputId = "timeInput", selected = default_time)
    updateSliderTextInput(session, inputId = "distanceInput", selected = default_dist)
    updateSliderTextInput(session, inputId = "heightInput", selected = default_height)
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
    
  #### Plot ####
  output$clinic <- renderPlot({
    if (is.null(values$dat)) {
      building_pl 
    } else {
      if (isTruthy(input$altIDs)) {
        plot_other_oids(building_pl, dat_i(), dat_other(), dat_alt())
      } else {
        plot_other_oids(building_pl, dat_i(), dat_other(), data.frame())
      }
    }
  }, height = 750, width = 1000)
  
  #### Table ####
  output$displayedIDs <- renderTable({
    if (!is.null(values$dat)) {
      
      datCurrentID <- dat_i() %>%
        group_by(patient_id) %>%
        summarize(last_height = last(height),
                  stand_height = standing_height(height)) %>%
        ungroup()
      
      datDisplayedIDs <- dat_other() %>%
        group_by(patient_id) %>%
        summarize(first_height = first(height),
                  stand_height = standing_height(height),
                  duration = as.numeric(difftime(last(time), first(time), units = "mins")),
                  timediff = first(timediff),
                  distance = first(distance)) %>%
        ungroup() %>%
        mutate(current_id = datCurrentID$patient_id,
               current_id_last_height = datCurrentID$last_height,
               current_id_stand_height = datCurrentID$stand_height) %>%
        mutate(last_height_diff = first_height - current_id_last_height,
               stand_height_diff = stand_height - current_id_stand_height) %>%
        mutate(across(contains("height"), ~ format(round(.x / 10), nsmall = 0)),
               timediff = format(timediff, nsmall = 0),
               duration = format(round(duration, 2), nsmall = 1),
               distance = format(round(distance, 1), nsmall = 1)) %>%
        mutate(last_height_comb = paste0(last_height_diff, " (", first_height, ", ", current_id_last_height, ")"),
               stand_height_comb = paste0(stand_height_diff, " (", stand_height, ", ", current_id_stand_height, ")")) %>%
        dplyr::select(current_id, patient_id, last_height_comb, stand_height_comb, duration, timediff, distance) %>%
        set_names("Pid", "Oid", "Last heightdiff (Oid, Pid) [cm]", "Stand heightdiff (Oid, Pid) [cm]", "Duration Oid [min]", "Timediff [sec]", "Distance [m]") %>%
        mutate_all(as.character) 
      
      n_possibleIDs <- nrow(datDisplayedIDs)
      
      if (n_possibleIDs > 0) {
        for (i in 1:n_possibleIDs) {
          col <- scales::hue_pal()(n_possibleIDs)[i]
          for (k in 2:6) {
            datDisplayedIDs[i,k] <- paste0('<span style="color:', col, '">', datDisplayedIDs[i,k], "</span>")
          }
        }
      }
      
      return(datDisplayedIDs)
    }
  }, sanitize.text.function = function(x) x)
}





#### Shiny App ####
shinyApp(ui = ui, server = server)
