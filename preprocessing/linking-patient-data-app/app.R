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

#### Settings ####
# graph parameters
pointsize <- 4

# shiny parameters
plot_wait_time <- 1000 # wait 1s until plot and table are generated

# search parameters 
time_choices <- c(10, 20, 30, 60, 120, 180, 300, 600, 900)
dist_choices <- c(0.5, 1, 3, 5)
height_choices <- c(10, 20, 50, 100)
default_time <- 30
default_dist <- max(dist_choices)
default_height <- min(height_choices)
default_time_sit <- 300
default_dist_sit <- min(dist_choices)
default_height_sit <- max(height_choices)

#### Building ####
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


#### Functions ####

standing_height <- function(x, min_height = 1500) {
  if (is.null(x) | all(is.na(x))) {
    return(NA)
  } else if (all(x < min_height)) {
    return(max(x))
  } else {
    quantile(x[x>=min_height], .9)
  }
}

filter_oids <- function(df, oid, max_timediff, max_distance, max_heightdiff, nextIDs = T) {
  
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
      filter(is.na(tracking_end)) %>%
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


plot_ids <- function(pl, df_i, df_pos, df_alt) {
  
  if (!is.null(df_i)) {
    if (nrow(df_i) > 0) {
      # df_i first and last track
      df_i_f <- slice(df_i, 1) 
      df_i_l <- slice(df_i, n()) 
      
      # df_i plot
      pl <- pl +
        geom_path(data = df_i, mapping = aes(x = x, y = y), color = "black") +
        geom_point(data = df_i_f, mapping = aes(x = x, y = y), shape = 1, color = "black", size = pointsize) +
        geom_point(data = df_i_l, mapping = aes(x = x, y = y), shape = 13, color = "black", size = pointsize)
    }
  }
  
  if (!is.null(df_pos)) {
    if (nrow(df_pos) > 0) {
      df_pos_f <- df_pos %>%
        group_by(patient_id) %>%
        slice(1)
      df_pos_l <- df_pos %>%
        group_by(patient_id) %>%
        slice(n())
      
      pl <- pl +
        geom_path(data = df_pos, mapping = aes(x = x, y = y, color = factor(patient_id))) +
        geom_point(data = df_pos_f, mapping = aes(x = x, y = y, color = factor(patient_id)), shape = 1, fill = "white", size = pointsize) +
        geom_point(data = df_pos_l, mapping = aes(x = x, y = y, color = factor(patient_id)), shape = 13, fill = "white", size = pointsize) +
        theme(legend.position = "bottom", legend.title = element_blank())
    }
  }
  
  if (!is.null(df_alt)) {
    if (nrow(df_alt) > 0) {
      df_alt_f <- df_alt %>%
        group_by(patient_id) %>%
        slice(1)
      df_alt_l <- df_alt %>%
        group_by(patient_id) %>%
        slice(n())
      
      pl  <- pl +
        geom_path(data = df_alt, mapping = aes(x = x, y = y, group = factor(patient_id)), alpha = .25) +
        geom_text_repel(data = df_alt_l, mapping = aes(x = x, y = y, group = factor(patient_id), label = patient_id), alpha = .66, size = 10 / cm(1)) +
        geom_point(data = df_alt_f, mapping = aes(x = x, y = y, group = factor(patient_id)), shape = 1, fill = "white", size = pointsize, alpha = .5) +
        geom_point(data = df_alt_l, mapping = aes(x = x, y = y, group = factor(patient_id)), shape = 13, fill = "white", size = pointsize, alpha = .5)
    }
  }
  
  return(pl)
}

table_ids <- function(df_i, df_pos) {
  datCurrentID <- df_i %>%
    group_by(patient_id) %>%
    summarize(last_height = last(height),
              stand_height = standing_height(height)) %>%
    ungroup()
  
  datDisplayedIDs <- df_pos %>%
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
    arrange(timediff) %>%
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


#### Shiny UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Load file"),
      textOutput("date", container = span),
      uiOutput("totalIDs"),
      uiOutput("cleanTracks"),
      uiOutput("totEntranceIDs"),
      br(),
      selectizeInput("ID", "Patient ID", choices = NULL, multiple = F, options = list(maxItems = 1)),
      actionButton("nextID", "Next ID"),
      actionButton("prevID", "Previous ID"),
      actionButton("nextEnteredID", "Next Entrance"),
      actionButton("prevEnteredID", "Previous Entrance"),
      br(),
      br(),
      uiOutput("currentDuration"),
      uiOutput("noLinks"),
      br(),
      radioButtons("quickInput", "Quick filter", choices = list("Moving" = 1, "Sitting" = 2), inline = T),
      sliderTextInput("time", "Time", from_min = min(time_choices), to_max = max(time_choices), selected = default_time, 
                      choices = time_choices, grid = T, post = "sec"),
      sliderTextInput("distance", "Distance", from_min = min(dist_choices), to_max = max(dist_choices), selected = default_dist, 
                      choices = dist_choices, grid = T, post = "m"),
      sliderTextInput("height", "Height", from_min = min(height_choices), to_max = max(height_choices), selected = default_height, 
                      choices = height_choices, grid = T, post = "cm"),
      selectizeInput("altID", "Show alternatives for ID", choices = NULL, options = list(maxItems = 1)),
      selectizeInput("posID", "Link with ID", choices = NULL, options = list(maxItems = 1)),
      actionButton("linkTrack", "Link IDs"),
      actionButton("unlinkTrack", "Unlink previous ID"),
      br(),
      br(),
      selectInput("terminalInput", "Stop linking", choices = c("ID lost", "ID exited")),
      actionButton("endTrack", "End track"),
      h5("Save file to"),
      textOutput("saveto")
      ),
    mainPanel(plotOutput("clinic", inline = T),
              br(),
              tableOutput("displayedIDs"))
  )
)



#### Shiny Sever ####
server <- function(input, output, session) {
  
  #### Load data ####
  values <- reactiveValues(dat = NULL, all_ids = NULL, entered_ids = NULL)
  observeEvent(input$file, {
    
    # get file
    file <- input$file
    
    # read data
    values$dat <- readRDS(file$datapath) 
    
    # add variable to store tracking end info
    if (is.null(values$dat$tracking_end)) {
      values$dat$tracking_end <- NA
    }
    
    # add patient id variable
    if (is.null(values$dat$patient_id)) {
      values$dat$patient_id <- values$dat$obs_id_new
    }
    
    values$dat <- values$dat %>%
      mutate(across(c(patient_id, obs_id, obs_id_new), ~ as.integer(.x))) 
    
    # get IDs to match
    match_ids <- values$dat %>%
      filter(is.na(tracking_end)) %>%
      group_by(patient_id) %>%
      summarize(entered = first(is_entrance)) %>%
      ungroup() 
    
    values$all_ids <- sort(unique(match_ids$patient_id))
    values$entered_ids <- sort(unique(filter(match_ids, entered)$patient_id))
    
    # update patient ID selection
    updateSelectizeInput(session, "ID", choices = values$all_ids, selected = min(values$all_ids), server = T)
    
    # get date
    output$date <- renderText({paste("Date:", as.character(as.Date(values$dat$time[1])))})
    
    # create directory to save file
    values$save_dir <- paste(dirname(dirname(getwd())), 
                             "data-clean", "Masi", "patient-tracking-data",
                             as.character(as.Date(values$dat$time[1])), 
                             sep = "/")
    if (!dir.exists(values$save_dir)) { dir.create(values$save_dir) }
    values$save_file <- paste0(values$save_dir, "/linked-patient-id-data.rds")
    
    # display directory where file is stored
    output$saveto <- renderText({ values$save_file })
    
  })
  
  
  #### File info ####
  # total number of patient IDs
  output$totalIDs <- reactive({
    if (!is.null(values$dat)) {
      paste("Total number of patient IDs:", as.character(n_distinct(values$dat$patient_id)))
    }
  })
  
  # entrance IDs 
  output$totEntranceIDs <- reactive({
    if (!is.null(values$dat)) {
      paste("Unfinished IDs starting at entrance:", as.character(length(values$entered_ids)))
    }
  })
  
  # number of clean tracks
  output$cleanTracks <- reactive({
    if (!is.null(values$dat)) {
      clTracks <- values$dat %>%
        filter(!is.na(tracking_end),
               tracking_end == "ID exited") %>%
        group_by(patient_id) %>%
        slice(1) %>%
        ungroup() %>%
        nrow() %>%
        as.character()
      paste("Clean tracks:", clTracks)
    }
  })
  
  
  #### Parameters ####
  
  # quick filter
  observeEvent(input$quickInput, {
    if (input$quickInput == 1) {
      updateSliderTextInput(session, inputId = "time", selected = default_time)
      updateSliderTextInput(session, inputId = "distance", selected = default_dist)
      updateSliderTextInput(session, inputId = "height", selected = default_height)
    } else {
      updateSliderTextInput(session, inputId = "time", selected = default_time_sit)
      updateSliderTextInput(session, inputId = "distance", selected = default_dist_sit)
      updateSliderTextInput(session, inputId = "height", selected = default_height_sit)
    }
  })
  
  # select next entered ID
  observeEvent(input$nextID, {
    req(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = values$all_ids[values$all_ids>as.numeric(input$ID)][1], server = T)
    updateRadioButtons(session, "quickInput", selected = 1)
  })
  
  # select next entered ID
  observeEvent(input$nextEnteredID, {
    req(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = values$entered_ids[values$entered_ids>as.numeric(input$ID)][1], server = T)
    updateRadioButtons(session, "quickInput", selected = 1)
  })
  
  # select previous entered ID
  observeEvent(input$prevID, {
    req(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = tail(values$all_ids[values$all_ids<as.numeric(input$ID)],1), server = T)
    updateRadioButtons(session, "quickInput", selected = 1)
  })
  
  # select previous entered ID
  observeEvent(input$prevEnteredID, {
    req(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = tail(values$entered_ids[values$entered_ids<as.numeric(input$ID)],1), server = T)
    updateRadioButtons(session, "quickInput", selected = 1)
  })
  
  
  #### ID Info ####
  
  # current track duration
  output$currentDuration <- reactive({
    req(input$ID)
    start_time <- head(values$dat$time[values$dat$patient_id==as.numeric(input$ID)], 1)
    end_time <- tail(values$dat$time[values$dat$patient_id==as.numeric(input$ID)], 1)
    duration <- format(round(as.numeric(difftime(end_time, start_time, units = "min")), 1), nsmall = 1)
    start_time <- format(start_time, "%H:%M:%S")
    end_time <- format(end_time, "%H:%M:%S")
    paste("Track from ", start_time, " to ", end_time, " (", duration,  " min)")
  })
  
  # current number of links
  output$noLinks <- reactive({
    paste("No. of IDs linked:", as.character(n_distinct(values$dat$obs_id[values$dat$patient_id == as.numeric(input$ID)]) - 1))
  })
  
  
  #### Data ####
  
  # ID
  dat_i <- reactive({
    req(input$ID)
    filter(values$dat, patient_id == as.numeric(input$ID))
  })
  
  # possible links
  dat_pos <- reactive({
    req(input$ID)
    posDF <- filter_oids(values$dat, as.numeric(input$ID), input$time, input$distance, input$height)
    posIDs <- sort(unique(posDF$patient_id))
    updateSelectizeInput(session, inputId = "posID", choices = posIDs)
    updateSelectizeInput(session, inputId = "altID", choices = posIDs)
    return(posDF)
  }) 
  
  # alternatives to possible links
  dat_alt <- reactive({
    req(input$ID)
    req(dat_i)
    req(dat_pos)
    req(input$altID)
    filter_oids(values$dat, input$altID, input$time, input$distance, input$height, nextIDs = F) %>%
      filter(patient_id != as.numeric(input$ID))
  }) 
  
  plot_dat <- reactive({
    tryCatch({
      dat_pos()
      plot_ids(building_pl, dat_i(), dat_pos(), dat_alt())
    },
    shiny.silent.error = function(e) {
      plot_ids(building_pl, dat_i(), NULL, NULL)
    })
  }) %>% debounce(plot_wait_time)
  
  
  table_dat <- reactive({
    req(dat_i)
    req(dat_pos)
    table_ids(dat_i(), dat_pos())
  }) %>% debounce(plot_wait_time)
  
  
  #### New link ####
  # make link
  observeEvent(input$linkTrack, {
    lid <- input$posID
    name_linkage_success <- paste("ID", input$ID, "linked to", lid, ".")
    shinyalert("Success", name_linkage_success, type = "success", timer = 1000)
    values$entered_ids <- values$entered_ids[values$entered_ids != lid]
    values$all_ids <- values$all_ids[values$all_ids != lid]
    values$dat <- mutate(values$dat, patient_id = ifelse(patient_id == lid, as.numeric(input$ID), patient_id))
    base::saveRDS(object = values$dat, file = values$save_file)
    updateSelectizeInput(inputId = "ID", choices = values$all_ids, selected = input$ID, server = T)
  })
  
  #### Remove link ####
  observeEvent(input$unlinkTrack, {
    last_id <- values$dat %>%
      filter(patient_id == as.numeric(input$ID)) %>%
      slice(n()) %>%
      dplyr::select(obs_id_new) %>%
      unlist()
    if (values$dat$is_entrance[values$dat$obs_id_new==last_id][1]) {
      values$entered_ids <- c(values$entered_ids, last_id)
    }
    values$all_ids <- c(values$all_ids, last_id)
    values$dat <- mutate(values$dat, patient_id = ifelse(obs_id_new == last_id, last_id, patient_id))
    base::saveRDS(object = values$dat, file = values$save_file)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = input$ID, server = T)
    shinyalert("Info", paste("Unlinking ID", last_id, "from", input$ID) , type = "info", timer = 1000)
  })
  
  
  #### End track ####
  observeEvent(input$endTrack, {
    values$dat$tracking_end[values$dat$patient_id==as.numeric(input$ID)] <- input$terminalInput
    base::saveRDS(object = values$dat, file = values$save_file)
    shinyalert("Done.", paste("Ending tracking of ID", input$ID, "because", input$terminalInput), type = "info", timer = 1000)
    values$entered_ids <- values$entered_ids[values$entered_ids != as.numeric(input$ID)]
    values$all_ids <- values$all_ids[values$all_ids != as.numeric(input$ID)]
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = values$all_ids[values$all_ids>as.numeric(input$ID)][1], server = T)
    updateRadioButtons(session, "quickInput", selected = 1)
    updateSliderTextInput(session, inputId = "time", selected = default_time)
    updateSliderTextInput(session, inputId = "distance", selected = default_dist)
    updateSliderTextInput(session, inputId = "height", selected = default_height)
  })
    
  #### Plot ####
  output$clinic <- renderPlot({
    plot_dat()
  }, height = 750, width = 1000)
  
  
  #### Table ####
  output$displayedIDs <- renderTable({
    table_dat()
  }, sanitize.text.function = function(x) x)
  
}





#### Shiny App ####
shinyApp(ui = ui, server = server)
