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
time_choices <- c(30, 60, 300, 600, 900)
dist_choices <- c(0.5, 1, 3, 5)
height_choices <- c(10, 20, 50, 100)
short_move_time <- 30
short_move_dist <- 5
short_move_height <- 10
long_move_time <- 60
long_move_dist <- 5
long_move_height <- 20
short_sit_time <- 60
short_sit_dist <- 0.5
short_sit_height <- 100
long_sit_time <- 900
long_sit_dist <- 1
long_sit_height <- 100
all_time <- max(time_choices)
all_dist <- max(dist_choices)
all_height <- max(height_choices)


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

filter_other_feat <- function(df, oid, nextIDs = 1) {
  
  df <- df %>%
    filter(is.na(tracking_end))
  
  if (nextIDs == 1) {
    
    # current id
    df_i <- df %>%
      filter(patient_id == oid) %>%
      slice(n()) 
    
    # pre time filter conditions
    earliest_start <- df_i$time[1] - lubridate::seconds(1)
    latest_start <- df_i$time[1] + lubridate::seconds(max(time_choices))
    
    # other potential ids
    df_other <- df %>%
      filter(patient_id != oid) %>%
      group_by(patient_id) %>%
      slice(1) %>% 
      ungroup() %>%
      filter(patient_id > oid, 
             between(time, earliest_start, latest_start)) 
    
  } else {
    
    # current id
    df_i <- df %>%
      filter(patient_id == oid) %>%
      slice(1) 
    
    # pre time filter conditions
    latest_end <- df_i$time[1] + lubridate::seconds(1)
    earliest_end <- df_i$time[1] - lubridate::seconds(max(time_choices))
    
    # other potential ids
    df_other <- df %>%
      filter(patient_id != oid) %>%
      group_by(patient_id) %>%
      slice(n()) %>%
      ungroup() %>%
      filter(patient_id < oid, 
             between(time, earliest_end, latest_end))
    
  }
  
  df_other <- base::merge(df_other, df_i, suffixes = c("_other", "_i"), by = NULL)
  
  return(df_other)
  
}

compute_other_feat <- function(df_other) {
  df_other %>%
    mutate(timediff = abs(as.numeric(difftime(time_other, time_i, units = "secs"))), 
           distance = convert_dist(euclidean(x_other, x_i, y_other, y_i)),
           heightdiff = abs(height_other - height_i) / 10)
}

subset_other_feat <- function(df_other, max_timediff, max_distance, max_heightdiff) {
  df_other %>%
    filter(timediff <= max_timediff,
           distance <= max_distance,
           heightdiff <= max_heightdiff)
}


plot_ids <- function(pl, df_i, df_pos, df_alt, direction) {
  
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
      
      df_pos <- mutate(df_pos, patient_id = factor(patient_id))
      if (direction == 2) {
        df_pos <- mutate(df_pos, patient_id = forcats::fct_rev(patient_id))
      }
      
      df_pos_f <- df_pos %>%
        group_by(patient_id) %>%
        slice(1)
      df_pos_l <- df_pos %>%
        group_by(patient_id) %>%
        slice(n())
      
      pl <- pl +
        geom_path(data = df_pos, mapping = aes(x = x, y = y, color = patient_id)) +
        geom_point(data = df_pos_f, mapping = aes(x = x, y = y, color = patient_id), shape = 1, fill = "white", size = pointsize) +
        geom_point(data = df_pos_l, mapping = aes(x = x, y = y, color = patient_id), shape = 13, fill = "white", size = pointsize) +
        theme(legend.position = "bottom", legend.title = element_blank())
    }
  }
  
  if (!is.null(df_alt)) {
    if (nrow(df_alt) > 0) {
      
      df_alt <- mutate(df_alt, patient_id = factor(patient_id))
      if (direction == 2) {
        df_alt <- mutate(df_alt, patient_id = forcats::fct_rev(patient_id))
      }
      
      df_alt_f <- df_alt %>%
        group_by(patient_id) %>%
        slice(1)
      df_alt_l <- df_alt %>%
        group_by(patient_id) %>%
        slice(n())
      
      pl  <- pl +
        geom_path(data = df_alt, mapping = aes(x = x, y = y, group = patient_id), alpha = .25) +
        geom_text_repel(data = df_alt_l, mapping = aes(x = x, y = y, group = patient_id, label = patient_id), alpha = .66, size = 10 / cm(1)) +
        geom_point(data = df_alt_f, mapping = aes(x = x, y = y, group = patient_id), shape = 1, fill = "white", size = pointsize, alpha = .5) +
        geom_point(data = df_alt_l, mapping = aes(x = x, y = y, group = patient_id), shape = 13, fill = "white", size = pointsize, alpha = .5)
    }
  }
  
  return(pl)
}

table_ids <- function(df_i, df_pos, direction) {
  
  if (is.null(df_i)) { return(NULL) }
  if (is.null(df_pos)) { return(NULL) }
  if (nrow(df_i) == 0) { return(NULL) }
  if (nrow(df_pos) == 0) { return(NULL) }
  
  # last values from ID
  if (direction == 1) {
    df_i <- df_i %>%
      mutate(stand_height = standing_height(height)) %>%
      tail(1)
  } else {
    df_i <- df_i %>%
      mutate(stand_height = standing_height(height)) %>%
      head(1)
  }
  
  
  # first values from possible links
  df_pos_1 <- df_pos %>%
    group_by(patient_id) %>%
    summarize(duration = as.numeric(difftime(last(time), first(time), units = "mins")),
              stand_height = standing_height(height)) %>%
    ungroup()
  if (direction == 1) {
    df_pos_2 <- df_pos %>%
      group_by(patient_id) %>%
      slice(1) %>%
      ungroup()
  } else {
    df_pos_2 <- df_pos %>%
      group_by(patient_id) %>%
      slice(n()) %>%
      ungroup()
  }
  
  df_pos <- left_join(df_pos_1, df_pos_2, by = "patient_id")
  
  
  # features
  df_feat <- base::merge(df_pos, df_i, suffixes = c("_pos", "_i"), by = NULL) 
  
  if (direction == 1) {
    df_feat <- df_feat %>%
      mutate(timediff = as.numeric(difftime(time_pos, time_i, units = "secs")),
             distance = convert_dist(euclidean(x_pos, x_i, y_pos,y_i)),
             last_heightdiff = height_pos - height_i,
             stand_heightdiff = stand_height_pos - stand_height_i)
  } else {
    df_feat <- df_feat %>%
      mutate(timediff = as.numeric(difftime(time_i, time_pos, units = "secs")),
             distance = convert_dist(euclidean(x_pos, x_i, y_pos,y_i)),
             last_heightdiff = height_i - height_pos,
             stand_heightdiff = stand_height_i - stand_height_pos)
  }
  
  df_feat <- df_feat %>%
    mutate(across(contains("height"), ~ format(round(.x / 10), nsmall = 0)),
           timediff = format(timediff, nsmall = 0),
           duration = format(round(duration, 2), nsmall = 1),
           distance = format(round(distance, 1), nsmall = 1)) %>%
    mutate(last_height_comb = paste0(last_heightdiff, " (", height_pos, ", ", height_i, ")"),
           stand_height_comb = paste0(stand_heightdiff, " (", stand_height_pos, ", ", stand_height_i, ")")) %>%
    dplyr::select(patient_id_i, patient_id_pos, last_height_comb, stand_height_comb, duration, timediff, distance) %>%
    set_names("Pid", "Oid", "Last heightdiff (Oid, Pid) [cm]", "Stand heightdiff (Oid, Pid) [cm]", "Duration Oid [min]", "Timediff [sec]", "Distance [m]") %>%
    arrange(Pid) %>%
    mutate_all(as.character) 
  
  if (direction == 2) {
    df_feat <- purrr::map_df(df_feat, rev)
  }
  
  n_possibleIDs <- nrow(df_feat)
  
  if (n_possibleIDs > 0) {
    for (i in 1:n_possibleIDs) {
      col <- scales::hue_pal()(n_possibleIDs)[i]
      for (k in 2:6) {
        df_feat[i,k] <- paste0('<span style="color:', col, '">', df_feat[i,k], "</span>")
      }
    }
  }
  
  return(df_feat)
}

display_duration <- function(st, et) {
  duration <- format(round(as.numeric(difftime(et, st, units = "min")), 1), nsmall = 1)
  st <- format(st, "%H:%M:%S")
  et <- format(et, "%H:%M:%S")
  paste(st, " to ", et, " (", duration,  " min)")
}



#### Shiny UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tags$style(
        HTML(".bold-prefix { font-weight: bold; margin-right: 5px; }")
      ),
      fileInput("file", "Load file"),
      div(
        tags$span("Date:", class = "bold-prefix"),
        textOutput("date"),
        style = "display: flex;"
      ),
      div(
        tags$span("Total patient IDs:", class = "bold-prefix"),
        textOutput("totalIDs"),
        style = "display: flex;"
      ),
      div(
        tags$span("Remaining entrance IDs:", class = "bold-prefix"),
        textOutput("entranceIDs"),
        style = "display: flex;"
      ),
      div(
        tags$span("Finished IDs:", class = "bold-prefix"),
        textOutput("finishedIDs"),
        style = "display: flex;"
      ),
      br(),
      selectizeInput("ID", "Patient ID", choices = -1, selected = -1, multiple = F, options = list(maxItems = 1)),
      actionButton("nextID", "Next ID"),
      actionButton("prevID", "Previous ID"),
      actionButton("nextEnteredID", "Next Entrance"),
      actionButton("prevEnteredID", "Previous Entrance"),
      br(),
      br(),
      radioButtons("direction", "Matching direction", choices = list("Forward" = 1, "Backward" = 2), inline = T),
      div(
        tags$span("ID Duration:", class = "bold-prefix"),
        textOutput("currentDuration"),
        style = "display: flex;"
      ),
      div(
        tags$span("ID Links:", class = "bold-prefix"),
        textOutput("currentLinks"),
        style = "display: flex;"
      ),
      br(),
      radioButtons("quick", "Quick filter", choices = list("Short move" = 1, "Long move" = 2,  
                                                                "Short sit" = 3, "Long sit" = 4,
                                                                "All" = 5), inline = T),
      sliderTextInput("time", "Time", from_min = min(time_choices), to_max = max(time_choices), selected = short_move_time, 
                      choices = time_choices, grid = T, post = "sec"),
      sliderTextInput("distance", "Distance", from_min = min(dist_choices), to_max = max(dist_choices), selected = short_move_dist, 
                      choices = dist_choices, grid = T, post = "m"),
      sliderTextInput("height", "Height", from_min = min(height_choices), to_max = max(height_choices), selected = short_move_height, 
                      choices = height_choices, grid = T, post = "cm"),
      selectizeInput("altID", "Show alternatives for ID", choices = -1, selected = -1, options = list(maxItems = 1)),
      selectizeInput("posID", "Link with ID", choices = -1, selected = -1, options = list(maxItems = 1)),
      actionButton("linkTrack", "Link IDs"),
      actionButton("unlinkLastID", "Unlink last ID"),
      actionButton("unlinkFirstID", "Unlink first ID"),
      br(),
      br(),
      selectInput("terminalInput", "Stop linking", choices = c("ID lost", "ID exited")),
      actionButton("endTrack", "End track"),
      div(
        tags$span("Saved path-file:", class = "bold-prefix"),
        textOutput("saveto"),
        style = "display: flex;"
      ),
      ),
    mainPanel(plotOutput("clinic", inline = T),
              br(),
              tableOutput("displayedIDs"))
  )
)



#### Shiny Sever ####
server <- function(input, output, session) {
  
  #### Load data ####
  values <- reactiveValues(dat = NULL, all_ids = NULL, entered_ids = NULL,
                           dat_i = NULL, dat_pos_feat = NULL, dat_pos = NULL, dat_alt = NULL,
                           tot_ids = 0, tot_entr_ids = 0, tot_fin_ids = 0,
                           i_start = NA, i_end = NA, i_links = 0)
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
    
    # load data and filter ended tracks
    values$dat <- values$dat %>%
      mutate(across(c(patient_id, obs_id, obs_id_new), ~ as.integer(.x))) %>%
      group_by(patient_id) %>%
      arrange(obs_id) %>%
      mutate(patient_id = last(obs_id)) %>%
      ungroup() 
    
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
    
    # total number of patient IDs
    values$tot_ids <- n_distinct(values$dat$patient_id)
    output$totalIDs <- renderText({values$tot_ids})
    
    # entrance IDs 
    values$tot_entr_ids <- length(values$entered_ids)
    output$totEntranceIDs <- renderText({ values$tot_entr_ids })
    
    # finished ids
    values$tot_fin_ids <- n_distinct(values$dat$patient_id[!is.na(values$dat$tracking_end)])
    output$finishedIDs <- renderText({ values$tot_fin_ids })
    
    #  date
    output$date <- renderText({ as.Date(values$dat$time[1]) })
    
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
  
  
  #### Parameters ####
  
  # quick filter
  observeEvent(input$quick, {
    if (input$quick == 1) {
      updateSliderTextInput(session, inputId = "time", selected = short_move_time)
      updateSliderTextInput(session, inputId = "distance", selected = short_move_dist)
      updateSliderTextInput(session, inputId = "height", selected = short_move_height)
    } else if (input$quick == 2) {
      updateSliderTextInput(session, inputId = "time", selected = long_move_time)
      updateSliderTextInput(session, inputId = "distance", selected = long_move_dist)
      updateSliderTextInput(session, inputId = "height", selected = long_move_height)
    } else if (input$quick == 3) {
      updateSliderTextInput(session, inputId = "time", selected = short_sit_time)
      updateSliderTextInput(session, inputId = "distance", selected = short_sit_dist)
      updateSliderTextInput(session, inputId = "height", selected = short_sit_height)
    } else if (input$quick == 4) {
      updateSliderTextInput(session, inputId = "time", selected = long_sit_time)
      updateSliderTextInput(session, inputId = "distance", selected = long_sit_dist)
      updateSliderTextInput(session, inputId = "height", selected = long_sit_height)
    } else {
      updateSliderTextInput(session, inputId = "time", selected = all_time)
      updateSliderTextInput(session, inputId = "distance", selected = all_dist)
      updateSliderTextInput(session, inputId = "height", selected = all_height)
    }
  })
  
  # select next entered ID
  observeEvent(input$nextID, {
    req(input$ID)
    oid <- as.numeric(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = values$all_ids[values$all_ids>oid][1], server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select next entered ID
  observeEvent(input$nextEnteredID, {
    req(input$ID)
    oid <- as.numeric(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = values$entered_ids[values$entered_ids>oid][1], server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select previous entered ID
  observeEvent(input$prevID, {
    req(input$ID)
    oid <- as.numeric(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = tail(values$all_ids[values$all_ids<oid],1), server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select previous entered ID
  observeEvent(input$prevEnteredID, {
    req(input$ID)
    oid <- as.numeric(input$ID)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = tail(values$entered_ids[values$entered_ids<oid],1), server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  
  #### Data ####
  
  # ID data and
  observeEvent(input$ID, {
    if (!is.null(values$dat)) {
      oid <- as.numeric(input$ID)
      
      # filter data
      values$dat_i <- values$dat %>% 
        filter(patient_id == oid) %>%
        dplyr::select(patient_id, obs_id, time, x, y, height)
      
      # filter others
      values$dat_pos_feat <- filter_other_feat(values$dat, oid, input$direction)
      # compute features
      values$dat_pos_feat <- compute_other_feat(values$dat_pos_feat)
      
      # duration
      values$i_start <- head(values$dat$time[values$dat$patient_id==oid], 1)
      values$i_end <- tail(values$dat$time[values$dat$patient_id==oid], 1)
      output$currentDuration <- renderText({ display_duration(values$i_start, values$i_end) })
      
      # number of links
      values$i_links <- n_distinct(values$dat_i$obs_id) - 1
      output$currentLinks <- renderText(values$i_links)
    }
  })
  
  observeEvent(input$direction, {
    if (!is.null(values$dat) & !is.null(values$dat_i) & !is.null(values$dat_pos_feat)) {
      oid <- as.numeric(input$ID)
      # filter others
      values$dat_pos_feat <- filter_other_feat(values$dat, oid, input$direction)
      # compute features
      values$dat_pos_feat <- compute_other_feat(values$dat_pos_feat)
    }
  })
  
  # possible link ID data
  observe({
    # get possible links
    if (!is.null(values$dat_pos_feat)) {
      oid <- as.numeric(input$ID)
      if (nrow(values$dat_pos_feat) > 0) {
        dat_pos_feat_sub <- subset_other_feat(values$dat_pos_feat, input$time, input$distance, input$height)
        if (nrow(dat_pos_feat_sub) > 0) {
          posIDs <- unique(dat_pos_feat_sub$patient_id_other)
          values$dat_pos <- filter(values$dat, patient_id %in% posIDs)
        } else {
          values$dat_pos <- NULL
        }
      } else {
        values$dat_pos <- NULL
      }
    } else {
      values$dat_pos <- NULL
    }
    
    # update selection
    if (!is.null(values$dat_pos)) {
      if (input$direction == 2) {
        posIDs <- rev(posIDs)
      }
      updateSelectizeInput(session, inputId = "posID", choices = posIDs)
      updateSelectizeInput(session, inputId = "altID", choices = posIDs)
    } else {
      updateSelectizeInput(session, inputId = "posID", choices = -1)
      updateSelectizeInput(session, inputId = "altID", choices = -1)
    }
  })
  
  # alternatives data
  observeEvent(input$altID, {
    if (!is.null(values$dat_pos)) {
      aid <- as.numeric(input$altID)
      oid <- as.numeric(input$ID)
      dat_pos_i <- filter(values$dat, patient_id == aid)
      values$dat_alt_feat <- filter_other_feat(values$dat, aid, ifelse(input$direction == 1, 2, 1)) %>%
        filter(patient_id_other != oid)
      if (nrow(values$dat_alt_feat) > 0) {
        values$dat_alt_feat <- compute_other_feat(values$dat_alt_feat)
        moving_alt <- subset_other_feat(values$dat_alt_feat, long_move_time, long_move_dist, long_move_height)
        sitting_alt <- subset_other_feat(values$dat_alt_feat, long_sit_time, long_sit_dist, long_sit_height)
        altIDs <- unique(moving_alt$patient_id_other, sitting_alt$patient_id_other)
        values$dat_alt <- filter(values$dat, patient_id %in% altIDs)
      } else {
        values$dat_alt <- NULL
      }
    } else {
      values$dat_alt <- NULL
    }
  })
  
  
  #### New link ####
  observeEvent(input$linkTrack, {
    # make link
    oid <- as.numeric(input$ID)
    lid <- input$posID
    linkDir <- input$direction
    name_linkage_success <- paste("ID", oid, "linked to", lid, ".")
    shinyalert("Success", name_linkage_success, type = "success", timer = 1000)
    if (linkDir == 1) {
      values$dat <- mutate(values$dat, patient_id = ifelse(patient_id == oid, lid, patient_id))
    } else {
      values$dat <- mutate(values$dat, patient_id = ifelse(patient_id == lid, oid, patient_id))
    }
    base::saveRDS(object = values$dat, file = values$save_file)
    
    # update selection
    if (linkDir == 1) {
      values$entered_ids <- values$entered_ids[values$entered_ids != oid]
      values$all_ids <- values$all_ids[values$all_ids != oid]
      updateSelectizeInput(inputId = "ID", choices = values$all_ids, selected = lid, server = T) 
    } else {
      values$entered_ids <- values$entered_ids[values$entered_ids != lid]
      values$all_ids <- values$all_ids[values$all_ids != lid]
      updateSelectizeInput(inputId = "ID", choices = values$all_ids, selected = oid, server = T) 
    }
    
    # update total counts
    # total number of patient IDs
    values$tot_ids <- values$tot_ids - 1
    output$totalIDs <- renderText({values$tot_ids})
    # entrance IDs 
    values$tot_entr_ids <- length(values$entered_ids)
    output$totEntranceIDs <- renderText({ values$tot_entr_ids })
  })
  
  #### Un-link ####
  observeEvent(input$unlinkLastID, {
    # unlink
    last_id <- as.numeric(input$ID)
    old_id <- tail(sort(unique(values$dat$obs_id[values$dat$patient_id==last_id])),2)[1]
    values$dat <- values$dat %>%
      mutate(patient_id = ifelse(patient_id == last_id, old_id, patient_id),
             patient_id = ifelse(obs_id == last_id, last_id, patient_id))
    base::saveRDS(object = values$dat, file = values$save_file)
    
    # update selection
    old_id_entered <- values$dat$is_entrance[values$dat$patient_id == old_id][1]
    if (old_id_entered) { values$entered_ids <- c(values$entered_ids, old_id) }
    values$all_ids <- c(values$all_ids, old_id)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = old_id, server = T)
    shinyalert("Info", paste("Unlinking ID", last_id, "from", old_id) , type = "info", timer = 1000)
    
    # update total counts
    # total number of patient IDs
    values$tot_ids <- values$tot_ids + 1
    output$totalIDs <- renderText({values$tot_ids})
    # entrance IDs 
    values$tot_entr_ids <- length(values$entered_ids)
    output$totEntranceIDs <- renderText({ values$tot_entr_ids })
  })
  
  observeEvent(input$unlinkFirstID, {
    # unlink
    last_id <- as.numeric(input$ID)
    old_id <- sort(unique(values$dat$obs_id[values$dat$patient_id==last_id]))[1]
    values$dat <- mutate(values$dat, patient_id = ifelse(obs_id == old_id, old_id, patient_id))
    base::saveRDS(object = values$dat, file = values$save_file)
    
    # update selection
    old_id_entered <- values$dat$is_entrance[values$dat$patient_id == old_id][1]
    if (old_id_entered) { values$entered_ids <- c(values$entered_ids, old_id) }
    values$all_ids <- c(values$all_ids, old_id)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = last_id, server = T)
    shinyalert("Info", paste("Unlinking ID", last_id, "from", old_id) , type = "info", timer = 1000)
    
    # update total counts
    # total number of patient IDs
    values$tot_ids <- values$tot_ids + 1
    output$totalIDs <- renderText({values$tot_ids})
    # entrance IDs 
    values$tot_entr_ids <- length(values$entered_ids)
    output$totEntranceIDs <- renderText({ values$tot_entr_ids })
  })
  
  
  #### End track ####
  observeEvent(input$endTrack, {
    # finish track
    values$dat$tracking_end[values$dat$patient_id==as.numeric(input$ID)] <- input$terminalInput
    base::saveRDS(object = values$dat, file = values$save_file)
    shinyalert("Done.", paste("Ending tracking of ID", input$ID, "because", input$terminalInput), type = "info", timer = 1000)
    values$entered_ids <- values$entered_ids[values$entered_ids != as.numeric(input$ID)]
    values$all_ids <- values$all_ids[values$all_ids != as.numeric(input$ID)]
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = values$all_ids[values$all_ids>as.numeric(input$ID)][1], server = T)
    updateRadioButtons(session, "quick", selected = 1)
    
    # update counts
    # finished ids
    values$tot_fin_ids <- values$tot_fin_ids + 1
    output$finishedIDs <- renderText({ values$tot_fin_ids })
  })
  
    
  #### Plot ####
  output$clinic <- renderPlot({
    plot_ids(building_pl, values$dat_i, values$dat_pos, values$dat_alt, input$direction)
  }, height = 750, width = 1000)
  
  #### Table ####
  output$displayedIDs <- renderTable({
    table_ids(values$dat_i, values$dat_pos, input$direction)
  }, sanitize.text.function = function(x) x)
  
}





#### Shiny App ####
shinyApp(ui = ui, server = server)
