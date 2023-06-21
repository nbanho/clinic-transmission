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
time_choices <- c(10, 20, 30, 45, 60, 300, 600, 900, 1500, 1800) # time_choices <- c(5, 10, 20, 30, 45, 60, 300, 600, 900, 1500, 1800) 
dist_choices <- c(0.5, 1, 2, 3, 4, 5, 7) # dist_choices <- c(.25, .5, 1., 1.5, 2., 3., 5.) 
height_choices <- c(10, 20, 50, 100, 200)
short_move_time <- 30 # 10 
short_move_dist <- 5 # 1.5
short_move_height <- 50
long_move_time <- 60 # 30 
long_move_dist <- 7 # 3 
long_move_height <- 100
short_sit_time <- 60
short_sit_dist <- 1 # .5
short_sit_height <- 100
long_sit_time <- 300
long_sit_dist <- 1
long_sit_height <- 100
all_time <- max(time_choices)
all_dist <- max(dist_choices)
all_height <- max(height_choices)


#### Building ####
building <- terra::vect(paste0("../../data-raw/", "Massi", "/building/clinic-vector.gpkg"))
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
    filter(is.na(tracking_end) | grepl("Lost", tracking_end))
  
  if (nextIDs == 1) {
    
    # current id
    df_i <- df %>%
      filter(patient_id == oid) %>%
      slice(n()) 
    
    # pre time filter conditions
    earliest_start <- df_i$date_time[1] - lubridate::seconds(3)
    latest_start <- df_i$date_time[1] + lubridate::seconds(max(time_choices))
    
    # other potential ids
    df_other <- df %>%
      filter(patient_id != oid) %>%
      group_by(patient_id) %>%
      slice(1) %>% 
      ungroup() %>%
      filter(patient_id > oid, 
             between(date_time, earliest_start, latest_start)) 
    
  } else {
    
    # current id
    df_i <- df %>%
      filter(patient_id == oid) %>%
      slice(1) 
    
    # pre time filter conditions
    latest_end <- df_i$date_time[1] + lubridate::seconds(3)
    earliest_end <- df_i$date_time[1] - lubridate::seconds(max(time_choices))
    
    # other potential ids
    df_other <- df %>%
      filter(patient_id != oid) %>%
      group_by(patient_id) %>%
      slice(n()) %>%
      ungroup() %>%
      filter(patient_id < oid, 
             between(date_time, earliest_end, latest_end))
    
  }
  
  df_other <- base::merge(df_other, df_i, suffixes = c("_other", "_i"), by = NULL)
  
  return(df_other)
  
}

compute_other_feat <- function(df_other, direction) {
  if (direction == 1) {
    df_other %>%
      mutate(timediff_raw = as.numeric(difftime(date_time_other, date_time_i, units = "secs")), 
             timediff = abs(timediff_raw),
             distance = convert_dist(euclidean(x_other, x_i, y_other, y_i)),
             heightdiff = abs(height_other - height_i) / 10)
  } else {
    df_other %>%
      mutate(timediff_raw = as.numeric(difftime(date_time_i, date_time_other, units = "secs")), 
             timediff = abs(timediff_raw),
             distance = convert_dist(euclidean(x_other, x_i, y_other, y_i)),
             heightdiff = abs(height_other - height_i) / 10)
  }
  
}

subset_other_feat <- function(df_other, max_timediff, max_distance, max_heightdiff) {
  df_other %>%
    filter(timediff <= max_timediff,
           distance <= max_distance,
           heightdiff <= max_heightdiff)
}


plot_ids <- function(pl, df_i, df_pos, df_alt, df_pos_feat) {
  
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
      
      pid_order <- df_pos_feat$patient_id_other[order(df_pos_feat$timediff_raw)]
      
      df_pos <- mutate(df_pos, patient_id = factor(patient_id, levels = pid_order))
      
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
    summarize(duration = duration_min_sec(first(date_time), last(date_time)),
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
      mutate(timediff = as.numeric(difftime(date_time_pos, date_time_i, units = "secs")),
             distance = convert_dist(euclidean(x_pos, x_i, y_pos,y_i)),
             last_heightdiff = height_pos - height_i,
             stand_heightdiff = stand_height_pos - stand_height_i)
  } else {
    df_feat <- df_feat %>%
      mutate(timediff = as.numeric(difftime(date_time_i, date_time_pos, units = "secs")),
             distance = convert_dist(euclidean(x_pos, x_i, y_pos,y_i)),
             last_heightdiff = height_i - height_pos,
             stand_heightdiff = stand_height_i - stand_height_pos)
  }
  
  df_feat <- df_feat %>%
    mutate(across(contains("height"), ~ format(round(.x / 10), nsmall = 0)),
           distance = format(round(distance, 1), nsmall = 1)) %>%
    mutate(last_height_comb = paste0(last_heightdiff, " (", height_pos, ", ", height_i, ")"),
           stand_height_comb = paste0(stand_heightdiff, " (", stand_height_pos, ", ", stand_height_i, ")")) %>%
    arrange(timediff) %>%
    mutate(timediff = format(timediff, nsmall = 0)) %>%
    dplyr::select(patient_id_i, patient_id_pos, last_height_comb, stand_height_comb, duration, timediff, distance) %>%
    set_names("Pid", "Oid", "Last heightdiff (Oid, Pid) [cm]", "Stand heightdiff (Oid, Pid) [cm]", "Duration Oid [min]", "Timediff [sec]", "Distance [m]") %>%
    mutate_all(as.character) 
  
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

duration_min_sec <- function(st, et) {
  duration_min <- as.integer(floor(difftime(et, st, units = "min")))
  duration_sec <- as.integer(as.numeric(difftime(et, st, units = "secs")) %% 60)
  duration <- paste(duration_min, "min", duration_sec, "sec")
  return(duration)
}

display_duration <- function(st, et) {
  duration <- duration_min_sec(st, et)
  st <- format(st, "%H:%M:%S")
  et <- format(et, "%H:%M:%S")
  paste(st, " to ", et, " (", duration,  ")")
}

display_id_counts <- function(df) {
  n_obs_id_new <- n_distinct(df$obs_id_new)
  n_pat_id <- n_distinct(df$patient_id)
  n_links <- n_obs_id_new - n_pat_id
  counts <- paste0("Initial: ", n_obs_id_new, ", Current: ", n_pat_id, ", Links: ", n_links)
  return(counts)
}

display_label_counts <- function(df) {
  n_unfinished <- n_distinct(df$patient_id[is.na(df$tracking_end)])
  n_finished <- n_distinct(df$patient_id[!grepl("Lost", df$tracking_end) & !is.na(df$tracking_end)])
  n_lost <- n_distinct(df$patient_id[grepl("Lost", df$tracking_end)])
  counts <- paste0("None: ", n_unfinished, ", Ended: ", n_finished, ", Lost: ", n_lost)
  return(counts)
}

update.all_ids <- function(df) {
  df %>%
    filter(is.na(tracking_end)) %>%
    group_by(patient_id) %>%
    summarize(last_time = last(date_time)) %>%
    ungroup() %>%
    arrange(last_time) %>%
    ungroup() %>%
    dplyr::select(patient_id) %>%
    unlist() %>%
    as.integer()
}

update.entered_ids <- function(df) {
  df %>%
    filter(is.na(tracking_end)) %>%
    group_by(patient_id) %>%
    summarize(first_entered = first(is_entrance),
              last_time = last(date_time)) %>%
    ungroup() %>%
    filter(first_entered) %>%
    arrange(last_time) %>%
    dplyr::select(patient_id) %>%
    unlist() %>%
    as.integer()
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
        tags$span("#IDs:", class = "bold-prefix"),
        textOutput("totalIDs"),
        style = "display: flex;"
      ),
      div(
        tags$span("#Labels:", class = "bold-prefix"),
        textOutput("totalLabels"),
        style = "display: flex;"
      ),
      br(),
      selectizeInput("ID", "Patient ID", choices = -1, selected = -1, multiple = F, options = list(maxItems = 1)),
      actionButton("firstID", "First ID"),
      actionButton("nextID", "Next ID"),
      actionButton("prevID", "Prev ID"),
      actionButton("nextEnteredID", "Next Entrance"),
      actionButton("prevEnteredID", "Prev Entrance"),
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
      selectInput("terminalInput", "Label or end tracking", choices = c("Entered + Exited",
                                                                        #"Entered WR + Exited",
                                                                        #"Entered + Exited WR",
                                                                        #"Entered WR + Exited WR",
                                                                        #"Lost enter + Exited",
                                                                        #"Lost enter + Exited WR",
                                                                        #"Entered + Lost exit",
                                                                        #"Entered WR + Lost exit",
                                                                        #"Lost enter + Lost exit",
                                                                        "Lost enter",
                                                                        "Lost exit",
                                                                        "Lost both",
                                                                        "Possible HCW",
                                                                        "Noise")),
      actionButton("endTrack", "Label/End track"),
      br(),
      br(),
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
                           dat_i = NULL, dat_pos_feat = NULL, dat_pos_feat_sub = NULL, dat_pos = NULL, dat_alt = NULL,
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
    
    # light preprocessing
    values$dat <- values$dat %>%
      mutate(across(c(patient_id, obs_id_new, obs_id), ~ as.integer(.x))) %>%
      dplyr::select(patient_id, obs_id_new, obs_id, tracking_end, match_type, everything())
    
    # get IDs to match
    values$all_ids <- update.all_ids(values$dat)
    values$entered_ids <- update.entered_ids(values$dat)
    
    # update patient ID selection
    updateSelectizeInput(session, "ID", choices = values$all_ids, server = T)
    
    # ID counts
    output$totalIDs <- renderText({ display_id_counts(values$dat) })
    
    # label counts
    output$totalLabels <- renderText({ display_label_counts(values$dat) })
    
    #  date
    output$date <- renderText({ as.character(as.Date(values$dat$date_time[1])) })
    
    # create directory to save file
    file_year <- lubridate::year(values$dat$date_time[1])
    file_date <- as.character(as.Date(values$dat$date_time[1]))
    values$save_dir <- paste(dirname(dirname(getwd())), "data-clean", "Massi", file_year, "patient-tracking", sep = "/")
    if (!dir.exists(values$save_dir)) { dir.create(values$save_dir, recursive = T) }
    values$save_file <- paste0(values$save_dir, "/", file_date, ".rds")
    
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
  
  #### Select ID ####
  # select first ID
  observeEvent(input$firstID, {
    all_ids <- as.integer(values$all_ids)
    first_id <- all_ids[1]
    updateSelectizeInput(session, inputId = "ID", choices = all_ids, selected = first_id, server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select next ID
  observeEvent(input$nextID, {
    req(input$ID)
    oid <- as.integer(input$ID)
    all_ids <- as.integer(values$all_ids)
    next_id <- all_ids[which(all_ids == oid) + 1]
    updateSelectizeInput(session, inputId = "ID", choices = all_ids, selected = next_id, server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select next entered ID
  observeEvent(input$nextEnteredID, {
    req(input$ID)
    oid <- as.integer(input$ID)
    all_ids <- as.integer(values$all_ids)
    entered_ids <- as.integer(values$entered_ids)
    next_entered_id <- entered_ids[entered_ids > oid][1]
    updateSelectizeInput(session, inputId = "ID", choices = all_ids, selected = next_entered_id, server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select previous ID
  observeEvent(input$prevID, {
    req(input$ID)
    oid <- as.integer(input$ID)
    all_ids <- as.integer(values$all_ids)
    prev_id <- all_ids[which(all_ids == oid) - 1]
    updateSelectizeInput(session, inputId = "ID", choices = all_ids, selected = prev_id, server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  # select previous entered ID
  observeEvent(input$prevEnteredID, {
    req(input$ID)
    oid <- as.integer(input$ID)
    all_ids <- as.integer(values$all_ids)
    entered_ids <- as.integer(values$entered_ids)
    if (oid < min(entered_ids)) {
      prev_entered_id <- min(entered_ids)
    } else {
      prev_entered_id <- tail(entered_ids[entered_ids < oid], 1)
    }
    updateSelectizeInput(session, inputId = "ID", choices = all_ids, selected = prev_entered_id, server = T)
    updateRadioButtons(session, "direction", selected = 1)
    updateRadioButtons(session, "quick", selected = 1)
  })
  
  
  #### Data ####
  
  # ID data and
  observeEvent(input$ID, {
    if (!is.null(values$dat)) {
      oid <- as.integer(input$ID)
      
      # filter data
      values$dat_i <- values$dat %>% 
        filter(patient_id == oid) %>%
        dplyr::select(patient_id, obs_id, date_time, x, y, height)
      
      # filter others
      values$dat_pos_feat <- filter_other_feat(values$dat, oid, input$direction)
      # compute features
      values$dat_pos_feat <- compute_other_feat(values$dat_pos_feat, input$direction)
      
      # duration
      values$i_start <- head(values$dat$date_time[values$dat$patient_id==oid], 1)
      values$i_end <- tail(values$dat$date_time[values$dat$patient_id==oid], 1)
      output$currentDuration <- renderText({ display_duration(values$i_start, values$i_end) })
      
      # number of links
      values$i_links <- n_distinct(values$dat_i$obs_id) - 1
      output$currentLinks <- renderText(values$i_links)
    }
  })
  
  observeEvent(input$direction, {
    if (!is.null(values$dat) & !is.null(values$dat_i) & !is.null(values$dat_pos_feat)) {
      oid <- as.integer(input$ID)
      # filter others
      values$dat_pos_feat <- filter_other_feat(values$dat, oid, input$direction)
      # compute features
      values$dat_pos_feat <- compute_other_feat(values$dat_pos_feat, input$direction)
    }
  })
  
  # possible link ID data
  observe({
    # get possible links
    if (!is.null(values$dat_pos_feat)) {
      oid <- as.integer(input$ID)
      if (nrow(values$dat_pos_feat) > 0) {
        values$dat_pos_feat_sub <- subset_other_feat(values$dat_pos_feat, input$time, input$distance, input$height)
        if (nrow(values$dat_pos_feat_sub) > 0) {
          posIDs <- values$dat_pos_feat_sub$patient_id_other
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
      posIDs <- posIDs[order(values$dat_pos_feat_sub$timediff_raw)]
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
      aid <- as.integer(input$altID)
      oid <- as.integer(input$ID)
      dat_pos_i <- filter(values$dat, patient_id == aid)
      values$dat_alt_feat <- filter_other_feat(values$dat, aid, ifelse(input$direction == 1, 2, 1)) %>%
        filter(patient_id_other != oid)
      if (nrow(values$dat_alt_feat) > 0) {
        values$dat_alt_feat <- compute_other_feat(values$dat_alt_feat, ifelse(input$direction == 1, 2, 1))
        moving_alt <- subset_other_feat(values$dat_alt_feat, long_move_time, long_move_dist, long_move_height)
        sitting_alt <- subset_other_feat(values$dat_alt_feat, long_sit_time, long_sit_dist, long_sit_height)
        altIDs <- unique(c(moving_alt$patient_id_other, sitting_alt$patient_id_other))
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
    oid <- as.integer(input$ID)
    lid <- as.integer(input$posID)
    if (lid == -1) {
      shinyalert("Error: -1", name_linkage_success, type = "success", timer = 1000)
    } else {
      linked_ids <- c(oid, lid)
      new_id <- max(linked_ids)
      linkDir <- input$direction
      name_linkage_success <- paste("ID", oid, "linked to", lid, ".")
      shinyalert("Success", name_linkage_success, type = "success", timer = 1000)
      values$dat$patient_id[values$dat$patient_id %in% linked_ids] <- new_id
      base::saveRDS(object = values$dat, file = values$save_file)
      
      # update selection
      values$all_ids <- update.all_ids(values$dat)
      values$entered_ids <- update.entered_ids(values$dat)
      updateSelectizeInput(inputId = "ID", choices = values$all_ids, selected = new_id, server = T)
      
      # update ID counts
      output$totalIDs <- renderText({ display_id_counts(values$dat) })
    }
  })
  
  #### Un-link ####
  observeEvent(input$unlinkLastID, {
    # unlink last ID
    last_id <- as.integer(input$ID)
    old_id <- tail(sort(unique(values$dat$obs_id_new[values$dat$patient_id==last_id])),2)[1] 
    shinyalert("Info", paste("Unlinking ID", last_id, "from", old_id) , type = "info", timer = 1000)
    values$dat$patient_id[values$dat$patient_id == last_id & values$dat$obs_id_new != last_id] <- old_id 
    base::saveRDS(object = values$dat, file = values$save_file)
    
    # update selection
    values$all_ids <- update.all_ids(values$dat)
    values$entered_ids <- update.entered_ids(values$dat)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = old_id, server = T)
    
    # update ID counts
    output$totalIDs <- renderText({ display_id_counts(values$dat) })
  })
  
  observeEvent(input$unlinkFirstID, {
    # unlink first ID
    last_id <- as.integer(input$ID)
    old_id <- sort(unique(values$dat$obs_id_new[values$dat$patient_id==last_id]))[1] 
    values$dat$patient_id[values$dat$obs_id_new == old_id] <- old_id 
    base::saveRDS(object = values$dat, file = values$save_file)
    shinyalert("Info", paste("Unlinking ID", last_id, "from", old_id) , type = "info", timer = 1000)
    
    # update selection
    values$all_ids <- update.all_ids(values$dat)
    values$entered_ids <- update.entered_ids(values$dat)
    updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = last_id, server = T)
    
    # update ID counts
    output$totalIDs <- renderText({ display_id_counts(values$dat) })
  })
  
  
  #### End track ####
  observeEvent(input$endTrack, {
    if (input$direction == 2) {
      shinyalert("Error", "Direction is backward. Please only end tracks forward.", type = "error", timer = 1000)
    } else {
      # finish track
      oid <- as.integer(input$ID)
      values$dat$tracking_end[values$dat$patient_id == oid] <- input$terminalInput
      base::saveRDS(object = values$dat, file = values$save_file)
      shinyalert("Done.", paste("Ending tracking of ID", input$ID, "because", input$terminalInput), type = "info", timer = 1000)
      values$all_ids <- update.all_ids(values$dat)
      values$entered_ids <- update.entered_ids(values$dat)
      next_id <- values$all_ids[values$all_ids > oid][1]
      updateSelectizeInput(session, inputId = "ID", choices = values$all_ids, selected = next_id, server = T)
      updateRadioButtons(session, "quick", selected = 1)
      
      # update label counts
      output$totalLabels <- renderText({ display_label_counts(values$dat) })
    }
  })
  
    
  #### Plot ####
  output$clinic <- renderPlot({
    plot_ids(building_pl, values$dat_i, values$dat_pos, values$dat_alt, values$dat_pos_feat_sub)
  }, height = 750, width = 1000)
  
  #### Table ####
  output$displayedIDs <- renderTable({
    table_ids(values$dat_i, values$dat_pos, input$direction)
  }, sanitize.text.function = function(x) x)
  
}





#### Shiny App ####
shinyApp(ui = ui, server = server)
