#### Libraries ###
library(shiny)
library(tidyverse)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(gridExtra)
source("../../utils/spatial.R")

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
  guides(fill = "none")
building_pl

#### Functions ####

filter_oids <- function(df, oid, max_timediff, max_distance) {
  
  # current id
  df_i <- df %>%
    filter(obs_id == oid) %>%
    slice(n())
  
  # other potential ids
  df_other <- df %>%
    filter(time > df_i$time[1])
  
  # other ids filtered for maximum timediff and distance
  df_other_feat <- df_other %>%
    group_by(obs_id) %>%
    slice(1) %>%
    ungroup() %>%
    merge(df_i, suffixes = c("_other", "_i"), by = NULL) %>%
    mutate(timediff = as.numeric(difftime(time_other, time_i, units = "secs")),
           distance = convert_dist(euclidean(x_other, x_i, y_other, y_i))) %>%
    filter(timediff <= max_timediff,
           distance <= max_distance)
  
  # paths of other ids
  df_other <- df_other %>%
    filter(obs_id %in% df_other_feat$obs_id_other)
  
  return(df_other)
}

plot_other_oids <- function(pl_oid, df_i, df_other) {
  
  df_i_f <- head(df_i, 1)
  df_i_l <- tail(df_i, 1)
  
  df_other_f <- df_other %>%
    group_by(obs_id) %>%
    slice(1)
  df_other_l <- df_other %>%
    group_by(obs_id) %>%
    slice(n())
  
  pointsize <- 4
  
  pl_oid +
    geom_path(data = df_i, mapping = aes(x = x, y = y), color = "black") +
    geom_point(data = df_i_f, mapping = aes(x = x, y = y), shape = 1, color = "black", size = pointsize) +
    geom_point(data = df_i_l, mapping = aes(x = x, y = y), shape = 13, color = "black", size = pointsize) +
    geom_path(data = df_other, mapping = aes(x = x, y = y, color = factor(obs_id))) +
    geom_point(data = df_other_f, mapping = aes(x = x, y = y, color = factor(obs_id)), shape = 1, fill = "white", size = pointsize) +
    geom_point(data = df_other_l, mapping = aes(x = x, y = y, color = factor(obs_id)), shape = 13, fill = "white", size = pointsize) +
    theme(legend.position = "bottom", legend.title = element_blank())
  
}

#### Shiny UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("fileInput", "Load file"),
      textOutput("date", container = span),
      br(),
      uiOutput("data"),
      sliderInput("timeInput", "Time", min = 5, max = 900, value = 30, step = 10, post = "s"),
      sliderInput("distanceInput", "Distance", min = 0.25, max = 5, value = 1, step = 0.25, post = "m"),
      numericInput("linkID", "Link patient with ID", value = -1, min = 0, max = 100000, step = 1),
      actionButton("linkTrack", "Link track"),
      textOutput("link", container = span),
      selectInput("terminalInput", "Stop", choices = c("ID lost", "ID exited")),
      actionButton("endTrack", "End track"),
      h5("Save file to:"),
      uiOutput("saveto")
      ),
    mainPanel(plotOutput("clinic"))
  )
)



#### Shiny Sever ####
server <- function(input, output, session) {
  
  # load data
  values <- reactiveValues(dat = NULL)
  observeEvent(input$fileInput, {
    file <- input$fileInput
    if (is.null(file)) {return(NULL)}
    values$dat <- dplyr::select(readRDS(file$datapath), -point, -poly)
    values$dat$tracking_end <- NA
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
  
  # get entered IDs
  entered_ids <- reactive({
    if (is.null(values$dat)) {return(-1)}
    else {
      values$dat %>%
        group_by(obs_id) %>%
        summarize(entered = first(is_entrance)) %>%
        ungroup() %>%
        filter(entered) %>%
        dplyr::select(obs_id) %>%
        unlist() %>%
        unique()
    }
  })
  
  # create input selection of entered IDs
  output$data <- renderUI({
    req(input$fileInput)
    selectInput("idInput", "Patient ID", choices = entered_ids(), selected = 1)
  })
  
  # get currently selected id
  current_id <- reactive({
    req(input$idInput)
    as.integer(input$idInput)
  })
  
  # get currently selected date
  current_time <- reactive({
    as.numeric(input$timeInput)
  })
  
  # get currently selected distance
  current_distance <- reactive({
    as.numeric(input$distanceInput)
  })
  
  # make linkage
  new_link_id <- eventReactive(input$linkTrack, {
    lid <- input$linkID
    possible_ids <- values$dat %>%
      filter(time > current_time(),
             obs_id != current_id()) %>%
      dplyr::select(obs_id) %>%
      unlist()
    if (!(lid %in% possible_ids)) {
      return(NULL)
    } else {
      return(lid)
    }
  })
  
  # update entered IDs
  output$link <- reactive({
    if (is.null(new_link_id())) {
      return("ERROR: ID not available.")
    } else {
      updated_entered_ids <- entered_ids()
      new_linked_id <- new_link_id()
      updated_entered_ids <- updated_entered_ids[updated_entered_ids != new_linked_id]
      updateSelectInput(inputId = "idInput", choices = updated_entered_ids)
      return("Successfully linked!")
    }
  })
  
  # make link, update and store data
  observeEvent(input$linkTrack, {
    if (!is.null(new_link_id())) {
      values$dat <- mutate(values$dat, obs_id = ifelse(obs_id == new_link_id(), current_id(), obs_id))
      base::saveRDS(object = values$dat, file = save_dir())
    }
  })
    
  output$clinic <- renderPlot({
    if (is.null(values$dat)) {
      building_pl 
    } else {
      dat_i <- filter(values$dat, obs_id == current_id())
      dat_other <- filter_oids(values$dat, current_id(), current_time(), current_distance())
      plot_other_oids(building_pl, dat_i, dat_other)
    } 
  }, height = 750, width = 1000)
}





#### Shiny App ####
shinyApp(ui = ui, server = server)