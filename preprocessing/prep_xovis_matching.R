#### Libraries ####

library(tidyverse)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(gridExtra)
library(VennDiagram)
source("utils/spatial.r")
source("utils/plotting.r")


#' Algorith to match observation IDs
#' 
#' @param date date %Y-%m-%d
#' @param max_time maximum time (in seconds) to look ahead for matches
#' @param min_standing_height minimum standing height
#' @param max_mov_time maximum time (in seconds) to look ahead for people moving
#' @param max_sit_time maximum time (in seconds) to look ahead for people sitting
#' @param max_tbroom_time maximum time (in seconds) to look ahead for someone being in the TB room
#' @param max_mov_distance maximum distance someone can make while moving
#' @param max_sit_distance maximum distance someone can make while sitting
#' @param max_heightdiff maximum height difference for a person not changing her posture
#' @param min_duration minimum time (in minutes) to stay in the clinic to be considered

match_xovis <- function(date = "2021-10-25", 
                        max_time = 30 * 60, 
                        min_standing_height = 1400, 
                        max_mov_time = c(3, 5, 5, 5, 10, 10, 15, 15, 30, 60), 
                        max_sit_time = c(5, 10, 20, 30, 45, 60, 90, 150, 300, 600),
                        max_tbroom_time = 300,
                        max_mov_distance = c(0.5, 0.5, 1, 2, 2, 3, 3, 3.5, 4, 4),
                        max_sit_distance = c(0.25, 0.25, 0.5, 0.5, 0.5), 
                        max_heightdiff = c(50, 50, 75, 100, 100, 125, 125, 150, 175, 200), 
                        min_duration = 5 * 60
                        ) {
  
  #### Data ####
  
  # xovis
  file <- paste0("data-raw/Masi/xovis/", date, "-with_roominfo.rds")
  save_dir <- paste0("data-clean/Masi/patient-tracking-data/", date, "/")
  if (!dir.exists(save_dir)) {
    dir.create(save_dir)
  }
  masi <- readRDS(file) 
  
  n_oid <- length(unique(masi$obs_id))
  
  # remove very short tracks
  
  masi <- masi %>%
    group_by(obs_id) %>%
    filter(! ( as.numeric(difftime(last(time), first(time), units = "secs")) < 10  &
                 convert_dist(euclidean(last(x), first(x), last(y), first(y))) < 1 ) ) %>%
    ungroup()
  
  n_oid_filt <- length(unique(masi$obs_id))
  
  message(sprintf("%i of %i (%i percent) observation IDs removed; %i remaining", 
                  n_oid-n_oid_filt, n_oid, round(100 - 100 * n_oid_filt / n_oid), n_oid_filt))
  
  # roomplan
  clinic <- vect("data-raw/Masi/building/clinic-vector.gpkg")
  clinic_sf <- sf::st_as_sf(clinic, crs = NA)
  st_crs(clinic_sf) <- NA
  clinic_df <- fortify(clinic_sf)
  
  
  #### Matching ####
  
  # initialize
  masi$patient_id <- masi$obs_id
  match_type <- c()
  no_matches <- c()
  round <- 1:length(max_mov_time)
  
  for (r in round) {
    
    print(sprintf("Round %i", r))
    
    ids <- masi %>%
      group_by(patient_id) %>%
      filter(!last(is_exit),
             !last(is_inside_reception)) %>%
      slice(1) %>%
      ungroup() %>%
      dplyr::select(patient_id) %>%
      unlist()
    
    while(length(ids) > 0) {
      
      # ID
      i <- ids[1]
      ids <- ids[ids!=i]
      
      # continue matching until no more matches can be found in this round
      continue_matching <- T
      
      while (continue_matching) {
        
        # select patient
        masi_p <- filter(masi, patient_id == i)
        masi_p_last <- tail(masi_p, 1)
        
        # check for possible matches
        possible_matches <- masi %>%
          group_by(patient_id) %>%
          filter(!first(is_entrance),
                 !first(is_inside_reception)) %>%
          slice(1) %>%
          ungroup() %>%
          filter(between(time, masi_p_last$time %m+% seconds(1), masi_p_last$time %m+% seconds(max_time))) 
        
        if (nrow(possible_matches) == 0) {
          
          continue_matching <- F
          
        } else {
          
          # compute features
          possible_matches <- possible_matches %>%
            mutate(distance = convert_dist( euclidean(x, masi_p_last$x, y, masi_p_last$y) ),
                   timediff = as.numeric(difftime(time, masi_p_last$time, units = c("secs"))),
                   heightdiff = abs(height - masi_p_last$height))
          
          # type: priority
          matches_priority <- possible_matches %>%
            filter(timediff <= min(max_mov_time),
                   distance <= min(max_mov_distance),
                   heightdiff <= min(max_heightdiff)) %>%
            mutate(match_type = "Priority") 
          
          # type: person moving
          matches_moving <- possible_matches %>%
            filter(timediff <= max_mov_time[r],
                   distance <= max_mov_distance[r],
                   heightdiff <= max_heightdiff[r],
                   (height & masi_p_last$height > min_standing_height)) %>%
            mutate(match_type = "Moving") 
          
          # type: person sitting for a long time unrecognized, e.g. head down or view blocked
          matches_sitting <- possible_matches %>%
            filter(timediff <= max_sit_time[r],
                   distance <= max_sit_distance[r],
                   is_seat, masi_p_last$is_seat) %>%
            mutate(match_type = "Seating area")
          
          # type: person inside tb room
          matches_tbroom <- possible_matches %>%
            filter(timediff <= max_tbroom_time,
                   (is_tbroom & masi_p_last$is_tbroom) ) %>%
            mutate(match_type = "TB room") 
          
          if (r >= 5) {
            # type: person re-appearing from care room
            matches_careroom <- possible_matches %>%
              filter(heightdiff <= max(max_heightdiff),
                     (is_in_room_left & masi_p_last$is_in_room_left) | (is_in_room_right & masi_p_last$is_in_room_right)) %>%
              mutate(match_type = "Care room")
            
            # combine matches
            matches <- rbind(matches_priority, matches_moving, matches_sitting, matches_tbroom, matches_careroom) 
          } else {
            matches <- rbind(matches_priority, matches_moving, matches_sitting, matches_tbroom) 
          }
          
          
          # number of possible matches
          no_matches <- c(no_matches, length(unique(matches$patient_id)))
          
          if (nrow(matches) == 0) {
            
            continue_matching <- F
            
          } else {
            
            # rank matches 
            matches <- matches %>%
              arrange(timediff, distance, heightdiff) %>% 
              slice(1)
            
            # make match
            masi$patient_id[masi$patient_id==matches$patient_id] <- masi_p_last$patient_id
            match_type <- c(match_type, matches$match_type)
            
            # message
            pid <- masi_p_last$patient_id
            oid <- matches$patient_id
            type <- matches$match_type
            dt <- matches$timediff
            dd <- as.character( round(matches$distance, 1) )
            dh <- as.character( round(matches$heightdiff / 10, 1) )
            message(sprintf("Linking %s with %s. Type: %s, t: %is, d: %sm, h: %scm.", pid, oid, type, dt, dd, dh))
            
            # remove ID from round
            ids <- ids[ids!=matches$patient_id]
            
            # check if patient exited
            if (tail(masi$is_exit[masi$patient_id == masi_p_last$patient_id], 1) |
                tail(masi$is_inside_reception[masi$patient_id == masi_p_last$patient_id], 1)) {
              
              continue_matching <- F
              
            }
          }
        }
      }
    }
  }
  
  masi <- masi %>%
    dplyr::select(-point, -poly) %>%
    dplyr::select(obs_id, patient_id, everything())
  saveRDS(masi, paste0(save_dir, "patient-id-data.rds"))
  
  #### Matching info ####
  
  # Match counts by type
  match_counts <- data.frame(match_type = match_type) %>%
    group_by(match_type) %>%
    summarize(value = n()) %>%
    ungroup()
  match_type_pl <- match_counts %>%
    mutate(percent = value / sum(value) * 100,
           percent = paste0(round(percent, 0), "%"),
           match_type = factor(match_type, levels = c("Priority", "Moving", "Seating area", "Care room", "TB room"))) %>%
    ggplot(aes(x = match_type, y = value)) +
    geom_bar(aes(fill = match_type), stat = "identity") +
    geom_text(aes(label = percent, color = match_type), vjust = -.25, size = 8 / cm(1)) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    labs(y = "Count",
         title = paste("Total links:", sum(match_counts$value)),
         subtitle = paste(length(unique(masi$patient_id)) - 1, "patient ids compared to", length(unique(masi$obs_id)), "observation ids")) +
    theme_bw2() +
    theme(axis.title.x = element_blank(), legend.position = "none")
  save_plot(match_type_pl, pdf_file = paste0(save_dir, "match-type-counts.pdf"), w = 20, h = 10)
  
  
  # Number of possible matches
  match_pos_no_pl <- data.frame(no_matches = no_matches) %>%
    filter(no_matches > 0) %>%
    ggplot(aes(x = no_matches)) +
    geom_histogram() +
    scale_x_continuous(breaks = scales::breaks_width(1)) +
    labs(x = "Number of possible matches", y = "Count") +
    theme_bw2() 
  save_plot(match_pos_no_pl, pdf_file = paste0(save_dir, "number-of-possible-matches.pdf"), w = 16, h = 10)
  
  
  #### Descriptives ####
  masi_match_descr <- masi %>%
    group_by(patient_id) %>%
    summarize(stayed = ifelse(difftime(last(time), first(time), units = "secs") >= min_duration, T, F),
              received = any(is_reception),
              time_after_received = ifelse(received, difftime(last(time), time[is_reception][1], units = "secs"), NA),
              entered = first(is_entrance),
              exited = last(is_exit)) %>%
    ungroup() 
  
  # Venn diagram
  n_masi <- length(unique(masi$patient_id))
  
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  venn.diagram(x = list(
    Stayed = masi_match_descr$patient_id[masi_match_descr$stayed],
    Reception = masi_match_descr$patient_id[masi_match_descr$received],
    Exited = masi_match_descr$patient_id[masi_match_descr$exited],
    Entered = masi_match_descr$patient_id[masi_match_descr$entered]),
    filename = paste0(save_dir, "venndiagramm.png"),
    output = F,
    height = 1000,
    width = 1000,
    imagetype="png" ,
    resolution = 300,
    compression = "lzw",
    lwd = 1,
    col=c("#440154ff", '#21908dff', '#fde725ff', '#839192'),
    fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3), alpha('#839192',0.3)),
    cex = 0.5,
    fontfamily = "sans",
    cat.cex = 0.3,
    cat.default.pos = "outer",
    cat.pos = c(-27, 27, 135, -135),
    cat.dist = c(0.055, 0.055, 0.085, 0.085),
    cat.fontfamily = "sans",
    cat.col = c("#440154ff", '#21908dff', '#fde725ff', '#839192'),
    main = paste( "Total number of patient IDs:", n_masi, ""),
    main.cex = 0.5,
  )
  
  #### Good subset ####
  
  # Subset of patients that entered, was received, and exited the clinic
  masi_sub <- masi %>%
    left_join(masi_match_descr %>% dplyr::select(patient_id, time_after_received, stayed, received, entered, exited), by = "patient_id") %>%
    filter(!is.na(time_after_received),
           time_after_received > min_duration) 
  
  # Duration of stay in this subset
  duration_sub_pl <- masi_sub %>%
    group_by(patient_id) %>%
    summarize(duration = as.numeric(difftime(last(time), first(time), units = "min"))) %>%
    ungroup() %>%
    ggplot(aes(x = duration)) +
    geom_histogram() +
    scale_x_continuous(expand = c(0,0), breaks = scales::breaks_width(10), limits = c(0, NA)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = "Duration [min]", y = "Count") +
    theme_bw2()
  save_plot(duration_sub_pl, pdf_file = paste0(save_dir, "good-patient-ids-duration.pdf"), w = 8, h = 6)
  
  
  # Patient trajectories
  clean_ids <- unique(masi_sub$patient_id)
  pdf(paste0(save_dir, "good-patient-ids.pdf"), width = 21 / cm(1), height = 14 / cm(1))
  for (i in 1:length(clean_ids)) {
    print(
      plot_single_track(masi %>% filter(patient_id == clean_ids[i]))
    )
  }
  dev.off()
  
}


#' Plot individual tracks from patients with linked observations
#' 
#' @param df data frame with columns patient_id, obs_id, time, x, y, and height
#' 

plot_single_track <- function(df) {
  
  # Descriptives
  descr <- df %>%
    mutate(dt = as.numeric(difftime(lead(time), time, units = "secs"))) %>%
    summarize(
      Total = as.numeric(difftime(last(time), first(time), units = "secs")),
      `Time in waiting room` = sum(dt[is_waitingroom], na.rm = T),
      `Time in passage` = sum(dt[is_passage], na.rm = T),
      `Time in TB room` = sum(dt[is_tbroom], na.rm = T),
      `Time in seating area` = sum(dt[is_seat], na.rm = T),
      `Time in care room` = sum(dt[is_in_room_left | is_in_room_right], na.rm = T),
      `Time at reception` = sum(dt[is_reception], na.rm = T)
    ) %>%
    gather() %>%
    mutate(value = ifelse(key == "Time at reception", paste0(value, "sec"), paste0(format(round(value / 60, 1), nsmall = 1), "min"))) %>%
    set_names(c("Variable", "Duration"))
  
  tt <- ttheme_default(base_size = 6)
  
  # Start and end of patient track
  se <- df %>%
    group_by(patient_id) %>%
    arrange(time) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  # Start and end of each observation
  se_obs <- df %>%
    group_by(patient_id, obs_id) %>%
    arrange(time) %>%
    slice(1, n()) %>%
    ungroup() 
  
  # Start and end of each observation
  tab <- se_obs %>%
    mutate(height = round(height / 10, digits = 0),
           time = format(time, "%H:%M:%S")) %>%
    dplyr::select(obs_id, time, x, y, height) %>%
    arrange(time) %>%
    mutate_all(as.character)
  
  # Start and end of each observation in plot
  se_obs <- se_obs %>%
    slice(c(-1, -n())) 
  
  # Plot 
  pl <- ggplot() +
    geom_sf(data = clinic_df, linewidth = 1, fill = NA) +
    geom_path(data = df, mapping = aes(x = x, y = y, color = factor(obs_id), group = factor(patient_id)), alpha = .5) +
    geom_point(data = se, mapping = aes(x = x, y = y, shape = type)) +
    geom_point(data = se_obs, mapping = aes(x = x, y = y, color = factor(obs_id)), size = 0.5) +
    scale_shape_manual(values = c(5, 13)) +
    scale_x_continuous(labels = function(x) x / 1000, breaks = seq(-10000, 20000, 1000)) +
    scale_y_continuous(labels = function(x) x / 1000, breaks = seq(-8000, 6000, 1000)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8)) 
  
  # Observations table: add colours to table
  g <- ggplot_build(pl)
  if (nrow(g$data[[4]]) > 0) {
    gcol <- g$data[[4]] %>% 
      dplyr::select(group, colour) %>%
      group_by(group) %>%
      slice(1) %>%
      ungroup()
    tab <- tab %>%
      mutate(group = rep(1:length(unique(tab$obs_id)), each = 2)) %>%
      left_join(gcol, by = "group") %>%
      dplyr::select(-group)
  } else {
    tab$colour <- "red"
  }
  
  # Show only a subset of tab if there are many links
  if (nrow(tab) > 20) {
    tab_top5 <- head(tab, 10)
    tab_bottom5 <- tail(tab, 10)
    mid_row <- data.frame(obs_id = "...", time = "...", x = "...", y = "...", height = "...", colour = "black")
    tab <- rbind(tab_top5, mid_row, tab_bottom5) 
  }
  
  cols <- matrix(rep(tab$colour, ncol(tab)), ncol = ncol(tab))
  ttcol <- ttheme_default(core=list(fg_params = list(col = cols),
                                    bg_params = list(col=NA)),
                          rowhead=list(bg_params = list(col=NA)),
                          colhead=list(bg_params = list(col=NA)),
                          base_size = 6)
  
  
  # combine grobs
  pl_tab <- grid.arrange(arrangeGrob(tableGrob(descr, rows = NULL, theme = tt), pl, nrow = 2), tableGrob(tab %>% dplyr::select(-colour), rows = NULL, theme = ttcol), ncol = 2)
  return(pl_tab)
}


#' Plot multiple tracks from multiple IDs
#' 
#' @param df data frame with columns id_col, x, y, and height
#' @param id_col the ID column by which to identify individual tracks
#' 

plot_track <- function(df, id_col = "obs_id") {
  colnames(df)[colnames(df)==id_col] <- "id" 
  
  df <- mutate(df, id = factor(id))
  
  df_se <- df %>%
    group_by(id) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  ggplot(clinic_df) +
    geom_sf(linewidth = 1, fill = NA) +
    geom_path(data = df, mapping = aes(x = x, y = y, group = id, color = id), alpha = .2) +
    geom_point(data = df_se, mapping = aes(x = x, y = y, color = id, shape = type), size = 3) +
    scale_shape_manual(values = c(5, 13)) +
    theme(legend.position = "bottom")
}


#' Plot IDs at a certain time
#' 
#' @param df data frame with columns patient_id, obs_id, time, x, y, and height
#' @param focus_id ID to highlight
#' @param k seconds to look ahead
#' @param date date as string
#' 

plot_ids <- function(df, focus_id, k = 300, max_distance = 5, date = "2021-10-25") {
  
  t <- df$time[df$obs_id==focus_id]
  t <- t[length(t)]
  
  df <- df %>%
    mutate(height = round(height / 10, 0),
           patient_id = factor(patient_id),
           obs_id = factor(obs_id))
  
  focus_id_df <- df %>%
    filter(obs_id == focus_id)
  
  focus_id_df_se <- focus_id_df %>%
    group_by(patient_id) %>%
    arrange(time) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  focus_id_lab <- focus_id_df %>%
    slice(n())
  
  possible_matches <- df %>%
    filter(patient_id != focus_id_df$patient_id[1]) %>%
    group_by(patient_id) %>%
    mutate(ft = first(time)) %>%
    ungroup() %>%
    filter(ft > t,
           between(time, t, t + seconds(k))) %>%
    group_by(obs_id) %>%
    mutate(distance = euclidean(first(x), focus_id_df$x[nrow(focus_id_df)], first(y), focus_id_df$y[nrow(focus_id_df)]),
           distance = convert_dist(distance)) %>%
    ungroup() %>%
    filter(distance <= max_distance)
  
  possible_matches_lab <- possible_matches %>%
    group_by(patient_id) %>%
    slice(1) 
  
  possible_matches_se <- possible_matches %>%
    group_by(patient_id) %>%
    arrange(time) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  linked_matches <- df %>%
    filter(patient_id != focus_id_df$patient_id[1]) %>%
    group_by(patient_id) %>%
    filter(first(time) <= t) %>%
    ungroup() %>%
    group_by(obs_id) %>%
    mutate(first(time) > t) %>%
    ungroup() %>%
    filter(between(time, t, t + seconds(k))) %>%
    group_by(obs_id) %>%
    mutate(distance = euclidean(first(x), focus_id_df$x[nrow(focus_id_df)], first(y), focus_id_df$y[nrow(focus_id_df)]),
           distance = convert_dist(distance)) %>%
    ungroup() %>%
    filter(distance <= max_distance)
  
  pl <- ggplot() +
    geom_sf(data = clinic_df, linewidth = 1, fill = NA) +
    geom_path(data = focus_id_df, mapping = aes(x = x, y = y, group = patient_id), color = "black") +
    geom_point(data = focus_id_df_se, mapping = aes(x = x, y = y, shape = type), color = "black") +
    geom_text(data = focus_id_lab, mapping = aes(x = x, y = y, label = height), size = 8 / cm(1), vjust = -.5, hjust = -.5) +
    geom_path(data = possible_matches, mapping = aes(x = x, y = y, color = patient_id)) +
    geom_point(data = possible_matches_se, mapping = aes(x = x, y = y, shape = type, color = patient_id)) +
    geom_text(data = possible_matches_lab, mapping = aes(x = x, y = y, label = height, color = patient_id), size = 8 / cm(1), vjust = -.5, hjust = -.5) +
    geom_point(data = linked_matches, mapping = aes(x = x, y = y, group = patient_id), color = "grey", alpha = 0.2) +
    geom_path(data = linked_matches, mapping = aes(x = x, y = y, group = patient_id), color = "grey", alpha = 0.2) +
    scale_x_continuous(labels = function(x) x / 1000, breaks = seq(-10000, 20000, 1000)) +
    scale_y_continuous(labels = function(x) x / 1000, breaks = seq(-8000, 6000, 1000)) +
    scale_shape_manual(values = c(5, 13)) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8)) 
  
  return(pl)
}


#### Command line ####

args <- commandArgs(trailingOnly = TRUE)
yr <- args[1]
mth <- args[2]
day <- args[3]

match_xovis(date = paste(yr, mth, day, sep = "-"))
