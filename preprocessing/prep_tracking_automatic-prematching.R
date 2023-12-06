#### Libraries ####

library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(parallel)
library(foreach)
source("utils/spatial.r")


#' Algorith to match observation IDs
#' 
#' @param location string
#' @param date date %Y-%m-%d
#' @param min_duration minimum duration of a track, otherwise filtered
#' @param min_total_distance minimum total distance of a track, otherwise filtered
#' @param min_time minimum time (in seconds) to look ahead for matches
#' @param max_time maximum time (in seconds) to look ahead for matches
#' @param max_mov_time maximum time (in seconds) to look ahead when moving
#' @param max_mov_distance maximum distance (in m) to look around when moving
#' @param max_mov_heightdiff maximum height difference (in cm) when moving
#' @param max_sit_time maximum time (in seconds) to look ahead when sitting
#' @param max_sit_distance maximum distance (in m) too look around when sitting
#' @param max_sit_heightdiff maximum height difference (in cm) when sitting

match_xovis <- function(location,
                        date, 
                        min_duration = 10,
                        min_total_distance = 2,
                        min_time = -1,
                        max_time = 30, 
                        max_mov_time = c(5, 10), 
                        max_mov_distance = 3,
                        max_mov_heightdiff = 20,
                        max_sit_time = c(30, 60),
                        max_sit_distance =  0.5,
                        max_sit_heightdiff = 80
                        
) {
  
  #### Data ####
  
  # xovis
  yr <- lubridate::year(as.Date(date))
  file <- paste0("data-raw/", location, "/", yr, "/patient-tracking/annotated/", date, ".rds")
  save_dir <- paste0("data-raw/", location, "/", yr, "/patient-tracking/pre-matched/", date)
  masi <- readRDS(file) 
  
  # info
  n_obs_id <- n_distinct(masi$obs_id)
  
  # pre filter
  masi <- masi %>%
    group_by(obs_id) %>%
    mutate(duration = as.numeric(difftime(last(date_time), first(date_time), units = "secs")),
           distance = convert_dist( euclidean(last(x), first(x), last(y), first(y)) )) %>%
    ungroup() %>%
    filter(!(duration <= min_duration & distance <= min_total_distance)) %>%
    dplyr::select(-distance, -duration)
  
  # info 
  n_obs_id_filt <- n_distinct(masi$obs_id)
  n_filtered <- n_obs_id - n_obs_id_filt
  
  #### Matching ####
  
  # initialize
  masi$obs_id_new <- masi$obs_id
  masi$match_type <- NA
  ids <- unique(masi$obs_id_new)
  
  while(length(ids) > 0) {
    
    # ID
    i <- ids[1]
    ids <- ids[ids!=i]
    
    # continue matching until no more matches can be found in this round
    continue_matching <- T
    
    while (continue_matching) {
      
      # select patient
      masi_p_last <- masi %>%
        filter(obs_id_new == i) %>%
        slice(n())
      
      # check for possible matches
      earliest_start <- masi_p_last$date_time + lubridate::seconds(min_time)
      latest_end <- masi_p_last$date_time + lubridate::seconds(max_time)
      possible_matches <- masi %>%
        filter(obs_id_new > i) %>%
        group_by(obs_id_new) %>%
        slice(1) %>%
        ungroup() %>%
        filter(between(date_time, earliest_start, latest_end)) 
      
      if (nrow(possible_matches) == 0) {
        
        continue_matching <- F
        
      } else {
        
        # compute features
        possible_matches <- possible_matches %>%
          mutate(distance = convert_dist( euclidean(x, masi_p_last$x, y, masi_p_last$y) ),
                 timediff = as.numeric(difftime(date_time, masi_p_last$date_time, units = c("secs"))),
                 heightdiff = abs(height - masi_p_last$height) / 10)
        
        # priority 1: moving
        matches <- possible_matches %>%
          filter(timediff <= max_mov_time[1],
                 distance <= max_mov_distance,
                 heightdiff <= max_mov_heightdiff) %>%
          mutate(match_type = "Moving")
        
        if (nrow(matches) == 0) {
          matches <- possible_matches %>%
            filter(timediff <= max_mov_time[2],
                   distance <= max_mov_distance,
                   heightdiff <= max_mov_heightdiff) %>%
            mutate(match_type = "Moving")
        }
        
        # priority 2: sitting
        if (nrow(matches) == 0 & masi_p_last$is_seat) {
          matches <- possible_matches %>%
            filter(timediff <= max_sit_time[1],
                   distance <= max_sit_distance,
                   heightdiff <= max_sit_heightdiff,
                   is_seat) %>%
            mutate(match_type = "Sitting")
        }
        
        if (nrow(matches) == 0 & masi_p_last$is_seat) {
          matches <- possible_matches %>%
            filter(timediff <= max_sit_time[2],
                   distance <= max_sit_distance,
                   heightdiff <= max_sit_heightdiff,
                   is_seat) %>%
            mutate(match_type = "Sitting")
        }
          
        # only match if there is only one possibility
        if (nrow(matches) != 1) {
          
          continue_matching <- F
          
        } else {
          
          # make match
          masi$obs_id_new[masi$obs_id_new == matches$obs_id_new] <- masi_p_last$obs_id_new
          masi$match_type[masi$obs_id == matches$obs_id] <- matches$match_type
          
          # message
          pid <- masi_p_last$obs_id_new
          oid <- matches$obs_id_new
          type <- matches$match_type
          dt <- matches$timediff
          dd <- as.character( round(matches$distance, 1) )
          dh <- as.character( round(matches$heightdiff, 1) )
          message(sprintf("Linking %s with %s. Type: %s, t: %is, d: %sm, h: %scm.", pid, oid, type, dt, dd, dh))
          
          # remove ID from round
          ids <- ids[ids != matches$obs_id_new]
        }
      }
    }
  }
  
  # save log
  n_obs_id_new <- n_distinct(masi$obs_id_new)
  n_matched <- n_obs_id_filt - n_obs_id_new
  match_type_counts <- masi %>%
    group_by(obs_id) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(match_type = ifelse(is.na(match_type), "Unmatched", match_type)) %>%
    group_by(match_type) %>%
    summarize(n = n()) %>%
    ungroup()
  n_type_moving <- match_type_counts$n[match_type_counts$match_type == "Moving"]
  n_type_sitting <- match_type_counts$n[match_type_counts$match_type == "Sitting"]
  n_type_unmatched <- match_type_counts$n[match_type_counts$match_type == "Unmatched"]
  info_df <- data.frame(
    Variable = c("#IDs before filtering", "#IDs filtered", "IDs after filtering",
                 "#IDs matched", "#IDs after matching",
                 "#Moving matches", "#Sitting matches", "#Ummatched"),
    Count = c(n_obs_id, n_filtered, n_obs_id_filt,
              n_matched, n_obs_id_new,
              n_type_moving, n_type_sitting, n_type_unmatched)
  )
  save_log_file <- paste0(save_dir, "_log", ".csv")
  write.csv(info_df, file = save_log_file, row.names = F)
  
  # save data
  masi <- masi %>% 
    dplyr::select(obs_id_new, obs_id, match_type, everything()) %>%
    group_by(obs_id_new) %>%
    mutate(obs_id_new = max(obs_id)) %>%
    ungroup() 
  save_data_file <- paste0(save_dir, ".rds")
  saveRDS(masi, save_data_file)
}


#### Run matching ####
args <- commandArgs(trailingOnly = TRUE)
cl <- args[1]
yr <- args[2]
no_cores <- as.numeric(args[3])

av_cores <- parallel::detectCores() - 1
n.cores <- ifelse(no_cores > av_cores, av_cores, no_cores)

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
registered <- ifelse(foreach::getDoParRegistered(), "Yes", "No")
message(sprintf("Info: Cluster registration? %s", registered))
message(sprintf("Info: Cluster using %i cores", foreach::getDoParWorkers()))

message(sprintf("Clinic: %s", cl))

# dates
files <- list.files(paste0("data-raw/", cl, "/", yr, "/patient-tracking/annotated/"))
sel_dates <- files[grepl("rds", files)]
sel_dates <- gsub(".rds", "", sel_dates, fixed = T)

foreach::foreach(i = 1:length(sel_dates), .packages = c("tidyverse", "lubridate")) %dopar% {
  d <- as.character(sel_dates[i])
  message(sprintf("-- Date: %s", d))
  match_xovis(location = cl, date = d)
}

parallel::stopCluster(cl = my.cluster)
