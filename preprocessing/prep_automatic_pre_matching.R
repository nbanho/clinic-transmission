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
#' @param max_time maximum time (in seconds) to look ahead for matches
#' @param min_standing_height minimum standing height
#' @param max_mov_time maximum time (in seconds) to look ahead for people moving
#' @param max_sit_time maximum time (in seconds) to look ahead for people sitting
#' @param max_mov_distance maximum distance someone can make while moving
#' @param max_sit_distance maximum distance someone can make while sitting
#' @param max_mov_heightdiff maximum height difference when moving
#' @param min_duration minimum duration of a track, otherwise filtered
#' @param min_total_distance minimum total distance of a track, otherwise filtered

match_xovis <- function(location,
                        date, 
                        max_time = 30 * 60, 
                        min_standing_height = 1500, 
                        max_mov_time = 10, 
                        max_mov_distance = 3,
                        max_sit_time = 30,
                        max_sit_distance =  0.5,
                        max_mov_heightdiff = 100,
                        min_duration = 5,
                        min_total_distance = 1
) {
  
  #### Data ####
  
  # xovis
  file <- paste0("data-raw/", location, "/xovis/", date, ".rds")
  save_dir <- paste0("data-raw/", location, "/patient-tracking-data/", date, ".rds")
  masi <- readRDS(file) 
  
  # pre filter
  masi <- masi %>%
    group_by(obs_id) %>%
    mutate(duration = as.numeric(difftime(last(time), first(time), units = "secs")),
           distance = convert_dist( euclidean(first(x), last(x), first(y), last(y)) ),
           first_entered = first(is_entrance),
           has_exited = any(is_exit)) %>%
    ungroup() %>%
    filter(!(duration <= min_duration & distance <= min_total_distance & !first_entered & !has_exited) ) %>%
    dplyr::select(-distance, -duration)
  
  #### Matching ####
  
  # initialize
  masi$obs_id_new <- masi$obs_id
  ids <- unique(masi$obs_id_new)
  
  while(length(ids) > 0) {
    
    # ID
    i <- ids[1]
    ids <- ids[ids!=i]
    
    # continue matching until no more matches can be found in this round
    continue_matching <- T
    
    while (continue_matching) {
      
      # select patient
      masi_p <- filter(masi, obs_id_new == i)
      masi_p_last <- tail(masi_p, 1)
      
      # check for possible matches
      possible_matches <- masi %>%
        group_by(obs_id_new) %>%
        slice(1) %>%
        ungroup() %>%
        filter(between(time, masi_p_last$time %m+% seconds(1), masi_p_last$time %m+% seconds(max_time))) %>%
        mutate(p_height = masi_p_last$height,
               p_is_seat = masi_p_last$is_seat)
      
      if (nrow(possible_matches) == 0) {
        
        continue_matching <- F
        
      } else {
        
        # compute features
        possible_matches <- possible_matches %>%
          mutate(distance = convert_dist( euclidean(x, masi_p_last$x, y, masi_p_last$y) ),
                 timediff = as.numeric(difftime(time, masi_p_last$time, units = c("secs"))),
                 heightdiff = abs(height - masi_p_last$height),
                 !is_seat)
        
        # priority 1: moving
        matches_moving <- possible_matches %>%
          filter(timediff <= max_mov_time,
                 distance <= max_mov_distance,
                 heightdiff <= max_mov_heightdiff,
                 !p_is_seat) %>%
          mutate(match_type = "Moving") 
        
        # priority 2: person sitting in seating area
        matches_sitting <- possible_matches %>%
          filter(timediff <= max_sit_time,
                 distance <= max_sit_distance,
                 p_is_seat,
                 height <= min_standing_height,
                 p_height <= min_standing_height) %>%
          mutate(match_type = "Seating area: sitting")
        
        # priority 3: person moving in seating area
        matches_moving_seating <- possible_matches %>%
          filter(timediff <= max_mov_time,
                 distance <= max_sit_distance,
                 p_is_seat) %>%
          mutate(match_type = "Seating area: moving") 
          
        # combine matches
        matches <- rbind(matches_moving, matches_sitting, matches_moving_seating) 
        
        if (nrow(matches) == 0) {
          
          continue_matching <- F
          
        } else {
          
          # rank matches 
          matches <- matches %>%
            arrange(timediff, distance, heightdiff) %>%
            slice(1)
          
          # make match
          masi$obs_id_new[masi$obs_id_new==matches$obs_id_new] <- masi_p_last$obs_id_new
          
          # message
          pid <- masi_p_last$obs_id_new
          oid <- matches$obs_id_new
          type <- matches$match_type
          dt <- matches$timediff
          dd <- as.character( round(matches$distance, 1) )
          dh <- as.character( round(matches$heightdiff / 10, 1) )
          message(sprintf("Linking %s with %s. Type: %s, t: %is, d: %sm, h: %scm.", pid, oid, type, dt, dd, dh))
          
          # remove ID from round
          ids <- ids[ids!=matches$obs_id_new]
          
          # check if patient exited
          if (tail(masi$is_exit[masi$obs_id_new == masi_p_last$obs_id_new], 1)) {
            
            continue_matching <- F
            
          }
        }
      }
    }
  }
  saveRDS(masi %>% dplyr::select(obs_id_new, obs_id, everything()), save_dir)
}


#### Run matching ####
args <- commandArgs(trailingOnly = TRUE)
clinics <- ifelse(args[1]=="Both", c("Masi", "Ocean"), args[1])
no_cores <- as.numeric(args[2])

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

for(cl in clinics) {
  
  message(sprintf("Clinic: %s", cl))
  
  # dates
  files <- list.files(paste0("data-raw/", cl, "/xovis/"))
  sel_dates <- files[grepl("rds", files)]
  sel_dates <- gsub(".rds", "", sel_dates, fixed = T)
  
  foreach::foreach(i = 1:length(sel_dates), .packages = c("tidyverse", "lubridate")) %dopar% {
    d <- as.character(sel_dates[i])
    message(sprintf("-- Date: %s", d))
    match_xovis(location = cl, date = d)
  }
}

parallel::stopCluster(cl = my.cluster)
