#### Libraries ####

library(tidyverse)


#### Data ####

# missing data environmental or tracking
file.remove(paste0("data-raw/Massi/2019/patient-tracking/pre-matched/", c("2019-08-13.rds", "2019-08-14.rds", "2019-08-15.rds")))
file.remove(paste0("data-raw/Massi/2019/patient-tracking/pre-matched/", c("2019-08-13_log.csv", "2019-08-14_log.csv", "2019-08-15_log.csv")))

files <- list.files("data-raw/Massi/2019/patient-tracking/pre-matched/", full.names = T)
files <- files[grepl('rds', files)]


#### Filter ####

for (f in files) {
  # load data 
  df <- readRDS(f) 
  
  # filter any observation IDs that were never in the waiting room
  df_filt <- df %>%
    group_by(obs_id) %>%
    filter(any(is_waitingroom) | any(is_exit)) %>%
    ungroup()
  
  # stats
  message(sprintf("Entries: %i (%i percent) filtered", nrow(df) - nrow(df_filt), round(100 * (1 - nrow(df_filt) / nrow(df)))))
  message(sprintf("IDs: %i (%i percent) filtered", n_distinct(df$obs_id_new) - n_distinct(df_filt$obs_id_new), round(100 * (1 - n_distinct(df_filt$obs_id_new) / n_distinct(df$obs_id_new)))))
  
  # save
  saveRDS(df_filt, file = gsub("pre-matched", "waiting-room", f))
}


