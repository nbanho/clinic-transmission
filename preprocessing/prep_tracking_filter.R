#### Libraries ####

library(tidyverse)


#### Data ####

# missing data environmental or tracking
env_dates <- as.Date(paste("2021-", c("10-13", "10-14", "10-15", "10-25", "11-04", "11-05", "11-10", "11-11", "11-12", "11-17", "11-18")))
files <- paste0("data-raw/Massi/2021/patient-tracking/pre-matched/", env_dates, ".rds")


#### Filter ####

for (f in files) {
  # load data 
  df <- readRDS(f) 
  
  # filter any observation IDs that were never in the waiting room
  df_filt <- df %>%
    group_by(obs_id) %>%
    filter(any(is_waitingroom) | any(is_exit)) %>%
    ungroup() %>%
    rename(date_time = time)
  
  # stats
  message(sprintf("Entries: %i (%i percent) filtered", nrow(df) - nrow(df_filt), round(100 * (1 - nrow(df_filt) / nrow(df)))))
  message(sprintf("IDs: %i (%i percent) filtered", n_distinct(df$obs_id_new) - n_distinct(df_filt$obs_id_new), round(100 * (1 - n_distinct(df_filt$obs_id_new) / n_distinct(df$obs_id_new)))))
  
  # save
  saveRDS(df_filt, file = gsub("pre-matched", "waiting-room", f))
}


