#### Libraries ####

library(tidyverse)
source("utils/spatial.r")


#### Data ####

files <- list.files("data-clean/patient-tracking/matched-clinical/", full.names = T)
files <- files[grepl("rds", files)]
df_list <- lapply(files, readRDS)


#### Activity ####
#' Determine movement, assuming that less than 0.25m movement in 1s means the person is still
#' sitting or standing, i.e. waiting. The 0.25 correspond to the cell size.
#' 

for (i in 1:length(df_list)) {
  df <- df_list[[i]] %>%
    group_by(patient_id) %>%
    arrange(time) %>%
    mutate(distance = euclidean(x, lag(x), y, lag(y))) %>%
    fill(distance, .direction = "up") %>% 
    ungroup() %>%
    mutate(distance = convert_dist(distance),
           activity = ifelse(distance <= .25, "waiting", "walking")) 
  
  saveRDS(df, files[[i]])
}
