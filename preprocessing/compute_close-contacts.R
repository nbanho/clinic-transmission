#### Libraries ####

library(tidyverse)
library(lubridate)
library(parallel)

source("utils/spatial.r")


#### Data ####

df <- do.call(rbind, lapply(list.files("data-clean/patient-tracking/linked-and-imputed/", full.names = T), readRDS)) %>%
  mutate(date = as.Date(time))

#### Functions ####

close_contacts <- function(i, x_i, y_i, t_i, close_dist = 1) {
  df_i <- df[df$patient_id != i & df$time == t_i, ]
  d_i <- convert_dist(euclidean(x_i, df_i[["x"]], y_i, df_i[["y"]]))
  df_i[["patient_id"]][d_i <= close_dist]
}

df$closeContacts <- mcmapply(close_contacts, df$patient_id, df$x, df$y, df$time, mc.cores = 6)

saveRDS(df, file = "data-clean/patient-tracking/augmented-and-combined-data.rds")