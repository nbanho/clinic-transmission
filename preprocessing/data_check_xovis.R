library(tidyverse)

files <- list.files("data-raw/Masi/xovis/Study_Data_Masi_2021/export", full.names = T)
file_names <- basename(files)
dates <- gsub("_exported_data[[:punct:]]csv", "", file_names)
dates <- gsub("_", "-", dates)

i = 1
print(dates[i])
masi <- read.csv(files[i], header = T) %>% 
  set_names(c("obs_id", "time", "x", "y", "height")) %>%
  mutate(time = as.POSIXct(paste(dates[i], time)))
min(masi$time); max(masi$time)
length(unique(masi$obs_id))
dt <- masi %>%
  group_by(obs_id) %>%
  mutate(dt = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
  ungroup() %>%
  na.omit()
summary(dt$dt)
sum(dt$dt>1)
sum(dt$dt<1)
plot(unique(sort(dt$time)))
bad_ones <- filter(dt, dt > 2 | dt < 0) 
bad_ones %>% View()
bad_ids <- unique(bad_ones$obs_id)

saveRDS(masi, paste0("data-raw/Masi/xovis/", dates[i], ".rds"))
saveRDS(masi %>% filter(!obs_id %in% bad_ids), paste0("data-raw/Masi/xovis/", dates[i], ".rds"))


# automated
file_dates <- c(paste0("2021-10-", c("13", "14", "15", "18", "19", "20", "22", "25")), 
                paste0("2021-11-", c("02", "04", "05", "10", "12", "18")))
files_aut <- paste0("data-raw/Masi/xovis/Study_Data_Masi_2021/export/", gsub("-", "_", file_dates), "_exported_data.csv")

for (i in 1:length(files_aut)) {
  masi <- read.csv(files_aut[i], header = T) %>% 
    set_names(c("obs_id", "time", "x", "y", "height")) %>%
    mutate(time = as.POSIXct(paste(file_dates[i], time)))
  dt <- masi %>%
    group_by(obs_id) %>%
    mutate(dt = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
    ungroup() %>%
    na.omit()
  bad_ones <- filter(dt, dt > 2 | dt < 0) 
  bad_ids <- unique(bad_ones$obs_id)
  saveRDS(masi %>% filter(!obs_id %in% bad_ids), paste0("data-raw/Masi/xovis/", file_dates[i], ".rds"))
}
