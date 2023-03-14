library(tidyverse)

files <- list.files("data-raw/Masi/xovis/Study_Data_Masi_2021/export", full.names = T)
file_names <- basename(files)
dates <- gsub("_exported_data[[:punct:]]csv", "", file_names)
dates <- gsub("_", "-", dates)

i = 21
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
bad_ones <- filter(dt, dt > 1 | dt < 1) 
bad_ones %>% View()
bad_ids <- unique(bad_ones$obs_id)

saveRDS(masi, paste0("data-raw/Masi/xovis/", dates[i], ".rds"))
saveRDS(masi %>% filter(!obs_id %in% bad_ids), paste0("data-raw/Masi/xovis/", dates[i], ".rds"))

  