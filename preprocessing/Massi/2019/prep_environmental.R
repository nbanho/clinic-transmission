#### Libraries ####

library(tidyverse)
library(haven)
library(lubridate)

source("utils/trans_risk.r")

#### Data ####

df <- read_dta("data-raw/Massi/2019/environmental/CleanCo2data.dta") %>%
  rename(room = location,
         date = RecDate,
         time = RecTime,
         temperature = temp,
         humidity = relhum,
         co2 = co2) %>%
  filter(room == "WaitingRoom") %>%
  mutate(room = "Waiting room",
         date_time = round_date(as.POSIXct(paste(date, format(time, format = "%H:%M:%S"))))) %>%
  dplyr::select(date, room, date_time, co2, temperature, humidity)


#### Filter ####

# get tracking files and dates
tracking_files <- list.files("data-raw/Massi/2019/patient-tracking/xovis/included", full.names = T)
tracking_dates <- basename(tracking_files)
tracking_dates <- as.Date(gsub("_exported_data.csv", "", tracking_dates, fixed = T), format = "%Y_%m_%d")

# check whether tracking dates are in environmental dates
env_dates <- unique(df$date)
unav_dates <- tracking_dates[!(tracking_dates %in% env_dates)]

# remove corresponding files from clean data
unav_files <- paste0("data-clean/Massi/2019/patient-tracking/", unav_dates, ".rds")
file.remove(unav_files)

# check whether environmental dates are in tracking dates
unav_dates_env <- env_dates[!(env_dates %in% tracking_dates)]
env_dates <- env_dates[!(env_dates %in% unav_dates_env)]

# subset CO2 data
df <- df %>%
  filter(date %in% env_dates)

# subset start and end times
tracking_files <- paste0("data-clean/Massi/2019/patient-tracking/", env_dates, ".rds")
se_tracking <- parallel::mclapply(tracking_files, function(f) {
  readRDS(f) %>%
    mutate(date = as.Date(date_time)) %>%
    group_by(date) %>%
    summarize(start_time = first(date_time),
              end_time = last(date_time)) %>%
    ungroup()
}, mc.cores = 6)
se_tracking <- do.call(rbind, se_tracking)
df <- df %>%
  left_join(se_tracking, by = "date") %>%
  mutate(is_in = ifelse(date_time >= start_time, ifelse(date_time <= end_time, T, F), F)) %>%
  filter(is_in) %>%
  dplyr::select(-is_in,-start_time,-end_time)

# plot co2
df %>%
  ggplot(aes(x = date_time, y = co2)) +
  geom_line() +
  facet_wrap(~ date, scales = "free_x")


#### No. of people ####

# load tracking data
tracking_data <- do.call(rbind, parallel::mclapply(tracking_files, readRDS, mc.cores = 6))

# count people
count_people <- function(time) {
  tracking_sub <- filter(tracking_data, is_waitingroom) 
  tracking_sub <- filter(tracking_sub, between(date_time, time, time + minutes(1)))
  n <- n_distinct(tracking_sub$obs_id)
  return(n)
}
df$no_people <- parallel::mcmapply(count_people, df$date_time, mc.cores = 6)

# plot no of people
df %>%
  ggplot(aes(x = date_time, y = no_people)) +
  geom_line() +
  facet_wrap(~ date, scales = "free") 

# remove 2019-08-15
file.remove(paste0("data-clean/Massi/2019/patient-tracking/", "2019-08-15", ".rds"))

# filter co2 data for 2019-08-15
df <- filter(df, date != as.Date("2019-08-15"))


#### Air change rate ####

# compute volume from room data
df <- df %>%
  mutate(volume = 10.55 * 5.7 * 3)

# nest data
aer <- df %>%
  rename(C = co2, 
         n = no_people,
         V = volume) %>%
  group_by(date, room) %>%
  arrange(date_time) %>%
  mutate(C1 = dplyr::lead(C)) %>%
  ungroup() %>%
  na.omit() %>%
  dplyr::select(date, room, date_time, C1, C, n, V) %>%
  nest(date_time, C1, C, n, V) 

# compute air change rates per day and room
aer$aer <- sapply(aer$data, estimate_aer)

aer %>%
  ggplot(aes(x = room, y = aer)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(aes(color = room), width = .2)

df <- df %>%
  left_join(aer %>% dplyr::select(date, room, aer), by = c("date", "room"))


#### Save data ###

df <- df %>%
  dplyr::select(date, room, date_time, aer, no_people, volume, co2, temperature, humidity)
saveRDS(df, "data-clean/Massi/2019/environmental/air-change-rate.rds")

