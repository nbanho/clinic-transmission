#### Libraries ####

library(tidyverse)
library(lubridate)
library(haven)

source("utils/trans_risk.r")

#### Data ####

# Backup data
files <- list.files("data-raw/environmental/backup/", full.names = T)

masi2021 <- do.call(rbind,
              lapply(files, function(f) {
                read.table(f, sep = "\t", header = F, skip = 33) %>%
                  set_names(c("date", "time", "humidity", "temperature", "co2", "ext")) %>%
                  dplyr::select(-ext) %>%
                  mutate(file = basename(f))
              })) %>%
  mutate(date_created = as.Date(stringi::stri_extract(file, regex = "\\d{8}"), "%Y%m%d"),
         date = as.Date(date, format = "%m/%d/%Y"),
         sensor = as.integer(gsub("-", "", stringi::stri_extract(file, regex = "-\\d{1}"))),
         date_time = parse_date_time(paste(date, time), '%Y-%m-%d %I:%M:%S %p', tz = "CET"),
         date_time = round_date(date_time, unit = "minutes"),
         date = as.Date(date_time)) %>%
  filter(sensor > 1) %>%
  mutate(room = ifelse(sensor == 2, "Waiting room", ifelse(sensor == 3, "Corridor", "TB room")),
         room = factor(room, levels = c("Waiting room", "Corridor", "TB room"))) %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity) %>%
  mutate(across(c(co2, temperature, humidity), as.numeric))

# Preprocessed data from Kathrin
masi2021_prep <- read_dta("data-raw/environmental/co2_data.dta") %>%
  rename(date = Date,
         co2 = CO2,
         temperature = Temperature,
         humidity = Humidity,
         room = location) %>%
  filter(room > 1) %>%
  mutate(room = ifelse(room == 2, "Waiting room", "TB room"),
         room = factor(room, levels = c("Waiting room", "TB room")),
         date_time = as.POSIXct(paste(date, format(new_time, format = "%H:%M:%S"))),
         across(c(co2, temperature, humidity), as.numeric)) %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity)

#### Filter ####

# Dates with tracking data
tracking_dates <- basename(list.files("data-clean/patient-tracking/linked-and-imputed"))
tracking_dates <- tracking_dates[grepl("rds", tracking_dates)]
tracking_dates <- gsub(".rds", "", tracking_dates, fixed = T)
tracking_dates <- as.Date(tracking_dates)

masi2021 <- masi2021 %>% 
  filter(date %in% tracking_dates)

masi2021 %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()

# the following dates are good to use in the backup data
env_dates <- as.Date(paste("2021-", c("10-13", "10-14", "10-15", "10-25", "11-04", "11-05", "11-10", "11-11", "11-12", "11-17", "11-18")))

masi2021_prep <- masi2021_prep %>%
  filter(date %in% tracking_dates)

masi2021_prep %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()

env_dates_prep <- as.Date(paste("2021-", c("10-25", "11-05", "11-10", "11-11", "11-12", "11-17", "11-18")))

env_dates_prep[!(env_dates_prep %in% env_dates)] # backup data contains all preprocessed data

masi2021 <- masi2021 %>%
  filter(date %in% env_dates)

masi2021 %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()


# Filter start and end time
#' Start time: 8:00 am
#' End time: 4:00pm

masi2021 <- masi2021 %>%
  filter(between(hour(date_time), 8, 16))

# clinic was probably closed in the afternoon of Oct 15 --> stop at 13:00

masi2021 <- masi2021 %>%
  filter(!(between(date_time, as.POSIXct("2021-10-15 13:00:00"), as.POSIXct("2021-10-15 23:55:00"))))

# plot
masi2021 %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()
  

#### Imputation ####

# show missings
masi2021 %>% 
  filter(is.na(co2) | is.na(temperature) | is.na(humidity))

# impute with later values the early missings on Oct 13 at 8:01 and 8:02
masi2021 <- masi2021 %>%
  group_by(date) %>%
  arrange(date_time) %>%
  fill(co2, temperature, humidity, .direction = "up") %>%
  ungroup()

# impute corridor data on Oct 13
#' On 10-13 the corridor data is too crude.
#' On other days, the corridor closely follows the waiting room.
#' On this day, we assume they are the same as well.

masi2021_1013_cr <- masi2021 %>%
  filter(date == as.Date("2021-10-13"),
         room == "Waiting room") %>%
  mutate(room = "Corridor")

masi2021 <- masi2021 %>%
  filter(!(date == as.Date("2021-10-13") & room == "Corridor")) %>%
  rbind(masi2021_1013_cr)

# plot
masi2021 %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()


#### Add occupancy ####

# tracking data
tracking_data <- do.call(rbind, lapply(list.files("data-clean/patient-tracking/linked-and-imputed", full.names = T), readRDS))

# compute number of people
count_people <- function(t, s) {
  if (s == "Waiting room") {
    tracking_sub <- filter(tracking_data, is_waitingroom) 
  } else if (s == "Corridor") {
    tracking_sub <- filter(tracking_data, is_passage)
  } else {
    tracking_sub <- filter(tracking_data, is_tbroom)
  }
  tracking_sub <- filter(tracking_sub, between(time, t, t + minutes(1)))
  n <- n_distinct(tracking_sub$patient_id)
  return(n)
}
masi2021$no_people <- parallel::mcmapply(count_people, masi2021$date_time, masi2021$room, mc.cores = 6)

masi2021 %>%
  ggplot(aes(x = date_time, y = no_people, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()


#### Air change rate ####
#' compute by daytime:
#' - morning: 8am to 12am
#' - afternoon: 12am to 4pm

# compute volume from room data
masi2021 <- masi2021 %>%
  mutate(volume = ifelse(room == "Waiting room", 10.55 * 5.7 * 3,
                         ifelse(room == "Corridor", 7.7 * 2.2 * 2.5,
                                4.75 * 3.5 * 3))) # TB room 

# nest data
aer <- masi2021 %>%
  mutate(daytime = ifelse(hour(date_time) <= 12, "Morning", "Afternoon")) %>%
  rename(C = co2, 
         n = no_people,
         V = volume) %>%
  group_by(date, daytime, room) %>%
  arrange(date_time) %>%
  mutate(C1 = dplyr::lead(C)) %>%
  ungroup() %>%
  na.omit() %>%
  dplyr::select(date, daytime, room, date_time, C1, C, n, V) %>%
  nest(date_time, C1, C, n, V) 

# compute air change rates per day and room
aer$aer_optim_res <- sapply(aer$data, estimate_aer)
aer$aer <- sapply(aer$aer_optim_res, function(l) l$par[1])
aer$Cr <- sapply(aer$aer_optim_res, function(l) l$par[2])
aer <- aer %>%
  left_join(masi2021 %>% dplyr::select(room, volume) %>% group_by(room) %>% slice(1) %>% ungroup()) %>%
  mutate(Q = aer * volume / 3600 * 1e3)

aer %>%
  ggplot(aes(x = room, y = aer)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(aes(color = room, shape = daytime), width = .2)


#### Save data ####

saveRDS(masi2021, "data-clean/environmental/continuous-iaq-data.rds")
saveRDS(aer, "data-clean/environmental/air-change-rate.rds")
