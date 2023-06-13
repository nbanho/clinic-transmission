#### Libraries ####

library(tidyverse)
library(lubridate)
library(haven)

source("utils/trans_risk.r")

#### Backup data ####

files <- list.files("data-raw/Massi/2021/environmental/backup/", full.names = T)

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

#### Preprocessed data from Kathrin ####

masi2021_prep <- read_dta("data-raw/Massi/2021/environmental/co2_data.dta") %>%
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

#### Filter dates with tracking data ####

tracking_dates <- basename(list.files("data-raw/Massi/2021/patient-tracking/"))
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


#### Filter start/end time of tracking ####

tracking_files <- list.files("data-raw/Massi/2021/patient-tracking/", full.names = T)
tracking_files <- tracking_files[grepl("rds", tracking_files)]
tracking_files <- tracking_files[as.Date(gsub(".rds", "", basename(tracking_files), fixed = T)) %in% env_dates]
tracking_data <- do.call(rbind, lapply(tracking_files, readRDS)) %>%
  mutate(date = as.Date(time)) %>%
  rename(date_time = time)

se_tracking_times <- tracking_data %>%
  group_by(date) %>%
  summarize(start_time = first(date_time),
            end_time = last(date_time)) %>%
  ungroup()

masi2021 <- masi2021 %>%
  left_join(se_tracking_times, by = "date") %>%
  mutate(is_in = ifelse(date_time >= start_time, ifelse(date_time <= end_time, T, F), F)) %>%
  filter(is_in) %>%
  dplyr::select(-is_in)

masi2021 %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()
  

#### Imputation ####

# linearly impute missing values in between
masi2021 <- masi2021 %>%
  group_by(date, room) %>%
  arrange(date_time) %>%
  mutate(across(c(co2, humidity, temperature), zoo::na.approx, na.rm = F, maxgap = Inf)) %>%
  ungroup()

# missing values at the end of 10-15 in waiting and TB room are assumed to follow the trend of the corridor
end_time_1015_wr <- max(masi2021$date_time[masi2021$date==as.Date("2021-10-15") & masi2021$room == "Waiting room"])
end_time_1015_cr <- max(masi2021$date_time[masi2021$date==as.Date("2021-10-15") & masi2021$room == "Corridor"])
missing_dt_1015 <- seq(end_time_1015_wr, end_time_1015_cr, by = "1 min")
missing_1015 <- masi2021 %>%
  filter(room == "Corridor",
         date_time %in% missing_dt_1015) %>%
  mutate(across(c(co2, temperature, humidity), ~ c(NA, diff(.x))))


missing_1015_wr <- masi2021 %>%
  filter(room == "Waiting room",
         date == as.Date("2021-10-15")) %>%
  arrange(date_time) %>%
  slice(n()) %>%
  rbind(missing_1015) %>%
  filter(!is.na(co2)) %>%
  mutate(across(c(co2, temperature, humidity), cumsum)) %>%
  slice(-1) %>%
  mutate(room = "Waiting room")

missing_1015_tb <- masi2021 %>%
  filter(room == "TB room",
         date == as.Date("2021-10-15")) %>%
  arrange(date_time) %>%
  slice(n()) %>%
  rbind(missing_1015) %>%
  filter(!is.na(co2)) %>%
  mutate(across(c(co2, temperature, humidity), cumsum)) %>%
  slice(-1) %>%
  mutate(room = "TB room")

masi2021 <- masi2021 %>%
  rbind(missing_1015_wr) %>%
  rbind(missing_1015_tb) %>%
  group_by(date, room) %>%
  arrange(date_time) %>%
  ungroup()

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

masi2021 %>% 
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()


#### Number of people in the room ####

# impute time steps in tracking data
tracking_data <- tracking_data %>%
  dplyr::select(obs_id_new, date, date_time, is_waitingroom, is_tbroom, is_passage) %>%
  group_by(date, obs_id_new) %>%
  complete(date_time = seq(min(date_time), max(date_time), by = "1 sec")) %>%
  fill(is_waitingroom, is_tbroom, is_passage, .direction = "downup") %>%
  ungroup()

# compute number of people
count_people <- function(time, location) {
  if (location == "Waiting room") {
    tracking_sub <- filter(tracking_data, is_waitingroom) 
  } else if (location == "Corridor") {
    tracking_sub <- filter(tracking_data, is_passage)
  } else {
    tracking_sub <- filter(tracking_data, is_tbroom)
  }
  tracking_sub <- filter(tracking_sub, between(date_time, time, time + minutes(1)))
  n <- n_distinct(tracking_sub$obs_id_new)
  return(n)
}
masi2021$no_people <- parallel::mcmapply(count_people, masi2021$date_time, masi2021$room, mc.cores = 6)

masi2021 %>%
  ggplot(aes(x = date_time, y = no_people, color = room)) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line()

# ignore late observations on 10-15
# masi2021 %>%
#   filter(date == "2021-10-15", 
#          date_time >= "2021-10-15 12:30:00") %>%
#   View() # --> set to 12:45:00

masi2021 <- masi2021 %>%
  filter(!(between(date_time, as.POSIXct("2021-10-15 12:45:00"), as.POSIXct("2021-10-15 18:00:00"))))

# set 11-12 missing to NA
# masi2021 %>%
#   filter(date == "2021-11-12") %>%
#   View() # --> between 7:25:00 and 8:16:00

masi2021 <- masi2021 %>%
  mutate(no_people = ifelse(date_time %in% seq(as.POSIXct("2021-11-12 07:25:00"), as.POSIXct("2021-11-12 08:16:00"), by = "1 min"), NA, no_people))


#### Air change rate ####

# compute volume from room data
masi2021 <- masi2021 %>%
  mutate(volume = ifelse(room == "Waiting room", 10.55 * 5.7 * 3,
                         ifelse(room == "Corridor", 7.7 * 2.2 * 2.5,
                                4.75 * 3.5 * 3)))

# nest data
aer <- masi2021 %>%
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

# start after NAs on 11-12
for (i in which(aer$date == as.Date("2021-11-12"))) {
  aer$data[[i]] <- aer$data[[i]] %>% 
    filter(date_time > as.POSIXct("2021-11-12 08:16:00"))
}

# compute air change rates per day and room
aer$aer <- sapply(aer$data, estimate_aer)

aer %>%
  ggplot(aes(x = room, y = aer)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(aes(color = room), width = .2)

# outliers on 11-11 in the waiting room and corridor
#' here we can use the steady-state method instant since both occupancy and CO2 hardly vary 
#' we use the median CO2 and number of people
#' we assume Cr = 400

n1111_wr <- median(masi2021$no_people[masi2021$date == "2021-11-11" & masi2021$room == "Waiting room"])
co1111_wr <- median(masi2021$co2[masi2021$date == "2021-11-11" & masi2021$room == "Waiting room"])
aer$aer[aer$date == "2021-11-11" & aer$room == "Waiting room"] <- steady_state_aer(n1111_wr, 0.004 * 60, 10.55 * 5.7 * 3, co1111_wr, 400)
n1111_cr <- median(masi2021$no_people[masi2021$date == "2021-11-11" & masi2021$room == "Corridor"])
co1111_cr <- median(masi2021$co2[masi2021$date == "2021-11-11" & masi2021$room == "Corridor"])
aer$aer[aer$date == "2021-11-11" & aer$room == "Corridor"] <- steady_state_aer(n1111_cr, 0.004 * 60, 10.55 * 5.7 * 3, co1111_cr, 400)

aer %>%
  ggplot(aes(x = date, y = aer, fill = room)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_date(breaks = "1 day")

# add to data
masi2021 <- masi2021 %>%
  left_join(aer %>% dplyr::select(date, room, aer), by = c("date", "room"))


#### Save data ####

masi2021 <- masi2021 %>%
  dplyr::select(date, room, date_time, aer, no_people, volume, co2, temperature, humidity)
saveRDS(masi2021, "data-clean/Massi/2021/environmental/air-change-rate.rds")
