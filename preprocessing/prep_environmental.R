#### Libraries ####

library(tidyverse)
library(lubridate)
library(haven)
library(zoo)

# linear imputation
lin_imp <- function(x, mg = 600) {
  na.locf(
    na.locf(
      na.approx(x, maxgap = mg, na.rm = FALSE),
      maxgap = mg, na.rm = FALSE
    ),
    fromLast = TRUE, maxgap = mg, na.rm = FALSE
  )
}

# coalesce / imputation
imp_xy <- function(x, y) {
  if (!is.na(x) & !is.na(y)) {
    return(mean(c(x, y)))
  } else if (!is.na(x)) {
    return(x)
  } else if (!is.na(y)) {
    return(y)
  } else {
    return(NA)
  }
}

source("utils/trans_risk.r")

#### Environmental data ####

# Backup data
files <- list.files("data-raw/environmental/backup/", full.names = T)

masi2021 <- do.call(
  rbind,
  lapply(files, function(f) {
    read.table(f, sep = "\t", header = F, skip = 33) %>%
      set_names(c("date", "time", "humidity", "temperature", "co2", "ext")) %>%
      dplyr::select(-ext) %>%
      mutate(file = basename(f))
  })
) %>%
  mutate(
    date_created = as.Date(stringi::stri_extract(file, regex = "\\d{8}"), "%Y%m%d"),
    date = as.Date(date, format = "%m/%d/%Y"),
    sensor = as.integer(gsub("-", "", stringi::stri_extract(file, regex = "-\\d{1}"))),
    date_time = parse_date_time(paste(date, time), "%Y-%m-%d %I:%M:%S %p", tz = "CET"),
    date_time = round_date(date_time, unit = "minutes"),
    date = as.Date(date_time)
  ) %>%
  filter(sensor > 1) %>%
  mutate(
    room = ifelse(sensor == 2, "Waiting room", ifelse(sensor == 3, "Corridor", "TB room")),
    room = factor(room, levels = c("Waiting room", "Corridor", "TB room"))
  ) %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity) %>%
  mutate(across(c(co2, temperature, humidity), as.numeric))

# Preprocessed data from Kathrin
masi2021_prep <- read_dta("data-raw/environmental/co2_data.dta") %>%
  rename(
    date = Date,
    co2 = CO2,
    temperature = Temperature,
    humidity = Humidity,
    room = location
  ) %>%
  filter(room > 1) %>%
  mutate(
    room = ifelse(room == 2, "Waiting room", "TB room"),
    room = factor(room, levels = c("Waiting room", "TB room")),
    date_time = as.POSIXct(paste(date, format(new_time, format = "%H:%M:%S"))),
    across(c(co2, temperature, humidity), as.numeric)
  ) %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity)


# filter
# Dates with tracking data
tracking_dates <- basename(list.files("data-clean/patient-tracking/linked-and-imputed"))
tracking_dates <- tracking_dates[grepl("rds", tracking_dates)]
tracking_dates <- gsub(".rds", "", tracking_dates, fixed = T)
tracking_dates <- as.Date(tracking_dates)

masi2021 <- masi2021 %>%
  filter(date %in% tracking_dates)

masi2021 %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

# the following dates are good to use in the backup data
env_dates <- as.Date(paste("2021-", c("10-13", "10-14", "10-15", "10-25", "11-04", "11-05", "11-10", "11-11", "11-12", "11-17", "11-18")))

masi2021_prep <- masi2021_prep %>%
  filter(date %in% tracking_dates)

masi2021_prep %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

env_dates_prep <- as.Date(paste("2021-", c("10-25", "11-05", "11-10", "11-11", "11-12", "11-17", "11-18")))

env_dates_prep[!(env_dates_prep %in% env_dates)] # backup data contains all preprocessed data

masi2021 <- masi2021 %>%
  filter(date %in% env_dates)

masi2021 %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()


# Filter start and end time
#' Start time: 8:00 am
#' End time: 4:00pm

masi2021 <- masi2021 %>%
  filter(between(hour(date_time), 8, 15))
masi2021_prep <- masi2021_prep %>%
  filter(between(hour(date_time), 8, 15))

# clinic was probably closed in the afternoon of Oct 15 --> stop at 13:00

masi2021 <- masi2021 %>%
  filter(!(between(
    date_time,
    as.POSIXct("2021-10-15 12:00:00"),
    as.POSIXct("2021-10-15 23:59:00")
  )))
masi2021_prep <- masi2021_prep %>%
  filter(!(between(
    date_time,
    as.POSIXct("2021-10-15 12:00:00"),
    as.POSIXct("2021-10-15 23:59:00")
  )))

# plot
masi2021 %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

# coalesce data
env <- full_join(
  masi2021,
  masi2021_prep,
  by = c("date", "date_time", "room")
)

env <- env %>%
  mutate(
    co2 = map2_dbl(co2.x, co2.y, imp_xy),
    temperature = map2_dbl(temperature.x, temperature.y, imp_xy),
    humidity = map2_dbl(humidity.x, humidity.y, imp_xy)
  )

env %>%
  filter(room == "Waiting room") %>%
  ggplot(aes(x = date_time)) +
  geom_line(aes(y = co2.x, color = "red")) +
  geom_line(aes(y = co2.y, color = "blue")) +
  geom_line(aes(y = co2, color = "green")) +
  facet_wrap(~date, scales = "free_x")

# plot
env %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

# mean by minute
env <- env %>%
  group_by(date, date_time, room) %>%
  summarise(
    co2 = mean(co2, na.rm = TRUE),
    temperature = mean(temperature, na.rm = TRUE),
    humidity = mean(humidity, na.rm = TRUE)
  ) %>%
  ungroup()

# missing data
# show missings
env %>%
  filter(is.na(co2) | is.na(temperature) | is.na(humidity))

# impute with later values the early missings on Oct 13 at 8:01 and 8:02
env <- env %>%
  group_by(date) %>%
  arrange(date_time) %>%
  fill(co2, temperature, humidity, .direction = "up") %>%
  ungroup()

# impute corridor data on Oct 13
#' On 10-13 the corridor data is too crude.
#' On other days, the corridor closely follows the waiting room.
#' On this day, we assume they are the same as well.

env_1013_cr <- env %>%
  filter(
    date == as.Date("2021-10-13"),
    room == "Waiting room"
  ) %>%
  mutate(room = "Corridor")

env <- env %>%
  filter(!(date == as.Date("2021-10-13") & room == "Corridor")) %>%
  rbind(env_1013_cr)

# plot
env %>%
  ggplot(aes(x = date_time, y = co2, color = room)) +
  facet_wrap(~date, scales = "free_x") +
  geom_line()

# save
env <- env %>%
  dplyr::select(date, date_time, room, co2, temperature, humidity)

saveRDS(env, "data-clean/environmental/continuous-iaq-data.rds")


#### Room occupancy ####

# tracking data
tracking_data <- do.call(
  rbind,
  lapply(
    list.files("data-raw/patient-tracking/annotated",
      full.names = TRUE
    ),
    readRDS
  )
) %>%
  rename(date_time = time) %>%
  mutate(date = as.Date(date_time)) %>%
  filter(date %in% tracking_dates) %>%
  filter(between(hour(date_time), 8, 15)) %>%
  mutate(
    room = ifelse(is_waitingroom, "Waiting room",
      ifelse(is_tbroom, "TB room",
        ifelse(is_passage, "Corridor", NA)
      )
    ),
    room = factor(room, levels = c("Waiting room", "Corridor", "TB room"))
  ) %>%
  filter(!is.na(room))

# count per second
no_ppl <- expand.grid(
  date = unique(tracking_data$date),
  room = c("Waiting room", "Corridor", "TB room")
)
no_ppl$date_time <- lapply(no_ppl$date, function(d) {
  ts <- as.POSIXct(paste(d, "08:00:00"))
  te <- as.POSIXct(paste(d, "16:00:00"))
  t <- seq(ts, te, by = "1 sec")
  return(t)
})
no_ppl <- unnest(no_ppl, date_time) %>%
  filter(!(between(
    date_time, as.POSIXct("2021-10-15 12:00:00"),
    as.POSIXct("2021-10-15 23:59:00")
  )))
no_ppl <- no_ppl %>%
  left_join(
    tracking_data %>%
      dplyr::select(room, date_time, obs_id),
    by = c("date_time", "room")
  ) %>%
  group_by(room, date, date_time) %>%
  summarise(
    n = ifelse(all(is.na(obs_id)), NA, n_distinct(obs_id, na.rm = TRUE))
  ) %>%
  ungroup()

no_ppl %>%
  ggplot(aes(x = date_time, y = n, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

no_ppl %>%
  group_by(date, room) %>%
  summarise(
    n = sum(n, na.rm = TRUE)
  )

# set NA in TB room to zero
no_ppl <- no_ppl %>%
  mutate(n = ifelse(is.na(n) & room == "TB room", 0, n))

# count per minute
no_ppl_min <- no_ppl %>%
  mutate(date_time = round_date(date_time, "minute")) %>%
  group_by(room, date, date_time) %>%
  summarise(
    n = ifelse(all(is.na(n)), NA, mean(n, na.rm = TRUE))
  ) %>%
  ungroup()

no_ppl_min %>%
  ggplot(aes(x = date_time, y = n, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# imputation
no_ppl_min <- no_ppl_min %>%
  group_by(room, date) %>%
  arrange(date_time) %>%
  mutate(n = lin_imp(n, mg = 40)) %>%
  ungroup()

no_ppl_min %>%
  ggplot(aes(x = date_time, y = n, color = room)) +
  geom_line() +
  facet_wrap(~date, scales = "free_x")

# save
saveRDS(no_ppl_min, "data-clean/patient-tracking/tracks-over-time.rds")


#### Air change rate ####
#' compute by daytime:
#' - morning: 8am to 12am
#' - afternoon: 12am to 4pm

aer <- left_join(
  env,
  no_ppl_min,
  by = c("date", "room", "date_time")
)

# add volume from room data
aer <- aer %>%
  mutate(volume = ifelse(room == "Waiting room", 10.55 * 5.7 * 3,
    ifelse(room == "Corridor", 7.7 * 2.2 * 2.5,
      4.75 * 3.5 * 3
    )
  )) # TB room

# nest data
aer <- aer %>%
  mutate(daytime = ifelse(hour(date_time) <= 12, "Morning", "Afternoon")) %>%
  rename(
    C = co2,
    V = volume
  ) %>%
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

aer %>%
  ggplot(aes(x = room, y = aer)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(aes(color = room, shape = daytime), width = .2)


# save
saveRDS(aer, "data-clean/environmental/air-change-rate.rds")
