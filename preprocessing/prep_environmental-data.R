#### Libraries ####

library(tidyverse)

#### Masi ####

# load data
masi <- readxl::read_xlsx("data-raw/Masi/environmental-data/co2_data follow-up_2.xlsx", 
                          sheet = "all_in_one") %>%
  rename(date = Date, 
         time = Time,
         humidity = Humidity,
         temperature = Temperature,
         co2 = CO2,
         location = Monitor) %>%
  mutate(exact_time = format(time, format = "%H:%M:%S"),
         time = format(time, format = "%H:%M"),
         exact_date_time = as.POSIXct(paste(date, exact_time), format = "%Y-%m-%d %H:%M:%S"),
         date_time = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M"),
         date = as.Date(date),
         location = ifelse(location == "TB room", "tb room", location),
         across(c(temperature, humidity, co2), as.numeric)) %>%
  dplyr::select(date, exact_date_time, date_time, location, co2, temperature, humidity)

# check
masi %>%
  ggplot(aes(x = date_time, y = co2, color = location)) +
  geom_line() +
  facet_wrap(~ date, scales = "free_x") +
  scale_x_datetime(date_labels = "%H:%M")
masi %>%
  group_by(location, date, date_time) %>%
  summarize(n = n()) %>%
  ggplot(aes(x = n)) + geom_histogram()
# masi %>%
#   group_by(location, date, date_time) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>%
#   arrange(date_time) %>%
#   View()

#' TODO: check reason for double entries
#' for now: use the first timestamp

masi_double_entries <- masi %>%
  group_by(location, date, date_time) %>%
  arrange(exact_date_time) %>%
  mutate(id = 1:n()) %>%
  filter(max(id) > 1) %>%
  ungroup() 

masi_double_entries %>%
  dplyr::filter(location == "tb room", 
                date %in% as.Date(c("2021-11-08", "2021-11-10"))) %>%
  ggplot(aes(x = exact_date_time, y = co2, color = factor(id))) +
  facet_wrap(~ date, scales = "free_x") +
  geom_line() +
  scale_x_datetime(date_labels = "%H:%M") +
  labs(y = "CO2 (ppm)", x = "Exact time", color = "TB Room ID") +
  theme(legend.position = c(0.875,0.85))
ggsave("insights/co2-double-entries.pdf", width = 16 / cm(1), height = 12 / cm(1))

masi <- masi %>%
  group_by(location, date, date_time) %>%
  arrange(exact_date_time) %>%
  slice(1) %>%
  ungroup() %>%
  dplyr::select(-exact_date_time)

selected_dates <- paste0("2021-", c("10-11", "10-12", "10-13", "10-14", "10-15", "10-25", "10-26", "10-27", "10-28", "11-05", "11-08", "11-10", "11-11", "11-12", "11-15", "11-16", "11-17", "11-18", "11-19"))

# filter
masi <- masi %>%
  filter(date %in% as.Date(selected_dates))

# waiting room is the average of waiting room and registration
wr <- masi %>%
  filter(location %in% c("registration", "waiting room")) %>%
  group_by(date, date_time) %>%
  summarize(across(c(co2, temperature, humidity), mean, na.rm = T)) %>%
  ungroup() %>%
  mutate(location = "waiting room")

# TODO: potentially integrate corridor CO2 data if available

# tb room
tb <- masi %>%
  filter(location == "tb room") 

# combine 
masi_prep <- rbind(wr, tb)

# create complete data frame from first to last observation
masi_complete <- masi_prep %>% 
  dplyr::select(date, date_time) %>%
  group_by(date) %>%
  arrange(date_time) %>%
  summarize(ft = first(date_time),
            lt = last(date_time)) %>%
  ungroup() %>%
  mutate(date_time = map2(ft, lt, seq, by = "1 min")) %>%
  unnest(date_time) 
masi_complete <- rbind(masi_complete %>% mutate(location = "waiting room"),
                       masi_complete %>% mutate(location = "tb room"))
masi_prep <- left_join(masi_complete %>% dplyr::select(-ft, -lt), masi_prep, by = c("date", "location", "date_time"))

# check
masi_prep %>%
  ggplot(aes(x = date_time, y = co2, color = location)) +
  geom_line() +
  facet_wrap(~ date, scales = "free_x") +
  scale_x_datetime(date_labels = "%H:%M")

# linearly interpolate missing values
message(sprintf("Number of missing minutes in CO2: %i", sum(is.na(masi_prep$co2))))
masi_prep <- masi_prep %>%
  group_by(date, location) %>%
  arrange(date_time) %>%
  mutate(across(c(co2, humidity, temperature), zoo::na.approx, na.rm = F)) %>%
  tidyr::fill(co2, humidity, temperature, .direction = "downup") %>%
  ungroup() 

saveRDS(masi_prep, "data-clean/Masi/environmental-data/co2-temp-humid.rds")

# dates with good data
save_dir <- paste0("data-clean/Masi/combined-data/", selected_dates)
for (i in 1:length(selected_dates)) {
  if (!dir.exists(save_dir[i])) {
    dir.create(save_dir[i])
  }
  masi_sub <- masi_prep %>%
    dplyr::filter(date == as.Date(selected_dates[i])) 
  saveRDS(masi_sub, paste0(save_dir[i], "/environmental-data.rds"))
}


  
