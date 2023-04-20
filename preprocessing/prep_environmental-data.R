#### Libraries ####

library(tidyverse)

#### Masi ####

masi <- do.call(rbind,
              lapply(files, function(f) {
                read.table(f, sep = "\t", header = F, skip = 33) %>%
                  set_names(c("date", "time", "humidity", "temperature", "co2", "ext")) %>%
                  dplyr::select(-ext) %>%
                  mutate(file = basename(f))
              })) %>%
  mutate(date_created = as.Date(stringi::stri_extract(file, regex = "\\d{8}"), "%Y%m%d"),
         date = as.Date(date, format = "%m/%d/%Y"),
         sensor = as.integer(gsub("-", "", stringi::stri_extract(file, regex = "-\\d{1}"))),
         date_time = parse_date_time(paste(date, time), '%Y-%m-%d %I:%M:%S %p'),
         across(c(humidity, temperature, co2), as.numeric)) 

masi %>% 
  filter(sensor == 1) %>%
  ggplot(aes(x = date_time, y = co2)) +
  geom_line() +
  facet_wrap(~ date, scales = "free_x") +
  scale_x_datetime(date_labels = "%H:%M")

# load data
#' masi <- readxl::read_xlsx("data-raw/Masi/environmental-data/co2_data follow-up_2.xlsx", sheet = "all_in_one") %>%
#'   rename(date = Date, 
#'          time = Time,
#'          humidity = Humidity,
#'          temperature = Temperature,
#'          co2 = CO2,
#'          location = Monitor) %>%
#'   mutate(exact_time = format(time, format = "%H:%M:%S"),
#'          time = format(time, format = "%H:%M"),
#'          exact_date_time = as.POSIXct(paste(date, exact_time), format = "%Y-%m-%d %H:%M:%S"),
#'          date_time = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M"),
#'          date = as.Date(date),
#'          location = ifelse(location == "TB room", "tb room", location),
#'          across(c(temperature, humidity, co2), as.numeric)) %>%
#'   dplyr::select(date, exact_date_time, date_time, location, co2, temperature, humidity)
#' 
#' # check
#' # masi %>%
#' #   ggplot(aes(x = date_time, y = co2, color = location)) +
#' #   geom_line() +
#' #   facet_wrap(~ date, scales = "free_x") +
#' #   scale_x_datetime(date_labels = "%H:%M")
#' # masi %>%
#' #   group_by(location, date, date_time) %>%
#' #   summarize(n = n()) %>%
#' #   ggplot(aes(x = n)) + geom_histogram()
#' 
#' # masi_double_entries <- masi %>%
#' #   group_by(location, date, date_time) %>%
#' #   arrange(exact_date_time) %>%
#' #   mutate(id = 1:n()) %>%
#' #   filter(max(id) > 1) %>%
#' #   ungroup() 
#' # 
#' # masi_double_entries %>%
#' #   dplyr::filter(location == "tb room", 
#' #                 date %in% as.Date(c("2021-11-08", "2021-11-10"))) %>%
#' #   ggplot(aes(x = exact_date_time, y = co2, color = factor(id))) +
#' #   facet_wrap(~ date, scales = "free_x") +
#' #   geom_line() +
#' #   scale_x_datetime(date_labels = "%H:%M") +
#' #   labs(y = "CO2 (ppm)", x = "Exact time", color = "TB Room ID") +
#' #   theme(legend.position = c(0.875,0.85))
#' # ggsave("insights/co2-double-entries.pdf", width = 16 / cm(1), height = 12 / cm(1))
#' 
#' #' filter double entries
#' #' second id (green line) is correct from double entries
#' masi <- masi %>%
#'   group_by(location, date, date_time) %>%
#'   arrange(exact_date_time) %>%
#'   slice(ifelse(n() == 1, 1, 2)) %>%
#'   ungroup() %>%
#'   dplyr::select(-exact_date_time)
#' 
#' # add corridor data
#' corridor_files <- list.files("data-raw/Masi/environmental-data/corridor/", full.names = T)
#' masi_corridor <- do.call(rbind,
#'               lapply(corridor_files, function(f) {
#'                 read.table(f, sep = "\t", header = F, skip = 33) %>%
#'                   set_names(c("date", "time", "humidity", "temperature", "co2", "ext")) %>%
#'                   dplyr::select(-ext) %>%
#'                   mutate(file = basename(f))
#'               })) %>%
#'   mutate(date_created = as.Date(stringi::stri_extract(file, regex = "\\d{8}"), "%Y%m%d"),
#'          date = as.Date(date, format = "%m/%d/%Y"),
#'          sensor = as.integer(gsub("-", "", stringi::stri_extract(file, regex = "-\\d{1}"))),
#'          date_time = parse_date_time(paste(date, time), '%Y-%m-%d %I:%M:%S %p'),
#'          across(c(humidity, temperature, co2), as.numeric),
#'          co2 = ifelse(co2 == 0, NA, co2),
#'          location = "corridor") %>%
#'   group_by(date_time) %>%
#'   slice(1) %>%
#'   ungroup() %>%
#'   dplyr::select(location, date, date_time, humidity, temperature, co2) 
#' masi <- rbind(masi, masi_corridor)
#' # masi %>%
#' #   # filter(location != "tb room") %>%
#' #   ggplot(aes(x = date_time, y = co2, color = location)) +
#' #   geom_line() +
#' #   facet_wrap(~date, scales = "free_x") +
#' #   scale_x_datetime(date_labels = "%H:%M") 
#' 
#' # leave out registration
#' masi <- masi %>%
#'   filter(location != "registration") 
#' 
#' # filter dates where patient tracking data is available
#' tracking_dates <- as.Date(paste0("2021-", c("10-13", "10-14", "10-15", "10-25", "11-05", "11-10", "11-12", "11-18")))
#' 
#' # filter
#' masi <- masi %>%
#'   filter(date %in% as.Date(selected_dates))
#' 
#' # create complete data frame from first to last observation
#' masi_complete <- masi_prep %>% 
#'   dplyr::select(date, date_time) %>%
#'   group_by(date) %>%
#'   arrange(date_time) %>%
#'   summarize(ft = first(date_time),
#'             lt = last(date_time)) %>%
#'   ungroup() %>%
#'   mutate(date_time = map2(ft, lt, seq, by = "1 min")) %>%
#'   unnest(date_time) 
#' masi_complete <- rbind(masi_complete %>% mutate(location = "waiting room"),
#'                        masi_complete %>% mutate(location = "tb room"))
#' masi_prep <- left_join(masi_complete %>% dplyr::select(-ft, -lt), masi_prep, by = c("date", "location", "date_time"))
#' 
#' # check
#' masi_prep %>%
#'   ggplot(aes(x = date_time, y = co2, color = location)) +
#'   geom_line() +
#'   facet_wrap(~ date, scales = "free_x") +
#'   scale_x_datetime(date_labels = "%H:%M")
#' 
#' # linearly interpolate missing values
#' message(sprintf("Number of missing minutes in CO2: %i", sum(is.na(masi_prep$co2))))
#' masi_prep <- masi_prep %>%
#'   group_by(date, location) %>%
#'   arrange(date_time) %>%
#'   mutate(across(c(co2, humidity, temperature), zoo::na.approx, na.rm = F)) %>%
#'   tidyr::fill(co2, humidity, temperature, .direction = "downup") %>%
#'   ungroup() 
#' 
#' saveRDS(masi_prep, "data-clean/Masi/environmental-data/co2-temp-humid.rds")
#' 
#' # dates with good data
#' save_dir <- paste0("data-clean/Masi/combined-data/", selected_dates)
#' for (i in 1:length(selected_dates)) {
#'   if (!dir.exists(save_dir[i])) {
#'     dir.create(save_dir[i])
#'   }
#'   masi_sub <- masi_prep %>%
#'     dplyr::filter(date == as.Date(selected_dates[i])) 
#'   saveRDS(masi_sub, paste0(save_dir[i], "/environmental-data.rds"))
#' }


  
