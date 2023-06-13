#### Libraries ####

library(tidyverse)
library(readxl)
library(lubridate)
library(haven)

#### Backup data ####

files <- list.files("data-raw/Masi/environmental-data/backup/", full.names = T)

for (f in files) {
  fn <- gsub(".XLS", ".csv", f)
  file.rename(f, fn)
}

as_date2 <- function(x) {
  d <- as.Date(x, format = "%m/%d/%Y")
  if (is.na(d)) {
    d <- as.Date(x, format = "%m.%d.%Y") 
  }
  return(d)
}

df <- do.call(rbind,
                   lapply(files, function(f) {
                     read.table(f, sep = "\t", header = F, skip = 33) %>%
                       set_names(c("date", "time", "humidity", "temperature", "co2", "ext")) %>%
                       dplyr::select(-ext) %>%
                       mutate(file = basename(f))
                   })) %>%
  mutate(date_created = as.Date(stringi::stri_extract(file, regex = "\\d{8}"), "%Y%m%d"),
         date = as.Date(date, format = "%m/%d/%Y"),
         sensor = as.integer(gsub("-", "", stringi::stri_extract(file, regex = "-\\d{1}"))),
         date_time = parse_date_time(paste(date, time), '%Y-%m-%d %I:%M:%S %p')) 

#### Preprocessed data ####

df_prep <- read_dta("data-raw/Masi/environmental-data/co2_data.dta") %>%
  mutate(date_time = as.POSIXct(paste(Date, format(new_time, "%H:%M:%S"))))

#### Correct timestamp ####

df_prep %>% 
  filter(location == 2) %>%
  filter(CO2 == max(CO2, na.rm = T))

df %>% 
  filter(sensor == 2) %>%
  filter(co2 == max(df_prep_1$CO2, na.rm = T))

df_1 %>% filter(co2 > max(df_prep_1$CO2, na.rm = T))

df %>%
  filter(date >= as.Date("2021-10-01"),
         date %in% as.Date(paste0("2021-", c("10-13", "10-14", "10-15", "10-25", "11-05", "11-10", "11-12", "11-18")))) %>%
  ggplot(aes(x = date_time, y = as.numeric(co2), color = factor(sensor))) +
  geom_line() +
  facet_wrap(~ date, scales = "free_x") +
  scale_x_datetime(breaks = "1 day")


df %>% group_by(sensor) %>% 
  summarize(unique_date = unique(date)) %>%
  filter(unique_date >= as.Date("2021-10-01")) %>% 
  ggplot(aes(x = unique_date, y = sensor)) +
  #facet_wrap(~ date_created) +
  geom_point() +
  scale_x_date(breaks = "1 day", date_labels = "%b %d")

df %>% 
  group_by(sensor, date_time) %>%
  summarize(v = var(co2)) %>%
  ungroup() %>%
  dplyr::select(v) %>%
  unlist() %>%
  unique()

xovis_dates <- list.files("data-raw/Masi/xovis/")
xovis_dates <- xovis_dates[grepl("rds", xovis_dates)]
xovis_dates <- gsub(".rds", "", xovis_dates)
xovis_dates <- as.Date(xovis_dates)
missing_xovis_dates <- xovis_dates[!(xovis_dates %in% as.Date(paste0("2021-", c("10-13", "10-14", "10-15", "10-25", "11-05", "11-10", "11-12", "11-18"))))]

df %>%
  filter(date %in% missing_xovis_dates) %>% 
  mutate(co2 = as.numeric(co2)) %>%
  ggplot(aes(x = date_time, y = co2, color = factor(sensor))) +
  geom_line() +
  scale_x_datetime(breaks = "1 day")
  
df %>%
  filter(date %in% missing_xovis_dates) %>%
  group_by(file, date, sensor) %>%
  summarize(start_time = head(time,1),
            end_time = tail(time, 1)) %>%
  ungroup()


df %>% 
  dplyr::filter(date_created == "2021-11-05") %>%
  group_by(date, sensor) %>%
  summarize(start_time = head(time,1),
            end_time = tail(time,1),
            n = n())


df_Nov4 <- df %>%
  dplyr::filter(date_created == "2021-11-05") %>%
  mutate(date = as.Date(ifelse(as.character(date) == "2012-01-29", "2021-11-04", as.character(date)))) %>%
  filter(date == as.Date("2021-11-04")) %>%
  mutate(date_time = parse_date_time(paste(date, time), '%Y-%m-%d %I:%M:%S %p')) %>%
  mutate(across(c(humidity, temperature, co2), as.numeric))

df_Nov4 %>%
  ggplot(aes(x = date_time, y = co2, color = as.factor(sensor))) +
  geom_line()

df %>%
  dplyr::filter(date_created == "2021-11-05",
                sensor == 1) %>%
  ggplot(aes(x = date_time, y = as.numeric(co2))) +
  geom_line()
