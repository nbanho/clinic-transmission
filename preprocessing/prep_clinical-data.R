#### Libraries ####

library(tidyverse)
library(lubridate)


#### Helper ####


#### Data ####

# Masi

df_pat_time_masi <- read.csv("data-raw/Masi/clinical-data/patient-time.csv") %>%
  rename(date = action_date) %>%
  mutate(time = parse_date_time(paste(date, time), '%m/%d/%Y %I:%M:%S %p'),
         date = as.Date(time)) %>%
  filter(!is.na(time))

df_tb_suspects <- read.csv("data-raw/Masi/clinical-data/tb-suspects.csv") %>%
  select(patient_id, result_txt) %>%
  rename(result = result_txt)

df <- left_join(df_pat_time_masi, df_tb_suspects) %>%
  rename(date_time = time) %>%
  mutate(tb_suspect = ifelse(is.na(result), "no", "yes"),
         time = format(date_time, "%H:%M:%S"),
         date_time = as.POSIXct(as.character(date_time))) %>%
  select(patient_id, date, time, date_time, tb_suspect)

saveRDS(df, "data-clean/Masi/clinical-data/clinical-data.rds")
