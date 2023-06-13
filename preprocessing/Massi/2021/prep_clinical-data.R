#### Libraries ####

library(tidyverse)
library(lubridate)


#### Helper ####


#### Data ####

# Masi

df_pat_time_masi <- read.csv("data-raw/Massi/2021/clinical/patient-time.csv") %>%
  rename(date = action_date) %>%
  mutate(time = parse_date_time(paste(date, time), '%m/%d/%Y %I:%M:%S %p'),
         date = as.Date(time)) %>%
  filter(!is.na(time))

df_tb_suspects <- read.csv("data-raw/Massi/2021/clinical/tb-suspects.csv") %>%
  dplyr::select(patient_id, result_txt) %>%
  rename(result = result_txt)

df <- left_join(df_pat_time_masi, df_tb_suspects) %>%
  rename(date_time = time) %>%
  mutate(tb_suspect = ifelse(is.na(result), "no", "yes"),
         time = format(date_time, "%H:%M:%S"),
         date_time = as.POSIXct(as.character(date_time))) %>%
  dplyr::select(patient_id, date, time, date_time, tb_suspect)

saveRDS(df, "data-clean/Masi/clinical-data/clinical-data.rds")

selected_dates <- unique(df$date)
save_dir <- paste0("data-clean/Masi/combined-data/", selected_dates)
for (i in 1:length(selected_dates)) {
  if (!dir.exists(save_dir[i])) {
    dir.create(save_dir[i])
  }
  df_sub <- df %>%
    dplyr::filter(date == as.Date(selected_dates[i])) 
  saveRDS(df_sub, paste0(save_dir[i], "/clinical-data.rds"))
}
