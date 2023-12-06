library(tidyverse)
library(lubridate)
options(digits.secs = 3)

# files
files <- list.files("data-raw/Masi/xovis/Study_Data_Masi_2021/export-server", full.names = T)
dates <- gsub("_exported_data[[:punct:]]csv", "", basename(files))
dates <- gsub("_", "-", dates)
files2 <- list.files("data-raw/Masi/xovis/Study_Data_Masi_2021/export-device", full.names = T)
dates2 <- gsub("_exported_data[[:punct:]]csv", "", basename(files2))
dates2 <- gsub("_", "-", dates2)

# read data
read_file_1 <- function(date) {
  read.csv(paste0("data-raw/Masi/xovis/Study_Data_Masi_2021/export-server/", gsub("-", "_", date) , "_exported_data.csv"), header = T) %>% 
    set_names(c("obs_id", "time", "x", "y", "height")) %>%
    mutate(time = as.POSIXct(paste(date, time)))
}
read_file_2 <- function(date) {
  read.csv(paste0("data-raw/Masi/xovis/Study_Data_Masi_2021/export-device/", gsub("-", "_", date), "_exported_data.csv"), header = F) %>% 
    set_names(c("obs_id", "date", "time", "x", "y", "height")) %>%
    mutate(datetime_ms = dmy_hms(paste(date, sub(":([^:]*)$", "\\.\\1", time)), tz = "CET"),
           datetime = round_date(datetime_ms)) %>%
    group_by(obs_id, datetime) %>%
    arrange(desc(datetime_ms)) %>%
    slice(1) %>%
    ungroup()
}

# check dates of files2
for (d in dates2) {
  cat(sprintf("Date: %s\n", d))
  dat <- read_file_2(d)
  print( unique(as.Date(dat$datetime)) )
}

# check data
check_data <- function(df) {
  cat( sprintf("Start time: %s\n", min(df$time)) )
  cat( sprintf("End time: %s\n", max(df$time)) )
  cat( sprintf("No. of observation IDs: %s\n", n_distinct(df$obs_id)) )
  dt <- df %>%
    group_by(obs_id) %>%
    mutate(dt = as.numeric(difftime(time, lag(time), units = "secs"))) %>%
    ungroup() %>%
    na.omit()
  bad_ones <- filter(dt, dt > 2 | dt < 0) 
  return(bad_ones)
}

# check bad data
check_bad_data <- function(df, check_df) {
  # Median dt
  bad_med_dt <- round(median(check_df$dt))
  cat( sprintf("Bad timediff (in s): %i \n", bad_med_dt))
  
  # bad IDs
  bad_ids <- unique(check_df$obs_id)
  all_ids <- unique(df$obs_id)
  prop_bad_ids <- round(100 * length(bad_ids) / length(all_ids))
  cat( sprintf("Bad IDs: %i (%i percent) \n", length(bad_ids), prop_bad_ids))
  
  # bad entries
  bad_entries <- df %>%
    filter(obs_id %in% bad_ids) %>%
    nrow()
  prob_bad_entries <- round(100 * bad_entries / nrow(df))
  cat( sprintf("Bad entries: %i (%i percent) \n", nrow(df), prob_bad_entries))
}

# update data
update_data <- function(df_x, df_y) {
  df_x_sum <- df_x %>%
    group_by(obs_id) %>%
    arrange(time) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(obs_id, time) %>%
    rename(time_x = time)
  df_y_sum <- df_y %>%
    group_by(obs_id) %>%
    arrange(datetime) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(obs_id, datetime) %>%
    rename(time_y = datetime)
  df_xy_sum <- left_join(df_x_sum, df_y_sum, by = "obs_id") %>%
    mutate(dt = as.numeric(difftime(time_x, time_y, units = "sec"))) %>%
    dplyr::select(obs_id, dt)
  med_dt <- median(df_xy_sum$dt)
  cat( sprintf("Median timediff (in s): %i", med_dt))
  df_x_new <- df_x %>%
    left_join(df_xy_sum, by = "obs_id") 
  df_x_new$time[is.na(df_x_new$dt)] <- df_x_new$time[is.na(df_x_new$dt)] - seconds(med_dt)
  df_x_new$time[!is.na(df_x_new$dt)] <- df_x_new$time[!is.na(df_x_new$dt)] - seconds(df_x_new$dt[!is.na(df_x_new$dt)])
  df_x_new <- df_x_new %>%
    dplyr::select(-dt) %>%
    full_join(df_y %>%
                dplyr::select(obs_id, datetime, x, y, height) %>%
                rename(time = datetime),
              by = c("obs_id", "time", "x", "y", "height")) %>%
    group_by(obs_id, time) %>%
    slice(1) %>%
    ungroup() 
  return(df_x_new)
}

# save data
save_data <- function(dat, date) {
  file_name <- paste0("data-raw/Masi/xovis/", date, ".rds")
  saveRDS(dat, file_name)
}

# only old dates
dates[!(dates %in% dates2)]

# 10-13
f1013_1 <- read_file_1("2021-10-13")
f1013_2 <- read_file_2("2021-10-13") 
f1013_merged <- update_data(df_x = f1013_1, df_y = f1013_2) # Med timediff: ~ -2.30min
ch_f1013_merged <- check_data(f1013_merged) # missing time steps
check_bad_data(df = f1013_merged, check_df = ch_f1013_merged)
f1013_merged <- f1013_merged %>%
  filter(!(obs_id %in% ch_f1013_merged$obs_id))
saveRDS(ch_f1013_merged$obs_id, file = "data-raw/Masi/xovis/partly_linked/2021-10-13_bad_ids.rds")
plot(unique(f1013_merged$time) %>% sort)
plot(unique(f1013_merged$obs_id %>% sort))
save_data(f1013_merged, "2021-10-13") 

# 10-14
f1014_1 <- read_file_1("2021-10-14")
f1014_2 <- read_file_2("2021-10-14") 
f1014_merged <- update_data(df_x = f1014_1, df_y = f1014_2) # Med timediff: ~ -5h
ch_f1014_merged <- check_data(f1014_merged)
plot(unique(f1014_merged$time) %>% sort)
plot(unique(f1014_merged$obs_id) %>% sort) 
save_data(f1014_merged, "2021-10-14")

# 10-15 
f1015_1 <- read_file_1("2021-10-15")
f1015_2 <- read_file_2("2021-10-15") 
f1015_merged <- update_data(df_x = f1015_1, df_y = f1015_2) # Med timediff: ~ 5min
ch_f1015_merged <- check_data(f1015_merged)
plot(unique(f1015_merged$time) %>% sort)
plot(unique(f1015_merged$obs_id) %>% sort)
save_data(f1015_merged, "2021-10-15")

# 10-18
f1018_1 <- read_file_1("2021-10-18")
f1018_2 <- read_file_2("2021-10-18")
f1018_merged <- update_data(df_x = f1018_1, df_y = f1018_2) # Med timediff: ~ 5min
ch_f1018_merged <- check_data(f1018_merged)
plot(unique(f1018_merged$time) %>% sort)
plot(unique(f1018_merged$obs_id) %>% sort)
save_data(f1018_merged, "2021-10-18")

# 10-19
f1019_1 <- read_file_1("2021-10-19")
f1019_2 <- read_file_2("2021-10-19")
f1019_merged <- update_data(df_x = f1019_1, df_y = f1019_2) # Med timediff: ~ 5min
ch_f1019_merged <- check_data(f1019_merged) # missing time steps
check_bad_data(f1019_merged, ch_f1019_merged)
f1019_merged <- f1019_merged %>%
  filter(!(obs_id %in% ch_f1019_merged$obs_id))
plot(unique(f1019_merged$time) %>% sort) 
plot(unique(f1019_merged$obs_id) %>% sort)
save_data(f1019_merged, "2021-10-19")

# 10-20
f1020_1 <- read_file_1("2021-10-20")
f1020_2 <- read_file_2("2021-10-20") 
f1020_merged <- update_data(df_x = f1020_1, df_y = f1020_2) # Med timediff: ~ 5min
ch_f1020_merged <- check_data(f1020_merged)
plot(unique(f1020_merged$time) %>% sort)
plot(unique(f1020_merged$obs_id) %>% sort)
save_data(f1020_merged, "2021-10-20")

# 10-22
f1022_1 <- read_file_1("2021-10-22")
f1022_2 <- read_file_2("2021-10-22") 
f1022_merged <- update_data(df_x = f1022_1, df_y = f1022_2) # Med timediff: ~ 7s
ch_f1022_merged <- check_data(f1022_merged)
check_bad_data(f1022_merged, ch_f1022_merged) # missing time steps
f1022_merged <- f1022_merged %>%
  filter(!(obs_id %in% ch_f1022_merged$obs_id))
plot(unique(f1022_merged$time))
plot(unique(f1022_merged$obs_id) %>% sort)
save_data(f1022_merged, "2021-10-22")

# 10-25
f1025_1 <- read_file_1("2021-10-25")
f1025_2 <- read_file_2("2021-10-25") 
f1025_merged <- update_data(df_x = f1025_1, df_y = f1025_2) # Med timediff: ~ 5min
ch_f1025_merged <- check_data(f1025_merged)
plot(unique(f1025_merged$time))
plot(unique(f1025_merged$obs_id) %>% sort)
save_data(f1025_merged, "2021-10-25")

# 10-26
f1026_1 <- read_file_1("2021-10-26")
f1026_2 <- read_file_2("2021-10-26") 
f1026_merged <- update_data(df_x = f1026_1, df_y = f1026_2) # Med timediff: ~ 2min
ch_f1026_merged <- check_data(f1026_merged) # missing time steps
check_bad_data(f1026_merged, ch_f1026_merged)
plot(unique(f1026_merged$time))
plot(unique(f1026_merged$obs_id) %>% sort)
#save_data(f1026_merged, "2021-10-26") # don't use: too many missing time steps

# 10-27
f1027_1 <- read_file_1("2021-10-27")
f1027_2 <- read_file_2("2021-10-27") 
f1027_merged <- update_data(df_x = f1027_1, df_y = f1027_2) # Med timediff: ~ -6min
ch_f1027_merged <- check_data(f1027_merged) # missing time steps
check_bad_data(f1027_merged, ch_f1027_merged)
plot(unique(f1027_merged$time))
plot(unique(f1027_merged$obs_id) %>% sort)
#save_data(f1027_merged, "2021-10-27") # don't use: too many missing time steps

# 10-28
f1028_1 <- read_file_1("2021-10-28")
f1028_2 <- read_file_2("2021-10-28") 
f1028_merged <- update_data(df_x = f1028_1, df_y = f1028_2) # Med timediff: ~ -2min
ch_f1028_merged <- check_data(f1028_merged) # missing time steps
check_bad_data(f1028_merged, ch_f1028_merged)
plot(unique(f1028_merged$time))
plot(unique(f1028_merged$obs_id) %>% sort)
#save_data(f1028_merged, "2021-10-28") # don't use: too many missing time steps

# 11-02
f1102_1 <- read_file_1("2021-11-02")
f1102_2 <- read_file_2("2021-11-02") 
f1102_merged <- update_data(df_x = f1102_1, df_y = f1102_2) # Med timediff: ~ 5min
ch_f1102_merged <- check_data(f1102_merged)
plot(unique(f1102_merged$time))
plot(unique(f1102_merged$obs_id) %>% sort)
save_data(f1102_merged, "2021-11-02")

# 11-03
f1103_1 <- read_file_1("2021-11-03")
f1103_2 <- read_file_2("2021-11-03") 
f1103_merged <- update_data(df_x = f1103_1, df_y = f1103_2) # Med timediff: ~ 7.5h
ch_f1103_merged <- check_data(f1103_merged)
plot(unique(f1103_merged$time))
plot(unique(f1103_merged$obs_id) %>% sort)
save_data(f1103_merged, "2021-11-03")

# 11-04
f1104_1 <- read_file_1("2021-11-04")
f1104_2 <- read_file_2("2021-11-04") 
f1104_merged <- update_data(df_x = f1104_1, df_y = f1104_2) # Med timediff: ~ 5min
ch_f1104_merged <- check_data(f1104_merged)
plot(unique(f1104_merged$time))
plot(unique(f1104_merged$obs_id) %>% sort)
save_data(f1104_merged, "2021-11-04")

# 11-05
f1105_1 <- read_file_1("2021-11-05")
f1105_2 <- read_file_2("2021-11-05") 
f1105_merged <- update_data(df_x = f1105_1, df_y = f1105_2) # Med timediff: ~ 5min
ch_f1105_merged <- check_data(f1105_merged)
plot(unique(f1105_merged$time))
plot(unique(f1105_merged$obs_id) %>% sort)
save_data(f1105_merged, "2021-11-05")

# 11-08
f1108_1 <- read_file_1("2021-11-08")
f1108_2 <- read_file_2("2021-11-08") 
f1108_merged <- update_data(df_x = f1108_1, df_y = f1108_2) # Med timediff: ~ 3h
ch_f1108_merged <- check_data(f1108_merged) # missing time steps
check_bad_data(f1108_merged, ch_f1108_merged)
plot(unique(f1108_merged$time))
plot(unique(f1108_merged$obs_id) %>% sort)
# save_data(f1108_merged, "2021-11-08") # don't use: too many missing time steps

# 11-09
f1109_1 <- read_file_1("2021-11-09")
f1109_2 <- read_file_2("2021-11-09") 
f1109_merged <- update_data(df_x = f1109_1, df_y = f1109_2) # Med timediff: ~ -1min
ch_f1109_merged <- check_data(f1109_merged)
check_bad_data(f1109_merged, ch_f1109_merged)
plot(unique(f1109_merged$time)) # missing time steps
plot(unique(f1109_merged$obs_id) %>% sort)
# save_data(f1109_merged, "2021-11-09") # don't use: too many missing time steps

# 11-10
f1110_1 <- read_file_1("2021-11-10")
f1110_2 <- read_file_2("2021-11-10") 
f1110_merged <- update_data(df_x = f1110_1, df_y = f1110_2) # Med timediff: ~ -4h
ch_f1110_merged <- check_data(f1110_merged)
plot(unique(f1110_merged$time))
plot(unique(f1110_merged$obs_id) %>% sort) # seem to have a few missing IDs at the start
save_data(f1110_merged, "2021-11-10")

# 11-12
f1112_1 <- read_file_1("2021-11-12")
f1112_2 <- read_file_2("2021-11-12") 
f1112_merged <- update_data(df_x = f1112_1, df_y = f1112_2) # Med timediff: ~ -1h
ch_f1112_merged <- check_data(f1112_merged)
plot(unique(f1112_merged$time)) # missing data between 7:20 and 8:15
plot(unique(f1112_merged$obs_id) %>% sort) # corresponding missing IDs
save_data(f1112_merged, "2021-11-12")

# 11-18
f1118_1 <- read_file_1("2021-11-18")
f1118_2 <- read_file_2("2021-11-18") 
f1118_merged <- update_data(df_x = f1118_1, df_y = f1118_2) # Med timediff: ~ -1h
ch_f1118_merged <- check_data(f1118_merged)
plot(unique(f1118_merged$time))
plot(unique(f1118_merged$obs_id) %>% sort) 
save_data(f1118_merged, "2021-11-18")


# new dates:
dates2[!(dates2 %in% dates)]

# 10-29
#f1029_1 <- read_file_1("2021-10-29") # does not exist
f1029_2 <- read_file_2("2021-10-29") %>%
  dplyr::select(obs_id, datetime, x, y, height) %>%
  rename(time = datetime)
ch_f1029_2 <- check_data(f1029_2)  # missing time steps
check_bad_data(f1029_2, ch_f1029_2)
plot(unique(f1029_2$time)) # time gap between around 8 and 10 am
plot(unique(f1029_2$obs_id) %>% sort)
#save_data(f1029_merged, "2021-10-29") # don't use: too many missing time steps

# 11-11
#f1111_1 <- read_file_1("2021-11-11") # does not exist
f1111_2 <- read_file_2("2021-11-11") %>%
  dplyr::select(obs_id, datetime, x, y, height) %>%
  rename(time = datetime)
ch_f1111_2 <- check_data(f1111_2) # short day from 9 to 11 am
plot(unique(f1111_2$time))
plot(unique(f1111_2$obs_id) %>% sort)
save_data(f1111_2, "2021-11-11")

# 11-17
#f1117_1 <- read_file_1("2021-11-17") # does not exist
f1117_2 <- read_file_2("2021-11-17") %>%
  dplyr::select(obs_id, datetime, x, y, height) %>%
  rename(time = datetime)
ch_f1117_2 <- check_data(f1117_2) # short day from 1 to 4 pm
plot(unique(f1117_2$time))
plot(unique(f1117_2$obs_id) %>% sort)
save_data(f1117_2, "2021-11-17")
