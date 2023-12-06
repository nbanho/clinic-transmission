#### Settings ####

# libraries
library(tidyverse)

# settings
load_path <- "data-clean/patient-tracking/finished-manual-linking/"
save_path <- "data-clean/patient-tracking/linked-and-imputed/"

# function to summarize tracking ends
summarize.tracking_end <- function(df) {
  df %>%
    group_by(patient_id) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(tracking_end) %>%
    unlist() %>%
    table()
}

# function to filter noise
filter.noise <- function(df) {
  df %>%
    group_by(patient_id) %>%
    filter(first(tracking_end) != "Noise") %>%
    ungroup()
}

# exclude tracking end
filter.exclude <- function(df, exclude = "Possible HCW") {
  df.sub <- df %>%
    group_by(patient_id) %>%
    filter(!(first(tracking_end) %in% exclude)) %>%
    ungroup()
}

# include tracking end
filter.include <- function(df, include = "Entered + Exited") {
  df.sub <- df %>%
    group_by(patient_id) %>%
    filter(first(tracking_end) %in% include) %>%
    ungroup()
}

# plot duration distribution
plot.duration <- function(df) {
  df %>%
    group_by(patient_id) %>%
    summarize(duration = difftime(last(time), first(time), units = "min")) %>%
    ungroup() %>%
    ggplot(aes(x = duration)) +
    geom_histogram()
}

# determine number of children
table.minHeight <- function(df, min_height = 1400) {
  df %>%
    group_by(patient_id) %>%
    summarize(height = max(height)) %>%
    ungroup() %>%
    mutate(below_min_height = ifelse(height <= 1400, "Small", "Large")) %>%
    dplyr::select(below_min_height) %>%
    unlist() %>%
    table()
} 

# subset relevant variables
subset <- function(df) {
  dplyr::select(df, -obs_id_new, -obs_id, -match_type, -is_seat)
}

#' impute
#' Assumption: between two linked tracks, we assume that the person stayed at the previous location

impute <- function(df) {
  df.complete <- df %>%
    dplyr::select(patient_id, time) %>%
    group_by(patient_id) %>%
    summarize(first_time = first(time),
              last_time = last(time)) %>%
    ungroup() %>%
    mutate(time = map2(first_time, last_time, seq, by = "1 sec")) %>%
    unnest(time) %>%
    dplyr::select(patient_id, time)
  
  df.imp <- df.complete %>%
    left_join(df) %>%
    group_by(patient_id) %>%
    fill(-time, .direction = "down") %>%
    ungroup()
}

# reception stays over time
plot.reception <- function(df) {
  df %>%
    group_by(patient_id) %>%
    mutate(at_reception = is_reception & lag(is_reception) & lag(is_reception, 2) & lag(is_reception, 3) & lag(is_reception, 4)) %>%
    filter(at_reception) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(time) %>%
    mutate(count_at_reception = cumsum(at_reception)) %>%
    ggplot(aes(x = time, y = count_at_reception)) +
    geom_step()
}

#### 2021-10-13 ####

# load
oct13 <- readRDS(paste0(load_path, "2021-10-13.rds"))

# tracking ends
summarize.tracking_end(oct13)

# filter noise
oct13_filt <- filter.noise(oct13)

# duration distribution
plot.duration(filter.exclude(oct13_filt))
plot.duration(filter.include(oct13_filt))

# check minimum height
table.minHeight(oct13_filt)

# subset
oct13_filt <- subset(oct13_filt)

# impute
oct13_imp <- impute(oct13_filt)

# at reception
plot.reception(filter.exclude(oct13_imp))

# save
saveRDS(oct13_imp, paste0(save_path, "2021-10-13.rds"))


#### 2021-10-15 ####

# load
oct15 <- readRDS(paste0(load_path, "2021-10-15.rds"))

# tracking ends
summarize.tracking_end(oct15)

# filter noise
oct15_filt <- filter.noise(oct15)

# duration distribution
plot.duration(filter.exclude(oct15_filt))
plot.duration(filter.include(oct15_filt))

# check minimum height
table.minHeight(oct15_filt)

# subset
oct15_filt <- subset(oct15_filt)

# impute
oct15_imp <- impute(oct15_filt)

# at reception
plot.reception(filter.exclude(oct15_imp))

# save
saveRDS(oct15_imp, paste0(save_path, "2021-10-15.rds"))


#### 2021-10-25 ####

# load
oct25 <- readRDS(paste0(load_path, "2021-10-25.rds"))

# tracking ends
summarize.tracking_end(oct25)

# filter noise
oct25_filt <- filter.noise(oct25)

# duration distribution
plot.duration(filter.exclude(oct25_filt))
plot.duration(filter.include(oct25_filt))

# check minimum height
table.minHeight(oct25_filt)

# subset
oct25_filt <- subset(oct25_filt)

# impute
oct25_imp <- impute(oct25_filt)

# at reception
plot.reception(filter.exclude(oct25_imp))

# save
saveRDS(oct25_imp, paste0(save_path, "2021-10-25.rds"))


#### 2021-11-04 ####

# load
nov4 <- readRDS(paste0(load_path, "2021-11-04.rds"))

# tracking ends
summarize.tracking_end(nov4)

# filter noise
nov4_filt <- filter.noise(nov4)

# duration distribution
plot.duration(filter.exclude(nov4_filt))
plot.duration(filter.include(nov4_filt))

# check minimum height
table.minHeight(nov4_filt)

# subset
nov4_filt <- subset(nov4_filt)

# impute
nov4_imp <- impute(nov4_filt)

# at reception
plot.reception(filter.exclude(nov4_imp))

# save
saveRDS(nov4_imp, paste0(save_path, "2021-11-04.rds"))


#### 2021-11-05 ####

# load
nov5 <- readRDS(paste0(load_path, "2021-11-05.rds"))

# tracking ends
summarize.tracking_end(nov5)

# filter noise
nov5_filt <- filter.noise(nov5)

# duration distribution
plot.duration(filter.exclude(nov5_filt))
plot.duration(filter.include(nov5_filt))

# check minimum height
table.minHeight(nov5_filt)

# subset
nov5_filt <- subset(nov5_filt)

# impute
nov5_imp <- impute(nov5_filt)

# at reception
plot.reception(filter.exclude(nov5_imp))

# save
saveRDS(nov5_imp, paste0(save_path, "2021-11-05.rds"))
