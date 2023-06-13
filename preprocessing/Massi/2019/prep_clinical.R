#### Libraries ####

library(tidyverse)
library(lubridate)
library(readxl)


#### Tracking data ####

# load tracking data
tracking_files <- list.files("data-clean/Massi/2019/patient-tracking/", full.names = T)
tracking_data <- do.call(rbind, parallel::mclapply(tracking_files, readRDS, mc.cores = 6))
tracking_data <- tracking_data %>%
  mutate(date = as.Date(date_time)) 

tracking_data %>%
  mutate(date_id = paste(date, obs_id)) %>%
  filter(date_id %in% sample(date_id, replace = F, size = 20)) %>%
  ggplot(aes(x = x, y = y, group = date_id)) +
  geom_path()


tracking_data <- tracking_data %>%
  nest(obs_id, date_time, x, y, height, is_waitingroom, is_reception)


#### Clinical data ####

# load clinical data
clinical <- read_xlsx("data-raw/Massi/2019/clinical/Clinical_data29Jan20.xlsx") %>%
  dplyr::select(patient_id, patient_age_years, patient_gender, GeneXpert, event_updated_at, Start_Date, Treatment_Outcome_Date) %>%
  rename(age = patient_age_years,
         gender = patient_gender,
         test_result = GeneXpert,
         date_time = event_updated_at,
         tb_treat_start = Start_Date,
         tb_treat_end = Treatment_Outcome_Date) %>%
  mutate(date_time = as.POSIXct(date_time, tz = "CET"),
         date = as.Date(date_time),
         across(c(tb_treat_start, tb_treat_end), ~ as.Date(.x))) 

# subset clinical data
clinical <- clinical %>%
  filter(date %in% unique(tracking_data$date))

# checks
#' look for people who tested negative or positive but did not initiate a TB treatment
#' and where not on a prior treatment
treatment_delay <- function(reg_date, test_result, treat_date) {
  if (!is.na(test_result)) {
    if (test_result == "NONE") {
      return(NA)
    } else {
      if (is.na(treat_date)) {
        return(-1) # no treatment initiated
      } else if (treat_date < reg_date) {
        return(0) # prior treatment
      } else {
        return(as.numeric(treat_date - reg_date)) # treatment delay
      }
    }
  } else {
    return(NA)
  }
}

clinical$treat_dt <- mapply(treatment_delay, reg_date = clinical$date, test_result = clinical$test_result, treat_date = clinical$tb_treat_start) 

clinical %>%
  filter(!is.na(treat_dt),
         treat_dt == -1)
# --> everyone with a test result was eventually treated
clinical %>%
  group_by(patient_id) %>%
  arrange(date_time) %>%
  filter(any(!is.na(treat_dt)),
         any(treat_dt > 0)) 
# --> few patients with treatment delay

#' look for people who initiated treatment 
#' although they did not have a prior test result

clinical %>%
  group_by(patient_id) %>%
  arrange(date_time) %>%
  filter(all(is.na(test_result)),
         all(!is.na(tb_treat_start)),
         tb_treat_start > date)
# --> one person started treatment during the study period without a GeneXpert test result

# define outcome
#' We consider as infectious everyone who is on treatment
#' or will be on treatment later,
#' and we assume they stay infectious until 28 days into treatment.
#' We assume as non-infectious everyone who completed a treatment.

clinical <- clinical %>%
  group_by(patient_id, date) %>%
  arrange(date_time) %>%
  summarize(date_time = date_time[1],
            age = age[1],
            gender = gender[1],
            infectious = ifelse(all(is.na(tb_treat_start)), "Unmasked",
                                ifelse(date <= max(tb_treat_start, na.rm = T) + days(28), "Infectious", "Non-infectious"))) %>%
  ungroup() 

# inspect results
summary(factor(clinical$infectious))

clinical %>%
  filter(infectious == "Infectious") %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = n)) +
  geom_bar(stat = "identity")

clinical %>%
  filter(infectious == "Infectious") %>% 
  filter(age < 16)
  

# nest data
clinical_nest <- clinical %>% 
  nest(patient_id, age, gender, date_time, infectious)


#### Matching #####

#' Criteria for linkage:
#' Median height > 1400mm
#' Passed by the reception (spent at least 1 second in the registration area)
#' At least 1 minutes in the clinic
#' Patient age > 15

# find time when attendee was in the registration area for k seconds
time_at_re <- function(x, y, k = 1) {
  is_at_re <- logical(length(x))
  for (i in 1:(length(x) - k + 1)) {
    if (all(x[i:(i+k-1)])) {
      is_at_re[i] <- T
    } else {
      is_at_re[i] <- F
    }
  }
  
  if (all(!is_at_re)) {
    return(NA)
  } else {
    # use the first time
    return(y[is_at_re][1])
  }
}

merged_info <- list()
k <- 0
for (d in unique(clinical_nest$date)) {
  
  # subset clinical
  sub_clin <- clinical_nest$data[[which(clinical$date == d)]] %>%
    filter(age > 15) 
  
  # subset tracking data
  sub_track <- tracking_data$data[[which(tracking_data$date == d)]] %>%
    group_by(obs_id) %>%
    summarize(height = max(height),
              duration = as.numeric(difftime(last(date_time), first(date_time), units = "min")),
              reg_date_time = time_at_re(is_reception, date_time)) %>%
    ungroup() %>%
    filter(duration > 1,
           !is.na(reg_date_time),
           height > 1400)
  
  # matching
  merged <- merge(sub_clin, sub_track, id = NULL) %>%
    mutate(timediff = as.numeric(difftime(date_time, reg_date_time), units = "secs")) %>%
    filter(between(timediff, 0, 15 * 60)) %>%
    group_by(patient_id) %>%
    arrange(timediff) %>%
    slice(1) %>%
    ungroup()
  
  k <- k + 1
  merged_info[[k]] <- merged %>%
    mutate(n_clinical = nrow(sub_clin),
           n_tracking = nrow(sub_track),
           date = d)
  
  tracking_data$data[[which(tracking_data$date == d)]] <- tracking_data$data[[which(tracking_data$date == d)]] %>%
    left_join(dplyr::select(merged, patient_id, obs_id, age, gender, infectious), by = "obs_id") %>%
    mutate(infectious = ifelse(is.na(infectious), "Unmasked", infectious)) %>%
    rename(clinical_id = patient_id) 
  
  message(sprintf("#Clinic: %i, #Tracking: %i, #Linked: %i", nrow(sub_clin), nrow(sub_track), nrow(merged)))
}

# Linkage results
merged_info <- do.call(rbind, merged_info)

merged_info %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date) %>%
  summarize(`Registered patients` = n_clinical[1],
            `Considered trackings` = n_tracking[1],
            `Linked IDs` = n()) %>%
  ungroup() %>%
  reshape2::melt("date") %>%
  ggplot(aes(x = date, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) 

infectious_tracks <- lapply(tracking_data$data, function(dat) {
  dat %>%
    filter(infectious == "Infectious") %>%
    group_by(obs_id) %>%
    mutate(dt = as.numeric(c(0, diff(date_time)))) %>%
    filter(is_waitingroom) %>%
    summarize(duration = sum(dt) / 60,
              date = as.Date(date_time[1])) %>%
    ungroup()
}) 
infectious_tracks <- do.call(rbind, infectious_tracks)
infectious_tracks <- left_join(tracking_data %>% dplyr::select(date), infectious_tracks, by = "date") 
View(infectious_tracks)

##### Save data ####

for (i in 1:nrow(tracking_data)) {
  # day of data
  tdf_i <- tracking_data$data[[i]]
  
  # save data
  saveRDS(tdf_i, paste0("data-clean/Massi/2019/patient-tracking/", tracking_data$date[i], ".rds"))
}


