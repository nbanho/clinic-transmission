#### Libraries ####

library(tidyverse)

#### Matching function ####

#' Matching clinical records to patient tracking data
#' 
#' @param sel_date date of tracking data
#' @param min_time_in_cl minimum time spent in clinic in order for the id to be considered a clinical attendee
#' @param min_time_at_re minimum time a track should have stayed at the reception
#' @param max_tolerance minimum and maximum delay of clinical record after a patient was at the reception
#' @param prio_diagnosed should only diagnosed TB patients be matched?  
#' @param prio_suspected should only suspected TB patients be matched? 

match_clinical_tracking <- function(
    sel_date, 
    min_time_in_cl = 5 * 60, 
    min_time_at_re = 5, 
    max_tolerance = c(0, 15 * 60),
    prio_diagnosed = T,
    prio_suspected = T) {
  
  # clinical data
  clinical <- readRDS(paste0("data-clean/clinical/clinical-data.rds")) %>%
    filter(date == sel_date) %>%
    rename(clinical_patient_id = patient_id,
           clinical_record_time = date_time) %>%
    group_by(clinical_patient_id) %>%
    arrange(clinical_record_time) %>%
    slice(1) %>%
    ungroup()
  
  if (prio_diagnosed & prio_suspected) {
    clinical <- filter(clinical, is_infectious | was_suspected)
  } else if (prio_diagnosed) {
    clinical <- filter(clinical, is_infectious)
  } else if (prio_suspected) {
    clinical <- filter(clinical, was_suspected)
  }
  
  # tracking data
  tracking <- readRDS(paste0("data-clean/patient-tracking/linked-and-imputed/", sel_date, ".rds")) %>%
    group_by(patient_id) %>%
    filter(as.numeric(difftime(last(time), first(time), units = "secs")) > min_time_in_cl) %>%
    ungroup()
  
  # tracking: time at reception
  tracking_at_re <- tracking %>%
    group_by(patient_id) %>%
    mutate(min_time_at_re = zoo::rollapply(is_reception, width = min_time_at_re, FUN = all, align = "right", fill = F)) %>%
    ungroup() %>%
    filter(min_time_at_re) %>%
    group_by(patient_id) %>%
    slice(1) %>% # Note: only first time at reception is considered
    ungroup() 
  
  # matching
  merged <- merge(tracking_at_re %>% rename(time_at_re = time) %>% dplyr::select(patient_id, time_at_re),
                  clinical %>% dplyr::select(clinical_patient_id, clinical_record_time, is_infectious, was_suspected),
                  id = NULL) %>%
    mutate(timediff = as.numeric(clinical_record_time - time_at_re, units = "secs")) %>%
    filter(timediff > max_tolerance[1]) %>%
    filter(timediff <= max_tolerance[2]) %>%
    group_by(patient_id) %>%
    arrange(timediff) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(clinical_patient_id) %>%
    arrange(timediff) %>%
    slice(1) %>%
    ungroup()
  
  # report
  n_clinical_pid <- nrow(clinical)
  n_track_at_re <- nrow(tracking_at_re)
  n_matched <- nrow(merged)
  n_infectious <- sum(merged$is_infectious)
  n_suspected <- sum(merged$was_suspected)
  n_tot_infectious <- sum(clinical$is_infectious)
  n_tot_suspected <- sum(clinical$was_suspected)
  
  sub_titl <- paste("Clinical records:", n_clinical_pid, "considered,", n_tot_infectious, "diagnosed,", n_tot_suspected, "suspected.\nLinked:",
                    n_matched, "considered,", n_infectious, "diagnosed,", n_suspected, "suspected.\nTracks at reception:",
                    n_track_at_re)
  
  report_pl <- merged %>%
    mutate(`TB status` = ifelse(is_infectious, "Diagnosed", ifelse(was_suspected, "Suspected", "Other")),
           `TB status` = factor(`TB status`, levels = c("Diagnosed", "Suspected", "Other"))) %>%
    ggplot(aes(y = timediff, x = `TB status`, fill = `TB status`)) +
    geom_boxplot() +
    labs(y = "Time difference [sec]",
         x = "TB status",
         title = "Mach time statistics",
         subtitle = sub_titl) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(text = element_text(size = 8), legend.position = "none")
  ggsave(plot = report_pl, filename = paste0("data-clean/patient-tracking/matched-clinical/matching-report-", sel_date, ".pdf"), width = 8 / cm(1), height = 6 / cm(1))
  
  # append tracking data
  tracking_linked_clinical <- left_join(tracking, dplyr::select(merged, patient_id, clinical_patient_id, is_infectious, was_suspected), by = "patient_id") 
  
  return(tracking_linked_clinical)
  
}

#### Masi ####

# dates
selected_dates <- list.files("data-clean/patient-tracking/linked-and-imputed/")
selected_dates <- selected_dates[grepl("rds", selected_dates)]
selected_dates <- gsub(".rds", "", selected_dates, perl = T)
save_dir <- "data-clean/patient-tracking/matched-clinical/"
for (i in 1:length(selected_dates)) {
  matched_df <- match_clinical_tracking(sel_date = selected_dates[i])
  saveRDS(matched_df, paste0(save_dir, selected_dates[i], ".rds"))
}