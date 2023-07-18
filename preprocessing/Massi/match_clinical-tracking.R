#### Libraries ####

library(tidyverse)

#### Matching function ####

#' Matching clinical records to patient tracking data
#' 
#' @param clinic clinic
#' @param sel_date date of tracking data
#' @param min_time_in_cl minimum time spent in clinic in order for the id to be considered a clinical attendee
#' @param min_time_at_re minimum time a track should have stayed at the reception
#' @param min_duration_after_re minimum time a patient should have been in the clinic after she was at the reception
#' @param max_tolerance maximum delay of clinical record after a patient was at the reception

match_clinical_tracking <- function(clinic, sel_date, min_time_in_cl = 5 * 60, min_time_at_re = 5, min_duration_after_re = 5 * 60, max_tolerance = 15 * 60) {
  
  # clinical data
  clinical <- readRDS(paste0("data-clean/", clinic, "/combined-data/", sel_date, "/clinical-data.rds")) %>%
    group_by(patient_id) %>%
    slice(1) %>%
    ungroup() %>%
    rename(clinical_patient_id = patient_id,
           clinical_record_time = date_time)
  
  # tracking data
  tracking <- readRDS(paste0("data-clean/", clinic, "/patient-tracking-data/", sel_date, "/", "/patient-id-data.rds")) %>%
    group_by(patient_id) %>%
    filter(as.numeric(difftime(last(time), first(time), units = "secs")) > min_time_in_cl) %>%
    ungroup()
  
  # tracking: time at reception
  track_time_at_re <- tracking %>%
    group_by(patient_id) %>%
    filter(sum(is_reception) > min_time_at_re) %>%
    mutate(time_at_re = time[is_reception][1]) %>%
    filter(as.numeric(difftime(last(time), time_at_re, units = "secs")) > min_duration_after_re) %>%
    slice(1) %>%
    ungroup() 
  
  # matching
  merged <- merge(track_time_at_re %>% dplyr::select(patient_id, time_at_re),
                  clinical %>% dplyr::select(clinical_patient_id, clinical_record_time, tb_suspect),
                  id = NULL) %>%
    mutate(timediff = as.numeric(difftime(clinical_record_time, time_at_re), units = "secs")) %>%
    filter(timediff <= max_tolerance, timediff > 0) %>%
    group_by(patient_id) %>%
    arrange(timediff) %>%
    slice(1) %>%
    ungroup()
  
  # report
  n_clinical_pid <- nrow(clinical)
  n_track_at_re <- nrow(track_time_at_re)
  n_matched <- nrow(merged)
  report_pl <- merged %>%
    ggplot(aes(x = timediff / 60)) +
    geom_histogram(bins = 10) +
    labs(x = "Time difference [min]",
         title = "Mach time statistics",
         subtitle = paste("Total matches:", n_matched, ";",
                          "Clinical records:", n_clinical_pid, ";",
                          "Matchable tracks:", n_track_at_re)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_bw() +
    theme(text = element_text(size = 8))
  ggsave(plot = report_pl, filename = paste0("data-clean/", clinic, "/patient-tracking-data/", sel_date, "/", "clinical-tracking-matching-report.pdf"), width = 16 / cm(1), height = 8 / cm(1))
  
  # append tracking data
  tracking_linked_clinical <- tracking %>%
    left_join(merged %>% dplyr::select(patient_id, clinical_patient_id, tb_suspect), by = "patient_id") 
  
  return(tracking_linked_clinical)
  
}

#### Masi ####

# dates
selected_dates <- list.files("data-clean/Masi/patient-tracking-data/")[-1]
save_dir <- paste0("data-clean/Masi/combined-data/", selected_dates)
for (i in 1:length(selected_dates)) {
  if (!dir.exists(save_dir[i])) {
    dir.create(save_dir[i])
  }
  matched_df <- match_clinical_tracking("Masi", selected_dates[i])
  saveRDS(matched_df, paste0(save_dir[i], "/tracking-linked-clinical-data.rds"))
}







