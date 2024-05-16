#### Libraries ####

library(tidyverse)
library(lubridate)


#### Data ####

# Masi

df_pat_time_masi <- read.csv("data-raw/clinical/patient-time.csv") %>%
       rename(date = action_date) %>%
       mutate(
              time = parse_date_time(paste(date, time), "%m/%d/%Y %I:%M:%S %p", tz = "CET"),
              date = as.Date(time)
       ) %>%
       filter(!is.na(time))

df_tb_pat <- readxl::read_excel("data-raw/clinical/CocT Masiphumelele_clinic data Apr_Nov 2021.xlsx", sheet = "Worksheet 1") %>%
       rename(
              age = `Age(Years)`,
              gender = Gender,
              tb_treat_start = `Treatment Start Date`,
              folder_number = `Folder Number`
       ) %>%
       dplyr::select(age, gender, tb_treat_start, folder_number) %>%
       mutate(
              gender = factor(gender, levels = c("Male", "Female")),
              tb_treat_start = as.Date(tb_treat_start),
              folder_number = as.integer(folder_number)
       )

df_tb_sus <- read.csv("data-raw/clinical/tb-suspects.csv") %>%
       mutate(
              was_suspected = T,
              sus_tested_positive = ifelse(result_txt %in% c("TB_GENEXP_POS_RIF_S", "TB_GENEXP_POS_RIF_R"), T, F),
              sus_started_treat = ifelse(care_status_txt %in% c("CARE_STATUS_STARTED", "CARE_STATUS_STARTED_OTHER"), T, F),
              sus_treat_start = ifelse(sus_started_treat, care_status_date, NA),
              sus_treat_start = as.Date(sus_treat_start, format = "%d.%m.%y")
       ) %>%
       dplyr::select(patient_id, was_suspected, sus_tested_positive, sus_started_treat, sus_treat_start)

df <- df_pat_time_masi %>%
       left_join(df_tb_pat) %>%
       left_join(df_tb_sus) %>%
       rename(date_time = time) %>%
       mutate(
              diff_treat_start_1 = as.numeric(date - tb_treat_start),
              diff_treat_start_2 = as.numeric(date - sus_treat_start),
              is_infectious_1 = ifelse(diff_treat_start_1 <= 28, T, F),
              is_infectious_2 = ifelse(diff_treat_start_2 <= 28, T, F),
              is_infectious = is_infectious_1 | is_infectious_2,
              time = format(date_time, "%H:%M:%S"),
              is_infectious = ifelse(is.na(is_infectious), F, is_infectious),
              was_suspected = ifelse(is.na(was_suspected), F, was_suspected)
       ) %>%
       group_by(patient_id) %>%
       fill(age, gender, .direction = "downup") %>%
       mutate(across(c(is_infectious, was_suspected), ~ ifelse(any(.x), T, F))) %>%
       ungroup()

#' Note that we implicitly assume that if non-infectious, they are potential
#' unmasked TB patients. I.e. we do not have / do not include information
#' about people that were recently cured.

saveRDS(df_count, "data-clean/clinical/clinical-data.rds")
