#### Libraries ####

library(tidyverse)

#### Helper ####


#### Masi ####

# Data

clinical_masi <- readRDS("data-clean/Masi/clinical-data/clinical-data.rds")
xovis_masi <- readRDS("data-clean/Masi/xovis/patients-time-location.rds")
# env_masi <- readRDS("data-clean/Masi/environmental-data/co2-levels.rds")


# Only first entry from clinical data

clinical_masi <- clinical_masi %>%
  group_by(patient_id, date) %>%
  slice(1) %>%
  ungroup()

# summary xovis

# xovis_reg_masi <- xovis_masi %>%
#   group_by(PersonID) %>%
#   summarise(Enter_Date = min(Time),
#             Exit_Date = max(Time),
#             InE = any(InE),
#             Height = max(Height),
#             TimepointEnteringtoRegistry = min(TimepointEnteringtoRegistry, na.rm = T)) %>%
#   ungroup() %>%
#   filter(Height >= 1400 & Height <= 2100,
#          InE,
#          !is.infinite(TimepointEnteringtoRegistry))

xovis_reg_masi <- xovis_masi %>%
  group_by(person_id) %>%
  summarise(min_time = min(time),
            max_time = max(time),
            time_registration = min(time_registration, na.rm = T)) %>%
  ungroup() 

# pre-filter clinical data
clinical_masi <- clinical_masi %>%
  filter(date_time >= min(xovis_reg_masi$time_registration),
         date_time <= max(xovis_reg_masi$time_registration))

# matching clinical and xovis data
#' match the date of first event in the clinical data 
#' to the person that last came to the Entrance 
#' with a tolerance of 15min

merge_masi <- merge(xovis_reg_masi %>% select(person_id, time_registration),
                    clinical_masi %>% select(patient_id, date_time, tb_suspect),
                    id = NULL) %>%
  mutate(timediff = as.numeric(difftime(date_time, time_registration), units = "secs")) %>%
  filter(timediff <= 15 * 60,
         timediff > 0) %>%
  group_by(patient_id) %>%
  arrange(timediff) %>%
  slice(1) %>%
  ungroup()

hist(merge_masi$timediff, breaks = c(0, 10, 30, 60, 120, 180, 300, 600, 900))

full_merge_masi <- xovis_masi %>% 
  left_join(merge_masi) %>%
  rename(clinical_entry_time = date_time,
         timediff_clinical_regist = timediff)

saveRDS(full_merge_masi, "data-clean/Masi/merged-clinical-xovis.rds")

# xovis_sub <- xovis_masi %>%
#   left_join(merge_masi %>% mutate(matched = T) %>% select(PersonID, matched)) %>%
#   filter(matched)
# 
# n_xovis_matched <- xovis_sub %>%
#   group_by(PersonID) %>%
#   summarize(n = n()) %>%
#   ungroup() 
# 
# sum(n_xovis_matched$n > 1)
# 
# n_xovis_matched %>%
#   mutate(levels_n = cut(n, c(0:5, Inf))) %>%
#   group_by(levels_n) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   ggplot(aes(x = levels_n, y = count)) +
#   geom_bar(stat = "identity")
# 
# xovis_sub_switches <- xovis_sub %>% 
#   mutate(In = paste(InWZ, InE, InTB)) %>%
#   group_by(PersonID) %>%
#   arrange(Time) %>%
#   mutate(switch = ifelse(In == dplyr::lag(In), T, F)) %>%
#   summarise(n_switch = sum(switch, na.rm = T)) %>%
#   ungroup()
# 
# sum(xovis_sub_switches$n_switch > 0)
# 
# most_watched <- xovis_sub_switches %>%
#   arrange(desc(n_switch)) %>%
#   slice(1:3)
# 
# most_watched <- xovis_sub %>%
#   filter(PersonID %in% most_watched$PersonID) %>%
#   mutate(Room = ifelse(InWZ, "Waiting room", ifelse(InE, "Entrance", ifelse(InTB, "TB room", "No room"))))
# 
# most_watched %>%
#   ggplot(aes(x = LastX, y = LastY, color = PersonID, shape = Room, group = PersonID)) +
#   geom_point() +
#   geom_line()
# 
# ggsave("insights/trace-most-watched.pdf")
