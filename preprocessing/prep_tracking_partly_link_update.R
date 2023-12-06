library(tidyverse)

#### Data for 10-13 ####
df_new <- readRDS("data-raw/Masi/patient-tracking-data/2021-10-13.rds")
bad_ids <- readRDS("data-raw/Masi/xovis/partly_linked/2021-10-13_bad_ids.rds")
df_old <- readRDS("data-raw/Masi/xovis/partly_linked/2021-10-13.rds")

# Difference in counts by id
oid_new <- df_new %>%
  group_by(obs_id) %>%
  summarize(n_new = n()) %>%
  ungroup()
oid_old <- df_old %>%
  group_by(obs_id) %>%
  summarize(n_old = n()) %>%
  ungroup()
oid <- full_join(oid_new, oid_old) %>%
  mutate(across(c(n_new, n_old), ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(n_diff = n_new - n_old)
oid %>%
  ggplot(aes(x = obs_id, y = n_diff)) +
  geom_line() +
  geom_hline(aes(yintercept = 10), color = "red") +
  scale_y_sqrt(breaks = c(0, 10, 100)) 
oid %>%
  filter(n_diff > 10) 
oid %>%
  filter(n_old == 0)
oid %>%
  filter(n_new == 0)

# ended IDs
df_old %>%
  filter(!is.na(tracking_end)) %>%
  group_by(obs_id) %>%
  slice(1) %>% 
  ggplot() +
  geom_histogram(aes(x = obs_id))

# update
df_new_upd <- df_new %>%
  left_join(df_old %>%
              dplyr::select(patient_id, obs_id, tracking_end) %>%
              group_by(obs_id) %>%
              slice(1) %>%
              ungroup(),
            by = "obs_id") %>%
  mutate(patient_id = ifelse(obs_id > 7000, obs_id_new, patient_id),
         patient_id = ifelse(is.na(patient_id), obs_id_new, patient_id),
         tracking_end = ifelse(grepl("Lost", tracking_end), NA, tracking_end)) %>%
  dplyr::select(patient_id, obs_id_new, obs_id, tracking_end, everything())

# save
saveRDS(df_new_upd, "data-raw/Masi/xovis/partly_linked/2021-10-13_updated.rds")


#### Data for 10-15 ####
df_new <- readRDS("data-raw/Masi/patient-tracking-data/2021-10-15.rds")
df_old <- readRDS("data-raw/Masi/xovis/partly_linked/2021-10-15.rds")

# Difference in counts by id
oid_new <- df_new %>%
  group_by(obs_id) %>%
  summarize(n_new = n()) %>%
  ungroup()
oid_old <- df_old %>%
  group_by(obs_id) %>%
  summarize(n_old = n()) %>%
  ungroup()
oid <- full_join(oid_new, oid_old) %>%
  mutate(across(c(n_new, n_old), ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(n_diff = n_new - n_old)
oid %>%
  ggplot(aes(x = obs_id, y = n_diff)) +
  geom_line() +
  geom_hline(aes(yintercept = 10), color = "red") +
  scale_y_sqrt(breaks = c(0, 10, 100)) 
oid %>%
  filter(n_diff > 10)
oid %>%
  filter(n_old == 0)
oid %>%
  filter(n_new == 0)

# ended IDs
df_old %>%
  filter(!is.na(tracking_end)) %>%
  group_by(obs_id) %>%
  slice(1) %>%
  ggplot() +
  geom_histogram(aes(x = obs_id))

# update
df_new_upd <- df_new %>%
  left_join(df_old %>%
              dplyr::select(patient_id, obs_id, tracking_end) %>%
              group_by(obs_id) %>%
              slice(1) %>%
              ungroup(),
            by = "obs_id") %>%
  mutate(patient_id = ifelse(is.na(patient_id), obs_id_new, patient_id),
         tracking_end = ifelse(grepl("Lost", tracking_end), NA, tracking_end))

# save
saveRDS(df_new_upd, "data-raw/Masi/xovis/partly_linked/2021-10-15_updated.rds")


#### Data for 10-25 ####
df_new <- readRDS("data-raw/Masi/patient-tracking-data/2021-10-25.rds")
df_old <- readRDS("data-raw/Masi/xovis/partly_linked/2021-10-25.rds")

# Difference in counts by id
oid_new <- df_new %>%
  group_by(obs_id) %>%
  summarize(n_new = n()) %>%
  ungroup()
oid_old <- df_old %>%
  group_by(obs_id) %>%
  summarize(n_old = n()) %>%
  ungroup()
oid <- full_join(oid_new, oid_old) %>%
  mutate(across(c(n_new, n_old), ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(n_diff = n_new - n_old)
oid %>%
  ggplot(aes(x = obs_id, y = n_diff)) +
  geom_line() +
  geom_hline(aes(yintercept = 10), color = "red") +
  scale_y_sqrt(breaks = c(0, 10, 100)) 
oid %>%
  filter(n_diff > 10)
oid %>%
  filter(n_old == 0)
oid %>%
  filter(n_new == 0)

# ended IDs
df_old %>%
  filter(!is.na(tracking_end)) %>%
  group_by(obs_id) %>%
  slice(1) %>%
  ggplot() +
  geom_histogram(aes(x = obs_id))

# update
df_new_upd <- df_new %>%
  left_join(df_old %>%
              dplyr::select(patient_id, obs_id, tracking_end) %>%
              group_by(obs_id) %>%
              slice(1) %>%
              ungroup(),
            by = "obs_id") %>%
  mutate(tracking_end = ifelse(obs_id > 9000, NA, tracking_end),
         patient_id = ifelse(obs_id > 9000, obs_id_new, patient_id),
         patient_id = ifelse(is.na(patient_id), obs_id_new, patient_id),
         tracking_end = ifelse(grepl("Lost", tracking_end), NA, tracking_end)) %>%
  dplyr::select(patient_id, obs_id_new, obs_id, tracking_end, everything())

# check
df_new_upd %>%
  group_by(patient_id) %>%
  mutate(unfinished = last(is.na(tracking_end)) & any(!is.na(tracking_end))) %>%
  ungroup() %>% 
  filter(unfinished) %>%
  View()

# save
saveRDS(df_new_upd, "data-raw/Masi/xovis/partly_linked/2021-10-25_updated.rds")
  

#### Data for 11-05 ####
df_new <- readRDS("data-raw/Masi/patient-tracking-data/2021-11-05.rds")
df_old <- readRDS("data-raw/Masi/xovis/partly_linked/2021-11-05.rds")

# Difference in counts by id
oid_new <- df_new %>%
  group_by(obs_id) %>%
  summarize(n_new = n()) %>%
  ungroup()
oid_old <- df_old %>%
  group_by(obs_id) %>%
  summarize(n_old = n()) %>%
  ungroup()
oid <- full_join(oid_new, oid_old) %>%
  mutate(across(c(n_new, n_old), ~ ifelse(is.na(.x), 0, .x))) %>%
  mutate(n_diff = n_new - n_old)
oid %>%
  ggplot(aes(x = obs_id, y = n_diff)) +
  geom_line() +
  geom_hline(aes(yintercept = 10), color = "red") +
  scale_y_sqrt(breaks = c(0, 10, 100)) 
oid %>%
  filter(n_diff > 10) %>% View()
oid %>%
  filter(n_old == 0) %>% View()
oid %>%
  filter(n_new == 0)

# ended IDs
df_old %>%
  filter(!is.na(tracking_end)) %>%
  group_by(obs_id) %>%
  slice(1) %>%
  ggplot() +
  geom_histogram(aes(x = obs_id))

# update
bad_time <- df_new %>%
  filter(obs_id == 16575) %>%
  tail(1)

df_new_upd <- df_new %>%
  left_join(df_old %>%
              dplyr::select(patient_id, obs_id, tracking_end) %>%
              group_by(obs_id) %>%
              slice(1) %>%
              ungroup(),
            by = "obs_id") %>%
  mutate(patient_id = ifelse(is.na(patient_id), obs_id_new, patient_id),
         tracking_end = ifelse(grepl("Lost", tracking_end), NA, tracking_end)) #%>%
  # group_by(patient_id) %>%
  # arrange(time) %>%
  # mutate(first_time = first(time)) %>%
  # ungroup() %>%
  # mutate(tracking_end = ifelse(first_time <= bad_time$time[1] & tracking_end == "Lost enter", NA, tracking_end)) %>%
  # dplyr::select(patient_id, obs_id_new, obs_id, tracking_end, everything(), -first_time)

# check
df_new_upd %>%
  group_by(patient_id) %>%
  mutate(unfinished = last(is.na(tracking_end)) & any(!is.na(tracking_end))) %>%
  ungroup() %>% 
  filter(unfinished) %>%
  View()

sum(is.na(df_new_upd$patient_id))

# save
saveRDS(df_new_upd, "data-raw/Masi/xovis/partly_linked/2021-11-05_updated.rds")
