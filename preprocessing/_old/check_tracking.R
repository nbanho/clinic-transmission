#### Libraries ####

library(tidyverse)


#### Data ####

files <- list.files("data-clean/Massi/2021/patient-tracking", full.names = T)
ex_file <- files[3]
df <- readRDS(ex_file)


#### Check ####

# stats
statDF <- df %>%
  group_by(patient_id) %>%
  summarize(
    duration = as.numeric(difftime(last(time), first(time), units = "min")),
    was_at_reception = any(is_reception)
  )

# good IDs
statDF_good <- statDF %>%
  filter(duration > 5,
         was_at_reception)

# visualize
df_good <- df %>%
  filter(patient_id %in% statDF_good$patient_id)

df_good %>%
  filter(patient_id %in% sample(statDF_good$patient_id, size = 10)) %>%
  ggplot(aes(x = x, y = y, color = factor(patient_id))) +
  geom_path()
