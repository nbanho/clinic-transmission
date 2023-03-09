#### Libraries ####

library(tidyverse)
library(haven)


#### Masi ####

# load data
masi <- read_dta("data-raw/Masi/environmental-data/co2_data.dta") %>%
  mutate(date = Date,
         time = format(new_time, "%H:%M:%S"),
         date_time = as.POSIXct(paste(Date, time)),
         location = Monitor) %>%
  rename(temperature = Temperature,
         humidity = Humidity,
         co2 = CO2) %>%
  select(date, time, date_time, location, co2, temperature, humidity) 

# waiting room is the average of waiting room and registration
wr <- masi %>%
  filter(location %in% c("registration", "waiting room")) %>%
  group_by(date, time, date_time) %>%
  summarize(across(c(co2, temperature, humidity), mean)) %>%
  ungroup() %>%
  mutate(location = "waiting room")

# assume: co2 level in passage is the same as in the waiting room
#' TODO: check if this assumptions is valid; 
#' maybe not because the door to the passage from the waiting room may be closed

pa <- wr %>%
  filter(location == "waiting room") %>%
  mutate(location = "passage") 

# tb room
tb <- masi %>%
  filter(location == "TB room") %>%
  mutate(location = "tb room")

# combine 
masi_prep <- rbind(wr, pa, tb)


saveRDS(masi_prep, "data-clean/Masi/environmental-data/co2-temp-humid.rds")
  
