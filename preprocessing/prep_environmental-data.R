#### Libraries ####

library(tidyverse)
library(haven)


#### Masi ####

df <- read_dta("data-raw/Masi/environmental-data/co2_data.dta") %>%
  mutate(date = Date,
         time = format(new_time, "%H:%M:%S"),
         date_time = as.POSIXct(paste(Date, time)),
         location = Monitor) %>%
  rename(temperature = Temperature,
         humidity = Humidity,
         co2 = CO2) %>%
  select(date, time, date_time, location, co2, temperature, humidity) 


saveRDS(df, "data-clean/Masi/environmental-data/co2-levels.rds")
  
