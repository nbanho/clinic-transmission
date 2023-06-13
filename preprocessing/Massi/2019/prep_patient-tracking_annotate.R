#### Libraries ####

library(tidyverse)
library(lubridate)
source("utils/spatial.r")

#### Data ####

# files
files <- list.files("data-raw/Massi/2019/patient-tracking/xovis/included", full.names = T)

# load data
df <- do.call(rbind, parallel::mclapply(files, function(f) {
  read.csv(f, header = F) %>%
    set_names(c("obs_id", "date", "time", "x", "y", "height")) %>%
    mutate(date = as.Date(date, "%d.%m.%Y"),
           date_time = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%OS", tz = "CET")) %>%
    dplyr::select(date, obs_id, date_time, x, y, height) %>% 
    mutate(date_time = round_date(date_time, unit = "second")) %>% 
    group_by(date, obs_id, date_time) %>%
    slice(1) %>%
    ungroup()
}, mc.cores = 6)) 


#### Roomplan ####

# roomplan
clinic <- vect("data-raw/Massi/building/clinic-vector.gpkg")
clinic_sf <- sf::st_as_sf(clinic, crs = NA)
st_crs(clinic_sf) <- NA
clinic_df <- fortify(clinic_sf)

#### Rotate data ####

ggplot() +
  geom_sf(data = clinic_df) +
  geom_bin2d(data = df %>%
               sample_frac(size = .25) %>%
               rotate_xy(181) %>%
               mutate(y = y - 950,
                      x = x - 7700), 
             mapping = aes(x = x, y = y), binwidth = 100) +
  scale_fill_stepsn(colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")[c(1,3,5,7,9)], breaks = c(1,10,100,1000,10000), limits = c(0,10000)) 

df <- df %>%
  rotate_xy(181) %>%
  mutate(y = y - 950,
         x = x - 7700)

#### Filter data ####

# compute duration and distance
dd_id <- df %>% 
  group_by(date, obs_id) %>%
  summarize(duration = as.numeric(difftime(last(date_time), first(date_time), units = "secs"))) %>%
  ungroup() 

sum(dd_id$duration > 10) / n_distinct(dd_id$date)

# filter observations with less than a minute duration
# dd_id <- dd_id %>%
#   mutate(no_noise = ifelse(duration > 10, T, F))
# df <- df %>%
#   left_join(dd_id %>% dplyr::select(obs_id, date, no_noise), by = c("date", "obs_id")) %>%
#   filter(no_noise) %>%
#   dplyr::select(-no_noise)


#### Code location ####

# make it iteratively
df_list <- df %>%
  nest(obs_id, date_time, height, x, y)

for (i in 1:nrow(df_list)) {
  # day of data
  df_i <- df_list$data[[i]]
  
  # only determine reception and waiting room
  df_i$point <- parallel::mcmapply(function(xi,yi) sfheaders::sf_point(c(xi,yi)), df_i$x, df_i$y, mc.cores = 6)
  df_i$poly = parallel::mclapply(df_i$point, sf::st_within, y = clinic_sf, mc.cores = 6)
  df_i$is_reception <- map_lgl(df_i$poly, function(p) ifelse(any(unlist(p) == 5), T, F))
  df_i$is_waitingroom <- map_lgl(df_i$poly, function(p) ifelse(any(unlist(p) == 2), T, F))
  df_i$is_seat = map_lgl(df_i$poly, function(p) ifelse(any(unlist(p) %in% c(7, 9, 10, 11)), T, F) ) 
  df_i$is_passage <- map_lgl(df_i$poly, function(p) ifelse(any(unlist(p) == 4), T, F))
  df_i$is_entrance <- map_lgl(df_i$poly, function(p) ifelse(any(unlist(p) == 6), T, F))
  df_i$is_exit <- map_lgl(df_i$poly, function(p) ifelse(any(unlist(p) %in% c(6, 14:15)), T, F))
  
  # save data
  saveRDS(df_i %>% dplyr::select(-point,-poly), paste0("data-raw/Massi/2019/patient-tracking/annotated/", df_list$date[i], ".rds"))
}




