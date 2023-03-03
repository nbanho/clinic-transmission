#### Libraries ####

library(tidyverse)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(VennDiagram)
source("utils/spatial.r")

# roomplan
clinic <- vect("data-raw/Masi/building/clinic-vector.gpkg")
clinic_sf <- sf::st_as_sf(clinic, crs = NA)
st_crs(clinic_sf) <- NA
clinic_df <- fortify(clinic_sf)

# xovis raw data files
files <- list.files("data-raw/Masi/xovis/Study_Data_Masi_2021/export", full.names = T)

for (f in files) {
  # file info
  file_name <- basename(f)
  date <- gsub("_exported_data[[:punct:]]csv", "", file_name)
  date <- gsub("_", "-", date)
  file_name <- paste0(date, "-", "with_roominfo")
  
  
  # read data
  df <- read.csv(f, header = T) %>% 
    set_names(c("obs_id", "time", "x", "y", "height"))  %>%
    mutate(time = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"))
  
  # add room info
  df$point <- map2(df$x, df$y, function(xi,yi) sfheaders::sf_point(c(xi,yi)))
  df$poly = map(df$point, sf::st_within, y = clinic_sf)
  df$is_waitingroom <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 2), T, F))
  df$is_tbroom <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 3), T, F))
  df$is_passage <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 4), T, F))
  df$is_reception <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 5), T, F))
  df$is_entrance <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 6), T, F))
  df$is_inside_reception <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 8), T, F))
  df$is_seat = map_lgl(df$poly, function(p) ifelse(any(unlist(p) %in% c(7, 9, 10, 11)), T, F) ) 
  df$is_in_room_left <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 12), T, F) ) 
  df$is_in_room_right <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) == 13), T, F) ) 
  df$is_exit <- map_lgl(df$poly, function(p) ifelse(any(unlist(p) %in% c(6, 14:15)), T, F)) # entrance and corridors are considered as exits
 
  saveRDS(df, file = paste0("data-raw/Masi/xovis/", file_name, ".rds")) 
}





