library(raster)
library(tidyverse)
library(terra)

# read.data <- function(x) {
# 
#   message(sprintf("Reading file %s", x))
# 
#   data <- read.table(x, header = F, sep = ",", na.strings = "NONE", blank.lines.skip = F)
#   data <- data[-1, ]
#   if (ncol(data) > 22) {
#     data <- data[ ,-ncol(data)]
#   }
#   colnames(data) <- data[1,]
#   data <- data[-1, ]
# 
#   data <- data %>%
#     filter(!is.na(PersonID)) %>%
#     rename(LastX = `LastX `) %>%
#     mutate(across(c(Time, ObservedSince, TimepointEnteringtoRegistry,TimepointclosestdistancetoRe),
#                   ~ paste(Date, .x)),
#            Date = as.Date(Date, format = "%d.%m.%Y"),
#            Timeframe = factor(Timeframe, levels = c("Morning", "Midday", "Afternoon")),
#            across(c(Time, ObservedSince, TimepointEnteringtoRegistry,TimepointclosestdistancetoRe),
#                   ~ strptime(.x, format = "%d.%m.%Y %H:%M:%S")),
#            across(c(LastX, LastY, Height, DistanceNextPerson, TotalMovement,
#                     TotalPersons, TotalSitting, TotalWZ, TotalE, TotalTB,
#                     RankEnteringtoRegistry, ClosestDistancetoRegistry),
#                   as.numeric),
#            across(c(RankEnteringtoRegistry, ClosestDistancetoRegistry),
#                   ~ ifelse(.x == 0, NA, .x)),
#            across(c(InWZ, InE, InTB), as.logical),
#            PersonID = paste0(Date, "_ID:", PersonID))
# 
#   return(data)
# 
# }
# 
# 
# #### Masi ####
# 
# # Data
# 
# masi_files <- list.files("data-raw/Masi/xovis", pattern = "Coordinates", full.names = T)
# masi_files <- masi_files[-1]
# df_masi <- map_dfr(masi_files, read.data) 
# xy <- df_masi %>% dplyr::select(LastX, LastY) %>% na.omit %>% distinct() %>% as.matrix() 
# 
# plot(xy)
# waiting_room <- draw("polygon")
# writeVector(waiting_room, "data-raw/Masi/building/waiting_room.shp")
# plot(xy)
# passage <- draw("polygon")
# writeVector(passage, "data-raw/Masi/building/passage.shp")
# plot(xy)
# tb_room <- draw("polygon")
# writeVector(tb_room, "data-raw/Masi/building/tb_room.shp")


# Data 

masi <- read.csv("data-raw/Masi/xovis/2021_10_25_coordinates_only.csv", header = F) %>%
  set_names(c("obs_id", "time", "x", "y", "height"))
xy <- masi %>% dplyr::select(x, y) %>% distinct() %>% as.matrix() 

# plot(xy)
# entrance <- draw("polygon")
# writeVector(entrance, "data-raw/Masi/building/entrance.shp")
# plot(xy)
# reception <- draw("polygon")
# writeVector(entrance, "data-raw/Masi/building/reception.shp")
# plot(xy)
# waiting_room <- draw("polygon")
# writeVector(waiting_room, "data-raw/Masi/building/waiting_room.shp")
# plot(xy)
# passage <- draw("polygon")
# writeVector(passage, "data-raw/Masi/building/passage.shp", overwrite = T)
# plot(xy)
# tb_room <- draw("polygon")
# writeVector(tb_room, "data-raw/Masi/building/tb_room.shp", overwrite = T)
# plot(xy)
# all_clinic <- draw("polygon")
# writeVector(tb_room, "data-raw/Masi/building/all_clinic.shp", overwrite = T)

er <- rbind(c(6225, -6050), c(11250, -5400), c(10800, -3100), c(9100, -3330), c(5850, -3800))
re <- rbind(c(5850, -3800), c(8275, -3435), c(7600, 225), c(5220, -150))
wr <- rbind(c(10750, -3100), c(15600, -2450), c(14790, 2100), c(14650, 2850), c(4950, 1500), c(5220, -150), c(5250, -350), c(5850, -3800), c(7350, -3575), c(8275, -3435), c(9000, -3350))
pa <- rbind(c(4950, 1500), c(5250, -350), c(3550, -600), c(450, -1130), c(-2400, -1575), c(-2350, -1900), c(-6800, -2500), c(-7100, -300))
tb <- rbind(c(-6800, -2500), c(-6200, -6800), c(-1675, -6300), c(-2350, -1900))
#exl <- rbind(c(-4700, -4400), c(-4425, -6350), c(-6300, -6525), c(-6750, -3000), c(-4930, -2800))
exu1 <- rbind(c(5900, 1300), c(7650, 1600), c(7800, 4000), c(5300, 3600))
exu2 <- rbind(c(11600, 2230), c(12600, 2400), c(12800, 4100), c(11100, 3850))
swr <- rbind(c(9000, -3350), c(15600, -2450), c(14790, 2100), c(8200, 1100))
spa1 <- rbind(c(-2400, -1575), c(450, -1130), c(400, -400), c(350, -70), c(-2550, -530))
roe1 <- rbind(c(450, -1130), c(400, -400), c(3430, 100), c(3550, -600), c(4300, -1700), c(200, -2350))
spa2 <- rbind(c(3550, -600), c(5250, -350), c(5050, 780), c(3350, 500), c(3430, 100))
roe2 <- rbind(c(1900, 650), c(2300, 2200), c(-2050, 1600), c(-1100, 150))
clinic <- rbind(cbind(object = 1, part = 1, er, hole = 0),
                cbind(object = 2, part = 2, wr, hole = 0),
                cbind(object = 3, part = 3, pa, hole = 0),
                cbind(object = 4, part = 4, tb, hole = 0),
                cbind(object = 5, part = 5, re, hole = 0),
                cbind(object = 6, part = 6, exu1, hole = 0),
                cbind(object = 7, part = 7, exu2, hole = 0),
                cbind(object = 8, part = 8, swr, hole = 0),
                cbind(object = 9, part = 9, spa1, hole = 0),
                cbind(object = 10, part = 10, spa2, hole = 0),
                cbind(object = 11, part = 11, roe1, hole = 0),
                cbind(object = 12, part = 12, roe2, hole = 0))
clinic <- vect(clinic, "polygons")
writeVector(clinic, "data-raw/Masi/building/clinic-vector.gpkg", overwrite = T)
clinic <- sf::st_as_sf(clinic)
clinic <- fortify(clinic)
ggplot(clinic) + 
  geom_point(data = masi %>% dplyr::select(x, y) %>% distinct(), mapping = aes(x = x, y = y), color = "blue", alpha = .05) +
  geom_sf(linewidth = 1, fill = NA) 
