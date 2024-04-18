# libraries
library(tidyverse)
library(png)
library(terra)
library(sf)
library(grid)

if (file.exists("../utils/spatial.r")) {
  source("../utils/spatial.r")
  clinic_img <- png::readPNG("../data-raw/building/clinic_clipped.png")
  building <- terra::vect(paste("../data-raw", "building", "clinic-vector.gpkg", sep = "/"))
} else {
  source("utils/spatial.r")
  clinic_img <- png::readPNG("data-raw/building/clinic_clipped.png")
  building <- terra::vect(paste("data-raw", "building", "clinic-vector.gpkg", sep = "/"))
}

building_sf <- sf::st_as_sf(building, crs = NA)
sf::st_crs(building_sf) <- NA

# rasterize

if (!exists("cellSize")) {
  stop("No local cellSize variable found")
}

rasterize.rooms <- function(cellSize) {
  waiting_room <- shapeToSpatial(building_sf$geometry[2], cellSize)
  passage <- shapeToSpatial(building_sf$geometry[4], cellSize)
  tb_room <- shapeToSpatial(building_sf$geometry[3], cellSize)
  return(list(waiting_room = waiting_room, corridor = passage, tb_room = tb_room))
}

rooms <- rasterize.rooms(cellSize)
waiting_room <- rooms$waiting_room
wrMat <- sP_to_matrix(waiting_room)
wrCoord <- create_coord_df(waiting_room)
corridor <- rooms$corridor
cdMat <- sP_to_matrix(corridor)
cdCoord <- create_coord_df(corridor)
tb_room <- rooms$tb_room
tbMat <- sP_to_matrix(tb_room)
tbCoord <- create_coord_df(tb_room)
roomCoord <- rbind(
  wrCoord %>% mutate(room = "Waiting room"),
  cdCoord %>% mutate(room = "Corridor"),
  tbCoord %>% mutate(room = "TB room")
)

# room dimensions
# - waiting room
dimWR <- c(10.55, 5.7, 3) # (length x width x height in m)
# - corridor
dimCD <- c(7.7, 2.2, 2.5)
# - TB room
dimTB <- c(4.75, 3.5, 3)

# plotting

plot_spatial <- function(pl, text_descr = 10) {
  entrance_lab <- textGrob(label = "Entrance", x = 0.1, y = 0.165, gp = gpar(fontsize = text_descr), just = c("left"))
  wr_lab <- textGrob(label = "Waiting room", x = 0.05, y = 0.45, gp = gpar(fontsize = text_descr), just = c("left"))
  tb_lab <- textGrob(label = "TB room", x = 0.75, y = 0.4, gp = gpar(fontsize = text_descr), just = c("left"))
  cr_lab <- textGrob(label = "Corridor", x = 0.6, y = 0.615, gp = gpar(fontsize = text_descr), just = c("left"))
  re_lab <- textGrob(label = "Registration", x = 0.44, y = 0.25, gp = gpar(fontsize = text_descr), just = c("left"), rot = 90)
  col <- RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")

  pl +
    ggpubr::background_image(clinic_img) +
    scale_y_continuous(expand = expansion(add = c(1700, 3000)), limits = c(-6500, 2000)) +
    scale_x_continuous(expand = expansion(add = c(400, 500)), limits = c(-16000, 7500)) +
    annotation_custom(entrance_lab, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(wr_lab, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(cr_lab, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(tb_lab, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    annotation_custom(re_lab, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme_custom(text_descr) +
    theme(
      text = element_text(size = text_descr),
      axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
      panel.background = element_rect(fill = col[1]), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# room data frames
# waiting_room_df <- fortify(waiting_room)
# passage_df <- fortify(passage)
# tb_room_df <- fortify(tb_room)
