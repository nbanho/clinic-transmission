# background image
clinic_img <- png::readPNG("../data-raw/building/clinic_clipped.png")

# building shapes
cellSize <- sqrt(1 * 1e9 / 3e3) 
building <- terra::vect(paste("../data-raw", "building", "clinic-vector.gpkg", sep = "/"))
building_sf <- sf::st_as_sf(building, crs = NA)
sf::st_crs(building_sf) <- NA
waiting_room <- shapeToSpatial(building_sf$geometry[2], cellSize)
passage <- shapeToSpatial(building_sf$geometry[4], cellSize)
tb_room <- shapeToSpatial(building_sf$geometry[3], cellSize)

# room data frames
waiting_room_df <- fortify(waiting_room)
passage_df <- fortify(passage)
tb_room_df <- fortify(tb_room)


entrance_lab <- textGrob(label = "Entrance", x = 0.1, y = 0.165, gp = gpar(size = text_descr / cm(1)), just = c("left"))
wr_lab <- textGrob(label = "Waiting room", x = 0.05, y = 0.45, gp = gpar(size = text_descr / cm(1)), just = c("left"))
tb_lab <- textGrob(label = "TB room", x = 0.75, y = 0.4, gp = gpar(size = text_descr / cm(1)), just = c("left"))
re_lab <- textGrob(label = "Registration", x = 0.44, y = 0.25, gp = gpar(size = text_descr / cm(1)), just = c("left"), rot = 90)