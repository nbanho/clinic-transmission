#' Compute euclidean distance in two dimensions
#' 
#' @param x1 x coordinate of first point
#' @param x2 x coordinate of second point
#' @param y1 y coordinate of first point
#' @param y2 y coordinate of second point
#' 

euclidean <- function(x1, x2, y1, y2) {
  sqrt( (x1-x2)^2 + (y1-y2)^2 )
}


#' Convert distance from px to m
#' 
#' @param x distance (in px)
#' 

convert_dist <- function(x) {
  x / 1e3
}


#' Convert shape to rastered spatial polygon data frame
#' 
#' @param sh shape
#' @param cell_size length/width of squared cell (in px)
#' 

shapeToSpatial <- function(sh, cell_size) {
  ng <- sf::st_make_grid(sh, cellsize = cell_size) 
  ng <- sf::st_sf(ng)
  ng <- sf::st_intersection(ng, sf::st_geometry(sf::st_as_sf(sh)))
  ng_types <- vapply(sf::st_geometry(ng), function(x) {class(x)[2]}, "")
  ng <- ng[grepl("*POLYGON", ng_types), ]
  roomSP <- sf::as_Spatial(st_sf(ng))
  return(roomSP)
}


#' Create data frame with coordinates (x,y,id) from spatial polygon data frame
#' 
#' @param spat_poly_df spatial polygon data frame
#' 

create_coord_df <- function(spat_poly_df) {
  cellCoords <- data.frame(coordinates(spat_poly_df)) 
  colnames(cellCoords) <- c("x", "y")
  cellCoords$id <- 1:nrow(cellCoords)
  return(cellCoords)
}


#' Compute volume from spatial polygon data frame 
#' 
#' @param spat_poly_df spatial polygon data frame
#' @param H height of the room

compute_volume <- function(spat_poly_df, H = 3) {
  A <- convert_dist(convert_dist(sum(area(spat_poly_df))))
  V <- A * H
  return(V)
}


#' Find raster cell id for data point based on euclidean distance
#' 
#' @param x x coordinate of data point
#' @param y y coordinate of data point
#' @param cellCoordsXY matrix of x and y coordinates of cells
#' 

find_raster <- function(x, y, cellCoordsXY) {
  # assigns cell id by closest distance to cell center coordinates
  # thus it assumes that the cells are square
  xy <- cbind(x, y)
  dist <- terra::distance(cellCoordsXY, xy, lonlat = F)
  closest_id <- which.min(dist[,1])
  # if the point lies on the edge of two cluster, randomly select one
  if (length(closest_id) > 1) {
    closest_id <- sample(closest_id, size = 1)
  }
  return(closest_id)
}

