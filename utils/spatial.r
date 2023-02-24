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


#' Plot individual tracks from patients with linked observations
#' 
#' @param df data frame with columns patient_id, obs_id, time, x, y, and height
#' 

plot_single_track <- function(df) {
  
  # Descriptives
  descr <- df %>%
    mutate(dt = as.numeric(difftime(lead(time), time, units = "secs"))) %>%
    summarize(
      Total = as.numeric(difftime(last(time), first(time), units = "secs")),
      `Time in waiting room` = sum(dt[is_waitingroom], na.rm = T),
      `Time in passage` = sum(dt[is_passage], na.rm = T),
      `Time in TB room` = sum(dt[is_tbroom], na.rm = T),
      `Time in seating area` = sum(dt[is_seating], na.rm = T),
      `Time in care room` = sum(dt[is_in_room1 | is_in_room2], na.rm = T),
      `Time at reception` = sum(dt[is_reception], na.rm = T)
    ) %>%
    gather() %>%
    mutate(value = ifelse(key == "Time at reception", paste0(value, "sec"), paste0(format(round(value / 60, 1), nsmall = 1), "min"))) %>%
    set_names(c("Variable", "Duration"))
  
  tt <- ttheme_default(base_size = 6)
  
  # Start and end of patient track
  se <- df %>%
    group_by(patient_id) %>%
    arrange(time) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  # Start and end of each observation
  se_obs <- df %>%
    group_by(patient_id, obs_id) %>%
    arrange(time) %>%
    slice(1, n()) %>%
    ungroup() 
  
  # Start and end of each observation
  tab <- se_obs %>%
    mutate(height = round(height / 10, digits = 0),
           time = format(time, "%H:%M:%S")) %>%
    dplyr::select(obs_id, time, x, y, height) %>%
    mutate_all(as.character)
  
  # Start and end of each observation in plot
  se_obs <- se_obs %>%
    slice(c(-1, -n())) 
  
  # Plot 
  pl <- ggplot() +
    geom_sf(data = clinic_df, linewidth = 1, fill = NA) +
    geom_path(data = df, mapping = aes(x = x, y = y, color = factor(obs_id), group = factor(patient_id)), alpha = .5) +
    geom_point(data = se, mapping = aes(x = x, y = y, shape = type)) +
    geom_point(data = se_obs, mapping = aes(x = x, y = y, color = factor(obs_id)), size = 0.5) +
    scale_shape_manual(values = c(5, 13)) +
    scale_x_continuous(labels = function(x) x / 1000, breaks = seq(-10000, 20000, 1000)) +
    scale_y_continuous(labels = function(x) x / 1000, breaks = seq(-8000, 6000, 1000)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8)) 
  
  # Observations table: add colours to table
  g <- ggplot_build(pl)
  if (nrow(g$data[[4]]) > 0) {
    gcol <- g$data[[4]] %>% 
      dplyr::select(group, colour) %>%
      group_by(group) %>%
      slice(1) %>%
      ungroup()
    tab <- tab %>%
      group_by(obs_id) %>%
      mutate(group = group_indices()) %>%
      left_join(gcol, by = "group") %>%
      dplyr::select(-group)
  } else {
    tab$colour <- "red"
  }
  
  # Show only a subset of tab if there are many links
  if (nrow(tab) > 20) {
    tab_top5 <- head(tab, 10)
    tab_bottom5 <- tail(tab, 10)
    mid_row <- data.frame(obs_id = "...", time = "...", x = "...", y = "...", height = "...", colour = "black")
    tab <- rbind(tab_top5, mid_row, tab_bottom5)
  }
  
  cols <- matrix(rep(tab$colour, ncol(tab)), ncol = ncol(tab))
  ttcol <- ttheme_default(core=list(fg_params = list(col = cols),
                                 bg_params = list(col=NA)),
                       rowhead=list(bg_params = list(col=NA)),
                       colhead=list(bg_params = list(col=NA)),
                       base_size = 6)
  
  
  # combine grobs
  pl_tab <- grid.arrange(arrangeGrob(tableGrob(descr, rows = NULL, theme = tt), pl, nrow = 2), tableGrob(tab %>% dplyr::select(-colour), rows = NULL, theme = ttcol), ncol = 2)
  return(pl_tab)
}


#' Plot multiple tracks from multiple IDs
#' 
#' @param df data frame with columns id_col, x, y, and height
#' @param id_col the ID column by which to identify individual tracks
#' 

plot_track <- function(df, id_col = "obs_id") {
  colnames(df)[colnames(df)==id_col] <- "id" 
  
  df <- mutate(df, id = factor(id))
  
  df_se <- df %>%
    group_by(id) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  ggplot(clinic_df) +
    geom_sf(linewidth = 1, fill = NA) +
    geom_path(data = df, mapping = aes(x = x, y = y, group = id, color = id), alpha = .2) +
    geom_point(data = df_se, mapping = aes(x = x, y = y, color = id, shape = type), size = 3) +
    scale_shape_manual(values = c(5, 13)) +
    theme(legend.position = "bottom")
}


#' Plot IDs at a certain time
#' 
#' @param df data frame with columns patient_id, obs_id, time, x, y, and height
#' @param t the time point
#' @param focus_id ID to highlight
#' @param k seconds to look ahead
#' @param date date as string
#' 

plot_ids <- function(df, t, focus_id, k = 300, date = "2021-10-25") {
  
  t <- as.POSIXct(paste(date, t))
  
  df <- df %>%
    mutate(height = round(height / 10, 0),
           patient_id = factor(patient_id),
           obs_id = factor(obs_id))
  
  focus_id_df <- df %>%
    filter(obs_id == focus_id)
  
  focus_id_df_se <- focus_id_df %>%
    group_by(patient_id) %>%
    arrange(time) %>%
    slice(c(1, n())) %>%
    mutate(type = c("start", "end")) %>%
    ungroup() 
  
  focus_id_lab <- focus_id_df %>%
    slice(n())
  
  possible_matches <- df %>%
    group_by(patient_id) %>%
    mutate(ft = first(time)) %>%
    ungroup() %>%
    filter(ft > t,
           between(time, t, t + seconds(k)),
           obs_id != focus_id,
           patient_id != focus_id_df$patient_id[1]) 
  
  possible_matches_lab <- possible_matches %>%
    group_by(patient_id) %>%
    slice(1) 
  
  linked_matches <- df %>%
    group_by(patient_id) %>%
    mutate(ftp = first(time)) %>%
    ungroup() %>%
    group_by(obs_id) %>%
    mutate(fto = first(time)) %>%
    ungroup() %>%
    filter(fto > t & ftp <= t,
           between(time, t, t + seconds(k)),
           obs_id != focus_id,
           patient_id != focus_id_df$patient_id[1]) 
  
  pl <- ggplot() +
    geom_sf(data = clinic_df, linewidth = 1, fill = NA) +
    geom_point(data = focus_id_df_se, mapping = aes(x = x, y = y, shape = type), color = "black") +
    geom_path(data = focus_id_df, mapping = aes(x = x, y = y, group = patient_id), color = "black") +
    geom_text(data = focus_id_lab, mapping = aes(x = x, y = y, label = height), size = 8 / cm(1), vjust = -.5, hjust = -.5) +
    geom_point(data = possible_matches, mapping = aes(x = x, y = y, color = patient_id)) +
    geom_path(data = possible_matches, mapping = aes(x = x, y = y, color = patient_id)) +
    geom_text(data = possible_matches_lab, mapping = aes(x = x, y = y, label = height, color = patient_id), size = 8 / cm(1), vjust = -.5, hjust = -.5) +
    geom_point(data = linked_matches, mapping = aes(x = x, y = y, group = patient_id), color = "grey", alpha = 0.2) +
    geom_path(data = linked_matches, mapping = aes(x = x, y = y, group = patient_id), color = "grey", alpha = 0.2) +
    scale_x_continuous(labels = function(x) x / 1000, breaks = seq(-10000, 20000, 1000)) +
    scale_y_continuous(labels = function(x) x / 1000, breaks = seq(-8000, 6000, 1000)) +
    scale_shape_manual(values = c(5, 13)) +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.title = element_blank(),
          text = element_text(size = 8),
          plot.margin = unit(c(5.5,1,5.5,5.5), "cm")) 
  
  return(pl)
}
