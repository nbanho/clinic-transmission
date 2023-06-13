library(raster)
library(tidyverse)
library(terra)
source("utils/spatial.r")

# Data 
files <- list.files("data-raw/Masi/xovis/", full.names = T)
files <- files[grepl("rds", files)]
masi <- do.call(rbind, lapply(files, readRDS))
masi_rot <- rotate_xy(masi, a = -9)

pl <- ggplot() + 
  geom_bin2d(data = masi_rot, mapping = aes(x = x, y = y), binwidth = 100) +
  scale_fill_stepsn(colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd")[c(1,3,5,7,9)], breaks = c(1,10,100,1000,10000), limits = c(0,10000)) 

pl


parallelogram <- function(x, y, w, b, r, up = T) {
  # vertical movement
  dx <- b/w * (x[2] - y[2])
  dy <- b/w * (y[1] - x[1])
  
  # top right point
  yv <- c(y[1] + dx, y[2] + dy)
  
  # top left point
  xv <- c(x[1] + dx, x[2] + dy)
  
  return(rbind(x, y, yv, xv))
}


line_point <- function(pt, l, r, start = T) {
  dy <- sin(r*pi/180) * l
  dx <- cos(r*pi/180) * l
  if (start) {
    pt_new <- c(pt[1] + dx, pt[2] + dy)
  } else {
    pt_new <- c(pt[1] - dx, pt[2] - dy)
  }
  return(pt_new)
}

# outline
width <- 23000
breadth <- 8250
rotation <- 0
bottom_left_pt <- c(-15800, -7500)
bottom_right_pt <- line_point(bottom_left_pt, width, rotation)
outline <- parallelogram(bottom_left_pt, bottom_right_pt, width, breadth, rotation)

# waiting room
wr_width <- 9750 + 800
wr_breadth <- 5700
wr_top_left_pt <- outline[4,]
wr_top_right_pt <- line_point(wr_top_left_pt, wr_width, rotation)
wr <- parallelogram(wr_top_left_pt, wr_top_right_pt, wr_width, -wr_breadth, rotation)

# sputum room
sp_width <- 4750
sp_breadth <- breadth - wr_breadth
sp_bottom_right_pt <- outline[2,]
sp_bottom_left_pt <- line_point(sp_bottom_right_pt, sp_width, rotation, start = F)
sp <- parallelogram(sp_bottom_left_pt, sp_bottom_right_pt, sp_width, sp_breadth, rotation)

# TB room
tb_width <- sp_width
tb_breadth <- 3500
tb_bottom_left_pt <- sp[4,]
tb_bottom_right_pt <- line_point(tb_bottom_left_pt, tb_width, rotation)
tb <- parallelogram(tb_bottom_left_pt, tb_bottom_right_pt, tb_width, tb_breadth, rotation)

# passage
pa_width <- width - wr_width
pa_breadth <- breadth - tb_breadth - sp_breadth
pa_top_left_pt <- wr[2,]
pa_top_right_pt <- line_point(pa_top_left_pt, pa_width, rotation)
pa <- parallelogram(pa_top_left_pt, pa_top_right_pt, pa_width, -pa_breadth, rotation)

# reception
re_width <- 1500
re_breadth <- euclidean(wr[3,1], pa[4,1], wr[3,2], pa[4,2]) + 500
re_bottom_left_pt <- line_point(wr[3, ], re_width, rotation, start = F)
re <- parallelogram(re_bottom_left_pt, wr[3,], re_width, re_breadth, rotation)

# entrance 
er_width <- 6500
er_breadth <- breadth - wr_breadth
er_top_left_pt <- line_point(wr[3,], er_width, rotation, start = F)
er <- parallelogram(er_top_left_pt, wr[3,], er_width, -er_breadth, rotation)

# seating area in waiting room
sa_wr_width <- wr_width - re_width - 900
sa_wr_breadth <- wr_breadth
sa_wr_bottom_right_pt <- line_point(wr[4,], sa_wr_width, rotation)
sa_wr <- parallelogram(wr[4,], sa_wr_bottom_right_pt, sa_wr_width, sa_wr_breadth, rotation)

# TB room entrance
# er_tb_width <- 850
# er_tb_breadth <- 1100
# er_tb_bottom_left_pt <- line_point(tb[3, ], er_tb_width, rotation, start = F)
# er_tb <- parallelogram(er_tb_bottom_left_pt, tb[3,], er_tb_width, er_tb_breadth, rotation)

# seating area passage left to TB room
sa_pa_leftTB_width <- 2375
sa_pa_leftTB_breadth <- 1500
sa_pa_leftTB_bottom_left_pt <- line_point(tb[4,], sa_pa_leftTB_width, rotation, start = F)
sa_pa_leftTB <- parallelogram(tb[4, ], sa_pa_leftTB_bottom_left_pt, sa_pa_leftTB_width, -sa_pa_leftTB_breadth)

# seating area passage right to TB room
sa_pa_rightTB_width <- 3000
sa_pa_rightTB_bottom_right_pt <- line_point(tb[4,], sa_pa_rightTB_width, rotation)
sa_pa_rightTB <- parallelogram(sa_pa_rightTB_bottom_right_pt, tb[4,], sa_pa_rightTB_width, -sa_pa_leftTB_breadth, rotation)

# reception entrance
er_re_width <- 1500
er_re_breadth <- 4000
er_re_top_right_pt <- c(sa_pa_leftTB[2,1],sa_pa_leftTB[2,2] + 750)
er_re_top_left_pt <- line_point(er_re_top_right_pt, er_re_width, rotation, start = F)
er_re <- parallelogram(er_re_top_left_pt, er_re_top_right_pt, er_re_width, -er_re_breadth, rotation)

# seating area passage right to reception
sa_pa_rightRE_width <- 2700
sa_pa_rightRE_bottom_left_pt <- pa[4,]
sa_pa_rightRE_bottom_right_pt <- line_point(sa_pa_rightRE_bottom_left_pt, sa_pa_rightRE_width, rotation)
sa_pa_rightRE <- parallelogram(sa_pa_rightRE_bottom_left_pt, sa_pa_rightRE_bottom_right_pt, sa_pa_rightRE_width, sa_pa_leftTB_breadth, rotation)

# entrance care room opposite TB room left
y_top_ex <- sa_pa_rightRE[4,2] + 350
er_cr_oppTBleft_width <- 1400
er_cr_oppTBleft_breadth <- 4000
er_cr_oppTBleft_bottom_left_pt <- c(sa_pa_rightRE[3,1], y_top_ex)
er_cr_oppTBleft_bottom_right_pt <- line_point(er_cr_oppTBleft_bottom_left_pt, er_cr_oppTBleft_width, rotation)
er_cr_oppTBleft <- parallelogram(er_cr_oppTBleft_bottom_left_pt, er_cr_oppTBleft_bottom_right_pt, er_cr_oppTBleft_width, er_cr_oppTBleft_breadth, rotation)

# entrace care room opposite TB right
er_cr_oppTBright_width <- 1600
er_cr_oppTBright_breadth <- er_cr_oppTBleft_breadth
er_cr_oppTBright_bottom_left_pt <- c(sa_pa_leftTB[3,1], y_top_ex)
er_cr_oppTBright_bottom_right_pt <- line_point(er_cr_oppTBright_bottom_left_pt, er_cr_oppTBright_width, rotation)
er_cr_oppTBright <- parallelogram(er_cr_oppTBright_bottom_left_pt, er_cr_oppTBright_bottom_right_pt, er_cr_oppTBright_width, er_cr_oppTBright_breadth, rotation)

# exit left
exl_width <- 2000
exl_breadth <- er_cr_oppTBleft_breadth
exl_bottom_left_pt <- c(-13200, y_top_ex)
exl_bottom_right_pt <- line_point(exl_bottom_left_pt, exl_width, rotation)
exl <- parallelogram(exl_bottom_left_pt, exl_bottom_right_pt, exl_width, exl_breadth, rotation)

# exit right
exr_width <- 2500
exr_breadth <- er_cr_oppTBleft_breadth
exr_bottom_left_pt <- c(-8500, y_top_ex)
exr_bottom_right_pt <- line_point(exr_bottom_left_pt, exr_width, rotation)
exr <- parallelogram(exr_bottom_left_pt, exr_bottom_right_pt, exr_width, exr_breadth, rotation)

# combine to form clinic
clinic <- rbind(cbind(object = 1, part = 1, outline, hole = 0),
                cbind(object = 2, part = 2, wr, hole = 0),
                cbind(object = 3, part = 3, tb, hole = 0),
                cbind(object = 4, part = 4, pa, hole = 0),
                cbind(object = 5, part = 5, re, hole = 0),
                cbind(object = 6, part = 6, er, hole = 0),
                cbind(object = 7, part = 7, sa_wr, hole = 0),
                cbind(object = 8, part = 8, er_re, hole = 0),
                cbind(object = 9, part = 9, sa_pa_leftTB, hole = 0),
                cbind(object = 10, part = 10, sa_pa_rightTB, hole = 0),
                cbind(object = 11, part = 11, sa_pa_rightRE, hole = 0),
                cbind(object = 12, part = 12, er_cr_oppTBleft, hole = 0),
                cbind(object = 13, part = 13, er_cr_oppTBright, hole = 0),
                cbind(object = 14, part = 14, exl, hole = 0),
                cbind(object = 15, part = 15, exr, hole = 0),
                cbind(object = 16, part = 16, sp, hole = 0))
clinic <- vect(clinic, "polygons")
writeVector(clinic, "data-raw/Masi/building/clinic-vector.gpkg", overwrite = T)
clinic <- sf::st_as_sf(clinic)
clinic <- fortify(clinic)
pl + geom_sf(data = clinic, linewidth = 1, fill = NA) 
