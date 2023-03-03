library(raster)
library(tidyverse)
library(terra)
source("utils/spatial.r")

# Data 
files <- list.files("data-raw/Masi/xovis/Study_Data_Masi_2021/export", full.names = T)
masi <- do.call(rbind, lapply(files, function(x) read.csv(x, header = T) %>% set_names(c("obs_id", "time", "x", "y", "height")))) 

pl <- ggplot() + 
  geom_bin2d(data = masi, mapping = aes(x = x, y = y), binwidth = 100) +
  scale_fill_viridis_c()

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
width <- 24000
breadth <- 8500
rotation <- 9.5
bottom_left_pt <- c(-6700, -9000)
bottom_right_pt <- line_point(bottom_left_pt, width, rotation)
outline <- parallelogram(bottom_left_pt, bottom_right_pt, width, breadth, rotation)

# waiting room
wr_width <- 10700
wr_breadth <- 5700
wr_top_right_pt <- outline[3,]
wr_bottom_right_pt <- line_point(wr_top_right_pt, wr_width, rotation, start = F)
wr <- parallelogram(wr_bottom_right_pt, wr_top_right_pt, wr_width, -wr_breadth, rotation)

# TB room
tb_width <- 5550
tb_breadth <- 6500
tb_bottom_left_pt <- outline[1,]
tb_bottom_right_pt <- line_point(tb_bottom_left_pt, tb_width, rotation)
tb <- parallelogram(tb_bottom_left_pt, tb_bottom_right_pt, tb_width, tb_breadth, rotation)

# passage
pa_width <- width - wr_width
pa_bottom_right_pt <- line_point(tb[4,], pa_width, rotation)
pa <- rbind(tb[4,], pa_bottom_right_pt, wr[1,], outline[4,])

# reception
re_width <- 2000
re_breadth <- euclidean(wr[4,1], pa[2,1], wr[4,2], pa[2,2])
re_bottom_right_pt <- line_point(wr[4, ], re_width, rotation)
re <- parallelogram(wr[4,], re_bottom_right_pt, re_width, re_breadth, rotation)

# entrance 
er_width <- 6500
er_breadth <- breadth - wr_breadth
er_top_right_pt <- line_point(wr[4,], er_width, rotation)
er <- parallelogram(wr[4,], er_top_right_pt, er_width, -er_breadth, rotation)

# seating area in waiting room
sa_wr_width <- wr_width - re_width - 700
sa_wr_breadth <- wr_breadth
sa_wr_bottom_left_pt <- line_point(wr[3,], sa_wr_width, rotation, start = F)
sa_wr <- parallelogram(sa_wr_bottom_left_pt, wr[3,], sa_wr_width, sa_wr_breadth, rotation)

# TB room entrance
# er_tb_width <- 850
# er_tb_breadth <- 1100
# er_tb_bottom_left_pt <- line_point(tb[3, ], er_tb_width, rotation, start = F)
# er_tb <- parallelogram(er_tb_bottom_left_pt, tb[3,], er_tb_width, er_tb_breadth, rotation)

# seating area passage left to TB room
sa_pa_leftTB_width <- 3250
sa_pa_leftTB_breadth <- 1200
sa_pa_leftTB_bottom_left_pt <- line_point(tb[3,], sa_pa_leftTB_width, rotation, start = F)
sa_pa_leftTB <- parallelogram(sa_pa_leftTB_bottom_left_pt, tb[3, ], sa_pa_leftTB_width, sa_pa_leftTB_breadth)

# seating area passage right to TB room
sa_pa_rightTB_width <- 2450
sa_pa_rightTB_breadth <- sa_pa_leftTB_breadth
sa_pa_rightTB_bottom_right_pt <- line_point(tb[3,], sa_pa_rightTB_width, rotation)
sa_pa_rightTB <- parallelogram(tb[3,], sa_pa_rightTB_bottom_right_pt, sa_pa_rightTB_width, sa_pa_rightTB_breadth, rotation)

# reception entrance
er_re_width <- 1500
er_re_breadth <- 4000
er_re_top_left_pt <- c(50, -900)
er_re_top_right_pt <- line_point(er_re_top_left_pt, er_re_width, rotation)
er_re <- parallelogram(er_re_top_right_pt, er_re_top_left_pt, er_re_width, er_re_breadth, rotation, up = F)

# seating area passage left to reception
sa_pa_leftRE_width <- sa_pa_rightTB_width
sa_pa_leftRE_breadth <- sa_pa_leftTB_breadth
sa_pa_leftRE_bottom_left_pt <- line_point(re[4,], sa_pa_leftRE_width, rotation, start = F)
sa_pa_leftRE <- parallelogram(sa_pa_leftRE_bottom_left_pt, re[4,], sa_pa_leftRE_width, sa_pa_leftRE_breadth, rotation)

# entrance care room opposite TB room left
er_cr_oppTBleft_width <- 2100
er_cr_oppTBleft_breadth <- 4000
er_cr_oppTBleft_bottom_left_pt <- c(-1900, 0)
er_cr_oppTBleft_bottom_right_pt <- line_point(er_cr_oppTBleft_bottom_left_pt, er_cr_oppTBleft_width, rotation)
er_cr_oppTBleft <- parallelogram(er_cr_oppTBleft_bottom_left_pt, er_cr_oppTBleft_bottom_right_pt, er_cr_oppTBleft_width, er_cr_oppTBleft_breadth, rotation)

# entrace care room opposite TB right
er_cr_oppTBright_width <- 1400
er_cr_oppTBright_breadth <- er_cr_oppTBleft_breadth
er_cr_oppTBright_bottom_left_pt <- c(1200, 550)
er_cr_oppTBright_bottom_right_pt <- line_point(er_cr_oppTBright_bottom_left_pt, er_cr_oppTBright_width, rotation)
er_cr_oppTBright <- parallelogram(er_cr_oppTBright_bottom_left_pt, er_cr_oppTBright_bottom_right_pt, er_cr_oppTBright_width, er_cr_oppTBright_breadth, rotation)

# exit left
exl_width <- 3000
exl_breadth <- er_cr_oppTBleft_breadth
exl_bottom_left_pt <- c(5600, 1250)
exl_bottom_right_pt <- line_point(exl_bottom_left_pt, exl_width, rotation)
exl <- parallelogram(exl_bottom_left_pt, exl_bottom_right_pt, exl_width, exl_breadth, rotation)

# exit left
exr_width <- 2200
exr_breadth <- er_cr_oppTBleft_breadth
exr_bottom_left_pt <- c(10900, 2000)
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
                cbind(object = 11, part = 11, sa_pa_leftRE, hole = 0),
                cbind(object = 12, part = 12, er_cr_oppTBleft, hole = 0),
                cbind(object = 13, part = 13, er_cr_oppTBright, hole = 0),
                cbind(object = 14, part = 14, exl, hole = 0),
                cbind(object = 15, part = 15, exr, hole = 0))
clinic <- vect(clinic, "polygons")
writeVector(clinic, "data-raw/Masi/building/clinic-vector.gpkg", overwrite = T)
clinic <- sf::st_as_sf(clinic)
clinic <- fortify(clinic)
pl + geom_sf(data = clinic, linewidth = 1, fill = NA) 
