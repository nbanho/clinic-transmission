#' Room:
#' We assume a room with length = 10m, breadth = 6m, and height = 3m.
#' We divide the room into a grid with cell length = 0.1m, i.e. 100 x 60 matrix.
#' 
#' Time:
#' We run our simulation for 1h with a time step of 1s.
#' 
#' Quanta emission:
#' There are two quanta emission sources:
#' 1. One in the center in the first five minutes.
#' 2. One near the top-right edge between minute 30 and 35.
#' 
#' The quanta emission rate is 4 quanta/hour.
#' We assume an exponential decrease with distance to source of emission.
#' 
#' Quanta removal:
#' Quanta is removed only through air exchange.
#' We assume a constant air exchange rate of 3 air changes per hour.
#' 
#' Quanta diffusion:
#' We assume radial diffusion at a constant rate of 0.05m2/min. 
#' 


#### Libraries ####
library(tidyverse)
source("utils/trans_risk.r")
source("utils/spatial.R")


#### Setup ####

# room
Lx <- 10
Ly <- 6
Lz <- 3
V <- 10 * 6 * 3
Vxy <- 1
dx <- dy <- sqrt(Vxy/Lz)
nx <- Lx / dx
ny <- Ly / dy

# initial quanta concentration
C0 <- matrix(0, nrow = ny, ncol = nx)

# time
TT <- 60 * 60
dt <- 1

# quanta emission rate
q <- 4 / 3600

# weights to source
W1 <- matrix(NA, nrow = ny, ncol = nx)
for (i in 1:ny) {
  for (j in 1:nx) {
    W1[i,j] <- euclidean(floor(ny/2), i, floor(nx/2), j) * dx
  }
}
W1 <- apply(W1, c(1,2), dIc, mu = 1)
W1 <- W1 / sum(W1)

W2 <- matrix(NA, nrow = ny, ncol = nx)
for (i in 1:ny) {
  for (j in 1:nx) {
    W2[i,j] <- euclidean(ny, i, nx, j) * dx
  }
}

# weights
W2 <- apply(W2, c(1,2), dIc, mu = 1)
W2 <- W2 / sum(W1)

# quanta removal rate
aer <- 3 / 3600

# quanta diffusion constant
D <- 0.05 


#### Spatiotemporal quanta concentration ####
C <- C0
CL <- list()

for (t in 1:TT) {
  # quanta emission
  if (t <= 5 * 60) {
    C <- emission(C, W1, q, dt)
  } else if (between(t, 30 * 60, 35 * 60)) {
    C <- emission(C, W1, q, dt)
  } 
  
  # quanta diffusion
  dC <- ReacTran::tran.2D(C, C.x.up = rep(0, nx), C.x.down = rep(0, nx),
                          C.y.up = rep(0, ny), C.y.down = rep(0, ny),
                          flux.x.up = rep(0, nx), flux.x.down = rep(0, nx),
                          flux.y.up = rep(0, ny), flux.y.down = rep(0, ny),
                          D.x = D, D.y = D, dx = dx, dy = dy)$dC
  C <- C + dC
  
  # quanta removal
  C <- removal(C, aer, dt = dt)
  
  CL[[t]] <- C
}


#### Plot ####

plotC <- function(C) {
  reshape2::melt(C) %>%
    ggplot(aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c() 
}

DFC <- do.call(rbind, lapply(seq_along(CL), function(t) reshape2::melt(CL[[t]]) %>% mutate(t = t) ))

DFC %>%
  filter(t %in% seq(1, 3600, 30)) %>%
  ggplot(aes(Var1, Var2, fill = value)) +
  facet_wrap(~ t) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, NA)) +
  theme(legend.position = "top", legend.key.width = unit(2, "cm"))
