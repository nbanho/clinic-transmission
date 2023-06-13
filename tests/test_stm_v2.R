# Libraries and function
library(tidyverse)
source("models/stm-v2.R")

# Setup room
dz <- 3
gridCellLength <- 0.25
nx <- 40
ny <- 20
dy <- gridCellLength * ny
dx <- gridCellLength * nx
roomDim <- c(dx, dy, dz)
c0 <- matrix(0, nrow = ny, ncol = nx)

# assume constant air exchange rate
aer <- data.frame(
  time = seq(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 12:00:00"), by = "1 sec"),
  aer = 3
)

# assume one infectious person in the middle of the room at the beginning staying there for one hour
inf <- data.frame(
  time = seq(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 09:00:00"), by = "1 sec"),
  x = 20,
  y = 10
)


# run model assuming no masks (pd = 1)
ct <- stm(c0, aer, inf, roomDim, gridCellLength)

# plot results
ctDF <- do.call(rbind, lapply(seq_along(ct), function(t) reshape2::melt(ct[[t]]) %>% mutate(t = t) ))

ctDF %>%
  filter(t %in% c(1, 60, 600, 1800, 3600, 3610, 3630, 3660, 3900, 4200, 7200, 14401)) %>%
  ggplot(aes(x = Var2, y = Var1, fill = value)) +
  facet_wrap(~ t) +
  geom_tile() +
  scale_fill_viridis_c(limits = c(0, NA)) +
  theme(legend.position = "top", legend.key.width = unit(2, "cm"))
