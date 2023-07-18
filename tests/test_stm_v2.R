# Libraries and function
library(tidyverse)
source("models/stm-v2.R")
source("utils/plotting.R")

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
  aer = 1
)

# assume one infectious person in the middle of the room at the beginning staying there for one hour
inf <- data.frame(
  time = seq(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 09:00:00"), by = "1 sec"),
  x = 20,
  y = 10
)


# run model assuming no masks (pd = 1)
ct <- stm(c0 = c0, aer = aer, inf = inf, roomDim = roomDim, cellLength = gridCellLength, pd = 1, q = 10, lambda = 0.5, k = 0, seed = 12345)

# plot results
ctDF <- do.call(rbind, lapply(seq_along(ct), function(t) reshape2::melt(ct[[t]]) %>% mutate(t = t) ))

t_names <- c("08:00:00am", "08:01:00am", "08:10:00am", "08:30:00am", "09:00:00", "09:00:10", "09:00:30", "09:01:00", "09:05:00", "09:10:00", "10:00:00", "12:00:00")
t_sel <- c(1, 60, 600, 1800, 3600, 3610, 3630, 3660, 3900, 4200, 7200, 14401)
names(t_names) <- t_sel

quanta_concn_pl <- ctDF %>%
  filter(t %in% t_sel) %>%
  mutate(t = dplyr::recode(t, !!! t_names),
         value = value * (1 / (gridCellLength^2*3))) %>%
  ggplot(aes(x = Var2, y = Var1, fill = value)) +
  facet_wrap(~ t) +
  geom_tile() +
  labs(fill = expression("Concentration (quanta/m"^3*")")) +
  scale_fill_viridis_c(limits = c(0, NA)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,40), labels = function(x) x * 0.25) +
  scale_y_continuous(expand = c(0,0), limits = c(0,20), labels = function(x) x * 0.25) +
  theme_bw2() +
  theme(legend.position = "bottom", legend.key.width = unit(2, "cm"),
        axis.title = element_blank())

quanta_concn_pl
save_plot(quanta_concn_pl, pdf_file = "tests/stm_v2-toy_example.pdf", w = 16, h = 10)
