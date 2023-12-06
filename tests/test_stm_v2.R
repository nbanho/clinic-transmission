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

# t_names <- c("08:00:00am", "08:01:00am", "08:10:00am", "08:30:00am", "09:00:00", "09:00:10", "09:00:30", "09:01:00", "09:05:00", "09:10:00", "10:00:00")
# t_sel <- c(1, 60, 600, 1800, 3600, 3610, 3630, 3660, 3900, 4200, 7200, 14401)
t_names <- c("08:01:00", "08:30:00", "09:00:00", "09:01:00", "09:05:00", "10:00:00")
t_sel <- c(60, 1800, 3600, 3660, 3900, 7200)
names(t_names) <- t_sel

quanta_concn_pl <- ctDF %>%
  filter(t %in% t_sel) %>%
  mutate(t = dplyr::recode(t, !!! t_names),
         value = value * (1 / (gridCellLength^2*3))) %>%
  ggplot(aes(x = Var2, y = Var1, fill = value)) +
  facet_wrap(~ t, ncol = 3) +
  geom_tile() +
  labs(fill = expression("Density (10"^-3*" quanta/m"^3*")"), x = "x length in m", y = "y width in m") +
  scale_fill_viridis_c(limits = c(0, NA), labels = function(x) x * 1000) +
  scale_x_continuous(expand = c(0,0), limits = c(0,40), labels = function(x) x * 0.25, breaks = seq(0,40,8)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,20), labels = function(x) x * 0.25, breaks = seq(0,20,4)) +
  theme_custom() +
  theme(legend.position = "bottom", legend.key.width = unit(1.1, "cm"), legend.key.height = unit(0.5, "cm"), legend.title = element_text(margin = margin(t = -15)),
        panel.spacing = unit(0.5, "cm"),
        #panel.grid.major = element_line(linewidth = 1, color = "black"),
        #panel.grid.minor = element_line(linewidth = .5, color = "black"),
        #plot.margin = margin(r = 10),
        text = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 12, margin = margin(b = 10)))

quanta_concn_pl
save_plot(quanta_concn_pl, 
          pdf_file = "tests/stm_v2-toy_example.pdf", 
          eps_file = "tests/stm_v2-toy_example.eps",
          w = 12, h = 9)
