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
c0 <- matrix(0, nrow = ny, ncol = nx, dimnames = list(as.character(1:ny), as.character(1:nx)))

# time 
time <- seq(as.POSIXct("2023-01-01 08:00:00"), as.POSIXct("2023-01-01 10:00:00"), by = "1 sec")

# assumptions
aer <- 3 / 3600
lambda <- 0 / 3600
deposit <- 0

# assume one infectious person in the middle of the room at the beginning staying there for one hour
inf <- data.frame(
  t = 1:(length(time)/2),
  x = "20",
  y = "10",
  q = 10 / 3600
)


# run model assuming no masks (pd = 1)
ct <- stm(time = time, c0 = c0, aer = aer, inf = inf, vol = prod(roomDim), cellLength = gridCellLength, pd = 0, lambda = lambda, deposit = deposit)

# plot results
ct_to_df <- function(t) {
  data.frame(ct[,,t]) %>% 
    add_rownames() %>% 
    reshape2::melt("rowname") %>%
    rename(x = rowname, y = variable) %>%
    mutate(y = gsub("X", "", y),
           across(c(x, y), as.numeric),
           time = t)
}
t_names <- c("08:01:00", "08:30:00", "09:00:00", "09:01:00", "09:05:00", "10:00:00")
t_sel <- c(60, 1800, 3600, 3660, 3900, 7200)
names(t_names) <- t_sel
ctDF <- do.call(rbind, lapply(t_sel, ct_to_df)) %>%
  mutate(time = dplyr::recode(time, !!! t_names),
         value = value * (1 / (gridCellLength^2*3))) 

quanta_concn_pl <- ctDF %>%
  ggplot(aes(x = y, y = x, fill = value)) +
  facet_wrap(~ time, ncol = 3) +
  geom_tile() +
  labs(fill = expression("Quanta/m"^3), x = "x length in m", y = "y width in m",
       title = "Example: Spatiotemporal model",
       subtitle = "Spatiotemporal quanta concentration with 1 infectious individual (centered, 8-9am)",
       caption = expression("Assumptions: 150m"^3*" room with q = 10 quanta/h and AER = 10 air changes/h")) +
  scale_fill_gradient(low = "red", high = "yellow", limits = c(0, NA), labels = function(x) round(x / (gridCellLength^2*dz), 2)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,40), breaks = seq(0,40,8), labels = function(x) x * .25) +
  scale_y_continuous(expand = c(0,0), limits = c(0,20), breaks = seq(0,20,4), labels = function(x) x * .25) +
  theme_custom() +
  theme(legend.position = "top", legend.key.width = unit(1.1, "cm"), legend.key.height = unit(0.5, "cm"), legend.title = element_text(margin = margin(t = -15)),
        panel.spacing = unit(0.5, "cm"),
        #panel.grid.major = element_line(linewidth = 1, color = "black"),
        #panel.grid.minor = element_line(linewidth = .5, color = "black"),
        #plot.margin = margin(r = 10),
        plot.title = element_text(size = 14),
        plot.title.position = "plot",
        plot.caption = element_text(size = 10),
        text = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 12, margin = margin(b = 10)))

quanta_concn_pl
save_plot(quanta_concn_pl, 
          pdf_file = "tests/stm_v2-toy_example.pdf", 
          eps_file = "tests/stm_v2-toy_example.eps",
          w = 16, h = 12)
