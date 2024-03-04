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
c0 <- matrix(
  0,
  nrow = ny,
  ncol = nx,
  dimnames = list(as.character(1:ny), as.character(1:nx))
)

# time
time <- seq(
  as.POSIXct("2023-01-01 08:00:00"),
  as.POSIXct("2023-01-01 10:00:00"),
  by = "1 sec"
)

# assumptions
aer <- 3 / 3600
lambda <- 0 / 3600
deposit <- 0

# one infectious person in room center for 1h
inf <- data.frame(
  t = 1:(length(time) / 2),
  x = "20",
  y = "10",
  q = 10 / 3600
)


# run model assuming no masks (pd = 1)
ct <- stm(
  time = time,
  c0 = c0,
  aer = aer,
  inf = inf,
  vol = prod(roomDim),
  cellLength = gridCellLength,
  pd = 0,
  lambda = lambda,
  deposit = deposit
)

# plot results
ct_to_df <- function(t) {
  data.frame(ct[, , t]) %>%
    add_rownames() %>%
    reshape2::melt("rowname") %>%
    rename(x = rowname, y = variable) %>%
    mutate(
      y = gsub("X", "", y),
      across(c(x, y), as.numeric),
      time = t
    )
}
t_names <- c(
  "08:01:00",
  "08:30:00",
  "09:00:00",
  "09:01:00",
  "09:05:00",
  "10:00:00"
)
t_sel <- c(60, 1800, 3600, 3660, 3900, 7200)
names(t_names) <- t_sel
ctDF <- do.call(rbind, lapply(t_sel, ct_to_df)) %>%
  mutate(
    time = dplyr::recode(time, !!!t_names),
    value = value * (1 / (gridCellLength^2 * 3))
  )

quanta_concn_pl <- ctDF %>%
  ggplot(aes(x = y, y = x, fill = value)) +
  facet_wrap(~time, ncol = 3) +
  geom_tile() +
  labs(
    fill = expression("Quanta/m"^3 * " x 10e"^-3),
    x = "x length in m", y = "y width in m"
  ) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
    labels = function(x) round(1e3 * x / (gridCellLength^2 * dz), 1)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 40),
    breaks = seq(0, 40, 8),
    labels = function(x) x * .25
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 20),
    breaks = seq(0, 20, 4),
    labels = function(x) x * .25
  ) +
  theme_custom() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(margin = margin(t = -15)),
    panel.spacing = unit(0.5, "cm"),
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

quanta_concn_pl
save_plot(quanta_concn_pl,
  pdf_file = "tests/stm_v2-toy_example.png",
  w = 16, h = 10
)
