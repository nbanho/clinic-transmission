# Libraries and function
library(tidyverse)
source("models/stm-v2.R")
source("utils/plotting.R")

# Setup room
dz <- 3
gridCellLength <- 0.25
nx <- 40
ny <- 40
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
aer <- 1 / 3600
lambda <- 0
deposit <- 0

# one infectious person in room center for 1h
inf <- data.frame(
  t = 1:(length(time) / 2),
  x = "20",
  y = "20",
  q = 2 / 3600
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
ct_df <- ct %>%
  reshape2::melt() %>%
  set_names(c("y", "x", "t", "q")) %>%
  mutate(xy = paste0(x, ",", y))

quanta_concn_pl <- ct_df %>%
  filter(t %in% c(1, 1800, 3600, 3660, 4200, 7200)) %>%
  mutate(t = case_when(
    t == 1 ~ "08:00",
    t == 1800 ~ "08:30",
    t == 3600 ~ "09:00",
    t == 3660 ~ "09:01",
    t == 4200 ~ "09:10",
    t == 7200 ~ "10:00"
  )) %>%
  ggplot(aes(x = y, y = x, fill = q)) +
  facet_wrap(~t, ncol = 3) +
  geom_tile() +
  labs(
    fill = expression(Quanta %.% 10^-3 * " / 0.1875m"^3),
    x = "x length in m", y = "y width in m", title = "a"
  ) +
  scale_fill_gradientn(
    colours = RColorBrewer::brewer.pal(n = 9, name = "YlOrRd"),
    labels = function(x) round(1e3 * x, 1)
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, 40),
    breaks = seq(0, 40, 8),
    labels = function(x) x * .25
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 40),
    breaks = seq(0, 40, 8),
    labels = function(x) x * .25
  ) +
  theme_custom() +
  theme(
    legend.position = "bottom",
    legend.key.width = unit(1.1, "cm"),
    legend.key.height = unit(0.5, "cm"),
    legend.title = element_text(margin = margin(t = -15)),
    panel.spacing = unit(0.5, "cm")
  )

save_plot(quanta_concn_pl,
  pdf_file = "tests/stm_v2-toy_example.png",
  w = 16, h = 13
)

# plot
time_pl <- ct_df %>%
  ggplot(aes(x = t, y = q, group = xy)) +
  geom_line(color = "grey70") +
  geom_vline(xintercept = 3600, linetype = "dashed", color = "red") +
  scale_x_continuous(
    breaks = c(1, 1800, 3600, 5400, 7200),
    labels = c("08:00", "08:30", "09:00", "09:30", "10:00")
  ) +
  scale_y_continuous(labels = function(x) round(1e3 * x, 1)) +
  labs(
    y = expression(Quanta %.% 10^-3 * " / 0.1875m"^3),
    title = "b"
  ) +
  coord_cartesian(expand = FALSE) +
  theme_custom() +
  theme(
    plot.margin = margin(r = 11),
    axis.title.x = element_blank()
  )

save_plot(time_pl,
  pdf_file = "tests/stm_v2-toy_example-B.png",
  w = 16, h = 8
)
