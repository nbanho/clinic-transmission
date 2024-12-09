library(tidyverse)
library(patchwork)
source("utils/plotting.R")

a <- readRDS("results/data/no-people-spatial.rds")
b <- readRDS("results/data/no-people-over-time.rds")
c <- readRDS("results/data/time-in-clinic.rds")
d <- readRDS("results/data/air-changes-per-hour.rds")

pl <- a + b + c + d + plot_layout(nrow = 2, heights = c(8, 6))

save_plot(
  pl,
  pdf_file = "results/data/data-summary.png",
  w = 16, h = 14
)
