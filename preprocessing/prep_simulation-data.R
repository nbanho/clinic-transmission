#### Libraries ####

library(tidyverse)
library(parallel)
library(lubridate)

source("utils/spatial.r")
source("utils/trans_risk.r")


#### Settings ####

# number of simulations
n <- 1e5

# cell grid size in px
cellSize <- 250

# Seed
seed <- 12345

# building
source("preprocessing/prep_building-rasterize.R")

# who is assumed to have tb
#' du: diagnosed and undiagnosed in proportion
#' ds: diagnosed and suspected
#' pu: sample among all clinical attendees in proportion to population prevalence
who_is_tb <- c("du", "ds", "pu")

# path to store cellSize dependend tracking data
track_file_path <- paste0("simulations/", "/", "tracking-df-", cellSize, "px-")

# dates
dates <- paste0("2021-", c(10, 10, 10, 11, 11), "-", c("13", "15", "25", "04", "05"))
daytimes <- c("both", "morning", "both", "both", "both")

# number of cores
ncores <- 6


#### Track data ####

for (i in 1:length(dates)) {
  # date
  d <- dates[i]

  # daytime
  dytm <- daytimes[i]
  t_start <- as.POSIXct(paste(d, "08:00:00"))
  t_end <- as.POSIXct(paste(d, "16:00:00"))
  tt <- seq(t_start, t_end, by = "1 sec")
  midday <- as.POSIXct(paste(d, "12:00:00"))
  midday_idx <- which(tt == midday)

  # existing track file
  track_file <- paste0(track_file_path, d, ".rds")

  # tracking
  if (!file.exists(track_file)) {
    # load data
    track <- readRDS(paste0("data-clean/patient-tracking/matched-clinical/", d, ".rds")) %>%
      mutate(activity = ifelse(activity == "waiting", 1, 2))

    # filter daytime
    if (dytm == "both") {
      track <- filter(track, between(time, t_start, t_end))
    } else if (dytm == "morning") {
      track <- filter(track, between(time, t_start, midday))
    } else if (dytm == "afternoon") {
      track <- filter(track, between(time, midday, t_end))
    } else {
      stop("Unknown daytime")
    }

    # determine cell
    track$cell_id <- NA
    track$cell_id[track$is_waitingroom] <- mcmapply(find_raster, track$x[track$is_waitingroom], track$y[track$is_waitingroom],
      MoreArgs = list(cellCoordsXY = as.matrix(wrCoord[, c("x", "y")])), mc.cores = ncores
    )
    track$cell_id[track$is_passage] <- mcmapply(find_raster, track$x[track$is_passage], track$y[track$is_passage],
      MoreArgs = list(cellCoordsXY = as.matrix(cdCoord[, c("x", "y")])), mc.cores = ncores
    )
    track$cell_id[track$is_tbroom] <- mcmapply(find_raster, track$x[track$is_tbroom], track$y[track$is_tbroom],
      MoreArgs = list(cellCoordsXY = as.matrix(tbCoord[, c("x", "y")])), mc.cores = ncores
    )
    track$cell_id <- unlist(track$cell_id)
    track$cell_x[track$is_waitingroom] <- as.character(wrCoord$x[track$cell_id[track$is_waitingroom]])
    track$cell_x[track$is_passage] <- as.character(cdCoord$x[track$cell_id[track$is_passage]])
    track$cell_x[track$is_tbroom] <- as.character(tbCoord$x[track$cell_id[track$is_tbroom]])
    track$cell_y[track$is_waitingroom] <- as.character(wrCoord$y[track$cell_id[track$is_waitingroom]])
    track$cell_y[track$is_passage] <- as.character(cdCoord$y[track$cell_id[track$is_passage]])
    track$cell_y[track$is_tbroom] <- as.character(tbCoord$y[track$cell_id[track$is_tbroom]])

    # determine time
    track$t <- as.numeric(difftime(track$time, t_start, units = "sec"))

    # cell height
    track$cell_height <- ifelse(track$is_waitingroom, dimWR[3],
      ifelse(track$is_passage, dimCD[3], dimTB[3])
    )
    track$cell_height <- 1 / convert_dist(1) * track$cell_height

    # inhalation rate depending on activity in m3/s
    track$inhal_rate <- ifelse(track$activity == 1,
      .5 * IR("sitting", "male") + .5 * IR("sitting", "female"),
      .5 * IR("walking", "male") + .5 * IR("walking", "female")
    )
    track$inhal_rate <- track$inhal_rate / 3600

    # save
    saveRDS(track, file = track_file)
  }
}



#### Assumptions ####

for (d in dates) {
  # track file
  track_file <- paste0(track_file_path, d, ".rds")

  if (file.exists(track_file)) {
    # load data
    track <- readRDS(track_file)
  } else {
    stop("Track file not available.")
  }

  # set seed
  set.seed(seed)

  # parameter file
  param_file <- paste0("simulations/", "/", "sim-param-", d, ".rds")

  if (file.exists(param_file)) {
    sim_param <- readRDS(param_file)
  } else {
    # quanta generation rate: bacillary load
    cb <- bact_load.mtb(n)
    q_wait <- sapply(cb, rq, activity = "waiting", breathing_weight = .8)
    q_walk <- sapply(cb, rq, activity = "walking", breathing_weight = .8)

    # mask wearing reduction
    q_masking_red <- rrmask(n)

    # viral inactivation rate
    lambda <- rlambda(n)

    # combine into tibble
    sim_param <- tibble(
      sim = 1:n,
      q_wait = q_wait,
      q_walk = q_walk,
      mask_red = q_masking_red,
      viral_inact = lambda
    )
  }

  for (w in who_is_tb) {
    # split diagnosed and undiagnosed TB patients
    track_undiag <- track %>%
      dplyr::filter(tracking_end != "Possible HCW") # always consider HCW as non-infectious

    if (w == "du") {
      # undiagnosed
      track_undiag <- dplyr::filter(track_undiag, !is_infectious | is.na(is_infectious))
      pid_undiag <- unique(track_undiag$patient_id)

      # number of undiagnosed TB patients
      n_undiag <- rundiag(n)

      # IDs of undiagnosed TB patients
      undiag_TB <- lapply(n_undiag, sample, x = pid_undiag, replace = F)

      # add to tibble
      sim_param <- sim_param %>%
        mutate(undiagTB_du = undiag_TB)
    } else if (w == "ds") {
      # no undiagnosed/unsuspected TB patients
      track_undiag <- dplyr::filter(track, patient_id == -1)
      pid_undiag <- -1

      # no undiagnosed TB patients
      undiag_TB <- as.list(replicate(n, as.integer(-1)))

      sim_param <- sim_param %>%
        mutate(undiagTB_ds = undiag_TB)
    } else if (w == "pu") {
      # only undiagnosed TB patients
      # track_undiag <- track_undiag
      pid_undiag <- unique(track_undiag$patient_id)

      # number of undiagnosed TB patients
      p_undiag <- rundiag(n, empirical = F)

      # IDs of undiagnosed TB patients
      undiag_TB <- lapply(p_undiag, function(p) pid_undiag[rbinom(length(pid_undiag), size = 1, prob = p) == 1])

      sim_param <- sim_param %>%
        mutate(undiagTB_pu = undiag_TB)
    }

    # save tibble
    saveRDS(sim_param, file = param_file)
  }
}
