require(tidyverse)
require(reshape2)
require(lubridate)
require(abind)
require(ReacTran)
require(parallel)

#' Simulate transmission risk in clinic
#'
#' Compute the spatiotemporal quanta concentration
#' and the individual risk of infection.
#'
#' @param sim simulation number (int)
#' @param pid_diag patient id of diagnosed TB cases (char)
#' @param q_wait_rate quanta generation rate per hour while waiting (float)
#' @param q_walk_rate quanta generation rate per hour while walking (float)
#' @param mask_red_rate mask reduction rate (float)
#' @param viral_inact_rate viral inactivation rate per hour (float)
#' @param grav_settl_rate gravitational settling rate per hour (float)
#' @param sim_path_to_file filepath to store sim results (str)
#' @param room_mat_list room matrices (named list)
#' @param room_volumes room volumes (named vector)
#' @param cell_length grid cell size in m (float)
#' @param t_start start time of simulation (POSIXct)
#' @param t_midday daytime split of simulation (POSIXct)
#' @param tt_morn vector of morning times (POSIXct)
#' @param tt_noon vector of afternoon times (POSIXct)
#' @param track_diag trackings of diagnosed TB (data.frame)
#' @param track_undiag trackings of undiagnosed TB (data.frame)
#' @param aer_df aer exchange rates by daytime and room (data.frame),
#' @param prop_cell_dist: number of cells quanta initially propagates (int)
#' @param mod: temp or spattemp (string)
#' @param save_quanta: whether to save quanta concentration (boolean)
#'

simulate_risk <- function(
    sim,
    pid_diag,
    q_wait_rate,
    q_walk_rate,
    mask_red_rate,
    viral_inact_rate,
    grav_settl_rate,
    sim_path_to_file,
    room_mat_list,
    room_volumes,
    cell_length,
    t_start,
    t_midday,
    tt_morn,
    tt_noon,
    track_diag,
    track_undiag,
    aer_df,
    prop_cell_dist,
    mod,
    save_quanta) {
  # simulate
  print(sprintf("Sim %i started", sim))

  # TB patient tracks
  track_tb_all <- rbind(
    track_diag,
    filter(
      track_undiag,
      patient_id %in% unlist(pid_diag)
    )
  )

  # susceptible patient tracks
  track_susc_all <- rbind(
    filter(track, tracking_end == "Possible HCW"),
    filter(
      track,
      tracking_end != "Possible HCW",
      !(patient_id %in% unlist(pid_diag))
    )
  )

  # initialize
  susc_quanta_exp <- list()

  for (room in c("Waiting room", "Corridor", "TB room")) {
    c0 <- room_mat_list[[room]]
    room_vol <- room_volumes[room]
    if (room == "Waiting room") {
      track_tb <- filter(track_tb_all, is_waitingroom)
      track_susc <- filter(track_susc_all, is_waitingroom)
    } else if (room == "Corridor") {
      track_tb <- filter(track_tb_all, is_passage)
      track_susc <- filter(track_susc_all, is_passage)
    } else if (room == "TB room") {
      track_tb <- filter(track_tb_all, is_tbroom)
      track_susc <- filter(track_susc_all, is_tbroom)
    }

    # activity-level specific quanta generation rate w/o masking
    track_tb <- track_tb %>%
      dplyr::select(time, cell_x, cell_y, activity) %>%
      rename(x = cell_x, y = cell_y) %>%
      mutate(
        q = ifelse(activity == 1,
          q_wait_rate, q_walk_rate
        ),
        q = q * mask_red_rate / 3600
      )

    # quanta conc in the morning
    track_tb_morn <- filter(track_tb, time <= t_midday)
    track_tb_morn$t <- as.numeric(difftime(
      track_tb_morn$time, t_start,
      units = "sec"
    ))

    if (mod == "spattemp") {
      ct_morn <- stm(
        time = tt_morn,
        c0 = c0,
        inf = dplyr::select(track_tb_morn, -time, -activity),
        cellLength = cell_length,
        pd = prop_cell_dist,
        vol = room_vol,
        aer = aer_df$aer[
          aer_df$room == room & aer_df$daytime == "Morning"
        ] / 3600,
        lambda = viral_inact_rate / 3600,
        deposit = grav_settl_rate / 3600
      )
    } else if (mod == "temp") {
      ct_morn <- tm(
        time = tt_morn,
        c0 = c0,
        inf = dplyr::select(track_tb_morn, -time, -activity),
        aer = aer_df$aer[
          aer_df$room == room & aer_df$daytime == "Morning"
        ] / 3600,
        lambda = viral_inact_rate / 3600,
        deposit = grav_settl_rate / 3600
      )
    } else {
      stop("Model does not exist")
    }

    if (save_quanta) {
      ct_morn_mean <- apply(ct_morn, c(1, 2), mean) %>%
        reshape2::melt() %>%
        rename(
          y = Var1,
          x = Var2
        ) %>%
        mutate(
          across(c(x, y), as.numeric),
          sim = sim
        ) %>%
        dplyr::select(sim, x, y, value)
      write.table(
        x = ct_morn_mean,
        file = paste0(sim_path_to_file, "quanta_conc-morning-", room, ".txt"),
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
    }

    # quanta conc in the afternoon
    if (max(track_susc$time) > t_midday) {
      c0_noon <- ct_morn[, , dim(ct_morn)[3]]
    } else {
      c0_noon <- c0
    }
    track_tb_noon <- filter(track_tb, time > t_midday)
    track_tb_noon$t <- as.numeric(difftime(
      track_tb_noon$time,
      t_midday + lubridate::seconds(1),
      units = "sec"
    ))


    if (mod == "spattemp") {
      ct_noon <- stm(
        time = tt_noon,
        c0 = c0_noon,
        inf = dplyr::select(track_tb_noon, -time, -activity),
        cellLength = cell_length,
        pd = prop_cell_dist,
        vol = room_vol,
        aer = aer_df$aer[
          aer_df$room == room & aer_df$daytime == "Afternoon"
        ] / 3600,
        lambda = viral_inact_rate / 3600,
        deposit = grav_settl_rate / 3600
      )
    } else if (mod == "temp") {
      ct_noon <- tm(
        time = tt_noon,
        c0 = c0_noon,
        inf = dplyr::select(track_tb_noon, -time, -activity),
        aer = aer_df$aer[
          aer_df$room == room & aer_df$daytime == "Afternoon"
        ] / 3600,
        lambda = viral_inact_rate / 3600,
        deposit = grav_settl_rate / 3600
      )
    } else {
      stop("Model does not exist")
    }

    if (save_quanta) {
      ct_noon_mean <- apply(ct_noon, c(1, 2), mean) %>%
        reshape2::melt() %>%
        rename(
          y = Var1,
          x = Var2
        ) %>%
        mutate(
          across(c(x, y), as.numeric),
          sim = sim
        ) %>%
        dplyr::select(sim, x, y, value)
      write.table(
        x = ct_noon_mean,
        file = paste0(sim_path_to_file, "quanta_conc-afternoon-", room, ".txt"),
        append = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
    }

    # compute quanta conc exposure by susc
    ct <- abind::abind(ct_morn, ct_noon, along = 3)
    if (nrow(track_susc) > 0) {
      track_susc$quanta <- mapply(
        function(t, x, y) {
          if (t == 0) {
            0
          } else {
            ct[y, x, t]
          }
        },
        t = track_susc$t,
        x = as.character(track_susc$cell_x),
        y = as.character(track_susc$cell_y)
      )
      track_susc <- track_susc %>%
        mutate(
          quanta_conc =
            inhal_rate * quanta /
              (cell_length^2 * (cell_height / 1e3))
        ) %>%
        dplyr::select(patient_id, quanta_conc)
    } else {
      track_susc <- track_susc %>%
        mutate(quanta_conc = numeric()) %>%
        dplyr::select(patient_id, quanta_conc)
    }

    susc_quanta_exp[[room]] <- track_susc
  }

  # compute transmission risk per susceptible
  trans_risk <- do.call(rbind, susc_quanta_exp) %>%
    group_by(patient_id) %>%
    summarize(quanta_conc = sum(quanta_conc)) %>%
    ungroup() %>%
    mutate(
      P = 1 - exp(-quanta_conc),
      sim = sim
    ) %>%
    dplyr::select(sim, patient_id, P)

  write.table(
    x = trans_risk,
    file = paste0(sim_path_to_file, "trans-risk", ".txt"),
    append = TRUE,
    row.names = FALSE,
    col.names = FALSE
  )

  return(sprintf("Sim %i completed", sim))
}

#' Run simulation from command line
#'
#' Setup simulation input data and parameters and run
#' nsim simulations, continuing from previous file.
#'
#' @param mth: month (character)
#' @param dy: day (character)
#' @param scenario: name of simulation (character)
#' @param cellsize: grid cell size in px (integer)
#' @param nsim: number of simulations to run (integer)
#' @param ncores: number of CPU cores to use (integer)
#'

eval(parse(text = paste(commandArgs(trailingOnly = TRUE), collapse = ";")))

# create scenario-specific directory to store results
scen_dir <- paste0("simulations/", scenario, "/")
if (!dir.exists(scen_dir)) {
  dir.create(scen_dir)
}
sim_path_to_file <- paste0(scen_dir, mth, "-", dy, "/")
if (!dir.exists(sim_path_to_file)) {
  dir.create(sim_path_to_file)
}

# continue existing simulation
if (file.exists(paste0(sim_path_to_file, "trans-risk.txt"))) {
  cont_n <- max(read.table(paste0(sim_path_to_file, "trans-risk.txt"))[, 1])
} else {
  cont_n <- 1
}
simulations <- cont_n:nsim

# split day into morning and afternoon
date <- paste0("2021-", mth, "-", dy)
t_start <- as.POSIXct(paste(date, "08:00:00"), tz = "CET")
t_midday <- as.POSIXct(paste(date, "12:00:00"), tz = "CET")
t_end <- as.POSIXct(paste(date, "16:00:00"), tz = "CET")
tt <- seq.POSIXt(t_start, t_end, by = "1 sec")
tt_morn <- seq.POSIXt(t_start, t_midday, by = "1 sec")
tt_noon <- seq.POSIXt(t_midday + 1, t_end, by = "1 sec")

# building data
building_file <- "data-clean/building/clinic-room-data.rds"
if (file.exists(building_file)) {
  building <- readRDS(building_file)
} else {
  stop("Building data file is missing.")
}

# tracking data
track_file <- paste0(
  "simulations/", "tracking-df-",
  cellsize, "px-",
  2021, "-", mth, "-", dy,
  ".rds"
)
if (file.exists(track_file)) {
  track <- readRDS(track_file)
} else {
  stop("Tracking data file does not exist.")
}

# aer exchange rates
env_file <- "data-clean/environmental/air-change-rate.rds"
if (file.exists(env_file)) {
  aer_df <- readRDS(env_file) %>%
    filter(
      month(date) == as.numeric(mth),
      day(date) == as.numeric(dy)
    )
} else {
  stop("Environmental data file does not exist.")
}

# simulation parameters
param_file <- paste0(
  "simulations/", "sim-param-",
  2021, "-", mth, "-", dy,
  ".rds"
)
if (file.exists(param_file)) {
  sim_param <- readRDS(param_file)
} else {
  stop("Simulation parameter file does not exist.")
}

# scenario with or without mask wearing
if (grepl("nomask", scenario)) {
  sim_param$mask_red <- 1
  prop_cell_dist <- 1
} else {
  prop_cell_dist <- 0
}

# scenario with fixed air exchange rate
if (grepl("vent", scenario)) {
  aer_df$aer <- 6
}

# load model
if (grepl("temporal", scenario)) {
  mod <- "temp"
  source("models/tm-v1.R")
} else {
  mod <- "spattemp"
  source("models/stm-v2.R")
}

# save quanta for main scenario
if (grepl("main", scenario)) {
  save_quanta <- TRUE
} else {
  save_quanta <- FALSE
}

# scenario-specific diagnosed/undiagnosed TB patients
track_undiag <- track %>%
  dplyr::filter(tracking_end != "Possible HCW")

if (grepl("suspected", scenario)) {
  track_diag <- dplyr::filter(track, is_infectious | was_suspected)
  track_undiag <- dplyr::filter(track, patient_id == -1)
  pid_diag <- unique(track_diag$patient_id)
} else if (grepl("prevalence", scenario)) {
  track_diag <- dplyr::filter(track, patient_id == -1)
  pid_diag <- sim_param$undiagTB_pu
} else {
  track_diag <- dplyr::filter(track, is_infectious)
  track_undiag <- dplyr::filter(
    track_undiag, !is_infectious | is.na(is_infectious)
  )
  pid_diag <- sim_param$undiagTB_du
}

# run simulation
mcmapply(
  FUN = simulate_risk,
  sim = simulations,
  pid_diag = pid_diag[simulations],
  q_wait_rate = sim_param$q_wait[simulations],
  q_walk_rate = sim_param$q_walk[simulations],
  mask_red_rate = sim_param$mask_red[simulations],
  viral_inact_rate = sim_param$viral_inact[simulations],
  grav_settl_rate = sim_param$settling_rate[simulations],
  MoreArgs = list(
    sim_path_to_file = sim_path_to_file,
    t_start = t_start,
    t_midday = t_midday,
    tt_morn = tt_morn,
    tt_noon = tt_noon,
    room_mat_list = building$room_mat_list,
    room_volumes = building$room_volumes,
    cell_length = cellsize / 1e3,
    track_diag = track_diag,
    track_undiag = track_undiag,
    aer_df = aer_df,
    mod = mod,
    prop_cell_dist = prop_cell_dist,
    save_quanta = save_quanta
  ),
  mc.cores = ncores
)
