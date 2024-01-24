#### Command line ###

# all command line arguments
args <- commandArgs(trailingOnly=TRUE)

# start simulation
#' e.g. if the previous simulation was interrupted
#' 1 means to start from beginning
cont_n <- as.numeric(args[1])

# end simulation
term_n <- as.numeric(args[2])

# month
mth <- args[3]

# day
dy <- args[4]

# analysis name
aname <- args[5]
adir <- paste0("simulations/", aname, "/")
if (!dir.exists(adir)) {
  dir.create(adir)
}

# grid cell length
cellSize <- as.numeric(args[6]) # in px

#' without mask wearing
#' 0: without mask-wearing
#' 1: with masking
is_masking <- as.numeric(args[7]) 

#' fixed air chnage rate
#' >0: fixed AER replacing empirical one(s)
#' 0: use empirical one(s)
fixed_aer <- as.numeric(args[8]) 

#' population-level prevalence
#' du: use diagnosed and undiagnosed TB patients
#' ds: use diagnosed and suspsected TB patients
#' pu: use population-level prevalence to sample from undiagnosed TB patients
#' for population-level prevalence in SA, see Moyo et al, 2022, Lancet ID, doi:10.1016/S1473-3099(22)00149-9
who_is_tb <- args[9]

#' type of model
#' temp: temporal model (no spatial diffusion, assuming well-mixed airspace)
#' spattemp: spatiotemporal model (including spatial diffusion, no well-mixed airspace)
mod <- args[10]

#' daytime
#' TODO: Should the number of undiagnosed patients be adpated if the clinic was only open for half a day?

#' single room
#' clinic: all rooms
#' war: waiting room
#' tbr: tb room
#' cor: corridor
sel_rooms <- args[11]
if (sel_rooms == "clinic") {
  sel_rooms <- c("Waiting room", "TB room", "Corridor")
} else if (sel_rooms == "war") {
  sel_rooms <- c("Waiting room")
} else if (sel_rooms == "tbr") {
  sel_rooms <- c("TB room")
} else if (sel_rooms == "cor") {
  sel_rooms <- c("Corridor")
} else {
  stop("Invalid rooms argument.")
}


# file containing simulation parameters
param_file <- paste0("simulations/", "/", "sim-param-", 2021, "-", mth, "-", dy, ".rds")

# path to store cellSize dependend tracking data
track_file <- paste0("simulations/", "/", "tracking-df-", cellSize, "px-", 2021, "-", mth, "-", dy, ".rds")

# path to file to save simulation results
sim_path_to_file <- paste0(adir, mth, "-", dy, "/")
if (!dir.exists(sim_path_to_file)) {
  dir.create(sim_path_to_file)
}


#### Libraries ####

library(tidyverse)
library(parallel)
library(lubridate)

source("utils/spatial.r")
source("utils/trans_risk.r")
source("models/stm-v2.R")


#### Time ####

date <- paste0("2021-",mth, "-", dy)
t_start <- as.POSIXct(paste(date, "08:00:00"))
midday <- as.POSIXct(paste(date, "12:00:00"))
t_end <- as.POSIXct(paste(date, "16:00:00"))
tt <- seq.POSIXt(t_start, t_end, by = "1 sec")
tt_morn <- seq.POSIXt(t_start, midday, by = "1 sec")
tt_noon <- seq.POSIXt(midday + 1, t_end, by = "1 sec")
midday_idx <- which(tt == midday)


#### Building ####

source("preprocessing/prep_building-rasterize.R")


#### Tracking data ####

if (file.exists(track_file)) { # check if file with that cell size already exists
  
  track <- readRDS(track_file)
  
} else {
  
  stop("Track file does not exist.")
  
}


#### TB patients ####

track_undiag <- track %>%
  dplyr::filter(tracking_end != "Possible HCW") # always consider HCW as non-infectious

if (who_is_tb == "du") {
  
  # diagnosed
  track_diag <- dplyr::filter(track, is_infectious)
  
  # undiagnosed
  track_undiag <- dplyr::filter(track_undiag, !is_infectious | is.na(is_infectious))
  
} else if (who_is_tb == "ds") {
  
  # diagnosed and suspected
  track_diag <- dplyr::filter(track, is_infectious | was_suspected)
  pid_diag <- unique(track_diag$patient_id)
  
  # no undiagnosed/unsuspected TB patients
  track_undiag <- dplyr::filter(track, patient_id == -1)
  
} else if(who_is_tb == "pu") {
  
  # no diagnosed TB patients
  track_diag <- dplyr::filter(track, patient_id == -1)
  
  # only undiagnosed TB patients
  # track_undiag <- track_undiag
  
  
}


#### Environmental data #####
aer_df <- readRDS("data-clean/environmental/air-change-rate.rds") %>%
  filter(month(date) == as.numeric(mth),
         day(date) == as.numeric(dy)) 

if (fixed_aer > 0) {
  aer_df$aer <- fixed_aer
}


#### Assumptions ####

if (file.exists(param_file)) {
  
  sim_param <- readRDS(param_file)
  
} else {
  
  stop("Parameter file does not exist.")
  
}


for (sim in cont_n:term_n) {
  
  print(paste0("Sim: ", sim))
  
  # TB patient tracks
  track_tb_all <- rbind(
    track_diag,
    filter(track_undiag, patient_id %in% sim_param[[paste0("undiagTB","_", who_is_tb)]][[sim]])
  )
  
  # no TB patient tracks but including HCW
  track_susc_all <- rbind(
    filter(track, tracking_end == "Possible HCW"),
    filter(track_undiag, ! (patient_id %in% sim_param[[paste0("undiagTB","_", who_is_tb)]][[sim]]))
  )
  
  # initialize
  susc_quanta_exp <- list()
  
  for (room in sel_rooms) {
    
    print(paste0("- ", room))
    
    # room setup
    if (room == "Waiting room") {
      
      # initial quanta concentration matrix
      c0 <- sP_to_matrix(waiting_room)
      
      # (no) TB patients in room
      track_tb <- filter(track_tb_all, is_waitingroom)
      track_susc <- filter(track_susc_all, is_waitingroom)
      
      # room dimension
      roomDim <- dimWR
      
    } else if (room == "Corridor") {
      
      # initial quanta concentration matrix
      c0 <- sP_to_matrix(corridor)
      
      # TB patients in room
      track_tb <- filter(track_tb_all, is_passage)
      track_susc <- filter(track_susc_all, is_passage)
      
      # room dimension
      roomDim <- dimCD
      
    } else if (room == "TB room") {
      
      # initial quanta concentration matrix
      c0 <- sP_to_matrix(tb_room)
      
      # TB patients in room
      track_tb <- filter(track_tb_all, is_tbroom)
      track_susc <- filter(track_susc_all, is_tbroom)
      
      # room dimension
      roomDim <- dimTB
      
    } else {
      stop("Error: Room invalid.")
    }
    
    # add quanta generation rate per second by activity level
    track_tb <- track_tb %>%
      dplyr::select(time, cell_x, cell_y, activity) %>%
      rename(x = cell_x, y = cell_y) %>%
      mutate(q = ifelse(activity == 1, sim_param$q_wait[sim_param$sim == sim], sim_param$q_walk[sim_param$sim == sim]),
             q = ifelse(is_masking == 1, q * sim_param$mask_red[sim_param$sim == sim], q),
             q = q / 3600)
    
    # quanta conc in the morning
    print("-- morning")
    track_tb_morn <- filter(track_tb, time <= midday)
    track_tb_morn$t <- as.numeric(difftime(track_tb_morn$time, t_start, units = "sec")) 
    
    if (mod == "spattemp") {
  
      ct_morn <- stm(time = tt_morn,
                     c0 = c0, 
                     inf = dplyr::select(track_tb_morn, -time, -activity),
                     cellLength = convert_dist(cellSize),
                     pd = ifelse(is_masking == 1, 0, 1),
                     vol = prod(roomDim),
                     aer = aer_df$aer[aer_df$room==room & aer_df$daytime=="Morning"] / 3600,
                     lambda = sim_param$viral_inact[sim_param$sim == sim] / 3600,
                     deposit = 0)
      
    } else if (mod == "temp") {
      
      stop("Model needs to be revised")
      
    } else {
      
      stop("Model does not exist")
      
    }
    
    ct_morn_mean <- apply(ct_morn, c(1,2), mean)
    saveRDS(ct_morn_mean, paste0(sim_path_to_file, "mean_quanta_conc-morning-", room, "-sim-", sim, ".rds"))
    
    
    # quanta conc in the afternoon
    print("-- afternoon")
    if (max(track_susc$time) > midday) {
      c0_noon <- ct_morn[,,dim(ct_morn)[3]]
    } else {
      c0_noon <- c0
    }
    track_tb_noon <- filter(track_tb, time > midday)
    track_tb_noon$t <- as.numeric(difftime(track_tb_noon$time, midday + lubridate::seconds(1), units = "sec")) 
    
      
    if (mod == "spattemp") {
      
      ct_noon <- stm(time = tt_noon,
                     c0 = c0_noon, 
                     inf = dplyr::select(track_tb_noon, -time, -activity),
                     cellLength = convert_dist(cellSize),
                     pd = ifelse(is_masking == 1, 0, 1),
                     vol = prod(roomDim),
                     aer = aer_df$aer[aer_df$room==room & aer_df$daytime=="Afternoon"] / 3600,
                     lambda = sim_param$viral_inact[sim_param$sim == sim] / 3600,
                     deposit = 0)
      
    } else if (mod == "temp") {
      
      # TODO: revise
      stop("Model needs to be revised")
      
    } else {
      
      stop("Model does not exist")
      
    }
    
    ct_noon_mean <- apply(ct_noon, c(1,2), mean)
    saveRDS(ct_noon_mean, paste0(sim_path_to_file, "mean_quanta_conc-afternoon-", room, "-sim-", sim, ".rds"))

    
    # compute quanta conc exposure by susc
    ct <- abind::abind(ct_morn, ct_noon, along = 3)
    if (nrow(track_susc) > 0) {
      track_susc$quanta <- mapply(function(t, x, y) {if (t == 0) 0 else {ct[y,x,t]}}, 
                                  t=track_susc$t, x=as.character(track_susc$cell_x), y=as.character(track_susc$cell_y))
      track_susc <- track_susc %>%
        mutate(quanta_conc = inhal_rate * quanta / (convert_dist(cellSize) ^ 2 * convert_dist(cell_height))) %>%
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
    mutate(P = 1 - exp(-quanta_conc)) 
  
  saveRDS(trans_risk, paste0(sim_path_to_file, "trans-risk-", sim, ".rds"))
}
