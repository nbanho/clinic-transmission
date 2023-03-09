# Libraries

library(tidyverse)
options(dplyr.summarise.inform = FALSE)
library(parallel)
library(foreach)

source("utils/trans_risk.R")
source("utils/spatial.R")


#' Prepare data for spatiotemporal model
#' 
#' @param clinic name of clinic
#' @param sel_date selected date of patient movement data
#' @param seed seed number
#' @param nSim number of simulations
#' @param cellVolume desired volume of cells to compute quanta concentration (in m3)
#' @param co2_exhaled exhaled CO2 per person (default: 40,000 ppm)
#' @param co2_outdoor CO2 level in outdoor air (default: 400 ppm)
#' @param ... additional arguments include the distributions below
#' @param rTBunmasked number of unmasked TB patients distribution
#' @param rMuIc mean of spatial emission weight distribution
#' @param dIc  spatial emission weight distribution
#' @param rq quanta emission rate distribution
#' @param rV average volume of exhaled gas distribution
#' @param rG CO2 generation rate distribution


prep_data <- function(
    clinic, 
    sel_date,
    seed = 12345,
    nSim = 1000,
    min_duration = 5,
    cellVolume = 1,
    co2_exhaled = 40000,
    co2_outdoor = 400,
    ...) {
  
  #### Data ####
  
  # Settings
  set.seed(seed)
  
  # patient movement data
  patMov <- readRDS(paste("data-clean",clinic,"patient-tracking-data",sel_date,"patient-id-data.rds",sep="/")) %>%
    group_by(patient_id) %>%
    filter(as.numeric(difftime(last(time), first(time), units = "mins")) > min_duration) %>%
    ungroup() %>%
    mutate(location = ifelse(is_waitingroom|is_passage, "waiting room", ifelse(is_tbroom, "tb room", NA)),
           hhmm = format(time, format = "%H:%M")) %>%
    filter(!is.na(location)) 
  patIDs <- unique(patMov$patient_id)
  
  # CO2 data
  co2 <- readRDS(paste("data-clean", clinic, "environmental-data", "co2-temp-humid.rds", sep = "/")) %>%
    filter(date == sel_date)
  time_period <- seq(min(co2$date_time), max(co2$date_time), by = "1 min")
  
  # room data
  cellSize <- sqrt(cellVolume * 1e9 / 3e3) 
  building <- terra::vect(paste("data-raw", clinic, "building", "clinic-vector.gpkg", sep = "/"))
  building_sf <- sf::st_as_sf(building, crs = NA)
  sf::st_crs(building_sf) <- NA
  waiting_room <- shapeToSpatial(building_sf$geometry[2], cellSize)
  wrVol <- compute_volume(waiting_room)
  wrCellCoords <- dplyr::select(create_coord_df(waiting_room), x, y)
  passage <- shapeToSpatial(building_sf$geometry[4], cellSize)
  paVol <- compute_volume(passage)
  paCellCoords <- dplyr::select(create_coord_df(passage), x, y)
  tb_room <- shapeToSpatial(building_sf$geometry[3], cellSize)
  tbVol <- compute_volume(tb_room)
  tbCellCoords <- dplyr::select(create_coord_df(tb_room), x, y)
  clinicCellCoords <- rbind(
    mutate(wrCellCoords, location = "waiting room"),
    mutate(tbCellCoords, location = "tb room"),
    mutate(paCellCoords, location = "waiting room")
  ) %>% mutate(id = 1:nrow(.))
  clinicCellCoordsXY <- cbind(clinicCellCoords$x, clinicCellCoords$y)
  
  # number of patients over time
  nDF <- do.call(rbind, parallel::mclapply(time_period, function(t) {
    patMov %>% 
      filter(between(time, t, t + lubridate::minutes(1))) %>%
      group_by(patient_id, location) %>%
      slice(1) %>% 
      ungroup %>%
      group_by(location) %>%
      summarize(n = n()) %>%
      ungroup %>%
      mutate(time = t)
  }, mc.cores = 16))
  nDFco2 <- co2 %>% 
    dplyr::select(date_time, location, co2) %>%
    rename(time = date_time) %>%
    left_join(nDF, by = c("time", "location")) %>%
    mutate(n = ifelse(is.na(n), 0, n),
           vol = ifelse(location == "tb room", tbVol, wrVol + paVol))
  
  # masked TB patients
  patMOV_TBmasked <-  filter(patMov, tb_suspect=="yes")
  patIDS_TBmasked <- unique(patMOV_TBmasked$patient_id)
  
  # unmasked TB patients 
  nTBmasked <- readRDS(paste0("data-clean/", clinic, "/clinical-data/clinical-data.rds")) %>%
    filter(date == as.Date(sel_date),
           tb_suspect=="yes") %>%
    group_by(patient_id) %>%
    slice(1) %>%
    ungroup() %>%
    nrow()
  patTBunmasked_potential <- patIDs[!(patIDs %in% patIDS_TBmasked)]
  
  # add cell id to patient movement data
  patMov$id <- parallel::mcmapply(find_raster, patMov$x, patMov$y, list(cellCoordsXY = clinicCellCoordsXY), mc.cores = 16)
  
  # input parameters
  inputPar <- data.frame(
    spatial_mu = rMuIc(nSim),
    quanta = rq(nSim),
    V = rV(nSim),
    co2_ex = co2_exhaled,
    co2_out = co2_outdoor,
    co2_gen = rG(nSim),
    n_TBunmasked = rTBunmasked(nSim, nTBmasked)
  )
  inputPar$quanta <- inputPar$quanta / 3600 # quanta emission rate per second

  # TB patient ids for each simulation
  tb_ids <- lapply(inputPar$n_TBunmasked, function(n) {
    patIDS_TBunmasked <- sample(patTBunmasked_potential, size = n)
    return(c(patIDS_TBmasked, patIDS_TBunmasked))
  })
  
  data_list <- list(
    time_period = time_period,
    patient_movements = patMov,
    co2_and_n = nDFco2,
    room_coordinates = clinicCellCoords,
    cell_volume = cellVolume,
    tb_patient_ids = tb_ids,
    input_parameters = inputPar
  )
  
  return(data_list)
}


#' Spatiotemporal modeling
#' 
#' @param time_period time period vector by minute
#' @param patMov patient movement data with time, x, y, location, id, tb_suspect
#' @param co2_and_n data on co2 levels and number of patients by grid cell and minute, i.e. co2, n, location, id, time
#' @param room_coords data frame of location, id, x, and y
#' @param cell_volume volume in m3 of grid cell
#' @param tb_ids patient ids of TB suspects
#' @param smu spatial mean distance quanta emission parameter 
#' @param q quanta emission rate per second
#' @param V average volume of exhaled gas
#' @param Ca CO2 in exhaled air
#' @param Co CO2 in outdoor air
#' @param G CO2 generation rate

spatiotemporal_model <- function(time_period, patMov, co2_and_n, room_coords, cell_volume, tb_ids, smu, q, V, Ca, Co, G) {
  
  # patient movement data of TB patients
  patMOV_TB <-  filter(patMov, patient_id %in% tb_ids)
  
  # initialize df to store quanta concentration by minute
  quantaDF <- room_coords %>%
    mutate(time = time_period[1] - lubridate::minutes(1),
           EQ = 0, # emitted quanta with spatial modeling
           EQ_ns = 0, # emitted quanta without spatial modeling
           RR = 0, # removal rate
           N = 0, # quanta concentration with spatial modeling
           N_ns = 0) # quanta concentration without spatial modeling
  quantaDF_prev <- quantaDF
  
  #### Temporal ####
  for (t in 1:length(time_period)) {
    # time point
    tp <- time_period[t] 
    quantaDF_new <- room_coords
    quantaDF_new$time <- tp
    
    #### Spatial quanta emission ####
    quanta_emission <- patMOV_TB %>%
      filter(between(time, tp, tp + lubridate::minutes(1))) %>%
      left_join(quantaDF_new, by = c("id", "location")) %>%
      group_by(location) %>%
      mutate(distance = convert_dist( euclidean(x.x, y.x, x.y, y.y) ),
             EQ = q * (distance / sum(distance)),
             EQ_ns = q / n()) %>%
      ungroup() 
    quanta_emission <- left_join(dplyr::select(quantaDF_new, location, id), quanta_emission, by = c("id", "location")) %>%
      mutate(across(c(EQ, EQ_ns), ~ ifelse(is.na(.x), 0, .x))) %>%
      group_by(id, location) %>%
      summarize(across(c(EQ, EQ_ns), ~ sum(.x))) %>%
      ungroup()
    
    #### Spatial quanta removal ####
    quanta_removal <- co2_and_n %>%
      filter(time == tp) %>%
      mutate(AER = compute_AER(G, Co, co2, n, vol),
             RR = compute_RR(AER) / 60) %>%
      dplyr::select(location, RR)
    
    #### Update quanta concentration #### 
    quantaDF_new <- quantaDF_new %>%
      left_join(quantaDF_prev %>% 
                  dplyr::select(id, location, N, N_ns) %>%
                  rename(N0 = N, N0_ns = N_ns),
                by = c("id", "location")) %>%
      left_join(quanta_emission, by = c("location", "id")) %>%
      left_join(quanta_removal, by = "location") %>%
      mutate(N = compute_Nt(EQ, RR, N0, cell_volume),
             N_ns = compute_Nt(EQ_ns, RR, N0_ns, cell_volume)) %>%
      dplyr::select(-N0, -N0_ns)
    quantaDF <- rbind(quantaDF, quantaDF_new)
    quantaDF_prev <- quantaDF_new
  }
  
  #### Risk of infection ####
  risk_of_infection <- patMov %>%
    dplyr::select(patient_id, location, id, hhmm) %>%
    filter(!(patient_id %in% tb_ids)) %>%
    left_join(quantaDF %>% 
                mutate(hhmm = format(time, format = "%H:%M")) %>%
                dplyr::select(id, location, hhmm, N, N_ns), 
              by = c("id", "location", "hhmm")) %>%
    group_by(patient_id) %>%
    mutate(exposure_time = n(),
           across(c(N, N_ns), ~ 1-exp(-sum(.x, na.rm = T)), .names = "P_{.col}")) %>%
    ungroup()
  
  return(risk_of_infection)
}


#### Run simulation ####

my.cluster <- parallel::makeCluster(
  parallel::detectCores() - 2, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()
foreach::getDoParWorkers()

clinics <- c("Masi")

for(cl in clinics) {
  
  message(sprintf("Clinic: %s", cl))
  sel_dates <- list.files(paste0("data-clean/", cl, "/patient-tracking-data/"))
  
  for (d in sel_dates) {
    
    message(sprintf("-- Date: %s", as.character(d)))
    clData <- prep_data(clinic = "Masi", sel_date = "2021-10-25")
    
    sim_results <- foreach::foreach(i = 1:nrow(clData$input_parameters), .combine = rbind, .packages = c("tidyverse")) %dopar% {
      
      result <- spatiotemporal_model(
        clData$time_period,
        clData$patient_movements,
        clData$co2_and_n,
        clData$room_coordinates,
        clData$cell_volume,
        clData$tb_patient_ids[i],
        clData$input_parameters$spatial_mu[i],
        clData$input_parameters$quanta[i],
        clData$input_parameters$V[i],
        clData$input_parameters$co2_ex[i],
        clData$input_parameters$co2_out[i],
        clData$input_parameters$co2_gen[i]
      )
      
      result$sim <- i
      
      return(result)
      
    }
    
    saveRDS(sim_results, paste0("simulations/", cl,  "/", d, ".rds"))
    
  }
}

parallel::stopCluster(cl = my.cluster)

