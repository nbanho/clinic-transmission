#### Libraries ####

library(tidyverse)
library(reshape2)
library(lubridate)
library(sf)
library(sfheaders)
library(raster)
library(terra)
library(gridExtra)
library(VennDiagram)
source("utils/spatial.r")
source("utils/plotting.r")

#### Parameters ####
max_height_ratio <- 2
min_height_ratio <- 0.5
min_standing_height <- 1.4
same_height_ratio_lower <- 0.95
same_height_ratio_upper <- 1.05
max_long_time <- 10 * 60 # maximum time (in sec) to look ahead for possible matches
max_short_time <- 10 # maximum time (in sec) to look for short matches of people moving
max_mov_distance <- 3 # maximum distance (in m) to look for someone moving
max_sisa_distance <- 1 # maximum distance (in m) to look for someone who is sitting down or standing up
min_duration <- 5 # minimum duration (in min) to stay in the clinic

#### Data ####

# xovis

masi <- read.csv("data-raw/Masi/xovis/2021_10_25_coordinates_only.csv", header = F) %>%
  set_names(c("obs_id", "time", "x", "y", "height")) %>%
  mutate(time = as.POSIXct(paste0("2021-10-25 ", time), format = "%Y-%m-%d %H:%M:%S"))

# roomplan
clinic <- vect("data-raw/Masi/building/clinic-vector.gpkg")
clinic_sf <- sf::st_as_sf(clinic, crs = NA)
st_crs(clinic_sf) <- NA
clinic_df <- fortify(clinic_sf)


# find polygon
masi$point <- map2(masi$x, masi$y, function(xi,yi) sfheaders::sf_point(c(xi,yi)))
masi$poly = map(masi$point, sf::st_within, y = clinic_sf)
masi$is_entrance <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 1), T, F))
masi$is_waitingroom <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 2), T, F))
masi$is_passage <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 3), T, F))
masi$is_tbroom <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 4), T, F))
masi$is_reception <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 5), T, F))
masi$is_exiting <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) %in% c(1, 6:7)), T, F) ) # entrance and corridors are considered as exits
masi$is_seating = map_lgl(masi$poly, function(p) ifelse(any(unlist(p) %in% 8:10), T, F) ) 
masi$is_in_room1 <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 11), T, F) ) 
masi$is_in_room2 <- map_lgl(masi$poly, function(p) ifelse(any(unlist(p) == 12), T, F) ) 



#### Descriptives ####

# descriptives overall
masi_descr <- masi %>%
  group_by(obs_id) %>%
  mutate(distance = convert_dist( euclidean(x, lag(x), y, lag(y)) ),
         hr = height / lag(height)) %>%
  ungroup() 

# height ratio quantiles
hr_q_pl <- data.frame(as.list(quantile(masi_descr$hr, probs = c(0, 0.01,0.05, 0.10, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1), na.rm = T))) %>%
  gather() %>%
  mutate(key = gsub("[[:punct::]]", "", key),
         key = gsub("X", "", key),
         key = as.numeric(key)) %>%
  ggplot(aes(x = key, y = value)) +
  geom_point() +
  geom_line() +
  geom_vline(aes(xintercept = 1), color = "red") +
  geom_vline(aes(xintercept = 99), color = "red") +
  geom_hline(aes(yintercept = same_height_ratio_lower), color = "blue") +
  geom_hline(aes(yintercept = same_height_ratio_upper), color = "blue") +
  scale_y_log10(breaks = c(seq(0, 10, 1), 0.5, 0.75, 0.8, 0.85, 0.9, 0.95, 1, 1.05, 1.1, 1.15, 1.2, 1.5)) +
  scale_x_continuous(breaks = c(0, 0.01,0.05, 0.10, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1) * 100) +
  labs(x = "Quantile", y = "Value") +
  theme_bw2()
save_plot(hr_q_pl, pdf_file = "results/patient-tracking-height-ratio-quantile.pdf", w = 16, h = 12)


# descriptives summarized by observation ID
masi_descr_byOID <- masi_descr %>%
  group_by(obs_id) %>% 
  summarize(n = n(),
            d = convert_dist( euclidean(last(x), first(x), last(y), first(y))),
            maxd_moving = max(distance[!is_seating&!is_tbroom], na.rm = T),
            maxd_seating = max(distance[is_seating], na.rm = T),
            maxhr = max(hr, na.rm = T),
            minhr = min(hr, na.rm = T),
            minh = min(height) / 10,
            maxh = max(height) / 10) %>%
  ungroup() 


# distance 
moving_distance_pl <- masi_descr_byOID %>%
  filter(n > max_short_time) %>%
  dplyr::select(maxd_moving, maxd_seating) %>%
  gather() %>%
  mutate(key = ifelse(key == "maxd_moving", "Moving area", "Seating area"),
         key = factor(key, levels = c("Moving area", "Seating area"))) %>%
  ggplot(aes(x = value, color = key, fill = key)) +
  geom_density(alpha = .3) +
  geom_vline(aes(xintercept = max_mov_distance), color = "red") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Maximum distance between two tracks per observation ID",
       y = "Density") +
  theme_bw2() +
  theme(legend.position = "top", legend.title = element_blank())
save_plot(moving_distance_pl, pdf_file = "results/patient-tracking-moving_distance-density-plot.pdf", w = 10, h = 8)

# height ratio
max_hr_pl <- masi_descr_byOID %>%
  filter(is.finite(maxhr),
         n > max_short_time) %>%
  ggplot(aes(x = maxhr)) +
  geom_histogram() +
  geom_vline(aes(xintercept = max_height_ratio), color = "red") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Maximum height ratio per observation ID",
       y = "Count") +
  theme_bw2()
min_hr_pl <- masi_descr_byOID %>%
  filter(is.finite(maxhr),
         n > max_short_time) %>%
  ggplot(aes(x = minhr)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_vline(aes(xintercept = min_height_ratio), color = "red") +
  labs(x = "Minimum height ratio per observation ID",
       y = "Count") +
  theme_bw2()
hr_pl <- arrangeGrob(max_hr_pl, min_hr_pl, ncol = 2, widths = c(8,8))
ggsave(plot = hr_pl, filename = "results/patient-tracking-min-and-max-height-ratio.pdf", width = 16 / cm(1), height = 8 / cm(1))

# height
min_height_pl <- masi_descr_byOID %>%
  filter(n > max_short_time) %>%
  ggplot(aes(x = minh)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Minimum height per observation ID",
       y = "Count") +
  theme_bw2()
max_height_pl <- masi_descr_byOID %>%
  filter(n > 10) %>%
  ggplot(aes(x = maxh)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Maximum height per observation ID",
       y = "Count") +
  theme_bw2()
height_pl <- arrangeGrob(min_height_pl, max_height_pl, ncol = 2, widths = c(8,8))
ggsave(plot = height_pl, filename = "results/patient-tracking-min-and-max-height.pdf", width = 16 / cm(1), height = 8 / cm(1))

# Duration
duration_pl <- masi_descr_byOID %>%
  ggplot(aes(x = n / 60)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Duration [min]", y = "Count") +
  theme_bw2()
save_plot(duration_pl, pdf_file = "results/patient-tracking-duration.pdf", w = 8, h = 6)

total_distance_pl <- masi_descr_byOID %>%
  ggplot(aes(x = d)) +
  geom_histogram()  +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Total distance from start to end [m]", y = "Count") +
  theme_bw2()
save_plot(total_distance_pl, pdf_file = "results/patient-tracking-total-distance.pdf", w = 8, h = 6)


#### Preprocessing ####

# Pre-filter observations: 
#' 1. filter very short observations that hardly move
#' 2. Filter minimum and maximum height ratios < 0.5 and > 2, respectively
#' 3. Filter minimum height > 0.7m (i.e. set minimum height effectively to 1.40m)

masi_filt <- masi %>%
  left_join(masi_descr_byOID %>% dplyr::select(obs_id, maxhr, minhr, minh, d, n)) %>%
  filter(maxhr <= max_height_ratio, # 2.
         minhr >= min_height_ratio, # 2.
         minh >= min_standing_height * min_height_ratio, # 3.
         !(n <= 5 & d < 1)) %>% # 1.)
  dplyr::select(-maxhr, -minhr, -minh, -d, -n)

message(sprintf("%i of %i (%i percent) rows removed", 
                nrow(masi) - nrow(masi_filt),
                nrow(masi),
                round(100 - 100 * nrow(masi_filt) / nrow(masi))))
message(sprintf("%i of %i (%i percent) observation IDs removed; %i remaining", 
                length(unique(masi$obs_id))-length(unique(masi_filt$obs_id)),
                length(unique(masi$obs_id)),
                round(100 - 100 * length(unique(masi_filt$obs_id)) / length(unique(masi$obs_id))),
                length(unique(masi_filt$obs_id))))

# initialize
times <- unique(masi_filt$time)
masi_filt$patient_id <- NA

matches_short_moving <- 0
matches_short_sitting <- 0
matches_short_standing <- 0
# matches_long_sitting <- 0
# matches_long_standing <- 0
matches_long_reappearing <- 0

pot_matches_per_lost <- list()
lost_per_pot_match <- list()

for (k in 1:length(times)) {
  t <- times[k]
  # set new patient ids
  new_pat_ids <- masi_filt %>% filter(time == t)
  for (p in 1:nrow(new_pat_ids)) {
    if (is.na(new_pat_ids$patient_id[p])) {
      masi_filt$patient_id[masi_filt$obs_id==new_pat_ids$obs_id[p]] <- new_pat_ids$obs_id[p]
    }
  }
  
  # find patient ids that get lost
  lost_patients <- masi_filt %>%
    filter(between(time, t, t %m+% seconds(1))) %>% # filter last + one future track
    group_by(patient_id) %>% 
    mutate(n = n(), # if patient is lost, then he has only one track
           lt = last(time), # if the last time point is greater than t, then this is a new patient
           exiting = first(is_exiting)) %>% # check if patient is exiting the clinic
    slice(1) %>% # select last track
    ungroup() %>%
    filter(n == 1, # filter lost patients, i.e. only having one track
           lt == t, # remove new patients
           !exiting) # filter patients not exiting
  
  if (nrow(lost_patients) > 0) {
    # find possible matches
    possible_matches <- masi_filt %>%
      filter(between(time, t %m+% seconds(1), t %m+% seconds(max_long_time)),
             is.na(patient_id)) %>%
      group_by(obs_id) %>%
      slice(1) %>%
      ungroup() 
    
    if (nrow(possible_matches) > 0) {
      colnames(possible_matches) <- paste0("right_", colnames(possible_matches))
      
      # compute features
      matches <- crossing(lost_patients %>% dplyr::select(patient_id, obs_id, time, x, y, height, is_seating, is_in_room1, is_in_room2),
                          possible_matches %>% dplyr::select(right_obs_id, right_time, right_x, right_y, right_height, 
                                                             right_is_seating, right_is_entrance, right_is_reception,
                                                             right_is_in_room1, right_is_in_room2) ) %>%
        mutate(distance = convert_dist(euclidean(x, right_x, y, right_y)),
               heightratio = right_height / height,
               type = ifelse(between(heightratio, same_height_ratio_lower, same_height_ratio_upper), "moving",
                             ifelse(heightratio < same_height_ratio_lower, "sitting down",
                                    ifelse(heightratio > same_height_ratio_upper, "standing up", "none"))),
               timediff = as.numeric(right_time - time)) 
      
      # match type 1: moving around with similar height
      matches_1 <- matches %>%
        filter(distance <= max_mov_distance,
               timediff <= max_short_time,
               type == "moving") %>%
        mutate(match_type = "moving")
      
      # match type 2: standing up inside seating area
      matches_2 <- matches %>%
        filter(distance <= max_sisa_distance,
               timediff <= max_mov_distance,
               type == "standing up",
               (is_seating & right_is_seating)) %>%
        mutate(match_type = "sitting down")
      
      # match type 3: sitting down inside seating area
      matches_3 <- matches %>%
        filter(distance <= max_sisa_distance,
               timediff <= max_mov_distance,
               type == "sitting down",
               (is_seating & right_is_seating)) %>%
        mutate(match_type = "standing up")
      
      # filter people that may have sat at the same place for a long time unrecognized
      # matches_long_sitters <- matches %>%
      #   filter(distance <= max_sisa_distance,
      #          timediff > max_short_time,
      #          (is_seating & right_is_seating)) %>%
      #   mutate(match_type = ifelse(type == "sitting down", "still sitting", "standing up after a long time"))
      
      # match type 4: people that re-appearing from a treatment room with the same height after a long time
      matches_4 <- matches %>%
        filter(timediff > max_short_time,
               (is_in_room1 & right_is_in_room1) | (is_in_room2 & right_is_in_room2),
               type == "moving") %>%
        mutate(match_type = "re-appearing from treatment room")
      
      # combine potential matches and rank matches by time difference
      matches <- rbind(matches_1, matches_2, matches_3, matches_4) %>%
        arrange(timediff)
      
      # make matches
      if (nrow(matches) > 0) {
        # descriptive info
        pot_matches_per_lost[[k]] <- matches %>%
          group_by(patient_id) %>%
          summarize(n = n()) %>%
          ungroup() %>%
          dplyr::select(n) %>%
          unlist() 
        lost_per_pot_match[[k]] <- matches %>%
          group_by(right_obs_id) %>%
          summarize(n = n()) %>%
          ungroup() %>%
          dplyr::select(n) %>%
          unlist() 
        
        open_matches <- T
        
        while (open_matches) {
          # link IDs
          masi_filt$patient_id[masi_filt$obs_id==matches$right_obs_id[1]] <- matches$patient_id[1]
          message(sprintf("Linking %s with %s who is %s", matches$patient_id[1], matches$right_obs_id[1], matches$match_type[1]))
          
          if (matches$match_type[1] == "moving") {
            matches_short_moving <- matches_short_moving + 1
          } else if (matches$match_type[1] == "sitting down") {
            matches_short_sitting <- matches_short_sitting + 1
          } else if (matches$match_type[1] == "standing up") {
            matches_short_standing <- matches_short_standing + 1
          # } else if (matches$match_type[1] == "still sitting") {
          #   matches_long_sitting <- matches_long_sitting + 1
          # } else if (matches$match_type[1] == "standing up after a long time") {
          #   matches_long_standing <- matches_long_standing + 1
          } else if (matches$match_type[1] == "re-appearing from treatment room") {
            matches_long_reappearing <- matches_long_reappearing + 1
          }
          
          # remove match from possible ones
          matches <- matches %>%
            filter(patient_id != matches$patient_id[1],
                   right_obs_id != matches$right_obs_id[1])
          
          open_matches <- ifelse(nrow(matches) > 0, T, F)
        }
      }
    } else {
      pot_matches_per_lost[[k]] <- 0
      lost_per_pot_match[[k]] <- 0
    }
  } else {
    pot_matches_per_lost[[k]] <- NA
    lost_per_pot_match[[k]] <- NA
  }
}

#### Reports ####

# Match counts by type
match_counts <- c(matches_short_moving, matches_short_sitting, matches_short_standing, matches_long_reappearing)
match_count_types <- c("Moving", "Sitting down", "Standing up", "Re-appearing")
tot_matches <- sum(match_counts)
sum_patient_ids <- length(unique(masi_filt$patient_id))
sum_obs_ids <- length(unique(masi_filt$obs_id))
match_type_pl <- tibble(
  key = match_count_types,
  value = match_counts,
) %>%
  mutate(percent = value / sum(value) * 100,
         percent = paste0(round(percent, 0), "%"),
         key = factor(key, levels = unique(key))) %>%
  ggplot(aes(x = key, y = value)) +
  geom_bar(aes(fill = key), stat = "identity") +
  geom_text(aes(label = percent, color = key), vjust = -.25, size = 8 / cm(1)) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  labs(y = "Count",
       title = paste("Total links:", tot_matches),
       subtitle = paste(sum_patient_ids, "patient ids compared to", sum_obs_ids, "observation ids")) +
  theme_bw2() +
  theme(axis.title.x = element_blank(), legend.position = "none")
save_plot(match_type_pl, pdf_file = "results/patient-tracking-match-type-counts.pdf", w = 16, h = 10)


# Potential matches per lost and lost per potential match
pot_matches_per_lost_pl <- data.frame(x = (unlist(pot_matches_per_lost))) %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.25) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Potential matches per lost patient IDs", y = "Count") +
  theme_bw2() 
lost_per_pot_match_pl <- data.frame(x = (unlist(lost_per_pot_match))) %>%
  ggplot(aes(x = x)) +
  geom_histogram(binwidth = 0.25) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(x = "Lost patient IDs per potential match", y = "Count") +
  theme_bw2()
per_match_pl <- arrangeGrob(pot_matches_per_lost_pl, lost_per_pot_match_pl, ncol = 2, widths = c(8,8))
ggsave(plot = per_match_pl, file = "results/patient-tracking-match-per.pdf", w = 16 / cm(1), h = 8 / cm(1))

# Post-filter IDs
masi_match_descr <- masi_filt %>%
  group_by(patient_id) %>%
  summarize(duration = difftime(last(time), first(time), units = "min"),
            stayed = ifelse(duration >= min_duration, T, F),
            received = any(is_reception),
            height_received = ifelse(any(received), mean(height[received]) / 10, 0), 
            tbroom = any(is_tbroom)) %>%
  ungroup() 

# Subset of patients staying longer in the clinic and passing by the reception
masi_sub <- masi_filt %>%
  left_join(masi_match_descr %>% dplyr::select(patient_id, stayed, received), by = "patient_id") %>%
  filter(stayed,
         received,
         height_received >= min_standing_height) 

n_masi_sub <- length(unique(masi_sub$patient_id))

futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
venn.diagram(x = list(
  #All = masi_match_descr$patient_id,
  Stayed = masi_match_descr$patient_id[masi_match_descr$stayed],
  Reception = masi_match_descr$patient_id[masi_match_descr$received],
  TBroom = masi_match_descr$patient_id[masi_match_descr$tbroom]),
  filename = "results/patient-tracking-venndiagramm.png",
  output = F,
  height = 1000,
  width = 1000,
  imagetype="png" ,
  resolution = 300,
  compression = "lzw",
  lwd = 1,
  col=c("#440154ff", '#21908dff', '#fde725ff'),
  fill = c(alpha("#440154ff",0.3), alpha('#21908dff',0.3), alpha('#fde725ff',0.3)),
  cex = 0.5,
  fontfamily = "sans",
  cat.cex = 0.3,
  cat.default.pos = "outer",
  cat.pos = c(-27, 27, 135),
  cat.dist = c(0.055, 0.055, 0.085),
  cat.fontfamily = "sans",
  cat.col = c("#440154ff", '#21908dff', '#fde725ff'),
  rotation = 1,
  main = paste(n_masi_sub, "patient IDs with potential to match at the reception"),
  main.cex = 0.5,
)

duration_sub_pl <- masi_sub %>%
  group_by(patient_id) %>%
  mutate(duration = as.numeric(last(time) - first(time) )) %>%
  ungroup() %>%
  ggplot(aes(x = duration)) +
  geom_histogram() +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Duration [min]", y = "Count") +
  theme_bw2()
save_plot(duration_sub_pl, pdf_file = "results/patient-tracking-good-patient-ids-duration.pdf", w = 8, h = 6)

clean_ids <- unique(masi_sub$patient_id) 
# clean_id_chunks <- split(clean_ids, ceiling(seq_along(clean_ids) / 8))
pdf("results/patient-tracking-good-patient-ids.pdf", width = 21 / cm(1), height = 14 / cm(1))
for (i in 1:length(clean_ids)) {
  print(
    plot_single_track(masi_sub %>% filter(patient_id == clean_ids[i]))
  )
}
dev.off()
