# Objective: Function to create paired plot 

# Name: Océane Boulesnane-Guengant
# Date: 2026/10/27

source("code/library.R")


# Function to create paired plots -----------------------------------------
fct_paired_plot <- function(df, IAS_target, seuil_cover_IAS_target, seuil_altitude, seuil_distance) {
  #### 1. Data preparation and summary ####
  
  # Step 1 : overall summary by unique_id
  df_summary <- df %>%
    group_by(unique_id) %>%
    summarise(
      cover_IAS_tot = sum(cover[cd_ref != "native"]),
      max_cover_other = max(ifelse(cd_ref != IAS_target & cd_ref != "native", cover, 0)), 
      cover_IAS_target = sum(cover[cd_ref == IAS_target]),
      max_IAS_target = max(ifelse(cd_ref == IAS_target, cover, 0)),
      cover_native = sum(cover[cd_ref == "native"]),
      habitat = first(habitat),
      altitude = first(altitude),
      long = first(long),
      lat = first(lat),
      .groups = "drop")
  
  #### 2. Selection of invaded and non invaded plots ####
  invaded <- df_summary %>%
    filter(max_IAS_target >= seuil_cover_IAS_target, 
           cover_IAS_tot < 2 * cover_IAS_target)
  
  non_invaded <- df_summary %>%
    filter(cover_IAS_tot <= 2)
  
  if (nrow(invaded) == 0 | nrow(non_invaded) == 0) {
    message("Not enough records of invaded or non invaded plots")
    return(data.frame())
  }
  
  #### 3. Convert to sf objects for spatial calculation ####
  invaded_sf <- st_as_sf(invaded, coords = c("long", "lat"), crs = 4326) %>% 
    st_transform(crs = 2975)
  non_invaded_sf <- st_as_sf(non_invaded, coords = c("long", "lat"), crs = 4326) %>% 
    st_transform(crs = 2975)
  
  #### 4. Loop of each habitat types ####
  habitats_unique <- unique(invaded$habitat)
  pairs_list <- list()
  used_non_invaded <- character(0)  # non invaded unique_id not already in use
  
  for (hab in habitats_unique) {
    env_hab <- invaded_sf %>% filter(habitat == hab)
    
    if (hab == "Syzygium_jambos_alien_forest") {
      non_env_hab <- non_invaded_sf %>% filter(habitat %in% c("submountain_rainforest", "lowland_rainforest"))
    } else {
      non_env_hab <- non_invaded_sf %>% filter(habitat == hab)
    }
    
    # Exclude plots that have already been paired
    non_env_hab <- non_env_hab %>% filter(!(unique_id %in% used_non_invaded))
    
    # Transpose if there are more non-invaded than invaded plots
    transpose <- nrow(env_hab) > nrow(non_env_hab)
    if (transpose) {
      temp <- env_hab
      env_hab <- non_env_hab
      non_env_hab <- temp
    }
    
    # Gluttonous matching (closest)
    matched <- list()
    used_ids <- character(0)
    
    for (i in 1:nrow(env_hab)) {
      e <- env_hab[i, ]
      
      # Filtering by distance
      dists <- st_distance(e, non_env_hab)
      dists <- as.numeric(dists)
      candidates <- non_env_hab
      candidates$distance <- dists
      
      candidates <- candidates %>%
        filter(distance <= seuil_distance,
               !(unique_id %in% used_ids))
      
      # Altitude
      candidates$delta_alt <- abs(candidates$altitude - e$altitude)
      candidates <- candidates %>% filter(delta_alt <= seuil_altitude)
      
      if (nrow(candidates) > 0) {
        best_match <- candidates[which.min(candidates$distance), ]
        
        if (!transpose) {
          pair <- data.frame(
            invaded = as.character(e$unique_id),
            habitat_invaded = as.character(e$habitat),
            cover_native_inv = e$cover_native,
            cover_IAS_target_env = e$cover_IAS_target,
            max_cover_other = e$max_cover_other,
            cover_IAS_tot_env = e$cover_IAS_tot,
            non_invaded = as.character(best_match$unique_id),
            habitat_non_invaded = as.character(best_match$habitat),
            cover_native_non_inv = best_match$cover_native,
            cover_IAS_target_non_env = best_match$cover_IAS_target,
            cover_IAS_tot_non_env = best_match$cover_IAS_tot,
            distance = best_match$distance,
            delta_alt = best_match$delta_alt
          )
        } else {
          pair <- data.frame(
            invaded = as.character(best_match$unique_id),
            habitat_invaded = as.character(best_match$habitat),
            cover_native_inv = best_match$cover_native,
            cover_IAS_target_env = best_match$cover_IAS_target,
            max_cover_other = best_match$max_cover_other,
            cover_IAS_tot_env = best_match$cover_IAS_tot,
            non_invaded = as.character(e$unique_id),
            habitat_non_invaded = as.character(e$habitat),
            cover_native_non_inv= e$cover_native,
            cover_IAS_target_non_env = e$cover_IAS_target,
            cover_IAS_tot_non_env = e$cover_IAS_tot,
            distance = best_match$distance,
            delta_alt = best_match$delta_alt
          )
        }
        
        matched[[length(matched) + 1]] <- pair
        used_ids <- c(used_ids, best_match$unique_id)
      }
    }
    
    if (length(matched) > 0) {
      df_pairs <- do.call(rbind, matched)
      used_non_invaded <- c(used_non_invaded, df_pairs$non_invaded)
      pairs_list[[hab]] <- df_pairs
    }
  }
  
  if (length(pairs_list) == 0) {
    return(data.frame())
  } else {
    results <- do.call(rbind, pairs_list)
    return(results)
  }
}


# Function for the verification of paired plots ---------------------------
fct_effet_verif <- function(data_PP) {
  pp_name_sp <- data_PP %>%
    dplyr::select(paired_id, invaded, non_invaded, cover_native_inv, cover_native_non_inv, cover_IAS_target_env, cover_IAS_tot_env, cover_IAS_tot_non_env, altitude_inv, altitude_non_inv, habitat_non_invaded, habitat_invaded)
  
  pp_inv <- pp_name_sp %>% dplyr::select(paired_id, invaded, cover_native_inv, cover_IAS_target_env, altitude_inv, cover_IAS_tot_env, habitat_invaded) %>%
    rename("unique_id" = "invaded", 
           "cover_native" = "cover_native_inv",
           "cover_IAS_target" = "cover_IAS_target_env", 
           "cover_IAS_tot" = "cover_IAS_tot_env", 
           "altitude" = "altitude_inv", 
           "habitat" = "habitat_invaded") 
  pp_inv$cover_IAS_other <- pp_inv$cover_IAS_tot - pp_inv$cover_IAS_target
  pp_inv$condition <- "invaded"
  
  pp_non_inv <- pp_name_sp %>% 
    dplyr::select(paired_id, non_invaded, cover_native_non_inv, cover_IAS_target_env, cover_IAS_tot_non_env, altitude_non_inv, habitat_non_invaded) %>%
    rename("unique_id" = "non_invaded", 
           "cover_native" = "cover_native_non_inv",
           "cover_IAS_target" = "cover_IAS_target_env",
           "cover_IAS_tot" = "cover_IAS_tot_non_env", 
           "altitude" = "altitude_non_inv", 
           "habitat" = "habitat_non_invaded")
  pp_non_inv$cover_IAS_other <- pp_non_inv$cover_IAS_tot - pp_non_inv$cover_IAS_target
  pp_non_inv$condition <- "non_invaded"
  
  pp_species_verif <- rbind(pp_inv, pp_non_inv)
  pp_species_verif$condition <- factor(pp_species_verif$condition)
  pp_species_verif$cover_IAS_other <- pp_species_verif$cover_IAS_tot - pp_species_verif$cover_IAS_target
  pp_species_verif
}
