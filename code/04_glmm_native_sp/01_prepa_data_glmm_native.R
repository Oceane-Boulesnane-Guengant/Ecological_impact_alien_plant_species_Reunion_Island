# Objective: preparation of the data to turn regression analyses on native species richness

# Name: Océane Boulesnane-Guengant
# Date: 2025/11/29



# Data importation --------------------------------------------------------
di_rel <- read.csv("data/02_di_complete_rel.csv", header = T, sep = ";", dec = ".")
di_esp <- read.csv("data/02_di_complete_sp.csv", header = T, sep = ";", dec = ".")

altitude <- di_rel %>% select(unique_id, altitude) %>% distinct_all()
habitat <- di_rel %>% select(unique_id, habitat) %>% distinct_all()
habitat$habitat <- ifelse(habitat$habitat == "Syzygium_jambos_alien_forest", "lowland_rainforest", habitat$habitat)

# -------------------------------------------------------------------------



# Sainte-Rose site --------------------------------------------------------
## Select the site ----

### Data native species 
data_rich <- di_rel %>% filter(site == "sterose")
unique_di <- data_rich %>% select(unique_id)

data_rich <- data_rich %>% select(unique_id, native_richness_herb, native_richness_shrub, native_richness_tree)

data_rich <- data_rich %>%
  mutate(native_richness_herb = replace_na(native_richness_herb, 0), 
         native_richness_shrub = replace_na(native_richness_shrub, 0),
         native_richness_tree = replace_na(native_richness_tree, 0))

data_rich <- data_rich %>%
  pivot_longer(cols = starts_with("native_richness"),
               names_to = "strata",
               values_to = "sp_richness") %>%
  mutate(strata = str_remove(strata, "native_richness_"))

data_rich$id_strata <- paste0(data_rich$unique_id, "_", data_rich$strata)
data_rich <- data_rich %>% select(-unique_id, -strata)


### Data alien species
data_alien <- di_esp %>%
  filter(site == "sterose") %>% 
  filter(statut == "alien")

data_alien$id_strata <- paste0(data_alien$unique_id, "_", data_alien$strata)

# Add non invaded plots

non_invaded <- as.data.frame(setdiff(data_rich$id_strata, data_alien$id_strata))
names(non_invaded) <- "id_strata"

non_invaded <- non_invaded %>%
  separate(id_strata, into = c("unique_id", "strata"), sep = "_")
non_invaded$id_strata <- paste0(non_invaded$unique_id, "_", non_invaded$strata)


non_invaded$major_vegetation_type <- "rainforest"
non_invaded$site <- "sterose"
non_invaded$cd_name <- "native_sp"
non_invaded$cd_ref <- 'native_sp'
non_invaded$name_sp <- "native"
non_invaded$cover <- NA
non_invaded$statut <- "native"


data_alien <- rbind(data_alien, non_invaded)

## Add species richness to data_alien -----

data_glmm_sterose <- left_join(data_alien, data_rich, by = "id_strata")


# -------------------------------------------------------------------------



# Volcan site -------------------------------------------------------------
## Select the site ----

### Data native species 
data_rich <- di_rel %>% filter(site == "volcan")
unique_di <- data_rich %>% select(unique_id)

### Only herbaceous stratum
data_rich <- data_rich %>% select(unique_id, native_richness_herb)

data_rich <- data_rich %>%
  pivot_longer(cols = starts_with("native_richness"),
               names_to = "strata",
               values_to = "sp_richness") %>%
  mutate(strata = str_remove(strata, "native_richness_"))

data_rich$id_strata <- paste0(data_rich$unique_id, "_", data_rich$strata)
data_rich <- data_rich %>% select(-unique_id, -strata)

### Data alien species
data_alien <- di_esp %>%
  filter(unique_id %in% unique_di$unique_id)

data_alien <- data_alien %>%
  filter(statut == "alien") %>%
  filter(strata == "herb")

data_alien$id_strata <- paste0(data_alien$unique_id, "_", data_alien$strata)

## Add species richness to data_alien ----
data_glmm_volcan <- left_join(data_alien, data_rich, by = "id_strata")

data_glmm <- rbind(data_glmm_sterose, data_glmm_volcan)


data_glmm <- left_join(data_glmm, altitude, by = "unique_id")
data_glmm <- left_join(data_glmm, habitat, by = "unique_id")

data_glmm$habitat <- ifelse(data_glmm$habitat == "Syzygium_jambos_alien_forest", "lowland_rainforest", data_glmm$habitat)


# -------------------------------------------------------------------------



# Prepa data for glmm analyses --------------------------------------------
## Data per site ----
sites <- unique(data_glmm$site)
list_data_lmer <- list()
for (sit in sites) {
  data_site <- data_glmm %>% filter (site == sit)
  
  IAS_target <- unique(data_site$cd_ref)
  list_IAS_target_lm <- list()
  
  for (ee in IAS_target) {
    data_IAS_target <- data_site %>%
      filter(unique_id %in% (data_site %>% filter(cd_ref == ee) %>% pull(unique_id)))
    
    data_IAS_target <- data_IAS_target %>% 
      group_by(major_vegetation_type, site, unique_id, strata) %>%
      summarise(cover_IAS_target = sum(cover[cd_ref == ee]), 
                cover_IAS_other = sum(cover[cd_ref != ee & statut != "indigene"]),
                altitude = first(altitude), 
                habitat = first (habitat),
                sp_richness = first(sp_richness),
                .groups = "drop")
    
    
    # on supprime la strata si TOUT est égal à 0
    data_ee <- data_IAS_target %>%
      group_by(strata) %>%
      filter(!all(cover_IAS_target == 0)) %>%
      ungroup()
    
    data_ee$cd_ref <- ee
    
    data_ee <- data_ee %>%
      relocate(cd_ref, .after = site)
    
    list_IAS_target_lm[[ee]] <- data_ee
    
    # Garder uniquement les data ayant plus de 25 releves
    
    list_IAS_target_lm <- lapply(list_IAS_target_lm, function(df) {
      if(length(unique(df$unique_id)) > 25) df else NULL
    })
    
    list_IAS_target_lm <- list_IAS_target_lm[!sapply(list_IAS_target_lm, is.null)]
    
    
  }
  
  list_data_lmer[[sit]] <- dplyr::bind_rows(list_IAS_target_lm)
  
}


data_all <- dplyr::bind_rows(list_data_lmer)

data_all[sapply(data_all, is.character)] <- lapply(data_all[sapply(data_all, is.character)], as.factor)


## add name sp ----
name_sp <- data_glmm %>% select(cd_ref, name_sp) %>% distinct_all()
data_all <- left_join(data_all, name_sp, by = "cd_ref")

data_all <- data_all %>%
  mutate(cover_IAS_other = ifelse(is.na(cover_IAS_other), 0, cover_IAS_other))

# -------------------------------------------------------------------------



# Nettoyer le data pour script --------------------------------------------

objet <- ls()
objet_conserver <- c("data_all")
objet_supprimer <- setdiff(objet, objet_conserver)
rm(list = objet_supprimer)
rm(objet, objet_conserver, objet_supprimer)

# -------------------------------------------------------------------------


