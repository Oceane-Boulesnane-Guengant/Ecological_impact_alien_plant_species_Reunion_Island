# Objective: preparation of the data to turn regression analyses on native population size
# 

# Name: Océane Boulesnane-Guengant
# Date: 2025/10/15



# Importation data --------------------------------------------------------

source("code/00_prepa_data/01_prepa_data_for_analyse.R")

# -------------------------------------------------------------------------



# Preparation data --------------------------------------------------------
## Habitat ----
data_analyse$habitat <- ifelse(data_analyse$habitat == "Syzygium_jambos_alien_forest", "low_forest", data_analyse$habitat)

## List per site ----
sites <- unique(data_analyse$site)
list_data_lmer <- list()
for (sit in sites) {
  data_site <- data_analyse %>% filter (site == sit)
  
  IAS_target <- setdiff(unique(data_site$name_sp), "native")
  list_IAS_target_lm <- list()
  
  for (IAS in IAS_target) {
    data_IAS_target <- data_site %>% 
      filter(unique_id %in% unique_id[name_sp == IAS])
    
    data_IAS_target <- data_IAS_target %>% 
      group_by(major_vegetation_type, site, unique_id, strata) %>%
      summarise(cover_native = sum(cover[name_sp == "native"]),
                cover_IAS_target = sum(cover[name_sp == IAS]), 
                cover_IAS_other = sum(cover[name_sp != IAS & name_sp != "native"]),
                altitude = first(altitude), 
                habitat = first (habitat),
                .groups = "drop")
    
    data_IAS_target$sum_cover <- data_IAS_target$cover_native + data_IAS_target$cover_IAS_target + data_IAS_target$cover_IAS_other
    
    # on supprime la strata si TOUT est égal à 0
    data_ee <- data_IAS_target %>%
      group_by(strata) %>%
      filter(!all(cover_IAS_target == 0)) %>%
      ungroup()
    
    data_ee$name_sp <- IAS
    
    data_ee <- data_ee %>%
      relocate(name_sp, .after = site)
    
    list_IAS_target_lm[[IAS]] <- data_ee
    
    # Garder uniquement les data ayant plus de 25 releves
    
    list_IAS_target_lm <- lapply(list_IAS_target_lm, function(df) {
      if(length(unique(df$unique_id)) > 25) df else NULL
    })
    
    list_IAS_target_lm <- list_IAS_target_lm[!sapply(list_IAS_target_lm, is.null)]
    
    
  }
  
  list_data_lmer[[sit]] <- dplyr::bind_rows(list_IAS_target_lm)
  
}


# -------------------------------------------------------------------------



# Final data --------------------------------------------------------------
data_all <- dplyr::bind_rows(list_data_lmer)

## Max cover of native species : 100% ----

data_all$cover_native <- ifelse(data_all$cover_native > 100, 100, data_all$cover_native)

data_all[sapply(data_all, is.character)] <- lapply(data_all[sapply(data_all, is.character)], as.factor)

## Transform native species for beta regression
data_all$cover_native_beta <- data_all$cover_native/100

epsilon <- 1e-03
data_all <- data_all %>%
  mutate(cover_native_beta = 
           ifelse(cover_native_beta == 0, 0 + epsilon,
                  ifelse(cover_native_beta == 1, 1 - epsilon, cover_native_beta)))

# -------------------------------------------------------------------------


# Clean the data ----------------------------------------------------------
## For script 02_analyse_regression_rainforest.R and 03_analyse_regression_subalpine.R
objet <- ls()
objet_conserver <- c("data_all")
objet_supprimer <- setdiff(objet, objet_conserver)
rm(list = objet_supprimer)
rm(objet, objet_conserver, objet_supprimer)

# -------------------------------------------------------------------------



