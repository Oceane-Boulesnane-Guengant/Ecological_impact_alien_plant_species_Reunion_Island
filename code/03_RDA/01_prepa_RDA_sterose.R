# Objective: data preparation for the RDA analyses in order to evaluated the impact of alien species on species composition in the rainforest vegetation

# Name: Océane Boulesnane-Guengant
# Date: 2025/11/30



# Data importation --------------------------------------------------------
di_rel <- read.csv("data/02_di_complete_rel.csv", header = T, sep = ",", dec = ".")
di_esp <- read.csv("data/02_di_complete_sp.csv", header = T, sep = ";", dec = ".")

# -------------------------------------------------------------------------



# Select the site ---------------------------------------------------------
di_rel <- di_rel %>% filter(site == "sterose")
di_esp <- di_esp %>% filter(site == "sterose")

# -------------------------------------------------------------------------



# Preparation of the data -------------------------------------------------
## Select line with native information ----
unique_id <- di_rel %>% select(unique_id) %>% 
  distinct()

## Create a data with plot with native information
id_strata <- di_esp %>% 
  filter(statut == "native") %>% 
  select(unique_id, strata) %>% 
  distinct_all()

id_strata <- id_strata %>% mutate(id_strata = paste(unique_id, strata, sep = "_"))

# -------------------------------------------------------------------------



# Preparation of the matrice ----------------------------------------------
## Native species variables ----
data_native <- di_esp %>% 
  filter(statut == "native") %>% 
  select(unique_id, cd_name, cd_ref, name_sp, strata)

data_native$presence <- 1

# Keep only species present in more than 5% of plots 
n_plots <- n_distinct(data_native$unique_id)
species_freq <- data_native %>%
  group_by(name_sp) %>%
  summarise(n_plots_present = n_distinct(unique_id), .groups = "drop") %>%
  mutate(freq = (n_plots_present / n_plots) * 100) # (5% → 13 plots)


sp_to_keep <- data_native %>%
  filter(presence == 1) %>%
  distinct(unique_id, cd_name) %>%
  group_by(cd_name) %>%
  summarise(nb_sites = n(), .groups = "drop") %>%
  filter(nb_sites >= 13) %>%
  pull(cd_name)

data_native <- data_native %>%
  filter(cd_name %in% sp_to_keep)

data_native <- data_native %>% subset(name_sp != "sp1")
data_native <- data_native %>% subset(name_sp != "sp2")

# Pivot the data 
data_native <- data_native %>% 
  mutate(id_strata = paste(unique_id, strata, sep = "_")) %>%
  select(id_strata, name_sp, presence) %>% 
  distinct_all() %>% 
  pivot_wider(names_from = name_sp, values_from = presence, values_fill = 0)

## Environmental variables ----
data_envi <- di_rel %>% select(unique_id, habitat, altitude)
data_envi <- left_join(id_strata, data_envi, by = "unique_id")
data_envi <- data_envi %>% select(-unique_id)


# For the Syzygium_jambos_alien_forestt habitat, transform to low_forest
data_envi$habitat <- ifelse(data_envi$habitat == "Syzygium_jambos_alien_forest", "lowland_rainforest", data_envi$habitat)

## Alien species variables ----

data_alien <- di_esp %>% filter(statut == "alien") %>% select(unique_id, cd_ref, name_sp, strata)
data_alien$presence <- 1

# Keep only species present in more than 5% of plots 
n_plots <- length(unique(data_alien$unique_id))
species_freq <- data_alien %>%
  group_by(name_sp) %>%
  summarise(n_plots_present = n_distinct(unique_id), .groups = "drop") %>%
  mutate(freq = (n_plots_present / n_plots) * 100) # (5% → 12 plots)

sp_to_keep <- data_alien %>%
  filter(presence == 1) %>%
  distinct(unique_id, cd_ref) %>%
  group_by(cd_ref) %>%
  summarise(nb_sites = n(), .groups = "drop") %>%
  filter(nb_sites >= 12) %>%
  pull(cd_ref)

data_alien <- data_alien %>%
  filter(cd_ref %in% sp_to_keep) %>% 
  select(unique_id, name_sp, strata, presence)

# Pivot the data 

data_alien <- data_alien %>%
  pivot_wider(names_from = name_sp, values_from = presence, values_fill = 0)

data_alien <- data_alien %>%
  unite("id_strata", unique_id, strata, sep = "_")

names(data_alien) <- gsub(" ", "_", names(data_alien))

# -------------------------------------------------------------------------



# Standardise variables ---------------------------------------------------
## Alien species and environmental variables ----
data_envi$altitude <- decostand(data_envi$altitude, method = "standardize")

# Combine data_envi with data_alien
data_alien <- data_envi %>% left_join(data_alien, by = "id_strata")
data_alien[is.na(data_alien)] <- 0

# Same number of row than native species data
id_native <- data_native %>% select(id_strata)
data_alien <- data_alien %>%
  semi_join(id_native, by = "id_strata")

# Keep only the unique id present in data_native

## Native species variables ---
data_native <- as.data.frame(data_native)
rownames(data_native) <- data_native$id_strata
data_native <- data_native %>% select(-id_strata)

data_alien$habitat <- ifelse(data_alien$habitat == "Syzygium_jambos_alien_forest", "lowland_rainforest", data_alien$habitat)



# -------------------------------------------------------------------------



# Clean the script --------------------------------------------------------
## For the script 03.RDA_sterose.R
objet <- ls()
objet_a_conserver <- c("data_alien", "data_native")
objet_supprimer <- setdiff(objet, objet_a_conserver)
rm(list = objet_supprimer)
rm(objet, objet_a_conserver, objet_supprimer)





