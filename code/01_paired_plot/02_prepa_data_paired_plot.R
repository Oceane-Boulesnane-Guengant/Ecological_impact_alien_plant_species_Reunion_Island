# Objective: Preparation of the paired plot data for each species, per site.

# Name: Océane Boulesnane-Guengant
# Date: 2026/10/27


# Importation data and function -------------------------------------------
## Data for the analyse ----
source("code/00_prepa_data/01_prepa_data_for_analyse.R")

## Paired plot function -----
source("code/01_paired_plot/01_function_for_paired_plot.R")


# -------------------------------------------------------------------------



# Creation of paired plot per site and species ----------------------------
## Loop : results into a list ----
paires_per_site <- list()
site_list <- unique(data_analyse$site)

for (site in site_list) {
  cat("Site treatment :", site, "\n")
  
  di_site <- data_analyse %>% filter(site == !!site)
  IAS_list <- setdiff(unique(di_site$cd_ref), "native")
  
  paires_list <- list()
  # Loop for species on the site
  for (ee in IAS_list) {
    paires_list[[ee]] <- fct_paired_plot(di_site, ee, 10, 100, 1000)
    # Avoid keeping NULL or empty values
    if (is.null(paires_list[[ee]]) || nrow(paires_list[[ee]]) == 0) {
      paires_list[[ee]] <- NULL
    }
  }
  
  # Fusionner les résultats en un seul dataframe pour ce site
  df_site <- bind_rows(paires_list, .id = "cd_ref")
  
  # Ajouter la colonne site si nécessaire
  if (nrow(df_site) > 0) {
    df_site <- df_site %>%
      mutate(site = site) %>%
      select(site, everything())
  }
  
  # Stocker le dataframe dans la liste par site
  paires_per_site[[site]] <- df_site
}

paires_AIS_target_all <- bind_rows(paires_per_site, .id = "site")
paires_AIS_target_all$paired_id <- 1:nrow(paires_AIS_target_all)
rownames(paires_AIS_target_all) <- paires_AIS_target_all$paired_id

paires_AIS_target_all <- paires_AIS_target_all %>%
  select(site, paired_id, everything())


# Clean data --------------------------------------------------------------
## Add the vegetation layers names ----
data_pp <- paires_AIS_target_all %>%
  mutate(major_vegetation_type = case_when(
    site %in% c("volcan") ~ "subalpine",
    site %in% c("sterose", "marlon") ~ "rainforest")) %>%
  select(major_vegetation_type, everything())

cd_ref <- data_analyse %>% select(cd_ref, name_sp) %>% distinct(cd_ref, .keep_all = TRUE) 

data_pp <- left_join(data_pp, cd_ref, by = "cd_ref")

## Number of paired plot per vegetation and species ----
nb_pp <- data_pp %>%
  group_by(major_vegetation_type, name_sp) %>%
  summarise(nb_id_unique = n_distinct(paired_id), .groups = "drop") %>% 
  arrange(desc(nb_id_unique))


## Add the elevation ----
### Invaded plots
alti_env <- data_analyse %>% select(unique_id, altitude) %>%
  rename("altitude_inv" = "altitude", 
         "invaded" = "unique_id") %>%
  distinct(invaded, .keep_all = TRUE)

alti_env$invaded <- as.character(alti_env$invaded)
alti_env$altitude_inv <- as.numeric(alti_env$altitude_inv)

data_pp <- left_join(data_pp, alti_env, by = "invaded")

### Non invaded plots
alti_non_env <- data_analyse %>% select(unique_id, altitude) %>%
  rename("altitude_non_inv" = "altitude", 
         "non_invaded" = "unique_id") %>%
  distinct(non_invaded, .keep_all = TRUE)

alti_non_env$non_invaded <- as.character(alti_non_env$non_invaded)
alti_non_env$altitude_non_inv <- as.numeric(alti_non_env$altitude_non_inv)

data_pp <- left_join(data_pp, alti_non_env, by = "non_invaded")


# -------------------------------------------------------------------------


# Clean the script and save -----------------------------------------------

rm(list = setdiff(ls(), "data_pp"))

save.image(file = "code/01_paired_plot/02_prepa_data_paired_plot.RData")
# -------------------------------------------------------------------------


