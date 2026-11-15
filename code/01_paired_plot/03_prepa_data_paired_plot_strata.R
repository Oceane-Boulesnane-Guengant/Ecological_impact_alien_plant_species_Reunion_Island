# objective: Regression analyses to evaluate the relationship between native species cover and alien species cover in each stratum in the rainforest vegetation

# Name: Océane Boulesnane-Guengant
# Date: 2026/10/27

# Importation data and function -------------------------------------------
## Paired plot data ----
load("code/01_paired_plot/02_prepa_data_paired_plot.RData")


## Data for the information ----
source("code/00_prepa_data/01_prepa_data_for_analyse.R")


# -------------------------------------------------------------------------



# Preparation of non invaded plots ----------------------------------------
## Non invaded plots ----
### (plots with < 2% of alien species cover)
non_invaded <- data_analyse %>% 
  group_by(unique_id, cd_ref) %>% 
  summarise(cover_tot = sum(cover), .groups = "drop") %>% 
  filter(cd_ref != "native") %>% 
  filter(cover_tot <= 2)


## Native cover per stratum -----
cover_native_strata <- data_analyse %>%
  filter(cd_ref == "native") %>%
  select(unique_id, strata, cover) %>%
  rename(cover_native = cover)


# -------------------------------------------------------------------------



# Information of invaded plots --------------------------------------------
## Retrieve the unique_ids of the invaded plot ----
id_pp_env <- data_pp %>% 
  select(paired_id, invaded) %>%
  rename(unique_id = invaded)


## Retrieve the native cover for these unique_ids ----
pp_env_stra <- cover_native_strata %>% 
  filter(unique_id %in% unique(id_pp_env$unique_id)) %>%
  rename(cover_native_env = cover_native)


## Add the paired id ----
pp_env_stra <- merge(pp_env_stra, id_pp_env, by = "unique_id")
pp_env_stra <- pp_env_stra %>% rename(id_env = unique_id)

# -------------------------------------------------------------------------



# Information of non invaded plots ----------------------------------------
## Retrieve the unique_ids of the non invaded plot ----
id_pp_non_inv <- data_pp %>% 
  select(paired_id, non_invaded) %>% 
  rename(unique_id = non_invaded)

## Retrieve the native cover for these unique_ids ----
pp_non_inv_stra <- cover_native_strata %>% 
  filter(unique_id %in% unique(id_pp_non_inv$unique_id)) %>%
  rename(cover_native_non_env = cover_native)

## Add paired id ----
pp_non_inv_stra <- left_join(id_pp_non_inv, pp_non_inv_stra, by = "unique_id", relationship = "many-to-many")

pp_non_inv_stra <- pp_non_inv_stra %>% 
  distinct(paired_id, unique_id, strata, cover_native_non_env) %>%
  rename(id_non_env = unique_id)

# -------------------------------------------------------------------------



# Combine invaded and non invaded plots  ----------------------------------
## Add the strata ----
data_strate <- right_join(pp_non_inv_stra, pp_env_stra, by = c("paired_id", "strata"))
data_strate <- data_strate %>% drop_na()



## Add information -----
info2 <- data_pp %>%
  select(paired_id, major_vegetation_type, site, name_sp, habitat_invaded, habitat_non_invaded, altitude_inv, altitude_non_inv)

data_pp_strata <- left_join(data_strate, info2, by = "paired_id")

# -------------------------------------------------------------------------



# Clean the data ----------------------------------------------------------
rm(list = setdiff(ls(), "data_pp_strata"))

# -------------------------------------------------------------------------