# Objective: Prepare the data from the simplified version of the standardised systematic sampling protocol

# Name : Océane Boulesnane Guengant
# Date : 2025/11/29



# Importation data --------------------------------------------------------
di_esp <- read.csv("data/01_di_simplified_sp.csv", header = T, sep = ";", dec = ",")
di_rel <- read.csv("data/01_di_simplified_rel.csv", header = T, sep = ";", dec = ",")

# Clean data
di_rel$lat <- as.numeric(gsub(" ", "", di_rel$lat))
di_rel$long <- as.numeric(gsub(" ", "", di_rel$long))

# -------------------------------------------------------------------------



# Preparation of the data -------------------------------------------------
## Native species ----
data_native <- di_rel %>% select(major_vegetation_type, site, unique_id, native_cover_herb, native_cover_shrub, native_cover_tree)
data_native <- data_native %>%
  pivot_longer( cols = starts_with("native_cover_"),
                names_to = "strata",
                values_to = "cover") %>%
  mutate(strata = sub("native_cover_", "", strata))

data_native$cd_name <- "native"
data_native$cd_ref <- "native"
data_native$name_sp <- "native"

## Add native species data to alien species ----
data_analyse <- rbind(di_esp, data_native)

### Delete line with cover == 0
data_analyse <- data_analyse %>% filter(cover != 0)

## Add environnemental information ----
data_enviro <- di_rel %>% select(unique_id, habitat, altitude, lat, long)
data_analyse <- left_join(data_analyse, data_enviro, by = "unique_id")

### Delete line with NA
data_analyse <- data_analyse %>%
  drop_na()

# -------------------------------------------------------------------------



# Clean the script --------------------------------------------------------

rm(data_enviro)
rm(data_native)
rm(di_esp)
rm(di_rel)

# -------------------------------------------------------------------------


