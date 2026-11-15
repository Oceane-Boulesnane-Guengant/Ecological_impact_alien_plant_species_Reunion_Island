# Objective: calculate the number of plots per species from the simplified version of the standardised systematic sampling protocol

# Name : Océane Boulesnane Guengant
# Date : 2025/11/29



# Import data -------------------------------------------------------------
source("code/00_prepa_data/01_prepa_data_for_analyse.R")
di_complete <- read.csv2("data/02_di_complete_sp.csv", header = T, sep = ";", dec = ";")

# Simplified version of the protocol --------------------------------------
## Subalpine vegetation ----
### Data
subalpine <- data_analyse %>%
  filter(major_vegetation_type == "subalpine")

### Number of plots per species

n_total_plots <- n_distinct(subalpine$unique_id)
subalpine <- subalpine %>%
  group_by(cd_ref) %>%
  summarise(n_releve = n_distinct(unique_id), 
            major_vegetation_type = first(major_vegetation_type),
            name_sp = first(name_sp), .groups = "drop") %>%
  mutate(freq_presence = (n_releve / n_total_plots) * 100) %>%
  ungroup


## Rainforest vegetation ----
### Data
rainforest <- data_analyse %>%
  filter(major_vegetation_type == "rainforest")

### Number of plots per species
n_total_plots <- n_distinct(rainforest$unique_id)


rainforest <- rainforest %>%
  group_by(cd_ref) %>%
  summarise(n_releve = n_distinct(unique_id), 
            major_vegetation_type = first(major_vegetation_type),
            name_sp = first(name_sp), .groups = "drop") %>%
  mutate(freq_presence = (n_releve / n_total_plots) * 100) %>%
  ungroup

nb_plot <- rbind(rainforest, subalpine)
nb_plot <- nb_plot %>%
  mutate(freq_presence = round(freq_presence, 3))

## Save the data ----
write.csv(nb_plot, "results/00_prepa_data/01_nb_plots_species_simplified.csv", row.names = F)



# -------------------------------------------------------------------------


# Complete version of the protocole ---------------------------------------
## Subalpine vegetation ----
### Data
subalpine <- di_complete %>%
  filter(major_vegetation_type == "subalpine") %>% 
  filter(statut == "alien")

### Number of plots per species

n_total_plots <- n_distinct(subalpine$unique_id)
subalpine <- subalpine %>%
  group_by(cd_ref) %>%
  summarise(major_vegetation_type = first(major_vegetation_type), 
            name_sp = first(name_sp), .groups = "drop", 
            n_releve = n_distinct(unique_id)) %>%
  mutate(freq_presence = (n_releve / n_total_plots) * 100) %>%
  ungroup

## Rainforest vegetation ----
### Data
rainforest <- di_complete %>%
  filter(major_vegetation_type == "rainforest") %>% 
  filter(statut == "alien")

### Number of plots per species
n_total_plots <- n_distinct(rainforest$unique_id)
rainforest <- rainforest %>%
  group_by(cd_ref) %>%
  summarise(major_vegetation_type = first(major_vegetation_type), 
            name_sp = first(name_sp), .groups = "drop", 
            n_releve = n_distinct(unique_id)) %>%
  mutate(freq_presence = (n_releve / n_total_plots) * 100) %>%
  ungroup

nb_plot <- rbind(rainforest, subalpine)
nb_plot <- nb_plot %>%
  mutate(freq_presence = round(freq_presence, 3))

## Save the data ----
write.csv(nb_plot, "results/00_prepa_data/01_nb_plots_species_complete.csv", row.names = F)

