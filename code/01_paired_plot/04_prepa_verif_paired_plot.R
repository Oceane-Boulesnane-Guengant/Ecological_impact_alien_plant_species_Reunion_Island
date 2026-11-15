# Objective: Data preparation for the verification of the assumption that native species cover in non invaded plot is independent to alien target species cover in invaded plot.

# Name: Océane Boulesnane--Guengant
# Date: 2026/10/27



# Choice of the study area ------------------------------------------------
## Choose between "rainforest" and "subalpine" vegetation type
major_vegetation_type <- "subalpine"
for_name <- major_vegetation_type
# -------------------------------------------------------------------------



# Importation data and function -------------------------------------------
## Paired plot data ----
load("code/01_paired_plot/02_prepa_data_paired_plot.RData")

## Data for the information ----
source("code/00_prepa_data/01_prepa_data_for_analyse.R")

## Function ----
source("code/01_paired_plot/01_function_for_paired_plot.R")

fct_choice_site <- function(major_vegetation_type_s) {
  major_vegetation_type_col <- c("rainforest", "subalpine")
  
  if(major_vegetation_type_s %in% major_vegetation_type_col) {
    return(subset(data_pp, major_vegetation_type == major_vegetation_type_s))
  } else {
    stop("Invalid site name. Choose from : ", 
         paste(major_vegetation_type_col, collapse = ", "))
  }
}

fct_choice_name <- function(major_vegetation_type_s) {
  
  data_liste <- list(major_vegetation_type_col <- c("rainforest", "subalpine"))
  
  if(major_vegetation_type_s %in% major_vegetation_type_col) {
    return(subset(data_analyse, major_vegetation_type == major_vegetation_type_s))
  } else {
    stop("Invalid site name. Choose from : ", 
         paste(major_vegetation_type_col, collapse = ", "))
  }
}
# -------------------------------------------------------------------------



# Preparation data --------------------------------------------------------
## Select the vegetation type choosen ----
data_PP <- fct_choice_site(major_vegetation_type)
for_name <- fct_choice_name(major_vegetation_type)

data_PP$invaded <- as.character(data_PP$invaded)
data_PP$non_invaded <- as.character(data_PP$non_invaded)

## Select only alien species with moreor equal to 15 paired plots ----
### Select species
target_species <- data_PP %>% 
  count(name_sp, name = "nb_paires") %>%
  arrange(desc(nb_paires)) %>%
  filter(nb_paires >= 15) %>%
  pull(name_sp) %>%
  as.character()

### Scientific names of each alien target species
full_name <- for_name %>% 
  dplyr::select(name_sp, cd_ref) %>% 
  distinct(cd_ref, .keep_all = T)

## Long format data ----
# Loop with long format paired plot (per species)
list_pp <- list()
for(IAS_target in target_species) {
  obj_name <- paste0("p_", IAS_target)
  filtered_data <- subset(data_PP, name_sp == IAS_target)
  long_data <- fct_effet_verif(filtered_data)
  
  # Create a name for each long data
  long_obj_name <- paste0("pp_", IAS_target, "_verif")
  list_pp[[long_obj_name]] <- long_data
}

# -------------------------------------------------------------------------



# Linear mixed-effect model -----------------------------------------------
## Model ----
# For each paired plot data invaded by alien target species (in list_pp)
model_lm_list <- list()
for(nom_objet in names(list_pp)) {
  name_sp_code <- sub("pp_(.*)_verif", "\\1", nom_objet)
  data_long <- list_pp[[nom_objet]]
  
  model <- lmerTest::lmer(cover_native ~ cover_IAS_target * condition + (1|paired_id), data = data_long)
  model_lm_list[[name_sp_code]] <- model
}

# -------------------------------------------------------------------------



# Clean script for "05_graph_matrics_across_species.R" -----------------------
objet <- ls()
objet_conserver <- c("target_species", "model_lm_list", "full_name", "list_pp", "major_vegetation_type")
objet_supprimer <- setdiff(objet, objet_conserver)
rm(list = objet_supprimer)
rm(objet, objet_conserver, objet_supprimer)



# -------------------------------------------------------------------------


