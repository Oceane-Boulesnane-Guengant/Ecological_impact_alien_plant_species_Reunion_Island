# Objective: Results of the paired plot analyses (analysis table and graph per vegetation layers)

# Name : Océane Boulesnane-Guengant
# Date: 2026/10/17



# Importation data and function -------------------------------------------

## Library (if needed) ----
source("code/library.R")


## Paired plot data ----
source("code/01_paired_plot/03_prepa_data_paired_plot_strata.R")


## Data for the information ----
source("code/00_prepa_data/01_prepa_data_for_analyse.R")
# -------------------------------------------------------------------------



# Choose the vegetation type and stratum ----------------------------------
## Choose the vegetation type: "rainforest" or "subalpine" ----
major_vegetation_types <- "rainforest"

# -------------------------------------------------------------------------



# Preparation of the data paired plot -------------------------------------
## Select the vegetation and strata choosen ----
data_pp_strata <- data_pp_strata %>% subset(major_vegetation_type == major_vegetation_types)
# data_pp_strata <- data_pp_strata %>% subset(strata == strata_s)

for_name <- data_analyse %>% filter(major_vegetation_type == major_vegetation_types)

full_name <- for_name %>% 
  dplyr::select(name_sp) %>% 
  distinct_all()

## Create a list with the paired plot data of each IAS ----
liste_pp_sp <- list()
for(st in unique(data_pp_strata$strata)) {
  data_strata <- subset(data_pp_strata, strata == st)
  
  for(IAS in unique(data_strata$name_sp)) {
    # Créer un nom d'objet sous la forme p_<code_name_sp>
    obj_name <- paste0(st, "_", IAS)
    
    # Filtrer les données pour cette espèce
    data_sp <- subset(data_strata, name_sp == IAS)
    if (nrow(data_sp) >= 15) {
      liste_pp_sp[[obj_name]] <- data_sp
    }
  }
}

nb_pp <- data_pp_strata %>%
  group_by(major_vegetation_type, name_sp, strata) %>%
  summarise(nb_id_unique = n_distinct(paired_id), .groups = "drop") %>% 
  arrange(desc(nb_id_unique))

nb_pp

## Save as csv. ----
# write.csv2(nb_pp, "results/01_paired_plot/nb_pp_rainforest.csv", row.names = F)
# write.csv2(nb_pp, "results/01_paired_plot/nb_pp_subalpine.csv", row.names = F)

# -------------------------------------------------------------------------



# Wilcoxon results --------------------------------------------------------
## Analysis ----
table_wilco <- lapply(names(liste_pp_sp), function(nom) {
  data <- liste_pp_sp[[nom]]
  
  # Extraire strata et espèce à partir du nom "strata_species"
  parts <- unlist(strsplit(nom, "_"))
  strata <- parts[1]
  species <- paste(parts[-1], collapse = " ")   # si nom d'espèce en plusieurs mots
  
  # Calcul des moyennes pour déterminer le sens de la différence
  median_env <- median(data$cover_native_env, na.rm = TRUE)
  median_non_env <- median(data$cover_native_non_env, na.rm = TRUE)
  
  # Détermination du sens (Positive, Négative, ou Égale si strict égalité)
  direction_diff <- case_when(
    median_env > median_non_env ~ "Positive",
    median_env < median_non_env ~ "Négative",
    TRUE                    ~ "Nulle")
  
  # Test Wilcoxon
  test <- wilcox.test(data$cover_native_env,
                      data$cover_native_non_env,
                      paired = TRUE,
                      exact = FALSE)
  
  tidy(test) %>% 
    mutate(species = species,
           strata = strata, 
           direction = direction_diff)}) %>% 
  bind_rows() %>% 
  relocate(strata, species, method, alternative, .before = everything())

table_wilco$method <- "Wilcoxon test"

table_wilco <- table_wilco %>%
  mutate(p.value = ifelse(p.value < 0.01,
                          format(p.value, scientific = TRUE, digits = 3),   
                          format(round(p.value, 3), nsmall = 3)))

table_wilco <- table_wilco %>% arrange(species)
tab_df(table_wilco, digits = 3)

table_wilco_wide <- table_wilco %>%
  select(species, strata, statistic, p.value) %>%
  pivot_wider(names_from = strata,
              values_from = c(statistic, p.value),
              names_glue = "{strata}_{.value}") %>%
  select(species,
         tree_statistic, tree_p.value,
         shrub_statistic, shrub_p.value,
         herb_statistic, herb_p.value)

tab_df(table_wilco_wide, digits = 3)

## Graphical representation ----
list_pp_sp_veg <- lapply(liste_pp_sp, function(df) {
  df %>% filter(major_vegetation_type == major_vegetation_types)})
list_pp_sp_veg <- Filter(function(df) nrow(df) > 0, list_pp_sp_veg)

list_plot <- list()

for (ee in seq_along(list_pp_sp_veg)) {
  
  data <- list_pp_sp_veg[[ee]]
  
  # Take the name "strata_species"
  nom_complet <- names(list_pp_sp_veg)[ee]
  
  # SSeparate strate and species
  parts <- unlist(strsplit(nom_complet, "_"))
  strata <- parts[1]
  species_code <- paste(parts[-1], collapse = "_")
  
  # Scientific names
  n_esp <- data.frame(name_sp = species_code)
  n_esp <- left_join(n_esp, full_name, by = "name_sp")
  
  nom_esp <- n_esp$name_sp  
  nom_esp_ita <- n_esp$name_sp  
  
  # Plot
  p <- ggpaired(data, 
                cond1 = "cover_native_non_env", 
                cond2 = "cover_native_env",
                line.size = NA,
                fill = "condition",
                palette = c("#2d9d49", "#bb0000")) +
    scale_x_discrete(labels = c("cover_native_non_env" = "Non invaded", 
                                "cover_native_env" = "Invaded")) +
    xlab(bquote(italic(.(nom_esp_ita)))) +
    ylab("Native species cover (%)") +
    labs(title = paste("Strata:", strata),
         fill = "Invasion status") +
    theme(plot.title = element_text(size = 10),
          axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 10),
          legend.position = "none")
  
  # Nom de l'objet dans la liste : "herb_Hypochaeris_radicata"
  list_plot[[nom_complet]] <- p
}

p <- ggarrange(plotlist = list_plot, common.legend = T)
p

# -------------------------------------------------------------------------



# Save data ---------------------------------------------------------------
## Table ----
name_save.csv <- paste0("results/01_paired_plot/", major_vegetation_types, "_boxplot_pp", ".csv")
write.csv2(table_wilco, name_save.csv, row.names = FALSE)


# -------------------------------------------------------------------------



