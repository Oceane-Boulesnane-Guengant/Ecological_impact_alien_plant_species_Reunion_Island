# Objective: verification of the assumption that native species cover in non invaded plot is independent to alien target species cover in invaded plot

# Name: Océane Boulesnane-Guengant
# Date: 2026/10/27



# Choice of the study area ------------------------------------------------
## Choose between "rainforest" and "subalpine" vegetation type
major_vegetation_type <- "subalpine"

# -------------------------------------------------------------------------



# Importation data --------------------------------------------------------
source("code/01_paired_plot/04_prepa_verif_paired_plot.R")

# -------------------------------------------------------------------------



# Graphical representation ------------------------------------------------
## Loop for each model (per species) ----
list_plot <- list()
for(i in seq_along(model_lm_list)) {
  
  # 1. dplyr::select data of species i 
  n_sp <- names(model_lm_list)[sapply(model_lm_list, identical, model_lm_list[[i]])] %>% 
    as.data.frame %>% rename("name_sp" = ".") 
  n_sp <- left_join(n_sp, full_name, by = "name_sp")
  
  name_sp <- n_sp$name_sp[1]
  name_esp_ita <-  n_sp$name_sp[1]
  
  # 2. Scientific names of the target alien species
  nom_data <- paste0("pp_", name_sp, "_verif")
  data_plot <- list_pp[[nom_data]]
  
  # 3. Plot
  model_esp <- model_lm_list[[i]]
  eff <- ggpredict(model_esp, terms = c("cover_IAS_target", "condition"))
  
  p <- ggplot(eff, aes(x = x, y = predicted, color = group)) +
    geom_point(data = data_plot, 
               aes(x = cover_IAS_target, y = cover_native, color = condition), 
               alpha = 0.5, size = 1.5) +
    geom_line(linewidth = 1.2) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
                alpha = 0.1, color = NA) +
    scale_color_manual(values = c("#bb0000", "#2d9d49"),
                       labels = c("Invaded", "Non invaded")) +  
    scale_fill_manual(values = c("#bb0000", "#2d9d49"), 
                      labels = c("Invaded", "Non invaded")) +
    labs(x = bquote("Cover of " * italic(.(name_esp_ita))),
         y = "Native species cover (predicted value)",
         color = "Invasion status", 
         fill = "Invasion status") +
    theme_classic()
  
  list_plot[[name_sp]] <- p
  
}

## Graphic ----
p <- ggarrange(plotlist = list_plot, common.legend = TRUE)
p


# -------------------------------------------------------------------------



# Estimate plots ----------------------------------------------------------
## Loop for each model (per species) ----
list_emtrends_result <- list()
for (ee in target_species) {
  model_esp <- model_lm_list[[ee]]
  
  result_esm <- emtrends(model_esp, ~ condition, var = "cover_IAS_target")
  pval_ems <- emmeans::test(result_esm) 
  
  result_esm <- result_esm %>% as.data.frame()
  pval_ems <- pval_ems %>% as.data.frame %>% dplyr::select(t.ratio, p.value)
  
  table_ems <- cbind(result_esm, pval_ems)
  table_ems
  
  name_model <- ee
  list_emtrends_result[[name_model]] <- table_ems
}

## Result ----
table_emtrends_all <- bind_rows(
  lapply(names(list_emtrends_result), function(nom) {
    df <- list_emtrends_result[[nom]]
    df$Model <- nom
    return(df)
  })) %>%
  relocate(Model) %>%
  mutate(cover_IAS_target.trend = signif(cover_IAS_target.trend, 3),
         SE = signif(SE, 4),
         df = signif(df, 4),
         lower.CL = signif(lower.CL, 4),
         upper.CL = signif(upper.CL, 4),
         t.ratio = signif(t.ratio, 4),
         p.value = ifelse(p.value < 0.001, sprintf("%.2e", p.value),
                          format(round(p.value, 4), nsmall = 4)))

table <- table_emtrends_all %>% filter(!(condition == "invaded"))
tab_df(table, digits = 3)



# -------------------------------------------------------------------------




