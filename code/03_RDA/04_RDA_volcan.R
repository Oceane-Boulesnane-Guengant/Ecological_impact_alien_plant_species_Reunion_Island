# Objective: Assessing the impact of alien species on native composition in the subalpine vegetation

# Name: Océane Boulesnane-Guengant
# Date: 2025/11/30



# Importation data --------------------------------------------------------
source("code/03_RDA/02_prepa_RDA_volcan.R")

# -------------------------------------------------------------------------



# RDA model ---------------------------------------------------------------
## Models ----
rda_model <- rda(data_native ~ Hypochaeris_radicata + Prunella_vulgaris + 
                   Anthoxanthum_odoratum + Erigeron_karvinskianus + Fragaria_vesca + 
                   Carex_pilulifera + Luzula_campestris +
                   Condition(habitat + altitude), data = data_alien)

## Graphical representation ----
ordiplot(rda_model, scaling = 2, 
         type = c("text"), 
         display = c("species", "bp"), 
         choices = c(1, 2))

## Statistics ----
anova.cca(rda_model) 
anova.cca(rda_model, by = "term")

summary(rda_model)

# -------------------------------------------------------------------------



# Calculation of the variance inflation factors ---------------------------
## Calculation of the vif ----
vif_values <- vif.cca(rda_model)
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

## Graphical representation ----
ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_col(fill = "#66c2a5") +
  coord_flip() +
  labs(title = "VIF des variables du modèle RDA",
       x = "Variables explicatives",
       y = "VIF") +
  theme_minimal()

# -------------------------------------------------------------------------



# Graphical representation ------------------------------------------------
## 1. Extraction of the scores scaling = 2 ----
habitat_strata <- data_alien %>% select(id_strata, habitat, strata)

species_df <- as.data.frame(scores(rda_model, display = "species", scaling = 2))
species_df$label <- rownames(species_df)

site_df <- as.data.frame(scores(rda_model, display = "sites", scaling = 2))
site_df$unique_id <- rownames(site_df)

row.names(habitat_strata) <- habitat_strata$id_strata
habitat_strata <- habitat_strata %>% rename(unique_id = id_strata)

site_df <- site_df %>%
  left_join(habitat_strata, by = "unique_id")
row.names(site_df) <- site_df$unique_id

bp_raw <- scores(rda_model, display = "bp", scaling = 2)
bp_df <- as.data.frame(bp_raw)
bp_df$label <- rownames(bp_df)


## 2. Apply a stretch factor to match ordiplot ----
arrow_scaling_factor <- 0.80

bp_df2 <- bp_df %>%
  mutate(RDA1 = RDA1 * arrow_scaling_factor,
         RDA2 = RDA2 * arrow_scaling_factor)

## 3. Plot ----
p <- ggplot() +
  geom_point(data = species_df, aes(x = RDA1, y = RDA2),
             color = "#2ca24e", size = 2) +
  geom_segment(data = bp_df2, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "#bb0000", size = 0.8) +
  geom_text(data = bp_df2, aes(x = RDA1, y = RDA2, label = label),
            color = "#bb0000",
            fontface = "bold",
            vjust = -0.4, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "RDA 1 (56.32%)", y = "RDA 2 (17.43%)") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 13),  
        axis.title.y = element_text(size = 13), 
        axis.text.x = element_text(size = 13),   
        axis.text.y = element_text(size = 13)) +
  scale_x_continuous(limits = c(-0.65, 0.21),
                     breaks = seq(-0.6, 0.2, by = 0.2),
                     labels = scales::number_format(accuracy = 0.1))

p
# -------------------------------------------------------------------------



# Save plots --------------------------------------------------------------

ggsave("results/03_RDA/subalpine_RDA_plot.png", plot = p,
       width = 8, height = 6, dpi = 300)

# -------------------------------------------------------------------------


