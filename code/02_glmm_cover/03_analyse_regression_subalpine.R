# Objective: Regression analyses to evaluate the relationship between native species cover and alien species cover in each stratum in the subalpine vegetation

# Name: Océane Boulesnane-Guengant
# Date: 2025/11/29



# Importation data --------------------------------------------------------
source("code/02_glmm_cover/01_prepa_data_glmm_strata.R")

# -------------------------------------------------------------------------



# Select the major vegetation type ----------------------------------------
major_vegetation_types <- "subalpine"
data_veg <- data_all %>% filter(major_vegetation_type == major_vegetation_types)

# -------------------------------------------------------------------------



# Regression analyses -----------------------------------------------------
## Agrostis capillaris ----
### Prepa data 
alien_target <- "Agrostis capillaris"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 40 plots

### Models
ARGCAP <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + cover_IAS_target,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(ARGCAP)

sim_res <- simulateResiduals(fittedModel = ARGCAP)
testUniformity(sim_res)

## Anthoxanthum odoratum ----
### Prepa data 
alien_target <- "Anthoxanthum odoratum"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 1106 plots

### Models
ANTODO <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + ns(cover_IAS_target, 2) + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)


summary(ANTODO)

sim_res <- simulateResiduals(fittedModel = ANTODO)
testUniformity(sim_res)

### Plot 

eff_cible <- ggpredict(ANTODO, terms = "cover_IAS_target  [all]")
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "firebrick") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "#CD5C5C") +                   
  labs(x = "Cover of Anthoxanthum odoratum (%)", 
       y = "Predicted native species cover (%)") +
  # theme_minimal(base_size = 12) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14))
  
  

which(diff(eff_cible$predicted) < 0)
eff_cible$x[84]
eff_cible[84, c("x", "predicted", "conf.low", "conf.high")]

## Carex pilulifera ----
### Prepa data 
alien_target <- "Carex pilulifera"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 172 plots

### Models
CARPIL <- glmmTMB(log10(cover_native + 1) ~ habitat - 1 + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(CARPIL)

sim_res <- simulateResiduals(fittedModel = CARPIL)
testUniformity(sim_res)


## Danthonia decumbens ----
### Prepa data 
alien_target <- "Danthonia decumbens"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 103 plots

### Models
DANDEC <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + cover_IAS_target + cover_IAS_other + scale(altitude),
                  family = gaussian(link = "identity"),
                  data = data_sp)


summary(DANDEC)

sim_res <- simulateResiduals(fittedModel = DANDEC)
testUniformity(sim_res)

## Erigeron karvinskianus ----
### Prepa data 
alien_target <- "Erigeron karvinskianus"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 300 plots

### Models
ERIKAR <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + ns(cover_IAS_target, 2) + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(ERIKAR)

sim_res <- simulateResiduals(fittedModel = ERIKAR)
testUniformity(sim_res)

### Plot
eff_cible <- ggpredict(ERIKAR, terms = "cover_IAS_target [all]")
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "firebrick") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, fill = "#CD5C5C") +
  labs(x = "Cover of Erigeron karvinskianus (%)", 
       y = "Predicted native species cover (%)") +
  # theme_minimal(base_size = 12)  +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14)) +
  ylim(0, 20)


## Fragaria vesca -----
### Prepa data 
alien_target <- "Fragaria vesca"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 36 plots

### Models
FRAVES <- glmmTMB(cover_native ~ habitat- 1,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(FRAVES)

sim_res <- simulateResiduals(fittedModel = FRAVES)
testUniformity(sim_res)


## Hypochaeris radicata -----
### Prepa data 
alien_target <- "Hypochaeris radicata"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 1450 plots

### Models
HYPRAD <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + ns(cover_IAS_target, 2) + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(HYPRAD)

sim_res <- simulateResiduals(fittedModel = HYPRAD)
testUniformity(sim_res)

### Plot

eff_cible <- ggpredict(HYPRAD, terms = "cover_IAS_target [all]")
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "firebrick") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, fill = "#CD5C5C") +
  labs(
    x = "Cover of Hypochaeris radicata (%)", 
    y = "Predicted native species cover (%)") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14))


## Luzula campestris ----
### Prepa data 
alien_target <- "Luzula campestris"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 263 plots

### Models
LUZCAM <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(LUZCAM)

sim_res <- simulateResiduals(fittedModel = LUZCAM)
testUniformity(sim_res)


## Plantago lanceolata ----
### Prepa data 
alien_target <- "Plantago lanceolata"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 70 plots

### Models
PLALAN <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + cover_IAS_target + scale(altitude),
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(PLALAN)

sim_res <- simulateResiduals(fittedModel = PLALAN)
testUniformity(sim_res)


## Prunella vulgaris ----
### Prepa data 
alien_target <- "Prunella vulgaris"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 218 plots

### Models
PRUVUL <- glmmTMB(log10(cover_native + 1) ~ habitat- 1 + cover_IAS_target + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(PRUVUL)

sim_res <- simulateResiduals(fittedModel = PRUVUL)
testUniformity(sim_res)


## Poa pratensis ---- 
### Prepa data 
alien_target <- "Poa pratensis"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)

data_sp <- data_sp %>% filter(strata != "tree") # don't have enough plots (1)
length(unique(data_sp$unique_id)) # 51 plots

### Models
POAPRA <- glmmTMB(cover_native ~ habitat - 1 + scale(altitude),
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(POAPRA)

sim_res <- simulateResiduals(fittedModel = POAPRA)
testUniformity(sim_res)

## Rumex acetosella ----
### Prepa data 
alien_target <- "Rumex acetosella"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)

data_sp <- data_sp %>% filter(strata != "shrub") # don't have enough plots (1)
length(unique(data_sp$unique_id)) # 62 plots

### Models
RUMACE <- glmmTMB(log10(cover_native + 1) ~ habitat - 1 + scale(altitude),
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(RUMACE)

sim_res <- simulateResiduals(fittedModel = RUMACE)
plot(sim_res)
testUniformity(sim_res)
## Ulex europaeus ---- 
### Prepa data 
alien_target <- "Ulex europaeus"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)

data_sp <- data_sp %>% filter(strata != "tree") # don't have enough plots (1)
length(unique(data_sp$unique_id)) # 69 plots

### Models
ULEEUR <- glmmTMB(log10(cover_native + 1) ~ habitat + strata - 1 + strata:cover_IAS_target + cover_IAS_other + (1|unique_id),
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(ULEEUR)

sim_res <- simulateResiduals(fittedModel = ULEEUR1)
testUniformity(sim_res)
# -------------------------------------------------------------------------



# All models --------------------------------------------------------------
## List od the model ----
mes_mods <- list(
  Agrostis_capillaris = ARGCAP,
  Anthoxanthum_odoratum = ANTODO,
  Carex_pilulifera = CARPIL,
  Danthonia_decumbens = DANDEC,
  Erigeron_karvinskianus = ERIKAR,
  Fragaria_vesca = FRAVES,
  Hypochaeris_radicata = HYPRAD,
  Luzula_campestris = LUZCAM,
  Plantago_lanceolata = PLALAN,
  Prunella_vulgaris = PRUVUL,
  Poa_pratensis = POAPRA,
  Rumex_acetosella = RUMACE,
  Ulex_europaeus = ULEEUR)

## All results in one table ----
coef_tables <- lapply(mes_mods, tidy, conf.int = F)

coef_tables <- lapply(names(mes_mods), function(name) {
  tb <- tidy(mes_mods[[name]], conf.int = F)
  tb$model <- name
  return(tb)
})

coef_df <- bind_rows(coef_tables)
coef_df <- coef_df %>%
  select(model, everything())

coef_df$p.value <- format(coef_df$p.value, scientific = TRUE, digits = 4)

kable(coef_df, caption = "Coefficients des modèles", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))  

# -------------------------------------------------------------------------



# save results ------------------------------------------------------------
write.csv2(coef_df, "results/02_glmm_cover/subalpine_results_models_native_cover.csv", row.names = F)

# -------------------------------------------------------------------------



