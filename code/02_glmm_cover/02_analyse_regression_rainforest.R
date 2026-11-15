# Objective: Regression analyses to evaluated the impact of alien species on native species population size in the rainforest vegetation

# Name: Océane Boulesnane-Guengant
# Date: 2025/10/15


# Importation data --------------------------------------------------------
source("code/02_glmm_cover/01_prepa_data_glmm_strata.R")

# -------------------------------------------------------------------------



# Select the major vegetation type ----------------------------------------
major_vegetation_types <- "rainforest"
data_veg <- data_all %>% filter(major_vegetation_type == major_vegetation_types)

# -------------------------------------------------------------------------



# Regression analyses -----------------------------------------------------
## Ardisia crenata ----
### Prepa data 
alien_target <- "Ardisia crenata"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 405 plots

### Models
ARDCRE <- glmmTMB(cover_native ~ (strata + site + habitat) - 1 + (strata : cover_IAS_target) + cover_IAS_other + scale(altitude) + (1|unique_id),
                  family = gaussian(link = "identity"),
                  dispformula = ~ habitat + strata,
                  data = data_sp)

summary(ARDCRE)

sim_res <- simulateResiduals(fittedModel = ARDCRE)
testUniformity(sim_res)


## Boehmeria penduliflora ----
### Prepa data 
alien_target <- "Boehmeria penduliflora"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)

data_sp <- data_sp %>% filter(strata != "herb") # don't have enough plots (2)
length(unique(data_sp$unique_id)) # 32 plots


### Models
BOEPEN <- glmmTMB(cover_native ~ habitat- 1 + cover_IAS_other,
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(BOEPEN)

sim_res <- simulateResiduals(fittedModel = BOEPEN)
testUniformity(sim_res)

## Litsea glutinosa ----
### Prepa data 
alien_target <- "Litsea glutinosa"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 508 plots

### Models

LITGLU <- glmmTMB(cover_native ~ (strata + site + habitat)- 1 + (strata : cover_IAS_target) + cover_IAS_other + scale(altitude) + (1|unique_id),
                  family = gaussian(link = "identity"),
                  dispformula = ~ habitat + strata - 1,
                  data = data_sp)

summary(LITGLU)

sim_res <- simulateResiduals(fittedModel = LITGLU)
testUniformity(sim_res)




## Miconia crenata ----
### Prepa data 
alien_target <- "Miconia crenata"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
data_sp <- data_sp %>% filter(strata != "tree") # don't have enough plots (1)

length(unique(data_sp$unique_id)) # 663 plots

### Models
MICCRE <- glmmTMB(cover_native ~ (strata + site + habitat)- 1 + (strata : cover_IAS_target) + (cover_IAS_other) + scale(altitude) + (1|unique_id),
                  family = gaussian(link = "identity"),
                  dispformula = ~ habitat + site,
                  ziformula = ~ cover_IAS_target,
                  data = data_sp)

summary(MICCRE)

sim_res <- simulateResiduals(fittedModel = MICCRE)
testUniformity(sim_res)

### Plot 

eff_cible <- ggpredict(MICCRE, terms = c("cover_IAS_target  [all]", 
                       "strata [herb]"))
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "firebrick") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "#CD5C5C") +                   
  labs(x = "Cover of Miconia crenata (%)", 
       y = "Predicted native species cover (%)") +
  # theme_minimal(base_size = 12) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14))

## Psidium cattleyanum ----
### Prepa data 
alien_target <- "Psidium cattleyanum"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 792 plots

### Models
PSICAT <- glmmTMB(cover_native_beta ~ (strata + site + habitat)- 1 + (strata : cover_IAS_target) + cover_IAS_other + scale(altitude) + (1|unique_id),
                  family = beta_family(link = "logit"),
                  dispformula = ~ habitat + strata,	
                  data = data_sp)


summary(PSICAT)

sim_res <- simulateResiduals(fittedModel = PSICAT)
testUniformity(sim_res) # visual acceptation

## Schinus terebinthifolius ----
### Prepa data 
alien_target <- "Schinus terebinthifolia"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
data_sp <- data_sp %>% filter(strata != "herb") # don't have enough plots (2)

length(unique(data_sp$unique_id)) # 40 plots

### Models
SCHTER <- glmmTMB(cover_native ~ strata + strata : cover_IAS_target + cover_IAS_other+ (1|unique_id),
                  family = gaussian(link = "identity"),
                  data = data_sp)

summary(SCHTER)

sim_res <- simulateResiduals(fittedModel = SCHTER)
testUniformity(sim_res)

## Syzygium jambos ----
### Prepa data 
alien_target <- "Syzygium jambos"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)
length(unique(data_sp$unique_id)) # 272 plots

### Models
SYZJAM <- glmmTMB(cover_native_beta ~ (strata + site + habitat)- 1 + (strata : cover_IAS_target) + cover_IAS_other + scale(altitude) + (1|unique_id),
                  family = beta_family(link = "logit"),
                  dispformula = ~ strata + habitat,
                  ziformula = ~ cover_IAS_target,
                  data = data_sp)



summary(SYZJAM)

sim_res <- simulateResiduals(fittedModel = SYZJAM)
testUniformity(sim_res) # visual acceptation

## Rubus alceifolius ----
### Prepa data 
alien_target <- "Rubus alceifolius"
data_sp <- data_veg %>% filter(name_sp == alien_target)
data_sp <- droplevels(data_sp)

data_sp <- data_sp %>% filter(strata != "tree") # don't have enough plots (4)

length(unique(data_sp$unique_id)) # 599 plots

### Models
RUBALC <- glmmTMB(sqrt(cover_native) ~ (strata + site + habitat)- 1 + (strata : cover_IAS_target) + cover_IAS_other + (1|unique_id),
                  family = gaussian(link = "identity"),
                  dispformula = ~habitat + site,
                  data = data_sp)




summary(RUBALC)

sim_res <- simulateResiduals(fittedModel = RUBALC)
testUniformity(sim_res) # visual acceptation




# -------------------------------------------------------------------------



# All models --------------------------------------------------------------
## List of the model ----
mes_mods <- list(
  Ardisia_crenata	=	ARDCRE,
  Boehmeria_penduliflora =	BOEPEN,
  Litsea_glutinosa =	LITGLU,
  Miconia_crenata	=	MICCRE,
  Psidium_cattleyanum	=	PSICAT,
  Schinus_terebinthifolius =	SCHTER,
  Syzygium_jambos	=	SYZJAM,
  Rubus_alceifolius	=	RUBALC)



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

kable(coef_df, caption = "Coefficients of models", digits = 3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))                


# -------------------------------------------------------------------------



# save results ------------------------------------------------------------
write.csv2(coef_df, "results/02_glmm_cover/rainforest_results_models_native_cover.csv", row.names = F)

