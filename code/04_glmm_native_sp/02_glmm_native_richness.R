
# Objective: Regression analyses to evaluate the relationship between native species richness and alien species cover in each stratum in the rainforest and subalpine vegetations

# Name: Océane Boulesnane-Guengant
# Date: 2025/11/29



# Importation -------------------------------------------------------------
source("code/04_glmm_native_sp/01_prepa_data_glmm_native.R")
# -------------------------------------------------------------------------




# Sainte Rose -------------------------------------------------------------
## Litsea glutinosa ----

IAS_target <- "Litsea glutinosa"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)

# Keep only tree stratum (in other strata, target alien species don't have cover above 10%)
data_esp <- data_esp %>% filter(strata == "tree")
length(unique(data_esp$unique_id)) # 45 plots
LITGLU0 <-	glmmTMB(sp_richness ~ cover_IAS_other + cover_IAS_target + habitat - 1,
                  family = poisson(link = "log"),
                  data = data_esp)

LITGLU <-	glmmTMB(sp_richness ~ cover_IAS_other + habitat - 1,
                  family = poisson(link = "log"),
                  data = data_esp)

summary(LITGLU)

sim_res <- simulateResiduals(fittedModel = LITGLU)
testUniformity(sim_res)      

### Plot 

eff_cible <- ggpredict(LITGLU0, terms = "cover_IAS_target  [all]")
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "firebrick") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "#CD5C5C") +                   
  labs(x = "Cover of Litsea glutinosa (%)", 
       y = "Predicted native species richness (%)") +
  # theme_minimal(base_size = 12) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14)) 
## Miconia crenata ----

IAS_target <- "Miconia crenata"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)
length(unique(data_esp$unique_id)) # 133 plots

MICCRE0 <-	glmmTMB(sp_richness ~ cover_IAS_other + (strata : cover_IAS_target) + habitat + strata - 1 + (1|unique_id),
                  family = poisson(link = "log"),
                  data = data_esp)

MICCRE <-	glmmTMB(sp_richness ~ cover_IAS_other + habitat + strata - 1 + (1|unique_id),
                  family = poisson(link = "log"),
                  data = data_esp)

summary(MICCRE0)

sim_res <- simulateResiduals(fittedModel = MICCRE0)
testUniformity(sim_res)     

### Plot 
eff_cible <- ggpredict(MICCRE0, terms = c("cover_IAS_target  [all]", 
                                         "strata [herb]"))
eff_cible <- ggpredict(MICCRE0, terms = "cover_IAS_target  [all]") 
                                          
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "#316588") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "#316588") +                   
  labs(x = "Cover of Miconia crenata (%)", 
       y = "Predicted native species richness (%)") +
  # theme_minimal(base_size = 12) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14))
  # ylim(0, 8)

## Psidium cattleyanum ----

IAS_target <- "Psidium cattleyanum"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)
length(unique(data_esp$unique_id)) # 108 plots


PSICAT <-	glmmTMB(sp_richness ~ strata:cover_IAS_target + cover_IAS_other + habitat + strata - 1 + (1|unique_id),
                  family = poisson(link = "log"),
                  data = data_esp)

summary(PSICAT)

sim_res <- simulateResiduals(fittedModel = PSICAT)
testUniformity(sim_res)      

## Rubus alceifolius ----
IAS_target <- "Rubus alceifolius"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)
length(unique(data_esp$unique_id)) # 140 plots

RUBALC <-	glmmTMB(sp_richness ~ strata:cover_IAS_target + cover_IAS_other  + habitat + strata- 1 + (1|unique_id),
                  family = poisson(link = "log"),
                  data = data_esp)

summary(RUBALC)

sim_res <- simulateResiduals(fittedModel = RUBALC)
testUniformity(sim_res)



# -------------------------------------------------------------------------



# Volcan  -----------------------------------------------------------------
## Anthoxanthum odoratum ----
IAS_target <- "Anthoxanthum odoratum"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)
length(unique(data_esp$unique_id)) # 75 plots


ANTODO <-	glmmTMB(sp_richness ~ cover_IAS_target + cover_IAS_other + habitat - 1,
                  ziformula = ~ cover_IAS_other,
                  family = poisson(link = "log"),
                  data = data_esp)


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
       y = "Predicted native species richness (%)") +
  # theme_minimal(base_size = 12) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14)) 

## Erigeron karvinskianus ----
IAS_target <- "Erigeron karvinskianus"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)
length(unique(data_esp$unique_id)) # 30 plots

ERIKAR <-	glmmTMB(sp_richness ~ cover_IAS_target  + habitat - 1,
                  family = poisson(link = "log"),
                  data = data_esp)


summary(ERIKAR)

sim_res <- simulateResiduals(fittedModel = ERIKAR)
testUniformity(sim_res)

### Plot 

eff_cible <- ggpredict(ERIKAR, terms = "cover_IAS_target  [all]")
plot(eff_cible)
df <- as.data.frame(eff_cible)

ggplot(df, aes(x = x, y = predicted)) +
  geom_line(size = 1.2, color = "firebrick") +            
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "#CD5C5C") +                   
  labs(x = "Cover of Erigeron karvinskianus (%)", 
       y = "Predicted native species richness (%)") +
  # theme_minimal(base_size = 12) +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14)) +
  ylim(0, 6) + xlim(0, 100)

## Hypochaeris radicata ----
IAS_target <- "Hypochaeris radicata"
data_esp <- data_all %>% filter(name_sp == IAS_target)
data_esp <- droplevels(data_esp)
length(unique(data_esp$unique_id)) # 68 plots

HYPRAD <-	glmmTMB(sp_richness ~ cover_IAS_target + cover_IAS_other + habitat - 1,
                  family = poisson(link = "log"),
                  data = data_esp)
summary(HYPRAD)

sim_res <- simulateResiduals(fittedModel = HYPRAD)
testUniformity(sim_res)



# -------------------------------------------------------------------------



# All models --------------------------------------------------------------
## List od the model ----
mes_mods <- list(
  Litsea_glutinosa = LITGLU,
  Miconia_crenata = MICCRE,
  Psidium_cattleyanum = PSICAT,
  Rubus_alceifolius = RUBALC,
  Anthoxanthum_odoratum = ANTODO,
  Erigeron_karvinskianus = ERIKAR,
  Hypochaeris_radicata = HYPRAD)

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
write.csv2(coef_df, "results/04_glmm_native_sp/results_models_native_richness.csv", row.names = F)

# -------------------------------------------------------------------------


