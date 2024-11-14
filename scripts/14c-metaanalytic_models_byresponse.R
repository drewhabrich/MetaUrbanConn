## HEADER---------------------------
## Script name: 14-metaanalytic_models_byresponse
##
## Purpose of script:
##
## Author: Andrew Habrich
##
## Date Created: 2024-09-05
## Date last Modified: 2024-09-05
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
#install.packages("pacman")
pacman::p_load(devtools, R.rsp, ape, phytools, flextable,
               tidyverse, patchwork, ggpubr, RColorBrewer, easystats,
               metafor, orchaRd, emmeans, ggpubr)

## 2. Read in data -----------------
es_data <- read_csv("data/14-effectsize_data_pooled.csv") %>% 
  ## remove 'fungus' and convert Arthropoda to invertebrates
  filter(Study_class != "fungus") %>% 
  mutate(Study_class = ifelse(Study_class == "arthropoda", "invertebrates", Study_class)) %>% 
  ## remove Conn_feat with <3 entries
  group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup()

## 3. Multivariate models by response metric ###################################
### 3.1 Species richness ------------------------------------------------------
es_dat_rich <- es_data %>% filter(r_metric == "species richness") %>% 
  filter(!is.na(absLat)) %>% 
  ## remove Conn_feat that have less than 3 entries
  group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup() %>% 
  ## remove study_class with <4 entries and rename arthropoda to invertebrates
  mutate(Study_class = ifelse(Study_class == "arthropoda", "invertebrates", Study_class)) %>% 
  group_by(Study_class) %>% filter(n() >= 4) %>% ungroup()

## construct a variance-covariance matrix for the species richness data
Vrich <- vcalc(vi = vi, cluster = studyID, obs = ES_no, data = es_dat_rich, rho = c(0.5),
               checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

## check correlations of variables
ggcorrplot::ggcorrplot(correlation(es_dat_rich %>% 
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, 
                                            Age_SD, greenbuiltratio, elevavg_m, green_avg, 
                                            green_km2_log, built_km2_log, prop_green_log)), 
                       type = "lower", lab = T, lab_size = 2) #looks fine

## generate a model for species richness
mod_rich0 <- rma.mv(yi = yi, V = Vrich, #NULL
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_richA <- rma.mv(yi = yi, V = Vrich, #FULL
                    mods = ~ factor(Conn_feat) + factor(Study_class) + 
                      area_km2_log + pop_dens_log + 
                      absLat + green_avg + Age_mean -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_richB <- update(mod_rich0, ~ . + factor(Study_class) -1)
mod_richC <- update(mod_rich0, ~ . + factor(Conn_feat) -1)
mod_richD <- update(mod_rich0, ~ . + absLat -1)
mod_richE <- update(mod_rich0, ~ . + green_avg -1)
mod_richF <- update(mod_rich0, ~ . + Age_mean -1)
mod_richG <- update(mod_rich0, ~ . + area_km2_log -1)
mod_richH <- update(mod_rich0, ~ . + pop_dens_log -1)
mod_richI <- update(mod_rich0, ~ . + factor(Study_class) + Age_mean -1)
mod_richJ <- update(mod_rich0, ~ . + factor(Study_class) + green_avg -1)
mod_richK <- update(mod_rich0, ~ . + factor(Study_class) + Age_mean + green_avg -1)

fitstats(mod_rich0, mod_richA, mod_richB, mod_richC, mod_richD, mod_richE, 
         mod_richF, mod_richG, mod_richH, mod_richI, mod_richJ, mod_richK, REML = F) #compare model AIC values
## LRT test for model comparison of single variables
anova(mod_richB, mod_richI)

## model summary
sr_fit <- update(mod_richB, method = "REML")
summary(sr_fit)
funnel(sr_fit, yaxis = "seinv")
r2_ml(sr_fit, data = es_dat_rich); i2_ml(sr_fit)
profile(sr_fit)

## robust parameter estimation
robust(sr_fit, cluster = studyID, clubSandwich = T)

## sensitivity analysis
# rho_range <- c(0.3, 0.5, 0.6, 0.8) ##define the range of rho values to test
# ML_VCV_range <- list() # repeatedly run the specified model with varying rho using a for loop
# for (i in 1:length(rho_range)) {
#   VCV_range <- vcalc(vi = vi, cluster = studyID, obs = ES_no, 
#                      rho = rho_range[i],
#                      data = es_dat_rich) # impute VCV matrix with varying rho
#   ML_VCV_range[[i]] <- rma.mv(yi = yi, 
#                               V = VCV_range, # VCV matrix with varying values of rho. 
#                               mods = factor(Study_class),
#                               random = list(~1 | studyID, 
#                                             ~1 | ES_no), 
#                               method = "REML", 
#                               test = "t", dfs = "contain",
#                               data = es_dat_rich)} # run model with different rho values.
##
## compare the AIC values of the models
#lapply(ML_VCV_range, AIC) #compare AIC values
## OUTPUT INTO A TABLE FOR COMPARISON
#AIC_table <- data.frame(rho = rho_range, AIC = unlist(lapply(ML_VCV_range, AIC)))
## compare parameter estimates of the models
#lapply(ML_VCV_range, coef) #compare parameter estimates

## Orchard and bubble plots
### STUDY CLASS MODEL
sr_class_plot <- orchard_plot(sr_fit, mod = "Study_class", condition.lab = "Taxon class", 
                              tree.order = emmeans(emmprep(sr_fit), ~Study_class) %>% as.data.frame() %>% #arrange by estimated effect
                                arrange(emmean) %>% select(Study_class) %>% pull() %>% str_to_sentence(),
                              angle = 0, xlab = "Correlation coefficient", transfm = "tanh", legend.pos = "none",
                              group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers 
sr_class_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
sr_class_plot$layers[[4]]$aes_params$size <- 1 #change the stroke (border) of mean estimates
sr_class_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
sr_class_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
sr_class_plot
## save to file
ggsave("output/figures/14_orchard_sprich_studyclass.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

### CONNECTIVITY FEATURE MODEL, arranged in declining order of effect size
sr_conn_plot <- orchard_plot(update(mod_richC, method = "REML"), mod = "Conn_feat", condition.lab = "Connectivity measure", 
                             tree.order = emmeans(emmprep(update(mod_richC, method = "REML")), ~Conn_feat) %>% 
                               #arrange by estimated effect
                               as.data.frame() %>% arrange(emmean) %>% select(Conn_feat) %>% pull() %>% str_to_sentence(), 
                             angle = 0, xlab = "Correlation coefficient", transfm = "tanh", colour = F, fill = T, legend.pos = "none",
                             group = "studyID",  k = TRUE, g = F, cb = F) #k = #ofES, g = #ofpapers
sr_conn_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
sr_conn_plot$layers[[4]]$aes_params$size <- 1 #change the size of mean estimates
sr_conn_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
sr_conn_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
sr_conn_plot$layers[[1]]$aes_params$fill <- c("grey") #change the colour of each effect size
sr_conn_plot
## save to file
ggsave("output/figures/14_orchard_sprich_connfeat.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

##combined ggarrange
ggarrange(sr_class_plot, sr_conn_plot, ncol = 2, nrow = 1,
          legend = "bottom", common.legend = T)
ggsave("output/figures/14_orchard_sprich_combo.png", plot = last_plot(), width = 12, height = 6, dpi = 300)

## bubble plots of continuous variables
bubble_plot(update(sr_fit, ~.+green_avg), mod = "green_avg", group = "studyID",
            ylab = "Correlation coefficient", xlab = "Avg. Greeness index", transfm = "tanh")
bubble_plot(update(sr_fit, ~.+Age_mean), mod = "Age_mean", group = "studyID",
            ylab = "Correlation coefficient", xlab = "Mean Urban age", transfm = "tanh")

## Model results at levels of 'greenness'
richmodel <- orchaRd::mod_results(update(sr_fit, ~.+E_GR_AV14), group = "studyID", mod = "Study_class", 
                                  at = list(E_GR_AV14 = c(0.20, 0.40, 0.60)), by = "E_GR_AV14")
orchaRd::orchard_plot(richmodel, xlab = "Effect size Fisher's Z", group = "studyID", angle = 45, g = FALSE, 
                      legend.pos = "top.left", condition.lab = "% Greenness") + 
  theme(legend.direction = "vertical")

## Model results at levels of 'urban age'
richmodel <- orchaRd::mod_results(update(sr_fit, ~.+Age_mean), group = "studyID", mod = "Study_class", 
                                  at = list(Age_mean = c(1, 4, 7)), by = "Age_mean")
orchaRd::orchard_plot(richmodel, xlab = "Effect size Fisher's Z", group = "studyID", angle = 45, g = FALSE, 
                      legend.pos = "top.left", condition.lab = "Mean Urban age") + 
  theme(legend.direction = "vertical")

# caterpillar plot
modSRG_res <- orchaRd::mod_results(sr_fit, mod = "Study_class", group = "studyID")
orchaRd::caterpillars(modSRG_res, mod = "Study_class", group = "studyID", overall = T,
                      transfm = "tanh", xlab = "Correlation coefficient")

#### CONNECTIVITY FEATURE VISUALIZATION
orchard_plot(update(mod_richC, method = "REML"), 
             mod = "Conn_feat",
             condition.lab = "Connectivity feature", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = T)
summary(update(mod_richC, method = "REML"))

### 3.2 Abundance -------------------------------------------------------------
es_dat_abun <- es_data %>% filter(r_metric == "abundance") %>% 
  filter(!is.na(absLat)) %>% 
  ## remove Conn_feat that have less than 3 entries
  group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup() %>% 
  ## remove study_class with <4 entries and rename arthropoda to invertebrates
  mutate(Study_class = ifelse(Study_class == "arthropoda", "invertebrates", Study_class)) %>% 
  group_by(Study_class) %>% filter(n() >= 4) %>% ungroup()

## construct a variance-covariance matrix for the abundance data
Vabun <- vcalc(vi = vi, cluster = studyID, obs = ES_no, data = es_dat_abun, rho = c(0.5),
               checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

## check correlations of variables
ggcorrplot::ggcorrplot(correlation(es_dat_abun %>% 
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, 
                                            Age_SD, greenbuiltratio, elevavg_m, green_avg, green_km2_log, 
                                            built_km2_log, prop_green_log)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for abundance
mod_abun0 <- rma.mv(yi = yi, V = Vabun,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_abun, method = "ML", test = "t", verbose = F)
mod_abunA <- rma.mv(yi = yi, V = Vabun,
                    mods = ~ factor(Conn_feat) + factor(Study_class) + 
                      area_km2_log + pop_dens_log + 
                      absLat + green_avg + Age_mean -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_abun, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_abunB <- update(mod_abun0, ~ . + factor(Study_class) -1)
mod_abunC <- update(mod_abun0, ~ . + factor(Conn_feat) -1)
## multivariate models
mod_abunD <- update(mod_abun0, ~ . + factor(Conn_feat) + factor(Study_class) + pop_dens_log + absLat + green_avg -1)
mod_abunE <- update(mod_abun0, ~ . + factor(Study_class) + absLat -1)
mod_abunF <- update(mod_abun0, ~ . + factor(Conn_feat) + absLat -1)
mod_abunG <- update(mod_abun0, ~ . + factor(Study_class) + green_avg -1)
mod_abunH <- update(mod_abun0, ~ . + factor(Conn_feat) + green_avg -1)
mod_abunI <- update(mod_abun0, ~ . + factor(Study_class) + green_avg + absLat -1)
mod_abunJ <- update(mod_abun0, ~ . + factor(Conn_feat) + green_avg + absLat -1)
mod_abunK <- update(mod_abun0, ~ . + factor(Study_class) + green_avg + absLat + pop_dens_log -1)
mod_abunL <- update(mod_abun0, ~ . + factor(Conn_feat) + green_avg + absLat + pop_dens_log -1)
fitstats(mod_abun0, mod_abunA, mod_abunB, mod_abunC, mod_abunD, mod_abunE, 
         mod_abunF, mod_abunG, mod_abunH, mod_abunI, mod_abunJ, mod_abunK, mod_abunJ, REML = F) #compare model AIC values
anova(mod_abun0, mod_abunB)
anova(mod_abun0, mod_abunC)
anova(mod_abunG, mod_abunI)
anova(mod_abunH, mod_abunJ)

## univariate models
mod_abun1 <- update(mod_abun0, ~ . + absLat)
mod_abun2 <- update(mod_abun0, ~ . + green_avg)
mod_abun3 <- update(mod_abun0, ~ . + Age_mean)
mod_abun4 <- update(mod_abun0, ~ . + area_km2_log)
mod_abun5 <- update(mod_abun0, ~ . + pop_dens_log)
fitstats(mod_abun0, mod_abunB, mod_abunC,
         mod_abun1, mod_abun2, mod_abun3, mod_abun4, mod_abun5, REML = F) #compare model AIC values

## model summary
ab_fit <- update(mod_abunG, method = "REML")
summary(ab_fit)
funnel(ab_fit, yaxis = "seinv")
r2_ml(ab_fit, data = es_dat_abun); i2_ml(ab_fit)
## robust parameter estimation
robust(ab_fit, cluster = studyID, clubSandwich = T, adjust = F)

## Orchard plot
b<-c("Plantae", "Invertebrates", "Arachnida", "Insecta", "Aves")
ab_class_plot <-orchard_plot(ab_fit, mod = "Study_class",condition.lab = "Taxon class", angle = 0,
                             legend.pos = "none",
                             tree.order = b, xlab = "Correlation coefficient", transfm = "tanh", group = "studyID",  k = T, g = F, cb = T) 
ab_class_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
ab_class_plot$layers[[4]]$aes_params$size <- 1 #change the size of mean estimates
ab_class_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
ab_class_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
ab_class_plot
# orchard_plot(ab_fit, mod = "Study_class",condition.lab = "Taxon class", angle = 0,
#              tree.order = emmeans(emmprep(ab_fit), ~Study_class) %>% as.data.frame() %>% #arrange by estimated effect
#                arrange(emmean) %>% select(Study_class) %>% pull() %>% str_to_sentence(),
#              xlab = "Correlation coefficient", transfm = "tanh", group = "studyID",  k = T, g = F, cb = T) 

# save to file
ggsave("output/figures/14_orchard_abun_studyclass.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

## bubble plot
bubble_plot(ab_fit, mod = "green_avg", group = "studyID", legend.pos = "none", transfm = "tanh",
            ylab = "Correlation coefficient", xlab = "Greenness proportion") + # add a red line at 0
  geom_hline(yintercept = 0, linetype = "longdash", color = "red") +
  theme(axis.text.x = element_text(size = 10)) #change the size of the y-axis text
ggsave("output/figures/14_bubble_abun_greenavg.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

## Model results at levels of 'greenness'
abunmodel <- orchaRd::mod_results(ab_fit, group = "studyID", mod = "Study_class", 
                                  at = list(green_avg = c(0.2, 0.4, 0.6)), by = "green_avg")
orchaRd::orchard_plot(abunmodel, xlab = "Correlation coefficient", group = "studyID", angle = 0, g = FALSE, 
                      legend.pos = "bottom.out", condition.lab = "% Greeness", transfm = "tanh") + 
  theme(legend.direction = "horizontal")
ggsave("output/figures/14_orchard_abun_greenavg2.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# caterpillar plot
modABG_res <- orchaRd::mod_results(ab_fit, mod = "Study_class", group = "studyID")
orchaRd::caterpillars(modABG_res, mod = "Study_class", group = "studyID", overall = T,
                      transfm = "tanh", xlab = "Correlation coefficient")

### CONNECTIVITY METRICS MODEL
ab_conn_plot <- orchard_plot(update(mod_abunC, method = "REML"), mod = "Conn_feat", 
                             condition.lab = "Connectivity measure", angle = 0,
                             tree.order = emmeans(emmprep(update(mod_abunC, method = "REML")), ~Conn_feat) %>% as.data.frame() %>% 
                               arrange(emmean) %>% select(Conn_feat) %>% pull() %>% str_to_sentence(),
                             xlab = "Correlation coefficient", transfm = "tanh", 
                             group = "studyID",  k = TRUE, g = F, cb = T) 
ab_conn_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
ab_conn_plot$layers[[4]]$aes_params$size <- 1 #change the size of mean estimates
ab_conn_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
ab_conn_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
ab_conn_plot$layers[[1]]$aes_params$fill <- "grey"
ab_conn_plot

## save to file
ggsave("output/figures/14_orchard_abun_connfeat.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

### 3.3 Occurrence ------------------------------------------------------------
es_dat_occ <- es_data %>% filter(r_metric == "occurrence") %>% 
  filter(!is.na(absLat)) %>% 
  ## remove Conn_feat that have less than 3 entries
  group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup() %>% 
  ## remove study_class with <4 entries and rename arthropoda to invertebrates
  mutate(Study_class = ifelse(Study_class == "arthropoda", "invertebrates", Study_class)) %>% 
  group_by(Study_class) %>% filter(n() >= 4) %>% ungroup()

## construct a variance-covariance matrix for the abundance data
Vocc <- vcalc(vi = vi, cluster = studyID, obs = ES_no, data = es_dat_occ, rho = c(0.5),
              checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

## check correlations of variables
ggcorrplot::ggcorrplot(correlation(es_dat_occ %>% 
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, Age_SD, greenbuiltratio,
                                            elevavg_m, green_avg, green_km2_log, built_km2_log, prop_green_log)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for occurrence
mod_occ0 <- rma.mv(yi = yi, V = Vocc,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "ML", test = "t", verbose = F)
mod_occA <- rma.mv(yi = yi, V = Vocc,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + area_km2_log + pop_dens_log + 
                     absLat + green_avg + Age_mean -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_occB <- update(mod_occ0, ~ factor(Conn_feat) -1)
mod_occC <- update(mod_occ0, ~ factor(Study_class) -1)
## conn_feat with other variables
mod_occD <- update(mod_occ0, ~ factor(Conn_feat) + area_km2_log -1)
mod_occE <- update(mod_occ0, ~ factor(Conn_feat) + pop_dens_log -1)
mod_occF <- update(mod_occ0, ~ factor(Conn_feat) + Pub_year -1)
mod_occG <- update(mod_occ0, ~ factor(Conn_feat) + absLat -1)
mod_occH <- update(mod_occ0, ~ factor(Conn_feat) + green_avg -1)
mod_occI <- update(mod_occ0, ~ factor(Conn_feat) + Age_mean -1)
mod_occJ <- update(mod_occ0, ~ factor(Conn_feat) + Pub_year + area_km2_log + pop_dens_log + absLat + green_avg -1)
## study_class with other variables
mod_occ1 <- update(mod_occ0, ~ factor(Study_class) + area_km2_log -1)
mod_occ2 <- update(mod_occ0, ~ factor(Study_class) + pop_dens_log -1)
mod_occ3 <- update(mod_occ0, ~ factor(Study_class) + Pub_year -1)
mod_occ4 <- update(mod_occ0, ~ factor(Study_class) + absLat -1)
mod_occ5 <- update(mod_occ0, ~ factor(Study_class) + green_avg -1)
mod_occ6 <- update(mod_occ0, ~ factor(Study_class) + Age_mean -1)
mod_occ7 <- update(mod_occ0, ~ factor(Study_class) + Pub_year + area_km2_log + pop_dens_log + absLat + green_avg -1)

## AIC model comparison
fitstats(mod_occ0, mod_occA, mod_occB, mod_occC, mod_occD,
         mod_occE, mod_occF, mod_occG, mod_occH, mod_occI, mod_occJ, REML = F) #compare model AIC values
fitstats(mod_occ0, mod_occA, mod_occB, mod_occC, mod_occ1,
         mod_occ2, mod_occ3, mod_occ4, mod_occ5, mod_occ6, mod_occ7, REML = F) #compare model AIC values

## Orchard plots for the null, taxa, and conn_feat models
orchard_plot(update(mod_occ0, method = "REML"), mod = "1", group = "studyID",
             xlab = "Correlation coefficient", transfm = "tanh", k = TRUE, g = F, cb = T)
orchard_plot(update(mod_occB, method = "REML"), mod = "Conn_feat", condition.lab = "", angle = 0,
             tree.order = emmeans(emmprep(update(mod_occB, method = "REML")), ~Conn_feat) %>% 
               as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% select(Conn_feat) %>% pull() %>% str_to_sentence(),
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers
orchard_plot(update(mod_occC, method = "REML"), mod = "Study_class", condition.lab = "", angle = 0,
             tree.order = emmeans(emmprep(update(mod_occC, method = "REML")), ~Study_class) %>% 
               as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% select(Study_class) %>% pull() %>% str_to_sentence(),
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers

## model summary
occ_fit <- update(mod_occB, method = "REML")
summary(occ_fit)
funnel(occ_fit, yaxis = "seinv")
r2_ml(occ_fit, data = es_dat_occ); i2_ml(occ_fit)
## robust parameter estimation
robust(occ_fit, cluster = studyID)

## Orchard and bubble plots
occ_conn_plot <- orchard_plot(occ_fit, mod = "Conn_feat", condition.lab = "Connectivity feature", angle = 0,
                              tree.order = emmeans(emmprep(occ_fit), ~Conn_feat) %>% as.data.frame() %>% 
                                arrange(emmean) %>% select(Conn_feat) %>% pull() %>% str_to_sentence(),
                              xlab = "Correlation coefficient", transfm = "tanh", legend.pos = "bottom.out",
                              group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers
occ_conn_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
occ_conn_plot$layers[[4]]$aes_params$size <- 1 #change the stroke (border) of mean estimates
occ_conn_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
occ_conn_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
occ_conn_plot
## save figure to file
ggsave("output/figures/14_orchard_occ_connfeat.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

### univariate models
# area_km2_log + pop_dens_log + absLat + green_avg + Age_mean -1
fitstats(update(mod_occ0, ~ . + area_km2_log),
         update(mod_occ0, ~ . + pop_dens_log),
         update(mod_occ0, ~ . + absLat),
         update(mod_occ0, ~ . + green_avg),
         update(mod_occ0, ~ . + Age_mean),
         mod_occ0)

### 3.4 Genetic distance ------------------------------------------------------
es_dat_gen <- es_data %>% filter(r_metric == "genetic similarity") %>% 
  filter(!is.na(absLat)) %>% 
  ## remove Conn_feat that have less than 3 entries
  group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup() %>% 
  ## remove study_class with <4 entries and rename arthropoda to invertebrates
  mutate(Study_class = ifelse(Study_class == "arthropoda", "invertebrates", Study_class)) %>% 
  group_by(Study_class) %>% filter(n() >= 4) %>% ungroup()

## construct a variance-covariance matrix for the abundance data
Vgene <- vcalc(vi = vi, cluster = studyID, obs = ES_no, data = es_dat_gen, rho = c(0.5),
               checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

## check correlations of variables
ggcorrplot::ggcorrplot(correlation(es_dat_gen %>% 
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, Age_SD, 
                                            greenbuiltratio, elevavg_m, green_avg, green_km2_log, 
                                            built_km2_log, prop_green_log)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for genetic distance
mod_gen0 <- rma.mv(yi = yi, V = Vgene,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "ML", test = "t", verbose = F)
mod_genA <- rma.mv(yi = yi, V = Vgene,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + area_km2_log + pop_dens_log + absLat + green_avg -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_genB <- update(mod_gen0, ~ factor(Conn_feat))
mod_genC <- update(mod_gen0, ~ factor(Study_class))
## conn_feat with other variables
mod_genD <- update(mod_gen0, ~ factor(Conn_feat) + area_km2_log -1)
mod_genE <- update(mod_gen0, ~ factor(Conn_feat) + pop_dens_log -1)
mod_genF <- update(mod_gen0, ~ factor(Conn_feat) + Pub_year -1)
mod_genG <- update(mod_gen0, ~ factor(Conn_feat) + absLat -1)
mod_genH <- update(mod_gen0, ~ factor(Conn_feat) + green_avg -1)
mod_genI <- update(mod_gen0, ~ factor(Conn_feat) + Age_mean + Pub_year -1)
mod_genJ <- update(mod_gen0, ~ factor(Conn_feat) + Age_mean -1)
mod_genK <- update(mod_gen0, ~ factor(Conn_feat) + Pub_year + area_km2_log + pop_dens_log + absLat + green_avg -1)

##univariate models
mod_gen1 <- update(mod_gen0, ~ area_km2_log)
mod_gen2 <- update(mod_gen0, ~ pop_dens_log)
mod_gen3 <- update(mod_gen0, ~ Pub_year)
mod_gen4 <- update(mod_gen0, ~ absLat)
mod_gen5 <- update(mod_gen0, ~ green_avg)
mod_gen6 <- update(mod_gen0, ~ Age_mean)
mod_gen7 <- update(mod_gen0, ~ Pub_year + area_km2_log + pop_dens_log + absLat + green_avg)

fitstats(mod_gen0, mod_genA, mod_genB, mod_genC, 
         mod_genD, mod_genE, mod_genF, mod_genG, mod_genH, mod_genI, mod_genJ, mod_genK, REML = F) #compare model AIC values
fitstats(mod_gen0, mod_genA, mod_genB, mod_genC, 
         mod_gen1, mod_gen2, mod_gen3, mod_gen4, mod_gen5, mod_gen6, mod_gen7, REML = F) #compare model AIC values

## model summary
gene_fit <- update(mod_genJ, method = "REML")
summary(gene_fit)
funnel(gene_fit, yaxis="seinv")
r2_ml(gene_fit, data = es_dat_gen); i2_ml(gene_fit)
## robust parameter estimation
robust(gene_fit, cluster = studyID, clubSandwich = T, adjust = F)

## Orchard plots
gs_conn_plot <- orchard_plot(gene_fit, 
                             mod = "Conn_feat",
                             condition.lab = "Connectivity feature", angle = 0,
                             xlab = "Correlation coefficient", transfm = "tanh", legend.pos = "bottom.out",
                             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers)
gs_conn_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
gs_conn_plot$layers[[4]]$aes_params$size <- 1 #change the stroke (border) of mean estimates
gs_conn_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
gs_conn_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
gs_conn_plot
## save figure to file
ggsave("output/figures/14_orchard_gen_connfeat.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

## bubble plots of continuous variables
bubble_plot(gene_fit, mod = "Age_mean", group = "studyID", transfm = "tanh",
            ylab = "Correlation coefficient", xlab = "Mean Urban age",
            legend.pos = "none") + #add a horizontal line at 0
  scale_x_continuous(breaks = 3:7, labels = c("1980","1960","1940","1920","Pre-1900")) +
  #increase x axis labels size
  theme(axis.text.x = element_text(size = 10)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
## save to file 
ggsave("output/figures/14_bubble_gen_age.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

# caterpillar plot
modGD_res <- orchaRd::mod_results(gene_fit, mod = "Conn_feat", group = "studyID")
orchaRd::caterpillars(modGD_res, mod = "Conn_feat", group = "studyID", overall = T,
                      transfm = "tanh", xlab = "Correlation coefficient")

#### TAXA VISUALIZATION
orchard_plot(update(mod_genC, ~.-1, method = "REML"),
             mod = "Study_class",
             condition.lab = "Taxa", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh",
             group = "studyID",  k = TRUE, g = F)

### 3.5 Beta-diversity --------------------------------------------------------
es_dat_beta <- es_data %>% filter(r_metric == "beta-diversity") %>% 
  filter(!is.na(absLat)) %>% 
  ## remove Conn_feat that have less than 3 entries
  group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup() 

## construct a variance-covariance matrix for the beta-diversity data
Vbeta <- vcalc(vi = vi, cluster = studyID, obs = ES_no, data = es_dat_beta, rho = c(0.5),
               checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

## check correlations of variables
ggcorrplot::ggcorrplot(correlation(es_dat_beta %>% 
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, Age_SD, 
                                            greenbuiltratio, elevavg_m, green_avg, green_km2_log, 
                                            built_km2_log, prop_green_log)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for beta-diversity
mod_beta0 <- rma.mv(yi = yi, V = Vbeta,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_beta, method = "ML", test = "t", verbose = F)
mod_betaA <- rma.mv(yi = yi, V = Vbeta,
                    mods = ~ factor(Conn_feat) + factor(Study_class) + area_km2_log + pop_dens_log + absLat + green_avg -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_beta, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_betaB <- update(mod_beta0, ~ factor(Conn_feat) -1)
mod_betaC <- update(mod_beta0, ~ factor(Study_class) -1)
## conn_feat with other variables
mod_betaD <- update(mod_beta0, ~ factor(Conn_feat) + area_km2_log -1)
mod_betaE <- update(mod_beta0, ~ factor(Conn_feat) + pop_dens_log -1)
mod_betaF <- update(mod_beta0, ~ factor(Conn_feat) + Pub_year -1)
mod_betaG <- update(mod_beta0, ~ factor(Conn_feat) + absLat -1)
mod_betaH <- update(mod_beta0, ~ factor(Conn_feat) + green_avg -1)
mod_betaI <- update(mod_beta0, ~ factor(Conn_feat) + Age_mean + Pub_year -1)
mod_betaJ <- update(mod_beta0, ~ factor(Conn_feat) + Age_mean -1)
mod_betaK <- update(mod_beta0, ~ factor(Conn_feat) + Pub_year + area_km2_log + pop_dens_log + absLat + green_avg -1)

##univariate models
mod_beta1 <- update(mod_beta0, ~ area_km2_log)
mod_beta2 <- update(mod_beta0, ~ pop_dens_log)
mod_beta3 <- update(mod_beta0, ~ Pub_year)
mod_beta4 <- update(mod_beta0, ~ absLat)
mod_beta5 <- update(mod_beta0, ~ green_avg)
mod_beta6 <- update(mod_beta0, ~ Age_mean)

## compare fit statistics
fitstats(mod_beta0, mod_betaA, mod_betaB, mod_betaC, 
         mod_betaD, mod_betaE, mod_betaF, mod_betaG, mod_betaH, mod_betaI, mod_betaJ, mod_betaK, REML = F) #compare model AIC values
fitstats(mod_beta0, mod_betaA, mod_betaB, mod_betaC,
         mod_beta1, mod_beta2, mod_beta3, mod_beta4, mod_beta5, mod_beta6, REML = F) #compare model AIC values

## model summary
beta_fit <- update(mod_betaD, method = "REML")
summary(beta_fit)
funnel(beta_fit, yaxis="seinv")
r2_ml(beta_fit, data = es_dat_beta); i2_ml(beta_fit)
## robust parameter estimation
robust(beta_fit, cluster = studyID)

## Orchard plots
beta_conn_plot <- orchard_plot(beta_fit, 
                               mod = "Conn_feat",
                               condition.lab = "Connectivity feature", angle = 0,
                               xlab = "Correlation coefficient", transfm = "tanh", legend.pos = "bottom.out",
                               group = "studyID",  k = TRUE
) #k = #ofES, g = #ofpapers
beta_conn_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
beta_conn_plot$layers[[4]]$aes_params$size <- 1 #change the stroke (border) of mean estimates
beta_conn_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
beta_conn_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
beta_conn_plot
## save figure to file
ggsave("output/figures/14_orchard_beta_connfeat.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

### bubble plot of the moderator effect
bubble_plot(beta_fit, mod = "area_km2_log", group = "studyID", transfm = "tanh",
            ylab = "Correlation coefficient", xlab = "City area (log)",
            legend.pos = "none") + #add a horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)

## save to file
ggsave("output/figures/14_bubble_beta_area.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

## Taxonomic plot
orchard_plot(update(mod_betaC, method = "REML"),
             mod = "Study_class",
             condition.lab = "Taxa", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh",
             group = "studyID",  k = TRUE, g = F)

############# OTHER RESPONSE METRICS #################
### MOVEMENT ###
mod_move <- rma.mv(yi = yi, V = vcalc(vi = vi, cluster = studyID, obs = ES_no, 
                                      data = es_data %>% filter(r_metric == "movement") %>% filter(!is.na(absLat)),
                                      rho = c(0.5), checkpd = T, nearpd = F),
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_data %>% filter(r_metric == "movement") %>% filter(!is.na(absLat)), 
                   method = "REML", test = "t", verbose = F)
orchard_plot(mod_move, mod = "1", group = "studyID", angle = 90, trunk.size = 1, branch.size = 1.5,
             xlab = "Correlation coefficient", transfm = "tanh", k = F, g = F) + 
  theme(axis.text.y = element_blank()) + xlab("Mean effect") +
  scale_fill_manual(values = "lightgreen") +
  scale_colour_manual(values = "forestgreen")
## orchard plot of conn feat
orchard_plot(update(mod_move, ~factor(Conn_feat) -1), mod = "Conn_feat", group = "studyID", angle = 0, trunk.size = 1, branch.size = 1.5,
             xlab = "Correlation coefficient", transfm = "tanh", k = T, g = F, legend.pos = "none") +
  xlab("Mean effect") 
## orchard plot of study class
orchard_plot(update(mod_move, ~factor(Study_class) -1), mod = "Study_class", group = "studyID", angle = 0, trunk.size = 1, branch.size = 1.5,
             xlab = "Correlation coefficient", transfm = "tanh", k = T, g = F, legend.pos = "none") +
  xlab("Mean effect")

