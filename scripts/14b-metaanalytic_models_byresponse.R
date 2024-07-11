## HEADER---------------------------
## Script name: 14-metaanalytic modelling- by response variable
##
## Purpose of script: Model the effects of moderators on the effects of connectivity on biodiversity. 
## 
##
## Author: Andrew Habrich
##
## Date Created: 2024-07-4
## Date last Modified: 2024-07-11
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list = ls())

## 1. Load relevant packages ###################################################
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
               ape, phytools, flextable)

### for stats
library(tidyverse)
library(easystats)
library(metafor)
library(emmeans)
library(ggpubr)

### Read in data ###############################################################
es_data <- read_csv("data/14-effectsize_data_pooled.csv")
### Approximate the Variance-Covariance Matrix of Dependent Effect Sizes or Outcomes (Used in multilevel meta-analytic models)
## cluster = studyID, subgroup = independent groups in cluster, obs = ES_no
Vpool <- vcalc(vi = vi, cluster = studyID, type = r_metric, obs = ES_no, data = es_data, rho = c(0.5, 0.5),
               checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

## 2. Meta-analytic models by response metric ##################################
#### Generate a general model and check diagnostics
modmv <- rma.mv(yi = yi, V = Vpool,
                random = list(~ 1 | studyID,
                              ~ 1 | ES_no),
                data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)
summary(modmv); funnel(modmv, yaxis = "seinv")
i2_ml(modmv); r2_ml(modmv, data = es_dat_pool)
# generate cluster-robust inference method of variance
robust(modmv, cluster = studyID, clubSandwich = T)
# generate a profile likelihood plot
profile(modmv, progbar = T, plot = T, refline = T)

### 2.1 Univariate model plots for pooled dataset ##############################
orchard_plot(update(modmv, ~factor(r_metric)), mod = "r_metric", group = "studyID",
             condition.lab = "Response metric", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", k = TRUE, g = F, cb = T)
orchard_plot(update(modmv, ~factor(Study_class)), mod = "Study_class", group = "studyID",
             condition.lab = "Taxon class", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", k = TRUE, g = F, cb = T)
orchard_plot(update(modmv, ~factor(Conn_feat)), mod = "Conn_feat", group = "studyID",
             condition.lab = "Connectivity feature", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", k = TRUE, g = F, cb = T)

bubble_plot(update(modmv, ~absLat), mod = "absLat", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Absolute Latitude")
bubble_plot(update(modmv, ~Pub_year), mod = "Pub_year", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Publication year")
bubble_plot(update(modmv, ~area_km2_log), mod = "area_km2_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(Urban area km2)")
bubble_plot(update(modmv, ~pop_dens_log), mod = "pop_dens_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(population/km2)")
bubble_plot(update(modmv, ~E_GR_AV14), mod = "E_GR_AV14", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Avg. Greeness index")
bubble_plot(update(modmv, ~Age_mean), mod = "Age_mean", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Avg. Urban age")
bubble_plot(update(modmv, ~Age_SD), mod = "Age_SD", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Std. dev. Urban age")

## Update the model with the response metric as a moderator
modmv_r <- update(modmv, ~factor(r_metric) -1)
summary(modmv_r)

# Estimate the cook's distance to identify influential studies (NOTE: Takes a long time to run)
cd <- cooks.distance.rma.mv(modmv_r, progbar = T, cluster = studyID, reestimate = T)
plot(cd, type="o", pch=19, xlab="Observed Outcome", ylab="Cook's Distance")
# What rows have a cook's distance greater than 1? 
cd[cd > 1] %>% as.data.frame() %>% arrange(desc(.))
# How many effect sizes are in each of these studies?
es_dat_pool %>% filter(studyID %in% names(cd[cd > 1])) %>% count(studyID) %>% arrange(desc(n))

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
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, Age_SD,
                                            elevavg_m, built15_km2, pop15, BUCAP15, E_GR_AV14, E_GR_AL14, 
                                            SDG_LUE9015,SDG_A2G14,SDG_OS15MX)), 
                       type = "lower", lab = T, lab_size = 2) #looks fine

## generate a model for species richness
mod_rich0 <- rma.mv(yi = yi, V = Vrich, #NULL
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_richA <- rma.mv(yi = yi, V = Vrich, #FULL
                    mods = ~ factor(Conn_feat) + factor(Study_class) + 
                      area_km2_log + pop_dens_log + 
                      absLat + E_GR_AV14 + Age_mean -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_richB <- update(mod_rich0, ~ . + factor(Study_class) -1)
mod_richC <- update(mod_rich0, ~ . + factor(Conn_feat) -1)
mod_richD <- update(mod_rich0, ~ . + absLat -1)
mod_richE <- update(mod_rich0, ~ . + E_GR_AV14 -1)
mod_richF <- update(mod_rich0, ~ . + Age_mean -1)
mod_richG <- update(mod_rich0, ~ . + area_km2_log -1)
mod_richH <- update(mod_rich0, ~ . + pop_dens_log -1)
mod_richI <- update(mod_rich0, ~ . + factor(Study_class) + Age_mean -1)
mod_richJ <- update(mod_rich0, ~ . + factor(Study_class) + E_GR_AV14 -1)
mod_richK <- update(mod_rich0, ~ . + factor(Study_class) + Age_mean + E_GR_AV14 -1)

fitstats(mod_rich0, mod_richA, mod_richB, mod_richC, mod_richD, mod_richE, 
         mod_richF, mod_richG, mod_richH, mod_richI, mod_richJ, mod_richK, REML = F) #compare model AIC values
## LRT test for model comparison of single variables
anova(mod_rich0, mod_richB)

## model summary
sr_fit <- update(mod_richB, method = "REML")
summary(sr_fit)
funnel(sr_fit, yaxis = "seinv")
r2_ml(sr_fit, data = es_dat_rich); i2_ml(sr_fit)
profile(sr_fit)

## extract the number of categories in the moderator variable
srvec <-emmeans(emmprep(sr_fit), ~factor(Study_class), data = es_dat_rich) %>% as.data.frame() %>% arrange(desc(emmean))

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
orchard_plot(sr_fit, 
             mod = "Study_class",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh",
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers

### CONNECTIVITY FEATURE MODEL, arranged in declining order of effect size
orchard_plot(update(mod_richC, method = "REML"), 
             mod = "Conn_feat",
             condition.lab = "Connectivity measure", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers

## bubble plots of continuous variables
bubble_plot(update(sr_fit, ~.+E_GR_AV14), mod = "E_GR_AV14", group = "studyID",
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
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, Age_SD,
                                            elevavg_m, built15_km2, pop15, BUCAP15, E_GR_AV14, E_GR_AL14, 
                                            SDG_LUE9015,SDG_A2G14,SDG_OS15MX)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for abundance
mod_abun0 <- rma.mv(yi = yi, V = Vabun,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_abun, method = "ML", test = "t", verbose = F)
mod_abunA <- rma.mv(yi = yi, V = Vabun,
                    mods = ~ factor(Conn_feat) + factor(Study_class) + 
                      area_km2_log + pop_dens_log + 
                      absLat + E_GR_AV14 + Age_mean -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_abun, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_abunB <- update(mod_abun0, ~ . + factor(Study_class) -1)
mod_abunC <- update(mod_abun0, ~ . + factor(Conn_feat) -1)
mod_abunD <- update(mod_abun0, ~ . + factor(Conn_feat) + factor(Study_class) + pop_dens_log + absLat + E_GR_AV14 -1)
mod_abunE <- update(mod_abun0, ~ . + factor(Study_class) + absLat -1)
mod_abunF <- update(mod_abun0, ~ . + factor(Conn_feat) + absLat -1)
mod_abunG <- update(mod_abun0, ~ . + factor(Study_class) + E_GR_AV14 -1)
mod_abunH <- update(mod_abun0, ~ . + factor(Conn_feat) + E_GR_AV14 -1)
mod_abunI <- update(mod_abun0, ~ . + factor(Study_class) + E_GR_AV14 + absLat -1)
mod_abunJ <- update(mod_abun0, ~ . + factor(Conn_feat) + E_GR_AV14 + absLat -1)
mod_abunK <- update(mod_abun0, ~ . + factor(Study_class) + E_GR_AV14 + absLat + pop_dens_log -1)
mod_abunL <- update(mod_abun0, ~ . + factor(Conn_feat) + E_GR_AV14 + absLat + pop_dens_log -1)

fitstats(mod_abun0, mod_abunA, mod_abunB, mod_abunC, mod_abunD, mod_abunE, 
         mod_abunF, mod_abunG, mod_abunH, mod_abunI, mod_abunJ, mod_abunK, mod_abunJ, REML = F) #compare model AIC values
anova(mod_abun0, mod_abunB)
anova(mod_abun0, mod_abunC)
anova(mod_abunG, mod_abunI)
anova(mod_abunH, mod_abunJ)

## model summary
ab_fit <- update(mod_abunK, method = "REML")
summary(ab_fit)
funnel(ab_fit, yaxis = "seinv")
r2_ml(ab_fit, data = es_dat_abun); i2_ml(ab_fit)
## robust parameter estimation
robust(ab_fit, cluster = studyID, clubSandwich = T, adjust = F)

## Orchard and bubble plots
orchard_plot(ab_fit, 
             mod = "Study_class",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers
bubble_plot(ab_fit, mod = "E_GR_AV14", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Avg. Greeness index")
bubble_plot(ab_fit, mod = "absLat", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Absolute latitude")
bubble_plot(ab_fit, mod = "pop_dens_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(population density per km2)")

bubble_plot(update(ab_fit, ~.+Age_mean), mod = "Age_mean", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Mean urban age")

## Model results at levels of 'greenness'
abunmodel <- orchaRd::mod_results(ab_fit, group = "studyID", mod = "Study_class", 
                                  at = list(E_GR_AV14 = c(0.2, 0.4, 0.6)), by = "E_GR_AV14")
orchaRd::orchard_plot(abunmodel, xlab = "Correlation coefficient", group = "studyID", angle = 45, g = FALSE, 
                      legend.pos = "top.left", condition.lab = "% Greeness") + 
                      theme(legend.direction = "vertical")

# caterpillar plot
modABG_res <- orchaRd::mod_results(ab_fit, mod = "Study_class", group = "studyID")
orchaRd::caterpillars(modABG_res, mod = "Study_class", group = "studyID", overall = T,
                      transfm = "tanh", xlab = "Correlation coefficient")

### CONNECTIVITY METRICS MODEL
orchard_plot(update(mod_abunC, method = "REML"), 
             mod = "Conn_feat",
             condition.lab = "Connectivity measure", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE)

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
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean, Age_SD,
                                            elevavg_m, built15_km2, pop15, BUCAP15, E_GR_AV14, E_GR_AL14, 
                                            SDG_LUE9015,SDG_A2G14,SDG_OS15MX)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for occurrence
mod_occ0 <- rma.mv(yi = yi, V = Vocc,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "ML", test = "t", verbose = F)
mod_occA <- rma.mv(yi = yi, V = Vocc,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + area_km2_log + pop_dens_log + 
                            absLat + E_GR_AV14 + Age_mean -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_occB <- update(mod_occ0, ~ factor(Conn_feat))
mod_occC <- update(mod_occ0, ~ factor(Study_class))

mod_occD <- update(mod_occ0, ~ factor(Conn_feat) + area_km2_log)
mod_occE <- update(mod_occ0, ~ factor(Conn_feat) + pop_dens_log)
mod_occF <- update(mod_occ0, ~ factor(Conn_feat) + Pub_year)
mod_occG <- update(mod_occ0, ~ factor(Conn_feat) + absLat)
mod_occH <- update(mod_occ0, ~ factor(Conn_feat) + E_GR_AV14)
mod_occI <- update(mod_occ0, ~ factor(Conn_feat) + Age_mean)
mod_occJ <- update(mod_occ0, ~ factor(Conn_feat) + Pub_year + area_km2_log + pop_dens_log + absLat + E_GR_AV14)

mod_occ1 <- update(mod_occ0, ~ factor(Study_class) + area_km2_log)
mod_occ2 <- update(mod_occ0, ~ factor(Study_class) + pop_dens_log)
mod_occ3 <- update(mod_occ0, ~ factor(Study_class) + Pub_year)
mod_occ4 <- update(mod_occ0, ~ factor(Study_class) + absLat)
mod_occ5 <- update(mod_occ0, ~ factor(Study_class) + E_GR_AV14)
mod_occ6 <- update(mod_occ0, ~ factor(Study_class) + Age_mean)
mod_occ7 <- update(mod_occ0, ~ factor(Study_class) + Pub_year + area_km2_log + pop_dens_log + absLat + E_GR_AV14)

fitstats(mod_occ0, mod_occA, mod_occB, mod_occC, mod_occD, 
         mod_occE, mod_occF, mod_occG, mod_occH, mod_occI, mod_occJ, REML = F) #compare model AIC values
fitstats(mod_occ0, mod_occA, mod_occB, mod_occC, mod_occ1, 
         mod_occ2, mod_occ3, mod_occ4, mod_occ5, mod_occ6, mod_occ7, REML = F) #compare model AIC values

## Orchard plots for the null, taxa, and conn_feat models
orchard_plot(update(mod_occ0, method = "REML"), mod = "1", group = "studyID",
             xlab = "Correlation coefficient", transfm = "tanh", k = TRUE, g = F, cb = T)
orchard_plot(update(mod_occB, method = "REML"), 
             mod = "Conn_feat",
             condition.lab = "", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers
orchard_plot(update(mod_occC, method = "REML"), 
             mod = "Study_class",
             condition.lab = "", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers

## model summary
occ_fit <- update(mod_occF, method = "REML")
summary(occ_fit)
funnel(occ_fit, yaxis = "seinv")
r2_ml(occ_fit, data = es_dat_occ); i2_ml(occ_fit)
## robust parameter estimation
robust(occ_fit, cluster = studyID, clubSandwich = T, adjust = F)

## Orchard and bubble plots
orchard_plot(occ_fit, 
             mod = "Study_class",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = F, cb = T) #k = #ofES, g = #ofpapers

bubble_plot(occ_fit, mod = "E_GR_AV14", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Avg. Greeness index")

## Model results at levels of 'greenness'
occrmodel <- orchaRd::mod_results(occ_fit, group = "studyID", mod = "Study_class", 
                                  at = list(E_GR_AV14 = c(0.2, 0.4, 0.6)), by = "E_GR_AV14")
orchaRd::orchard_plot(occrmodel, xlab = "Correlation coefficient", group = "studyID", angle = 45, g = FALSE, 
                      legend.pos = "top.left", condition.lab = "% Greeness") + 
  theme(legend.direction = "vertical")

# caterpillar plot
modOCC_res <- orchaRd::mod_results(occ_fit, mod = "Study_class", group = "studyID")
orchaRd::caterpillars(modOCC_res, mod = "Study_class", group = "studyID", overall = T,
                      transfm = "tanh", xlab = "Correlation coefficient")

### 3.4 Genetic distance ------------------------------------------------------
es_dat_gen <- es_data %>% filter(r_metric == "genetic dist.") %>% 
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
                                            elevavg_m, built15_km2, pop15, BUCAP15, E_GR_AV14, E_GR_AL14, 
                                            SDG_LUE9015,SDG_A2G14,SDG_OS15MX)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## generate a model for genetic distance
mod_gen0 <- rma.mv(yi = yi, V = Vgene,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "ML", test = "t", verbose = F)
mod_genA <- rma.mv(yi = yi, V = Vgene,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + area_km2_log + pop_dens_log + absLat + E_GR_AV14 -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_genB <- update(mod_gen0, ~ factor(Conn_feat))
mod_genC <- update(mod_gen0, ~ factor(Study_class))
mod_genD <- update(mod_gen0, ~ factor(Conn_feat) + area_km2_log -1)
mod_genE <- update(mod_gen0, ~ factor(Conn_feat) + pop_dens_log -1)
mod_genF <- update(mod_gen0, ~ factor(Conn_feat) + Pub_year -1)
mod_genG <- update(mod_gen0, ~ factor(Conn_feat) + absLat -1)
mod_genH <- update(mod_gen0, ~ factor(Conn_feat) + E_GR_AV14 -1)
mod_genI <- update(mod_gen0, ~ factor(Conn_feat) + Age_mean + Pub_year -1)
mod_genJ <- update(mod_gen0, ~ factor(Conn_feat) + Pub_year + area_km2_log + pop_dens_log + absLat + E_GR_AV14 -1)

mod_gen1 <- update(mod_gen0, ~ factor(Study_class) + area_km2_log -1)
mod_gen2 <- update(mod_gen0, ~ factor(Study_class) + pop_dens_log -1)
mod_gen3 <- update(mod_gen0, ~ factor(Study_class) + Pub_year -1)
mod_gen4 <- update(mod_gen0, ~ factor(Study_class) + absLat -1)
mod_gen5 <- update(mod_gen0, ~ factor(Study_class) + E_GR_AV14 -1)
mod_gen6 <- update(mod_gen0, ~ factor(Study_class) + Age_mean -1)
mod_gen7 <- update(mod_gen0, ~ factor(Study_class) + Pub_year + area_km2_log + pop_dens_log + absLat + E_GR_AV14 -1)

fitstats(mod_gen0, mod_genA, mod_genB, mod_genC, mod_genD, 
         mod_genE, mod_genF, mod_genG, mod_genH, mod_genI, mod_genJ, REML = F) #compare model AIC values
fitstats(mod_gen0, mod_genA, mod_genB, mod_genC, mod_gen1, 
         mod_gen2, mod_gen3, mod_gen4, mod_gen5, mod_gen6, mod_gen7, REML = F) #compare model AIC values

## model summary
gene_fit <- update(mod_genI, method = "REML")
summary(gene_fit)
funnel(gene_fit, yaxis="seinv")
r2_ml(gene_fit, data = es_dat_gen); i2_ml(gene_fit)
## robust parameter estimation
robust(gene_fit, cluster = studyID, clubSandwich = T, adjust = F)

## Orchard and bubble plots
orchard_plot(gene_fit, 
             mod = "Conn_feat",
             condition.lab = "Connectivity feature", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = T)
bubble_plot(gene_fit, mod = "Age_mean", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Mean Urban age") + #add a horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)
bubble_plot(gene_fit, mod = "Pub_year", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Publication year") + #add a horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5)

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