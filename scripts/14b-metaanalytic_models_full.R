## HEADER---------------------------
## Script name: 14-metaanalytic_models_full
##
## Purpose of script: Model the effects of moderators on the effects of connectivity on biodiversity. 
## 
##
## Author: Andrew Habrich
##
## Date Created: 2024-07-04
## Date last Modified: 2024-11-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list = ls())

## 1. Load relevant packages ###################################################
pacman::p_load(devtools, R.rsp, ape, phytools, flextable,
               tidyverse, patchwork, ggpubr, RColorBrewer, easystats,
               metafor, orchaRd, emmeans)

### Read in data ###############################################################
es_data <- read_csv("data/13-effectsize_data_pooled.csv") %>% 
  ## remove 'fungus' and convert Arthropoda to invertebrates
  filter(Study_class != "fungus") %>% 
  filter(r_metric != "species density") %>% 
  filter(!is.na(absLat)) %>% 
  filter(!is.na(yi)) %>% 
  mutate(ES_no = paste0("es", ES_no)) %>% 
  relocate(City_clean, .after = City) %>% 
  relocate(r_metric_bin, .after = r_metric) %>%
  relocate(connfeat_bin, .after = Conn_feat) %>%
  relocate(geog_region, .after = Country) %>%
  select(-c(Study_no, biome_list, rcorr_type, Conn_buffer, NOTES)) %>% 
  ## Create a new bin for study class, group insecta, arachnida, and invertebrates together
  mutate(Study_class = ifelse(Study_class %in% c("arthropoda","insecta","arachnida"), 
                              "invertebrates", Study_class)) %>% 
  mutate(Study_class = ifelse(Study_class %in% c("amphibia", "reptilia"), 
                              "herptiles", Study_class)) %>% 
  mutate(connfeat_bin = ifelse(connfeat_bin %in% c("adjacency", "other"), 
                               "adjacency", connfeat_bin))

#### Approximate the Variance-Covariance Matrix of Dependent Effect Sizes or Outcomes 
## cluster = studyID, subgroup = independent groups in cluster, obs = ES_no
Vpool <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, data = es_data, 
               rho = c(0.5, 0.5), #correlation between effect sizes within the same study and between r_metrics
               checkpd = T, nearpd = F) #calculate sampling variance, clustered by Study ID

### map of cities
es_data %>% filter(!is.na(lat) & !is.na(long)) %>%
  distinct(City_clean, Country, lat, long) %>%
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  #plot using mapview (note need to use options(viewer = NULL) to view in RStudio)
  mapview::mapview(zcol = "City_clean", col.regions = "blue",
                   viewer.suppress = TRUE, legend = F) %>%
  mapview::removeMapJunk(junk = c("layersControl", "zoomControl", "homeButton", "drawToolbar", "easyButton"))

## 2. Meta-analytic models by response metric ##################################
#### Generate a general model and check diagnostics
modmv <- rma.mv(yi = yi, V = Vpool,
                random = list(~ 1 | studyID/r_metric_bin/ES_no),
                data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)

summary(modmv); funnel(modmv, yaxis = "seinv")
i2_ml(modmv); r2_ml(modmv, data = es_data)
# generate cluster-robust inference method of variance
robust(modmv, cluster = studyID, clubSandwich = T)
# generate a profile likelihood plot (THIS WILL TAKE A LONG TIME TO RUN)
profile(modmv, progbar = T, plot = T, refline = T)

### 2.1 Univariate model plots for pooled dataset ##############################
# intercept model
orchard_plot(modmv, mod = "1", group = "studyID", angle = 90, trunk.size = 1, branch.size = 1.5,
             xlab = "Correlation coefficient", transfm = "tanh", k = F, g = F) + 
  theme(axis.text.y = element_blank()) + xlab("Mean effect") +
  scale_fill_manual(values = "lightgreen") +
  scale_colour_manual(values = "forestgreen") +
  # add the I2 statistics as a geom annotation
  geom_text(aes(x = 1.5, y = 0.5, label = paste0("italic(I)^{2} == ", round(i2_ml(modmv)[1], 2), "*\"%\"")),
            color = "black", size = 5, hjust = 0.5, vjust = 0.5, check_overlap = T, parse = T)
# ggsave("output/figures/14_orchard_plot_pooled.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

## Univariate with response metric bins
modmv_r <- update(modmv, ~ factor(r_metric_bin) -1)
print(modmv_r) 
orchard_plot(modmv_r, mod = "r_metric_bin", group = "studyID", angle = 0, trunk.size = 1, branch.size = 1.5,
             tree.order = emmeans(emmprep(modmv_r), ~r_metric_bin) %>% as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% pull(r_metric_bin) %>% str_to_sentence(),
             xlab = "Correlation coefficient", transfm = "tanh", k = T, g = T, cb = T)
robust(modmv_r, cluster = studyID, clubSandwich = T)

## Univariate with connectivity feature
modmv_c <- update(modmv, ~factor(connfeat_bin) -1)
print(modmv_c) 
orchard_plot(modmv_c, mod = "connfeat_bin", group = "studyID", angle = 0, trunk.size = 1, branch.size = 1.5,
             tree.order = emmeans(emmprep(modmv_c), ~connfeat_bin) %>% as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% pull(connfeat_bin) %>% str_to_sentence(),
             xlab = "Correlation coefficient", transfm = "tanh", 
             k = T, g = T, cb = T, weights = "prop")
## forest plot of the effect sizes for the connfeat_bin
caterpillars(modmv_c, mod = "connfeat_bin", group = "studyID", 
             xlab = "Effect size Fisher's Z")

## Univariate with taxa
modmv_t <- update(modmv, ~factor(Study_class) -1)
print(modmv_t) 
orchard_plot(modmv_t, mod = "Study_class", group = "studyID", angle = 0, trunk.size = 1, branch.size = 1.5,
             tree.order = emmeans(emmprep(modmv_t), ~Study_class) %>% as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% pull(Study_class) %>% str_to_sentence(),
             xlab = "Correlation coefficient", transfm = "tanh", k = T, g = T, cb = T)
robust(modmv_t, cluster = studyID, clubSandwich = T)

### univariate models with continuous moderators
# moderator models
modmv_alat <- update(modmv, ~absLat)
modmv_puby <- update(modmv, ~Pub_year)
modmv_area <- update(modmv, ~area_km2_log)
modmv_popd <- update(modmv, ~pop_dens_log)
modmv_grn <- update(modmv, ~green_avg)
modmv_age <- update(modmv, ~Age_mean_m)
# plot the effect sizes for each moderator
uni_modlist <- list(
bubble_plot(modmv_alat, mod = "absLat", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Absolute Latitude")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red"),
bubble_plot(modmv_puby, mod = "Pub_year", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Publication year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red"),
bubble_plot(modmv_area, mod = "area_km2_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(Urban area km2)")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red"),
bubble_plot(modmv_popd, mod = "pop_dens_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(population/km2)")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red"),
bubble_plot(modmv_grn, mod = "green_avg", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Avg. Greeness index")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red"),
bubble_plot(modmv_age, mod = "Age_mean_m", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Avg. Urban age")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
)
# collected plot of univariate models
ggarrange(plotlist = uni_modlist, ncol = 3, nrow = 2, align = "hv", labels = "AUTO")

### 2.2 Multivariate models for pooled dataset #################################
## Run a correlation with all the potential moderators to test for multicollinearity
ggcorrplot::ggcorrplot(correlation(es_data %>% 
                                     select(absLat, Pub_year, area_km2_log, pop_dens_log, Age_mean_m, 
                                            Age_SD_mea, greenbuiltratio, elevavg_m, green_avg, green_km2_log, 
                                            built_km2_log, prop_green_log)), 
                       type = "lower", lab = T, lab_size = 2) #several correlations, consider removing some variables

## Fit the full model
modmv_p <- rma.mv(yi = yi, V = Vpool,
                  random = list(~ 1 | studyID/r_metric_bin/ES_no),
                  data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F,
                  mods = ~ factor(connfeat_bin) + factor(Study_class) + factor(r_metric_bin) +
                           area_km2_log + pop_dens_log + absLat + green_avg + Age_mean_m -1)
summary(modmv_p)
i2_ml(modmv_p); r2_ml(modmv_p, data = es_data)
robust(modmv_p, cluster = studyID, clubSandwich = T)
## estimate the profile likelihood to see if we are even able to estimate the model
profile(modmv_p, progbar = T, plot = T, refline = T, parallel = "multicore", ncpus = 4)

#### test for influencial studies
#### DFBETA
infl_mod <- dfbetas(modmv_p, cluster = studyID,
                    progbar = T, parallel = "multicore", ncpus = 4,
                    reestimate = FALSE)
## convert the rownames to IDs
infl_mod <- infl_mod %>% mutate(id = rownames(.))

#### COOKS DISTANCE
cd <- cooks.distance.rma.mv(modmv_p, progbar = T, cluster = studyID, 
                            parallel = "multicore", ncpus = 4,
                            reestimate = F)
plot(cd)
## what points are above 0.10?
cd[cd > 0.10] %>% as.data.frame() %>% arrange(desc(.))

## visualize
###response metric model
pal <- brewer.pal(n = 11, name = "RdYlBu")
resp_plot <- orchard_plot(modmv_p, mod = "r_metric_bin", group = "studyID", 
             tree.order = emmeans(emmprep(modmv_p), ~r_metric_bin) %>% as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% pull(r_metric_bin) %>% str_to_sentence(),
             angle = 0, legend.pos = "none", transfm = "tanh", k = T, g = F, cb = T,
             alpha = 0.40, trunk.size = 1, branch.size = 1.5, fill = T, 
             xlab = "Correlation coefficient")+
  scale_fill_manual(values = pal, aesthetics = c("fill", "colour")) 
resp_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
resp_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
resp_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
resp_plot
#ggsave("output/figures/14_orchard_plot_respmetric.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

###connectivity feature model
conn_plot <- orchard_plot(modmv_p, mod = "Conn_feat", group = "studyID",
             tree.order = emmeans(emmprep(modmv_p), ~Conn_feat) %>% as.data.frame() %>% #arrange by estimated effect
               arrange(emmean) %>% select(Conn_feat) %>% pull() %>% str_to_sentence(),
             angle = 0, legend.pos = "bottom.out", transfm = "tanh", k = T, g = F, cb = T,
             alpha = 0.40, trunk.size = 1, branch.size = 1.5, fill = T,
             xlab = "Correlation coefficient")
conn_plot$layers[[4]]$aes_params$fill <- "white" #change the fill of mean estimates
conn_plot$layers[[4]]$aes_params$stroke <- 2 #change the stroke (border) of mean estimates
conn_plot$layers[[1]]$aes_params$colour <- "black" #change the border of each effect size
conn_plot
#ggsave("output/figures/14_orchard_plot_connfeat.png", plot = last_plot(), width = 6, height = 6, dpi = 300)

## 3. Model selection #########################################################
### Fit the models
mod_null <- rma.mv(yi = yi, V = Vpool, #NULL
                    random = list(~ 1 | studyID/r_metric_bin/ES_no),
                    data = es_data, method = "ML", test = "t", dfs = "contain", verbose = F)
mod_full <- rma.mv(yi = yi, V = Vpool, #FULL
                    mods = ~ factor(connfeat_bin) + factor(Study_class) + factor(r_metric_bin) +
                             area_km2_log + pop_dens_log + absLat + green_avg + Age_mean_m,
                    random = list(~ 1 | studyID/r_metric_bin/ES_no),
                    data = es_data, method = "ML", test = "t", dfs = "contain", verbose = F)
# univariate models with categorical moderator variables
mod_resp <- update(mod_null, ~ . + factor(r_metric_bin) -1)
mod_connA <- update(mod_null, ~ . + factor(connfeat_bin) -1)
mod_connB <- update(mod_null, ~ . + factor(Conn_feat) -1)
mod_taxa <- update(mod_null, ~ . + factor(Study_class) -1)
# univariate models with continuous moderator variables
mod_area <- update(mod_null, ~ . + area_km2_log)
mod_popd <- update(mod_null, ~ . + pop_dens_log)
mod_alat <- update(mod_null, ~ . + absLat)
mod_grn <- update(mod_null, ~ . + green_avg)
mod_age <- update(mod_null, ~ . + Age_mean_m)

# compare models
fitstats(mod_null, mod_full, 
         mod_resp, mod_connA, mod_connB, mod_taxa, 
         mod_area, mod_popd, mod_alat, mod_grn, mod_age)

### multivariate models
#### additive models
mod_add1 <- update(mod_null, ~ . + area_km2_log + pop_dens_log + absLat + green_avg + Age_mean_m)
mod_add2 <- update(mod_null, ~ . + area_km2_log + green_avg) #city size and greeness
mod_add3 <- update(mod_null, ~ . + area_km2_log + Age_mean_m) #city size and development age
mod_add4 <- update(mod_null, ~ . + area_km2_log + factor(Study_class) -1) 
mod_add5 <- update(mod_null, ~ . + area_km2_log + Age_mean_m + factor(Study_class) -1) 
mod_add6 <- update(mod_null, ~ . + area_km2_log + pop_dens_log + Age_mean_m) #population density and age, anthropogenic
fitstats(mod_null, mod_full, mod_add1, mod_add2, mod_add3, mod_add4, mod_add5, mod_add6)

## bubble plot of the effect size
mod_add <- update(mod_add4, method = "REML") 
area_bubble <- orchaRd::mod_results(mod_add, mod = "area_km2_log", group = "studyID", weights = "prop",)

## continuous bubble plot with colours by taxa
orchaRd::bubble_plot(area_bubble , group = "studyID", mod = "area_km2_log", xlab = "log(Urban area km2)",
                     legend.pos = "none", alpha = 0.25, transfm = "tanh", k = T, g = T) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(data = es_data,
             aes(x = area_km2_log, y = tanh(yi), color = Study_class, size = 1/sqrt(vi), alpha = 0.6)) +
  scale_color_discrete()#add legend for the colour of the points

## orchard plot with values conditioned on specific area values
taxaarea_orch <- orchaRd::mod_results(mod_add, mod = "Study_class", group = "studyID", weights = "prop",
                                   by = "area_km2_log", at = list(area_km2_log = c(3, 6, 9)))
orchaRd::orchard_plot(taxaarea_orch, mod = "area_km2_log", group = "studyID", angle = 0,
                      trunk.size = 1, branch.size = 1.5, legend.pos = "bottom.out",
                      xlab = "log(Urban area km2)", k = T, g = T, cb = T, alpha = 0.25)

#### interaction models
mod_resp_area <- update(mod_null, ~ . + factor(r_metric_bin)*area_km2_log)
mod_resp_popd <- update(mod_null, ~ . + factor(r_metric_bin)*pop_dens_log)
mod_resp_grn <- update(mod_null, ~ . + factor(r_metric_bin)*green_avg)
mod_resp_age <- update(mod_null, ~ . + factor(r_metric_bin)*Age_mean_m)

mod_conn_area <- update(mod_null, ~ . + factor(connfeat_bin)*area_km2_log)
mod_conn_popd <- update(mod_null, ~ . + factor(connfeat_bin)*pop_dens_log)
mod_conn_grn <- update(mod_null, ~ . + factor(connfeat_bin)*green_avg)
mod_conn_age <- update(mod_null, ~ . + factor(connfeat_bin)*Age_mean_m)

mod_taxa_area <- update(mod_null, ~ . + factor(Study_class)*area_km2_log)
mod_taxa_popd <- update(mod_null, ~ . + factor(Study_class)*pop_dens_log)
mod_taxa_grn <- update(mod_null, ~ . + factor(Study_class)*green_avg)
mod_taxa_age <- update(mod_null, ~ . + factor(Study_class)*Age_mean_m)

interactionsx <- fitstats(mod_null, mod_full,
         mod_resp_area, mod_resp_popd, mod_resp_grn, mod_resp_age,
         mod_conn_area, mod_conn_popd, mod_conn_grn, mod_conn_age,
         mod_taxa_area, mod_taxa_popd, mod_taxa_grn, mod_taxa_age) 
t_int <- data.table::transpose(interactionsx)
t_int$mod <- colnames(interactionsx)
colnames(t_int) <- rownames(interactionsx)
#remove : from column names
colnames(t_int) <- gsub(":", "", colnames(t_int))
#change the 6th column name with NA to be 'model'
colnames(t_int)[6] <- "model"
#arrange in descending AICc order
t_int <- t_int %>% arrange(AICc)
#save to csv
write_csv(t_int, "output/tables/14_rmv_model_comparison.csv")
t_int

## bubble plot of the interaction effect
mod_ra <- update(mod_resp_age, method = "REML")

area_bubble <- orchaRd::mod_results(mod_ra, mod = "Age_mean_m", group = "studyID", weights = "prop",
                                   by = "r_metric_bin")
orchaRd::bubble_plot(area_bubble , group = "studyID", mod = "Age_mean_m", xlab = "Mean development Age (2-decade bins)",
                     legend.pos = "none", alpha = 0.25, transfm = "tanh", k = T, g = T) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

## conditioned at specific area values
resparea_orch <- orchaRd::mod_results(mod_ra, mod = "r_metric_bin", group = "studyID", weights = "prop",
                                   by = "area_km2_log", at = list(area_km2_log = c(3, 6, 9)))
orchaRd::orchard_plot(resparea_orch, mod = "r_metric_bin", group = "studyID", angle = 0, 
                      trunk.size = 1, branch.size = 1.5, legend.pos = "bottom.out",
                      xlab = "Fisher's Zr", k = T, g = T, cb = T, alpha = 0.25)

#### 3.0 Model plots ###########################################################
## Plots
es_data$r_metric_bin <- factor(es_data$r_metric_bin)
es_data$Study_class <- factor(es_data$Study_class)
es_data$connfeat_bin <- factor(es_data$connfeat_bin)

### RESPONSE * TAXA
mod_resp_taxa <- rma.mv(yi = yi, V = Vpool,
                   mods = ~ + r_metric_bin*Study_class,
                   random = list(~ 1 | studyID/r_metric_bin/ES_no),
                   data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)

summary(mod_resp_taxa)
rownames(mod_resp_taxa$beta)[1] = "(Intercept)"
RT = qdrg(object = mod_resp_taxa)   # named argument 'object' is essential
mod_rt <- emmeans(RT, ~r_metric_bin | Study_class)
summary(mod_rt)

## interaction plot
em_t <- emmip(mod_rt, ~r_metric_bin | Study_class, CIs = TRUE, plotit = FALSE)
## create a column for significance of the effect size based on the confidence intervals
em_t <- em_t %>% mutate(signif = ifelse(LCL > 0 | UCL < 0, "yes", "no"))

### plot the effect size for each response metric bin using ggplot and facet wrap for each taxa group
em_t %>% filter(!is.na(yvar)) %>% 
  ggplot(aes(x = r_metric_bin, y = yvar, ymin = LCL, ymax = UCL)) +
  geom_pointrange(size = 0.5, linewidth = 1) +
  geom_point(aes(fill = signif), shape = 21, size = 3) +
  facet_wrap(~ Study_class, nrow = 2, ncol = 3) +
  scale_fill_manual(values = c("yes" = "black", "no" = "white")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Biological response metric", y = "Fisher's Zr")

### RESPONSE * CONNECTIVITY
mod_resp_conn <- rma.mv(yi = yi, V = Vpool,
                   mods = ~ + r_metric_bin*connfeat_bin,
                   random = list(~ 1 | studyID/r_metric_bin/ES_no),
                   data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)
print(mod_resp_conn)
rownames(mod_resp_conn$beta)[1] = "(Intercept)"
RC = qdrg(object = mod_resp_conn)   # named argument 'object' is essential
mod_rc <- emmeans(RC, ~r_metric_bin | connfeat_bin)
summary(mod_rc)

## interaction plot
em_c <- emmip(mod_rc, ~r_metric_bin | connfeat_bin, CIs = TRUE, plotit = FALSE)
### plot the effect size for each response metric bin using ggplot and facet wrap for each taxa group
em_c %>% filter(!is.na(yvar)) %>% ggplot(aes(x = r_metric_bin, y = yvar, ymin = LCL, ymax = UCL)) +
  geom_pointrange(size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~connfeat_bin, nrow = 3, ncol = 3) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Biological response metric", y = "Fisher's Zr")

emmip(mod_rc, ~r_metric_bin | connfeat_bin, type = "response",
             CIs = TRUE, PIs = TRUE, plotit = TRUE) + coord_flip()

### CONNECTIVTY * TAXA
mod_conn_taxa <- rma.mv(yi = yi, V = Vpool,
                   mods = ~ + connfeat_bin*Study_class,
                   random = list(~ 1 | studyID/r_metric_bin/ES_no),
                   data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)
print(mod_conn_taxa)
rownames(mod_conn_taxa$beta)[1] = "(Intercept)"
CT = qdrg(object = mod_conn_taxa)   # named argument 'object' is essential
mod_ct <- emmeans(CT, ~connfeat_bin | Study_class)
summary(mod_ct)

## interaction plot
em_ct <- emmip(mod_ct, ~connfeat_bin | Study_class, CIs = TRUE, plotit = FALSE)
### plot the effect size for each response metric bin using ggplot and facet wrap for each taxa group
em_ct %>% filter(!is.na(yvar)) %>% ggplot(aes(x = connfeat_bin, y = yvar, ymin = LCL, ymax = UCL)) +
  geom_pointrange(size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Study_class, nrow = 2, ncol = 4) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "Connectivity feature", y = "Fisher's Zr")

#### 3.1 Model diagnostics ######################################################
## small study effect?
regtest(yi, vi, ni = ni, data = es_data, model = "rma", predictor = "ninv")

es_data$es_se <- sqrt(es_data$vi)
es_data$es_se2 <- sqrt(1/es_data$ni)
mod_ess <- rma.mv(yi = yi, V = Vpool, mods = ~ es_se2, 
                  random = list(~ 1 | studyID/r_metric_bin/ES_no),
                  data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)
bubble_plot(mod_ess, mod = "es_se", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Adjusted sampling error")
funnel(mod_ess, yaxis = "seinv", xlab = "Effect size Fisher's Z")
print(mod_ess) #When using Egger’s test, not interested the regression weight β1, but in the intercept β0

## pub year?
mod_year <- rma.mv(yi = yi, V = Vpool, mods = ~ Pub_year, 
                   random = list(~ 1 | studyID/r_metric_bin/ES_no),
                   data = es_data, method = "REML", test = "t", dfs = "contain", verbose = F)
bubble_plot(mod_year, mod = "Pub_year", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Publication year")

### Model dredging and averaging
eval(metafor:::.MuMIn) 
# dredge to produce all possible models
mod.candidate <- MuMIn::dredge(mod_full, beta = "none", fixed = "factor(r_metric_bin)",
                        evaluate = TRUE, rank = "AICc", trace = 2) 
### save as RDS file for later if possible
saveRDS(mod.candidate, "output/tables/14_rmv_candidate_models.rds")
##read in the candidate models
mod.candidate <- readRDS("output/tables/14_rmv_candidate_models.rds")

### save the candidate models dataframe to csv; avoids having to dredge again
modeldredge <- tibble(mod.candidate)
#write_csv(modeldredge, "output/tables/14_rmv_candidate_models.csv")

### the top models within 2 AIC points
topmod <- subset(mod.candidate, delta <= 2, recalc.weights = T)

### model averaging
modelavg <- MuMIn::model.avg(topmod)
summary(modelavg)

### JUST TOP2 MODELS
summary(MuMIn::model.avg(topmod[1:2]))