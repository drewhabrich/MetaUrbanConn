rm(list = ls())

## 1. Load relevant packages ###################################################
pacman::p_load(devtools, R.rsp, ape, phytools, flextable,
               tidyverse, patchwork, ggpubr, RColorBrewer, easystats,
               metafor, orchaRd, emmeans, parallel)

### Read in data ###############################################################
es_data <- read_csv("data/13-effectsize_data_pooled.csv") %>% 
  ## Remove 'fungus' and convert Arthropoda to invertebrates
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

## coerce to factor class for modelling
es_data$r_metric_bin <- factor(es_data$r_metric_bin)
es_data$Study_class <- factor(es_data$Study_class)
es_data$connfeat_bin <- factor(es_data$connfeat_bin)
## create new factor combination columns to estimate parameters using orchard plot
es_data <- es_data %>% 
  mutate(classresp = Study_class:r_metric_bin) %>% 
  mutate(respconn = r_metric_bin:connfeat_bin) %>% 
  mutate(classconn = Study_class:connfeat_bin)

### set a series of rho values to test for variance-covariance range
rho_range <- c(0.3, 0.5, 0.7, 0.9)

## 2. Categorical mods ############################################################
### 2.1. studyclass and response metrics ---------------------------------------
## Filter out combinations of classresp that have <3 entries
es_a <- es_data %>% group_by(classresp) %>% filter(n() > 2) %>% ungroup()
Va <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, 
            data = es_a, 
            rho = c(0.5, 0.5), #correlation between effect sizes within the same study and between r_metrics
            checkpd = T, nearpd = F)

#### Fit model and extract marginal effects using emmeans and emmip
mod_resp_taxa <- rma.mv(yi = yi, V = Va,
                        mods = ~ r_metric_bin*Study_class,
                        random = list(~ 1 | studyID/r_metric_bin/ES_no),
                        data = es_a, method = "REML", test = "t", dfs = "contain", verbose = F)
rownames(mod_resp_taxa$beta)[1] = "(Intercept)"
## define new levels for the response variable
em_a_levels <- c("species richness", "occurrence", 
                 "abundance", "diversity indices", "functional diversity",
                 "genetic similarity", "community similarity", "movement")
## interaction plot
em_a <- emmip(emmeans(qdrg(object = q), specs = "r_metric_bin", by = "Study_class"),
              Study_class ~ r_metric_bin, CIs = TRUE, plotit = FALSE) %>% 
  ## create a column for significance of the effect size based on the confidence intervals
  mutate(signif = ifelse(LCL > 0 | UCL < 0, "yes", "no")) %>% 
  mutate(r_metric_bin = factor(r_metric_bin, levels = em_a_levels)) 
## Add the number of studies in each group to the emmip object
k_a <- es_a %>% group_by(Study_class, r_metric_bin) %>% summarise(n = n()) %>% mutate(k = paste0("k = ", n))
## join to the emmip object for the columns that match the study_class and r_metric_bin
em_a <- left_join(em_a, k_a, by = c("r_metric_bin" = "r_metric_bin", "Study_class" = "Study_class"))

### plot the effect size for each response metric bin using ggplot and facet wrap for each taxa group
ggplot() +
  # Add the raw data distributions using geom_quasirandom from ggbeeswarm
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggbeeswarm::geom_quasirandom(data = es_a, aes(y = yi, x = r_metric_bin, colour = r_metric_bin), 
                               width = 0.5, alpha = 0.6, size = 2, shape = 21) +
  geom_pointrange(data = em_a, aes(x = r_metric_bin, y = yvar, ymin = LCL, ymax = UCL, fill = signif),
                  size = 0.5, linewidth = 1, na.rm = T) +
  geom_point(data = em_a, aes(x = r_metric_bin, y = yvar, fill = signif), shape = 21, size = 3, na.rm = T) +
  #plot by facets and reorder the facet_wrap  
  facet_wrap(~ fct_relevel(Study_class, "aves", "mammalia", "invertebrates", "herptiles", "plantae"), 
               nrow = 2, ncol = 3, scales = "fixed") +
  geom_text(data = em_a, aes(x = r_metric_bin, y = (max(es_a$yi) + (max(es_a$yi) * 0.1)), label = k), 
            hjust = 1, vjust = -1, size = 3, size.unit = "mm", na.rm = T) +
  scale_fill_manual(values = c("yes" = "black", "no" = "white")) +
  scale_x_discrete(limits = rev(em_a_levels)) +
  coord_flip() +
  theme_bw() + theme(legend.position = "none") + labs(x = "Biological response metric", y = "Fisher's Zr") 
  
### sensitivity analysis for rho values 
moda_VCV_range <- list() # repeatedly run the specified model with varying rho
for (i in 1:length(rho_range)) {
  VCV_range <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, 
                     data = es_a, 
                     rho = c(rho_range[i], 0.5), 
                     checkpd = T, nearpd = F
  ) # impute VCV matrix with varying rho
  moda_VCV_range[[i]] <- rma.mv(yi = yi, V = VCV_range,
                                mods = ~ r_metric_bin*Study_class,
                                random = list(~ 1 | studyID/r_metric_bin/ES_no),
                                data = es_a, method = "REML", test = "t", dfs = "contain", verbose = F
  )} # run model with different rho values.
## Save to RDS to compare 
saveRDS(moda_VCV_range, "output/15_sensitivity_analysis_studyclass.rds")

### 2.2. response metrics and connectivity features -----------------------------------
es_b <- es_data %>% group_by(respconn) %>% filter(n() > 2) %>% ungroup()
Vb <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, data = es_b, 
             rho = c(0.5, 0.5), #correlation between effect sizes within the same study and between r_metrics
             checkpd = T, nearpd = F)

#### Fit model and extract marginal effects using emmeans and emmip
mod_resp_conn <- rma.mv(yi = yi, V = Vb,
                        mods = ~ r_metric_bin*connfeat_bin,
                        random = list(~ 1 | studyID/r_metric_bin/ES_no),
                        data = es_b, method = "REML", test = "t", dfs = "contain", verbose = F)
rownames(mod_resp_conn$beta)[1] = "(Intercept)"
## define new levels for the response variable
em_b_levels <- c("species richness", "occurrence", 
                 "abundance", "diversity indices", "functional diversity",
                 "genetic similarity", "community similarity", "movement")
## interaction plot
em_b <- emmip(emmeans(qdrg(object = mod_resp_conn), specs = "r_metric_bin", by = "connfeat_bin"),
              connfeat_bin ~ r_metric_bin, CIs = TRUE, plotit = FALSE) %>% 
  ## create a column for significance of the effect size based on the confidence intervals
  mutate(signif = ifelse(LCL > 0 | UCL < 0, "yes", "no")) %>% 
  mutate(r_metric_bin = fct_relevel(r_metric_bin, em_b_levels)) 
## Add the number of studies in each group to the emmip object
k_b <- es_b %>% group_by(connfeat_bin, r_metric_bin) %>% summarise(n = n()) %>% mutate(k = paste0("k = ", n))
## join to the emmip object for the columns that match the study_class and r_metric_bin
em_b <- left_join(em_b, k_b, by = c("r_metric_bin" = "r_metric_bin", "connfeat_bin" = "connfeat_bin"))

### plot the effect size for each response metric bin using ggplot and facet wrap for each taxa group
ggplot() +
  # Add the raw data distributions using geom_quasirandom from ggbeeswarm
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggbeeswarm::geom_quasirandom(data = es_b, aes(y = yi, x = r_metric_bin, colour = r_metric_bin), 
                               width = 0.5, alpha = 0.6, size = 2, shape = 21) +
  geom_pointrange(data = em_b, aes(x = r_metric_bin, y = yvar, ymin = LCL, ymax = UCL, fill = signif),
                  size = 0.5, linewidth = 1, na.rm = T) +
  geom_point(data = em_b, aes(x = r_metric_bin, y = yvar, fill = signif), shape = 21, size = 3, na.rm = T) +
  #plot by facets and reorder the facet_wrap
  facet_wrap(~ fct_relevel(connfeat_bin, "adjacency","area","proximity","graph network","potential","permeability"), 
             nrow = 2, ncol = 3, scales = "fixed") +
  geom_text(data = em_b, aes(x = r_metric_bin, y = (max(es_b$yi) + (max(es_b$yi) * 0.1)), label = k), 
            hjust = 1, vjust = -1, size = 3, size.unit = "mm", na.rm = T) +
  scale_fill_manual(values = c("yes" = "black", "no" = "white")) +
  scale_x_discrete(limits = rev(em_b_levels)) +
  coord_flip() +
  theme_bw() + theme(legend.position = "none") + labs(x = "Biological response metric", y = "Fisher's Zr") 

### sensitivity analysis for rho values 
modb_VCV_range <- list() # repeatedly run the specified model with varying rho
for (i in 1:length(rho_range)) {
  VCV_range <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, 
                     data = es_b, 
                     rho = c(rho_range[i], 0.5), 
                     checkpd = T, nearpd = F
  ) # impute VCV matrix with varying rho
  modb_VCV_range[[i]] <- rma.mv(yi = yi, V = VCV_range,
                                mods = ~ r_metric_bin*connfeat_bin,
                                random = list(~ 1 | studyID/r_metric_bin/ES_no),
                                data = es_b, method = "REML", test = "t", dfs = "contain", verbose = F
  )} # run model with different rho values.
## Save to RDS to compare 
saveRDS(modb_VCV_range, "output/15_sensitivity_analysis_respmetrics.rds")

### 2.3. studyclass and connectivity features ----------------------------------
es_c <- es_data %>% group_by(classconn) %>% filter(n() > 2) %>% ungroup()
Vc <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, data = es_c, 
             rho = c(0.5, 0.5), #correlation between effect sizes within the same study and between r_metrics
             checkpd = T, nearpd = F)

#### Fit model and extract marginal effects using emmeans and emmip
mod_conn_taxa <- rma.mv(yi = yi, V = Vc,
                        mods = ~ connfeat_bin*Study_class,
                        random = list(~ 1 | studyID/r_metric_bin/ES_no),
                        data = es_c, method = "REML", test = "t", dfs = "contain", verbose = F)
rownames(mod_conn_taxa$beta)[1] = "(Intercept)"
## define new levels for the response variable
em_c_levels <- c("adjacency", "area", "proximity", "graph network", "potential", "permeability")
## interaction plot
em_c <- emmip(emmeans(qdrg(object = mod_conn_taxa), specs = "connfeat_bin", by = "Study_class"),
              Study_class ~ connfeat_bin, CIs = TRUE, plotit = FALSE) %>% 
  ## create a column for significance of the effect size based on the confidence intervals
  mutate(signif = ifelse(LCL > 0 | UCL < 0, "yes", "no")) 
## Add the number of studies in each group to the emmip object
k_c <- es_c %>% group_by(Study_class, connfeat_bin) %>% summarise(n = n()) %>% mutate(k = paste0("k = ", n))
## join to the emmip object for the columns that match the study_class and r_metric_bin
em_c <- left_join(em_c, k_c, by = c("connfeat_bin" = "connfeat_bin", "Study_class" = "Study_class")) %>% 
           mutate(connfeat_bin = factor(connfeat_bin, levels = em_c_levels))

### plot the effect size for each response metric bin using ggplot and facet wrap for each taxa group
ggplot() +
  # Add the raw data distributions using geom_quasirandom from ggbeeswarm
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggbeeswarm::geom_quasirandom(data = es_c, aes(y = yi, x = connfeat_bin, colour = connfeat_bin), 
                               width = 0.5, alpha = 0.6, size = 2, shape = 21) +
  geom_pointrange(data = em_c, aes(x = connfeat_bin, y = yvar, ymin = LCL, ymax = UCL, fill = signif),
                  size = 0.5, linewidth = 1, na.rm = T) +
  geom_point(data = em_c, aes(x = connfeat_bin, y = yvar, fill = signif), shape = 21, size = 3, na.rm = T) +
  #plot by facets and reorder the facet_wrap
  facet_wrap(~ fct_relevel(Study_class, "aves", "mammalia", "invertebrates", "herptiles", "plantae"), 
             nrow = 1, ncol = 5, scales = "fixed") +
  geom_text(data = em_c, aes(x = connfeat_bin, y = (max(es_c$yi) + (max(es_c$yi) * 0.1)), label = k), 
            hjust = 1, vjust = -1, size = 3, size.unit = "mm", na.rm = T) +
  scale_fill_manual(values = c("yes" = "black", "no" = "white")) +
  scale_x_discrete(limits = rev(em_c_levels)) +
  coord_flip() +
  theme_bw() + theme(legend.position = "none") + labs(x = "Connectivity feature", y = "Fisher's Zr") 
  
### sensitivity analysis for rho values 
modc_VCV_range <- list() # repeatedly run the specified model with varying rho
for (i in 1:length(rho_range)) {
  VCV_range <- vcalc(vi = vi, cluster = studyID, type = r_metric_bin, obs = ES_no, 
                     data = es_c, 
                     rho = c(rho_range[i], 0.5), 
                     checkpd = T, nearpd = F
  ) # impute VCV matrix with varying rho
  modc_VCV_range[[i]] <- rma.mv(yi = yi, V = VCV_range,
                                mods = ~ connfeat_bin*Study_class,
                                random = list(~ 1 | studyID/r_metric_bin/ES_no),
                                data = es_c, method = "REML", test = "t", dfs = "contain", verbose = F
  )} # run model with different rho values.
## Save to RDS to compare 
saveRDS(modc_VCV_range, "output/15_sensitivity_analysis_connfeat.rds")

### Sensitivity analysis comparison
moda_VCV_range <- readRDS("output/15_sensitivity_analysis_studyclass.rds")
modb_VCV_range <- readRDS("output/15_sensitivity_analysis_respmetrics.rds")
modc_VCV_range <- readRDS("output/15_sensitivity_analysis_connfeat.rds")

## Extract the model coefficients for each rho value and put it in a table
summary_table <- data.frame()
summary(moda_VCV_range[[1]])
summary(moda_VCV_range[[2]])
summary(moda_VCV_range[[3]])
summary(moda_VCV_range[[4]])

## 3. Continuous mods ##########################################################