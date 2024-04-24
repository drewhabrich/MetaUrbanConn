## HEADER---------------------------
## Script name: 14-metaanalytic modelling
##
## Purpose of script: Model the effects of moderators on the effects of connectivity on biodiversity. Each response variable has their own model.
##
## Author: Andrew Habrich
##
## Date Created: 2024-03-11
## Date last Modified: 2024-04-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list = ls())

# install.packages('pacman')
# devtools::install_github("daniel1noble/orchaRd", ref = "main", force = TRUE)
pacman::p_load(devtools, tidyverse, metafor, patchwork, R.rsp, orchaRd, emmeans,
               ape, phytools, flextable)

## 1. Load relevant packages--------
### for stats
library(tidyverse)
library(easystats)
library(metafor)
library(emmeans)

### for figures
library(DataExplorer)
library(ggpubr)

### custom functions ###
## function to remove accents from city names
remove_accents <- function(x) {
  stringi::stri_trans_general(x, "latin-ascii")
}

## 2. Load data ---------------------
es_dat <- read_csv("./data/13-effectsize_data.csv")
ghs <- readxl::read_xlsx("./raw_data/ghsl/globalhumansettlement2019.xlsx")
oecd <- read_csv("./raw_data/oecd/cityyear_oecd_manualmerge.csv", na = "na")

### remove accents from city names
es_dat$City_clean <- remove_accents(es_dat$City)
ghs$city <- remove_accents(ghs$city)

### 2.1 Data cleaning and pre-processing ----
es_dat$Study_year <- stringr::str_remove(es_dat$Study_year, "-.*") %>% as.numeric(es_dat$Study_year)

## create a copy of the dataframe to manipulate
cities <- es_dat
## cleanup the country names and city names ### HOW DO I MAKE FUZZY MATCHING WORK?
cities$City_clean <- cities$City_clean %>% str_replace("Braunschweig", "Brunswick")
cities$City_clean <- cities$City_clean %>% str_replace("Washington, D.C.", "Washington D.C.")
cities$City_clean <- cities$City_clean %>% str_replace("Taipei", "New Taipei [Taipei]")
cities$City_clean <- cities$City_clean %>% str_replace("Osaka", "Osaka [Kyoto]")
cities$City_clean <- cities$City_clean %>% str_replace("Kobe", "Osaka [Kyoto]")
cities$City_clean <- cities$City_clean %>% str_replace("Cheonan", "Cheonan-si")
cities$City_clean <- cities$City_clean %>% str_replace("Manila", "Quezon City [Manila]")
cities$City_clean <- cities$City_clean %>% str_replace("San Diego", "Tijuana")
cities$City_clean <- cities$City_clean %>% str_replace("San Francisco", "San Jose")
cities$City_clean <- cities$City_clean %>% str_replace("New Jersey", "New York")
cities$City_clean <- cities$City_clean %>% str_replace("Vitoria", "Vila Velha")

citieslist <- cities %>% select(City_clean, Country) %>% distinct() 
## merge the distinct cities from the es_data that are in the global human settlement dataframe
cityghsl <- citieslist %>% left_join(ghs, by = c("City_clean" = "city", "Country" = "country")) %>% 
  slice(-41) ## remove row 41 from the dataframe (the faulty nanjing entry)

## save the dataframe to a csv file #uncomment to save
#write_csv(cityghsl, "./raw_data/ghsl/cityghsl.csv")

## merge columns from city_dat to es_dat, matched based on city and study_year
cities <- left_join(cities, cityghsl, by = c("City_clean" = "City_clean", 
                                             "Country" = "Country"))
## merge columns from oecd to new cities dataframe, matched based on city and country and study year
cities <- left_join(cities, oecd, by = c("City_clean" = "City_clean", 
                                         "Country" = "Country", 
                                         "Study_year" = "Study_year"))

## remove columns that are not needed
cities <- cities %>% select(-c(Pub_type, DOI, Study_effect, Conn_tool, r_method, corr_type,
                               mi, ri, ti, fi, pi, bi, var_sdi, var_type, R2i, 
                               country_code, xborder_country, urban_incity, UC_NM_SRC)) 

# reverse the sign if the metric was dependent on 'impervious/developed surfaces' to be consistent with the other metrics
cities <- cities %>% mutate(Conn_lcclass = ifelse(is.na(.$Conn_lcclass), "n/a", .$Conn_lcclass))
cities$yi <- ifelse(cities$Conn_lcclass == "grey", cities$yi * -1, cities$yi)
# combine taxa levels to create unique identifier
cities <- cities %>% mutate(study_taxagroup = paste(Study_class, Study_order, Study_spec, sep = "-"))
# convert latitude to absolute value
cities <- cities %>% mutate(absLat = abs(lat))
# remove rows with NA in absLat
cities <- cities %>% filter(!is.na(absLat))

### FOR OECD data
## how many columns have missing values for urban green space and population density?
cities %>% select(City_clean, Country, Study_year, urbarea_km2, hpd_km2, green_prop) %>% plot_missing()
## what are those missing rows?
cities %>% select(City_clean, Country, Study_year, urbarea_km2, hpd_km2, green_prop) %>% filter(is.na(urbarea_km2) | is.na(hpd_km2) | is.na(green_prop))

## histograms of study year, urban area, human population density, and green space
cities %>% select(Study_year, urbarea_km2, hpd_km2, green_prop) %>% plot_histogram()
## log transform the urban area, human population density, and green space and plot histograms
cities <- cities %>% mutate(urbarea_km2_log = log(urbarea_km2 + 1),
                            hpd_km2_log = log(hpd_km2 + 1),
                            green_prop_log = log(green_prop + 1))
cities %>% select(urbarea_km2_log, hpd_km2_log, green_prop_log) %>% plot_histogram()
####

### 2.2 Data exploration ----------------
### what is the number of unique studies?
n_distinct(cities$studyID)
### what is the number of unique combinations of study class, order, and species?
cities %>%
  group_by(studyID) %>%
  summarise(distinct_combinations = n_distinct(paste(Study_class, Study_order, Study_spec, sep = "-")))
## what is the number of response metrics per study?
cities %>%
  group_by(studyID) %>%
  summarise(distinct_metrics = n_distinct(r_metric))
## how many studies have multiple response metrics?
cities %>%
  group_by(studyID) %>%
  summarise(distinct_metrics = n_distinct(r_metric)) %>%
  filter(distinct_metrics > 1)
## list the distinct reponse metrics by count
cities %>%
  group_by(r_metric) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

## what is the number of distinct connectivity features per study (in descending order)?
cities %>%
  group_by(studyID) %>%
  summarise(distinct_conn = n_distinct(Conn_feat)) %>%
  arrange(desc(distinct_conn))

### 2.3 SUBSET DATA ####
### Combine similar response metrics into a single category (e.g., species richness, abundance, etc.)
es_dat_pool <- cities %>%
  mutate(r_metric = case_when(
    str_detect(r_metric, "beta diversity|dissimilarity$") ~ "beta-diversity", #combine dissimilarity metrics
    str_detect(r_metric, "movement|return time|activity ratio") ~ "movement", #combine movement metrics
    str_detect(r_metric, "evenness") ~ "evenness", #combine evenness metrics
    str_detect(r_metric, "functional") ~ "functional traits", #combine functional trait metrics
    TRUE ~ r_metric)) %>%
  group_by(r_metric) %>%
  filter(n() >= 10) %>% ungroup()

## list the distinct reponse metrics by count
es_dat_pool %>%
  group_by(r_metric) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#Subset the data to only include the richness, abundance, shannon diversity, occurrence, and genetic distance metrics
es_dat_max <- cities %>%
  filter(r_metric %in% c("species richness", "abundance", "shannon diversity", "occurrence", "genetic dist."))

### Approximate the Variance-Covariance Matrix of Dependent Effect Sizes or Outcomes (Used in multilevel meta-analytic models)
## cluster = studyID, subgroup = independent groups in cluster, obs = ES_no
# Vpool <- vcalc(vi = vi, cluster = studyID, type = r_metric, obs = ES_no, data = es_dat_pool, rho = c(0.5, 0.3),
#            checkpd = T, nearpd = T) #calculate sampling variance, clustered by Study ID
# Vmax <- vcalc(vi = vi, cluster = studyID, type = r_metric, obs = ES_no, data = es_dat_max, rho = c(0.5, 0.3),
#              checkpd = T, nearpd = T) #calculate sampling variance, clustered by Study ID)

## 3. Meta-analytic model ----------------------------------------------------
### 3.0 global model; all response metrics; random effect models structure----
# multi-level model (NOTE: These are identical ways of writing the formula)
modMV0 <- rma.mv(yi = yi, V = vi, 
                    data = cities, method = "ML", test = "t")
modMV1 <- rma.mv(yi = yi, V = vi, random = list(~ 1 | studyID,
                                                ~ 1 | ES_no),
                    data = cities, method = "ML", test = "t")
modMV2 <- rma.mv(yi = yi, V = vi, random = ~ 1 | studyID,
                    data = cities, method = "ML", test = "t")
modMV3 <- rma.mv(yi = yi, V = vi, random = ~ 1 | ES_no,
                    data = cities, method = "ML", test = "t")

#compare full random effects (ESid nested in studyID) to no random effects against no random effects
metafor::anova.rma(modMV0, modMV1) 
fitstats(modMV0, modMV1, modMV2, modMV3) #compare model AIC values

## Generate a general model and check diagnostics
modmv <- rma.mv(yi = yi, V = vi, 
                mods = ~ factor(Study_class) -1,
                random = list(~ 1 | studyID,
                              ~ 1 | ES_no), 
                data = es_dat_pool, method = "REML", test = "t", dfs = "contain", 
                verbose =T)
summary(modmv); funnel(modmv)

# Generate a "global" model WITH all the moderator variables of interest
modmvG <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(Study_class) +
                          factor(r_metric) +
                          factor(Conn_feat) +
                          absLat +
                          Pub_year+
                          urbarea_km2_log +
                          hpd_km2_log +
                          green_prop_log -1,
                random = list(~ 1 | studyID,
                              ~ 1 | ES_no),
                data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T,
                control = list(rel.tol = 1e-6))
summary(modmvG)
funnel(modmvG)
profile(modmvG, progbar = T, plot = T)

### 3.1 Global model set ----
## generate a set of models with different combinations of moderators
### check correlation of variables
correlation(es_dat_pool %>% select(absLat, Pub_year, urbarea_km2_log, hpd_km2_log, green_prop_log)) #looks fine

## model set
modmvNull0 <- rma.mv(yi = yi, V = vi, #null intercept model
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)
modmvNull1 <- rma.mv(yi = yi, V = vi, #simple model with only seperate effects for response metric
                    mods = ~ factor(r_metric) -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)
modmvG0 <- rma.mv(yi = yi, V = vi, #global model (with interaction, green_prop temp removed) 
                  mods = ~ factor(Study_class) +
                    factor(r_metric)*factor(Conn_feat) +
                    absLat +
                    Pub_year +
                    urbarea_km2_log +
                    hpd_km2_log -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain")
modmvG1 <- rma.mv(yi = yi, V = vi, #global model (with interaction, green_prop temp removed) 
                  mods = ~ factor(Study_class)*factor(r_metric) +
                    factor(Conn_feat) +
                    absLat +
                    Pub_year + 
                    urbarea_km2_log +
                    hpd_km2_log -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)
modmvG2<- rma.mv(yi = yi, V = vi, #interactions with fewer moderators
                  mods = ~ factor(Study_class) +
                    factor(r_metric)*factor(Conn_feat) +
                    hpd_km2_log +
                    absLat -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)
modmvG3<- rma.mv(yi = yi, V = vi, #only continuous factors
                  mods = ~ factor(Study_class)*factor(r_metric) + 
                   factor(Conn_feat) +
                   absLat +
                   hpd_km2_log -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)

modmvG4 <- rma.mv(yi = yi, V = vi, #urban area and pubyear removed
                  mods = ~ factor(Study_class)*factor(r_metric) +
                    absLat +
                    hpd_km2_log -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)
modmvG5 <- rma.mv(yi = yi, V = vi, #green prob and urbn area removed
                  mods = ~ factor(r_metric)*factor(Conn_feat) +
                    absLat +
                    hpd_km2_log -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)
modmvG6<- rma.mv(yi = yi, V = vi, 
                  mods = ~ factor(Study_class) +
                    factor(r_metric) +
                    factor(Conn_feat) +
                    absLat +
                    hpd_km2_log -1,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "ML", test = "t", dfs = "contain", verbose = T)

fitstats(modmvNull0, modmvNull1) #compare model AIC 
fitstats(modmvG0, modmvG1, modmvG2, modmvG3, modmvG4, modmvG5, modmvG6) #compare model AIC 

### DREDGE THE MODEL TO FIND THE BEST MODEL(S) REQUIRES "ML" method
eval(metafor:::.MuMIn) # use eval() function to extract helper functions from MuMIn and make them usable in metafor.
mod.candidates <- dredge(modmvG0, fixed = "r_metric", beta = "none", #present unstandardized parameter coefficients
                         rank = "AICc",
                         evaluate = T, trace = 2) #dredge the model to find the best model(s)
mod.candidates
### plot model weights
plot(mod.candidates, which = "weights", main = "Model weights")

### 3.2 Univariate models ----
modx <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(r_metric)*factor(Conn_feat) -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
summary(modx)
r2_ml(modx, data = es_dat_pool)
i2_ml(modx)

## generate univariate models for each moderator
mod_resp <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(r_metric) + absLat -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
summary(mod_resp)
orchard_plot(mod_resp, 
             mod = "r_metric", 
             condition.lab = "Response metric", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "ES_no",  k = TRUE, g = TRUE)
orchard_plot(mod_resp, 
             mod = "Study_class", 
             condition.lab = "Taxa group", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "ES_no",  k = TRUE, g = TRUE)

mod_gp <- rma.mv(yi = yi, V = vi,
       mods = ~ factor(r_metric) + green_prop_log -1,
       random = list(~ 1 | studyID,
                     ~ 1 | ES_no),
       data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
bubble_plot(mod_gp, mod = "green_prop_log", group = "studyID", 
            ylab = "Effect size Fisher's Z", xlab = "Green space proportion")

mod_area <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(r_metric) + urbarea_km2_log -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
bubble_plot(mod_area, mod = "urbarea_km2_log", group = "studyID", 
            ylab = "Effect size Fisher's Z", xlab = "log Urban area (km^2)")

mod_hpd <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(r_metric) + hpd_km2_log -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
bubble_plot(mod_hpd, mod = "hpd_km2_log", group = "studyID", 
            ylab = "Effect size Fisher's Z", xlab = "log Human Pop.density (Persons/km^2)")

mod_pyear <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(r_metric) + Pub_year -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
bubble_plot(mod_pyear, mod = "Pub_year", group = "studyID", 
            ylab = "Effect size Fisher's Z", xlab = "Year of publication")

mod_lat <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(r_metric) + absLat -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
bubble_plot(mod_lat, mod = "absLat", group = "studyID", 
            ylab = "Effect size Fisher's Z", xlab = "Absolute Latitude")

### 3.3 Figures and final model ----
modnull <- rma.mv(yi = yi, V = vi,
                  random = list(~ 1 | studyID,
                                ~ 1 | ES_no),
                  data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
summary(modnull)
r2_ml(modnull, data = es_dat_pool)

## remove rows with na in absLat
es_dat_pool <- es_dat_pool %>% filter(!is.na(absLat))

modmvGX <- rma.mv(yi = yi, V = vi, #urban area and pubyear removed
                             mods = ~ factor(Study_class)*factor(r_metric) +
                               absLat +
                               hpd_km2_log -1,
                             random = list(~ 1 | studyID,
                                           ~ 1 | ES_no),
                             data = es_dat_pool, method = "REML", test = "t", dfs = "contain", verbose = T)
summary(modmvGX)
funnel(modmvGX)

### WORKAROUND TO WORK WITH EMMEANS
rownames(modmvGX$beta)[1] = "(Intercept)"
MG <- qdrg(object = modmvGX)
MG
emmeans(MG, ~ Study_class|r_metric) %>% plot() + geom_vline(xintercept = 0, linetype = "dashed")
emmeans(MG, ~ r_metric|Study_class) %>% plot() + geom_vline(xintercept = 0, linetype = "dashed")
## model fit
r2_ml(modmvGX, data = es_dat_pool)
i2_ml(modmvGX)
profile(modmvGX, progbar = T, plot = T)

##marginal means (requires non-redundant factors)
em <- emmprep(modmvGX)
mod_results(MG, mod = "Study_class", by = "r_metric", group = "ES_no")

## plot marginal means
emmeans(em, ~ factor(r_metric) | Study_class, data = es_dat_pool, marginal = T) %>% 
  plot(xlab = "Taxon class", ylab = "Effect size Fisher's Z", type = "response", 
       point = T, errorbar = "confidence", point.args = list(size = 2)) +
  geom_vline(xintercept = 0, linetype = "dashed")

## Orchard plot
### by study class
orchard_plot(modmvGX, 
             mod = "r_metric", by = "Study_class",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "ES_no",  k = TRUE, g = TRUE)
### by response metric
orchard_plot(modmvGX, 
             mod = "r_metric", 
             condition.lab = "Response metric", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "ES_no",  k = TRUE, g = TRUE)

### by continuous variables
bubble_plot(modmvGX, mod = "absLat", group = "studyID", 
            ylab = "Effect size Fisher's Z", xlab = "Absolute Latitude")
bubble_plot(modmvGX, mod = "hpd_km2_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(Human Pop. Density km2)")

############################## SPECIES RICHNESS ################################
es_dat_rich <- es_dat_pool %>% filter(r_metric == "species richness") %>% filter(!is.na(absLat))
## remove Conn_feat that have less than 3 entries
es_dat_rich <- es_dat_rich %>% group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup()

## generate a model for species richness
mod_richA <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(Conn_feat) + factor(Study_class) + urbarea_km2_log + hpd_km2_log + absLat -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_rich, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_richB <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + hpd_km2_log + absLat -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_rich, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_richC <- rma.mv(yi = yi, V = vi,
                    mods = ~ factor(Conn_feat) + hpd_km2_log + absLat -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_richD <- rma.mv(yi = yi, V = vi,
                    mods = ~ factor(Study_class) + hpd_km2_log + absLat -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_richE <- rma.mv(yi = yi, V = vi,
                    mods = ~ factor(Conn_feat) -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_rich, method = "REML", test = "t", dfs = "contain", verbose = T)
fitstats(mod_richA, mod_richB, mod_richC, mod_richD, mod_richE, REML = FALSE) #compare model AIC values

funnel(mod_richE)
summary(mod_richE)
r2_ml(mod_richE, data = es_dat_rich)
i2_ml(mod_richE)

orchard_plot(mod_richB, 
             mod = "Study_class",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = TRUE)
orchard_plot(mod_richE, 
             mod = "Conn_feat",
             condition.lab = "Conn. feature", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = TRUE)
bubble_plot(mod_richC, mod = "hpd_km2_log", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "log(Human Pop. Density km2)")
bubble_plot(mod_richC, mod = "absLat", group = "studyID",
            ylab = "Effect size Fisher's Z", xlab = "Absolute Latitude")

############################## ABUNDANCE ################################
es_dat_abun <- es_dat_pool %>% filter(r_metric == "abundance") %>% filter(!is.na(absLat))
## remove Conn_feat that have less than 3 entries
es_dat_abun <- es_dat_abun %>% group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup()

## generate a model for abundance
mod_abunA <- rma.mv(yi = yi, V = vi, struc = "UN",
                 mods = ~ factor(Conn_feat) + factor(Study_class) + urbarea_km2_log + hpd_km2_log + absLat -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_abun, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_abunB <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + hpd_km2_log + absLat -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_abun, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_abunC <- rma.mv(yi = yi, V = vi,
                    mods = ~ factor(Conn_feat) + hpd_km2_log + absLat -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_abun, method = "REML", test = "t", dfs = "contain", verbose = T)
fitstats(mod_abunA, mod_abunB, mod_abunC, REML = FALSE) #compare model AIC values
funnel(mod_abunC)
summary(mod_abunC)
r2_ml(mod_abunC, data = es_dat_abun)
i2_ml(mod_abunC)

orchard_plot(mod_abunB, 
             mod = "Study_class",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "ES_no",  k = TRUE, g = TRUE)
orchard_plot(mod_abunC, 
             mod = "Conn_feat",
             condition.lab = "Conn. feature", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "ES_no",  k = TRUE, g = TRUE)
bubble_plot(mod_abunC, mod = "hpd_km2_log", group = "studyID", transfm = "tanh",
            ylab = "Effect size Fisher's Z", xlab = "log(Human Pop. Density km2)")
bubble_plot(mod_abunC, mod = "absLat", group = "studyID", transfm = "tanh",
            ylab = "Effect size Fisher's Z", xlab = "Absolute Latitude")

############################### OCCURRENCE ################################
es_dat_occ <- es_dat_pool %>% filter(r_metric == "occurrence") %>% filter(!is.na(absLat)) %>%
  filter(Study_class != "arthropoda") ## temporarily remove arthropods since it is a strange category
## remove Conn_feat that have less than 3 entries
es_dat_occ <- es_dat_occ %>% group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup()

## generate a model for occurrence
mod_occA <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(Conn_feat) + factor(Study_class) + urbarea_km2_log + hpd_km2_log + absLat -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_occB <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + hpd_km2_log + absLat -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_occC <- rma.mv(yi = yi, V = vi,
                    mods = ~ factor(Conn_feat) + hpd_km2_log + absLat -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_occD <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) + factor(Study_class) -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_occE <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_occF <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Study_class) -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_occG <- rma.mv(yi = yi, V = vi,
                   mods = ~ urbarea_km2_log + hpd_km2_log + absLat -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_occ, method = "REML", test = "t", dfs = "contain", verbose = T)

fitstats(mod_occA, mod_occB, mod_occC, mod_occD, mod_occE, mod_occF, mod_occG, REML = FALSE) #compare model AIC values
funnel(mod_occD)
model_performance(mod_occD)
summary(mod_occD)
r2_ml(mod_occD, data = es_dat_occ)
i2_ml(mod_occC)

orchard_plot(mod_occD, 
             mod = "Conn_feat",
             condition.lab = "Conn. feature", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = TRUE)
orchard_plot(mod_occD, 
             mod = "Study_class",
             condition.lab = "Conn. feature", angle = 0,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = TRUE)

################################ GENETIC DISTANCE ################################
es_dat_gen <- es_dat_pool %>% filter(r_metric == "genetic dist.") %>% filter(!is.na(absLat))
## remove Conn_feat that have less than 3 entries
es_dat_gen <- es_dat_gen %>% group_by(Conn_feat) %>% filter(n() >= 3) %>% ungroup()

## generate a model for genetic distance
mod_genA <- rma.mv(yi = yi, V = vi,
                 mods = ~ factor(Conn_feat) + factor(Study_class) + urbarea_km2_log + hpd_km2_log + absLat -1,
                 random = list(~ 1 | studyID,
                               ~ 1 | ES_no),
                 data = es_dat_gen, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_genB <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) + factor(Study_class) + hpd_km2_log + absLat -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_genC <- rma.mv(yi = yi, V = vi,
                    mods = ~ factor(Conn_feat) + hpd_km2_log + absLat -1,
                    random = list(~ 1 | studyID,
                                  ~ 1 | ES_no),
                    data = es_dat_gen, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_genD <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) + factor(Study_class) -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "REML", test = "t", dfs = "contain", verbose = T)
mod_genE <- rma.mv(yi = yi, V = vi,
                   mods = ~ factor(Conn_feat) -1,
                   random = list(~ 1 | studyID,
                                 ~ 1 | ES_no),
                   data = es_dat_gen, method = "REML", test = "t", dfs = "contain", verbose = T)

fitstats(mod_genA, mod_genB, mod_genC, mod_genD, mod_genE, REML = FALSE) #compare model AIC values
funnel(mod_genE)
summary(mod_genE)
r2_ml(mod_genE, data = es_dat_gen)
i2_ml(mod_genE)

orchard_plot(mod_genE, 
             mod = "Conn_feat",
             condition.lab = "Taxon class", angle = 45,
             xlab = "Correlation coefficient", transfm = "tanh", 
             group = "studyID",  k = TRUE, g = TRUE)

## 4. Summary
## create a table with the model formula and model-performance for each of the best-fit models for each response metric
mod_summary <- data.frame(
  response_metric = c("species richness", "abundance", "occurrence", "genetic dist."),
  model_formula = c(deparse(formula(mod_richE)[[2]]), 
                    deparse(formula(mod_abunC)[[2]]), 
                    deparse(formula(mod_occD)[[2]]), 
                    deparse(formula(mod_genE)[[2]])
  ))
## add new columns for model fit R2 marginal and conditional and I2 per response metric
mod_summary <- mod_summary %>%
  mutate(R2_marginal = c(r2_ml(mod_richE)[1],
                         r2_ml(mod_abunC)[1],
                         r2_ml(mod_occD)[1],
                         r2_ml(mod_genE)[1]),
         R2_conditional = c(r2_ml(mod_richE)[2],
                            r2_ml(mod_abunC)[2],
                            r2_ml(mod_occD)[2],
                            r2_ml(mod_genE)[2]),
         I2total = c(i2_ml(mod_richE)[1],
                     i2_ml(mod_abunC)[1],
                     i2_ml(mod_occD)[1],
                     i2_ml(mod_genE)[1]),
         I2between = c(i2_ml(mod_richE)[2],
                      i2_ml(mod_abunC)[2],
                      i2_ml(mod_occD)[2],
                      i2_ml(mod_genE)[2]),
         I2within = c(i2_ml(mod_richE)[3],
                      i2_ml(mod_abunC)[3],
                      i2_ml(mod_occD)[3],
                      i2_ml(mod_genE)[3])
  )

model_performance(mod_richE, data = es_dat_rich, metrics = "all", estimator = "REML")
model_performance(mod_abunC, data = es_dat_abun, metrics = "all", estimator = "REML")
model_performance(mod_occD, data = es_dat_occ, metrics = "all", estimator = "REML")
model_performance(mod_genE, data = es_dat_gen, metrics = "all", estimator = "REML")

## TODO ####
# robust estimates of parameters
# test for publication bias
# test for heterogeneity
# test for leave1out influence