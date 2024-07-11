## HEADER---------------------------
## Script name: 14a-metaanalytic modelling-response metrics dataset preparation
##
## Purpose of script: Model the effects of moderators on the effects of connectivity on biodiversity. 
## Each response variable has their own model.
##
## Author: Andrew Habrich
##
## Date Created: 2024-07-4
## Date last Modified: 2024-07-4
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
#### global human settlement layer data https://data.jrc.ec.europa.eu/dataset/53473144-b88c-44bc-b4a3-4583ed1f547e
ghsl <- readxl::read_xlsx("./raw_data/ghsl/globalhumansettlement2019.xlsx")
#### city age data from https://doi.org/10.1371/journal.pone.0160471
cityage <- read_csv("./raw_data/cityage/cityage_merge.csv")

### 2.1 Data cleaning and pre-processing ----
### fix the study year column
es_dat$Study_year <- stringr::str_remove(es_dat$Study_year, "-.*") %>% as.numeric(es_dat$Study_year)
### remove accents from city names
es_dat$City_clean <- remove_accents(es_dat$City)
ghsl$city <- remove_accents(ghsl$city)

## create a copy of the dataframe to manipulate
cities <- es_dat
## cleanup the country names and city names
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
cityghsl <- citieslist %>% left_join(ghsl, by = c("City_clean" = "city", "Country" = "country")) %>% 
  slice(-41) ## remove row 41 from the dataframe (the faulty nanjing entry)

## merge columns from city_dat to es_dat, matched based on city and study_year
cities <- left_join(cities, cityghsl, by = c("City_clean" = "City_clean", 
                                             "Country" = "Country"))
## merge age data by city and country, selecting only the age metric
cities <- left_join(cities, cityage %>% select(c(City_clean, Country, Age_mean, Age_SD)), 
                    by = c("City_clean" = "City_clean", 
                           "Country" = "Country"))

## remove columns that are not needed
cities <- cities %>% select(-c(Pub_type, DOI, Study_effect, Conn_tool, r_method, corr_type,
                               mi, ri, ti, fi, pi, bi, var_sdi, var_type, R2i, 
                               country_code, xborder_country, urban_incity, UC_NM_SRC)) 

### FOR GHSL data
## create new columns for city variables
cities <- cities %>% 
  mutate(absLat = abs(lat),
         prop_green = area_km2/E_GR_AT14,
         pop_dens = pop15/area_km2,
         green15_km2 = area_km2 - built15_km2,
         E_GR_AV14 = as.numeric(E_GR_AV14))

## log transform some of the city bariables
cities <- cities %>% 
  mutate(area_km2_log = log(area_km2 + 1), 
         built15_km2_log = log(built15_km2 + 1),
         pop15_log = log(pop15 + 1),
         BUCAP15_log = log(BUCAP15 + 1),
         E_GR_AT14_log = log(E_GR_AT14 + 1),
         prop_green_log = log(prop_green + 1),
         pop_dens_log = log(pop_dens + 1),
         green15_km2_log = log(green15_km2 + 1))

## what are the unique cities? extract the lat long too
cities %>% select(City_clean, Country, lat, long) %>% distinct() %>% arrange(Country)
## save to a csv file. uncomment to save
# write_csv(cities %>% select(City_clean, Country, lat, long) %>% distinct() %>% arrange(Country),
#          "./data/14-cities_data.csv")

### 2.2 Convert for consistent direction of effect sizes ####
## what are the distinct lcclass types?
cities %>% select(Conn_lcclass) %>% distinct()

# for resist, inverse the relationship by multiplying yi by -1 to get permeability 
cities <- cities %>% mutate(yi = ifelse(Conn_lcclass == "resist", yi*-1, yi)) 
# for distance, inverse the relationship by multiplying yi by -1 to get proximity to sites
cities <- cities %>% mutate(yi = ifelse(Conn_lcclass == "dist", yi*-1, yi))
# for dist-green, inverse the relationship by multiplying yi by -1 to get proximity to greenspace
cities <- cities %>% mutate(yi = ifelse(Conn_lcclass == "dist-green", yi*-1, yi))
# for areas with grey, inverse the relationship by multiplying yi by -1 to get area non-grey
cities <- cities %>% mutate(yi = ifelse(Conn_lcclass == "grey", yi*-1, yi))

## Change the conn_feat to be approriate for direction of effects
### find and replace all instances of distance to proximity, while keeping the rest of the string the same
cities <- cities %>% mutate(Conn_feat = str_replace(Conn_feat, "distance", "proximity")) %>% 
                     mutate(Conn_feat = str_replace(Conn_feat, "resistance", "permeability"))

### 2.3 Combine similar response metrics into a single category (e.g., species richness, abundance, etc.) ####
es_dat_pool <- cities %>%
  mutate(r_metric = case_when(
    str_detect(r_metric, "beta diversity|dissimilarity$") ~ "beta-diversity", #combine dissimilarity metrics
    str_detect(r_metric, "movement|return time|activity ratio") ~ "movement", #combine movement metrics
    str_detect(r_metric, "evenness") ~ "evenness", #combine evenness metrics
    str_detect(r_metric, "functional") ~ "functional traits", #combine functional trait metrics
    TRUE ~ r_metric)) %>%
  group_by(r_metric) %>%
  filter(n() >= 10) %>% ungroup()

es_dat_pool %>% count(r_metric) %>% arrange(desc(n))

## Aggregate specific studies that have LARGE number of effect sizes
es_dat_agg1 <- es_dat_pool %>% filter(studyID == "Delgado de la flor_2020_80") %>% 
                                  escalc(measure = "ZCOR", yi = yi, vi = vi, data = .) 

es_dat_agg2 <- es_dat_pool %>% filter(studyID == "Herrmann_2023_88") %>% 
                                  escalc(measure = "ZCOR", yi = yi, vi = vi, data = .)
  
#aggregate the effect sizes, clustered by the r_metric, subsetted by connectivity metric
agg1 <- aggregate(es_dat_agg1 %>% filter(Conn_feat == "area"), 
                  cluster = r_metric, rho = 0.5)
agg2 <- aggregate(es_dat_agg1 %>% filter(Conn_feat == "mean proximity"), 
                  cluster = r_metric, rho = 0.5)

#aggregate the effect sizes, subsetted for each study_class
agg3 <- aggregate(es_dat_agg2 %>% filter(Conn_feat == "area"), 
                  cluster = r_metric, rho = 0.5)
agg4 <- aggregate(es_dat_agg2 %>% filter(Conn_feat == "proximity-weighted area"),
                  cluster = r_metric, rho = 0.5)
agg5 <- aggregate(es_dat_agg2 %>% filter(Conn_feat == "graph network"),
                  cluster = r_metric, rho = 0.5)

## combine the aggregated effect sizes with the original dataset
### remove the aggregated studies from the original dataset
es_dat_pool <- es_dat_pool %>% filter(!studyID %in% c("Delgado de la flor_2020_80", "Herrmann_2023_88"))
es_dat_pool <- bind_rows(es_dat_pool, agg1, agg2, agg3, agg4, agg5)

## renumber the effect sizes
es_dat_pool <- es_dat_pool %>% mutate(ES_no = row_number())
# how many effect sizes per response metric? sorted in descending order
es_dat_pool %>% count(r_metric) %>% arrange(desc(n))

### CLEANUP ENVIRONMENT ####
rm(list = c("es_dat", "cityghsl", "citieslist", "ghsl", "cityage", "cities",
            "es_dat_agg1", "es_dat_agg2", "agg1", "agg2", "agg3", "agg4", "agg5"))

# save to csv ######
write_csv(es_dat_pool, "./data/14-effectsize_data_pooled.csv")