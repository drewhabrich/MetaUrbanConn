## HEADER---------------------------
## Script name: 13-metaanalytic modelling-response metrics dataset preparation
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

## 1. Load relevant packages--------
pacman::p_load(devtools, tidyverse, R.rsp, flextable, easystats, #basic packages
               metafor, orchaRd, emmeans, #meta-analysis packages
               DataExplorer, ggpubr, patchwork) #data exploration and viz packages 

### custom functions ###
## function to remove accents from city names
remove_accents <- function(x) {
  stringi::stri_trans_general(x, "latin-ascii")
}

## 2. Load data ---------------------
es_dat <- read_csv("./data/12-effectsize_data.csv")
#### global human settlement layer data https://data.jrc.ec.europa.eu/dataset/53473144-b88c-44bc-b4a3-4583ed1f547e
ghsl <- readxl::read_xlsx("./raw_data/ghsl/globalhumansettlement2019.xlsx")
#### city age data from https://doi.org/10.1371/journal.pone.0160471
cityage <- read_csv("./raw_data/cityage/cityage_polygons.csv") 

### 2.1 Data cleaning and pre-processing ----
### fix the study year column
es_dat$Study_year <- stringr::str_remove(es_dat$Study_year, "-.*") %>% as.numeric(es_dat$Study_year)
### remove accents from city names
es_dat$City_clean <- remove_accents(es_dat$City)
ghsl$city <- remove_accents(ghsl$city)
cityage$City_clean <- remove_accents(cityage$eFUA_name)

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
cities$City_clean <- cities$City_clean %>% str_replace("Mexico city", "Mexico City")

citieslist <- cities %>% select(City_clean, Country) %>% distinct() 
## merge the distinct cities from the es_data that are in the global human settlement dataframe
cityghsl <- citieslist %>% left_join(ghsl, by = c("City_clean" = "city", "Country" = "country")) %>% 
  slice(-41) ## remove row 41 from the dataframe (the faulty nanjing entry)

## merge columns from city_dat to es_dat, matched based on city and study_year
cities <- left_join(cities, cityghsl, by = c("City_clean" = "City_clean", 
                                             "Country" = "Country"))
## merge age data by city and country, selecting only the age metric
cities <- left_join(cities, cityage %>% select(c(City_clean, Cntry_name, Age_mean_m, Age_SD_mea)), 
                    by = c("City_clean" = "City_clean", 
                           "Country" = "Cntry_name"))

### FOR GHSL data merge the city data with the appropriate study year for the effect size
cities <- cities %>% group_by(studyID) %>% 
  mutate(study_year_bin = case_when(Study_year <= 1999 ~ "before 1999",
                                    Study_year >= 2000 & Study_year <= 2013 ~ "2000-2013",
                                    Study_year >= 2014 ~ "after 2014")) %>% ungroup()

## create a new greenness column matching the study_yearbin to the appropriate column
cities <- cities %>% mutate(green_avg = case_when(study_year_bin == "before 1999" ~ E_GR_AV90,
                                                  study_year_bin == "2000-2013" ~ E_GR_AV00,
                                                  study_year_bin == "after 2014" ~ E_GR_AV14),
                            green_km2 = case_when(study_year_bin == "before 1999" ~ E_GR_AT90,
                                                  study_year_bin == "2000-2013" ~ E_GR_AT00,
                                                  study_year_bin == "after 2014" ~ E_GR_AT14),
                            built_km2 = case_when(study_year_bin == "before 1999" ~ built90_km2,
                                                  study_year_bin == "2000-2013" ~ built00_km2,
                                                  study_year_bin == "after 2014" ~ built15_km2),
                            pop_km2 = case_when(study_year_bin == "before 1999" ~ pop90,
                                                  study_year_bin == "2000-2013" ~ pop00,
                                                  study_year_bin == "after 2014" ~ pop15)) 
## create new columns for city variables
cities <- cities %>% 
  mutate(absLat = abs(lat),
         prop_green = area_km2/green_km2,
         pop_dens = pop_km2/area_km2,
         greenbuiltratio = green_km2/built15_km2,
         green_avg = as.numeric(green_avg))

## log transform some of the city variables
cities <- cities %>% 
  mutate(area_km2_log = log(area_km2 + 1), 
         built_km2_log = log(built_km2 + 1),
         pop_km2 = log(pop_km2 + 1),
         green_km2_log = log(green_km2 + 1),
         prop_green_log = log(prop_green + 1),
         pop_dens_log = log(pop_dens + 1))

## Save to the cityname and latlong data to a csv file. uncomment to save
#write_csv(cities %>% select(City_clean, Country, lat, long) %>% distinct() %>% arrange(Country),
#          "./data/13-cities_data.csv")

## remove columns that are not needed
es_cities <- cities %>% select(-c(Pub_type, Study_effect, Conn_tool, r_method, corr_type,
                                  mi, ri, ti, fi, pi, bi, var_sdi, var_type, R2i, 
                                  country_code, xborder_country, urban_incity, UC_NM_SRC)) 

### 2.2 Convert responses and connectivity metrics for consistent direction of effect sizes ####
#### CONN METRICS
# for resist, inverse the relationship by multiplying yi by -1 to get permeability 
es_cities <- es_cities %>% mutate(yi = ifelse(Conn_lcclass == "resist", yi*-1, yi)) 
# for distance, inverse the relationship by multiplying yi by -1 to get proximity to sites
es_cities <- es_cities %>% mutate(yi = ifelse(Conn_lcclass == "dist", yi*-1, yi))
# for dist-green, inverse the relationship by multiplying yi by -1 to get proximity to greenspace
es_cities <- es_cities %>% mutate(yi = ifelse(Conn_lcclass == "dist-green", yi*-1, yi))
# for areas with grey, inverse the relationship by multiplying yi by -1 to get area non-grey
es_cities <- es_cities %>% mutate(yi = ifelse(Conn_lcclass == "grey", yi*-1, yi))

## Change the conn_feat to be appropriate for direction of effects
### find and replace all instances of distance to proximity, while keeping the rest of the string the same
es_cities <- es_cities %>% mutate(Conn_feat = str_replace(Conn_feat, "distance", "proximity")) %>% 
                           mutate(Conn_feat = str_replace(Conn_feat, "resistance", "permeability"))

#### RESPONSE METRICS
es_cities <- es_cities %>% #inverse the relationship for genetic dist to similarity
                           mutate(yi = ifelse(r_metric == "genetic dist.", yi*-1, yi)) %>% 
                           mutate(r_metric = str_replace(r_metric, "genetic dist.", "genetic similarity")) %>% 
                           #inverse the relationship for community dissimilarity metrics to similarity
                           mutate(yi = ifelse(str_detect(r_metric, "dissimilarity$"), yi*-1, yi)) %>%
                           mutate(r_metric = str_replace(r_metric, "dissimilarity$", "similarity"))

### 2.3 Combine similar response metrics into bins and aggregate studies that have outlier datapoints ####
es_dat_pool <- es_cities %>%
  mutate(r_metric_bin = case_when(
    r_metric %in% c("movement", "movement distance", "effective movement distance", 
                    "movement frequency", "movement presence", "movement probability", 
                    "movement speed", "activity ratio", "return time") ~ "movement",
    r_metric %in% c("jaccard similarity", "bray-curtis similarity", 
                    "sorensen similarity", "nestedness temperature") ~ "community similarity",
    r_metric %in% c("pielou evenness","shannon diversity", 
                    "phylogenetic diversity","simpson reciprocal index") ~ "diversity indices",
    r_metric %in% c("abundance", "population size") ~ "abundance",
    r_metric %in% c("functional richness", "functional beta diversity", 
                    "functional evenness", "functional diversity") ~ "functional diversity",
    TRUE ~ r_metric))

## Aggregate specific studies that have LARGE number of effect sizes
es_dat_agg1 <- es_dat_pool %>% filter(studyID == "Delgado de la flor_2020_80") %>% 
                                  escalc(measure = "ZCOR", yi = yi, vi = vi, data = .) 
es_dat_agg2 <- es_dat_pool %>% filter(studyID == "Herrmann_2023_88") %>% 
                                  escalc(measure = "ZCOR", yi = yi, vi = vi, data = .)
es_dat_agg3 <- es_dat_pool %>% filter(studyID == "Pla-Narbona_2022_24") %>% 
                                  escalc(measure = "ZCOR", yi = yi, vi = vi, data = .)
es_dat_agg4 <- es_dat_pool %>% filter(studyID == "Berthon_2021_124") %>% 
                                  escalc(measure = "GEN", yi = yi, vi = vi, data = .)
  
#Delgado de la flor_2020_80 aggregate the effect sizes, clustered by the r_metric, subsetted by connectivity metric
agg1 <- aggregate(es_dat_agg1 %>% filter(Conn_feat == "area"), 
                  cluster = r_metric_bin, rho = 0.5)
agg2 <- aggregate(es_dat_agg1 %>% filter(Conn_feat == "mean proximity"), 
                  cluster = r_metric_bin, rho = 0.5)

#Herrmann_2023_88 aggregate the effect sizes, subsetted for each study_class
agg3 <- aggregate(es_dat_agg2 %>% filter(Conn_feat == "area"), 
                  cluster = r_metric_bin, rho = 0.5)
agg4 <- aggregate(es_dat_agg2 %>% filter(Conn_feat == "proximity-weighted area"),
                  cluster = r_metric_bin, rho = 0.5)
agg5 <- aggregate(es_dat_agg2 %>% filter(Conn_feat == "graph network"),
                  cluster = r_metric_bin, rho = 0.5)

#Pla-Narbona_2022_24 aggregate the effects sizes across the whole dataset
agg6 <- aggregate(es_dat_agg3, cluster = r_metric_bin, rho = 0.5)

#Berthon_2021_124 aggregate the effects sizes across the whole dataset
agg7 <- aggregate(es_dat_agg4, cluster = r_metric_bin, rho = 0.5) 
agg7$Study_spec <- as.character(agg7$Study_spec)

#Brown_2002_68 aggregate the effects sizes across the whole dataset
agg8 <- aggregate(escalc(measure = "GEN", yi = yi, vi = vi, 
                         data = es_dat_pool %>% filter(studyID == "Brown_2002_68") ), 
                  cluster = studyID, rho = 0.5)

## combine the aggregated effect sizes with the original dataset
### remove the aggregated studies from the original dataset
es_dat_pool <- es_dat_pool %>% filter(!studyID %in% c("Delgado de la flor_2020_80", 
                                                      "Herrmann_2023_88",
                                                      "Pla-Narbona_2022_24",
                                                      "Berthon_2021_124",
                                                      "Brown_2002_68"))
es_dat_pool <- bind_rows(es_dat_pool, agg1, agg2, agg3, agg4, agg5, agg6, agg7, agg8)

## renumber the effect sizes
es_dat_pool <- es_dat_pool %>% mutate(ES_no = row_number())
# how many effect sizes per response metric? sorted in descending order
es_dat_pool %>% count(r_metric_bin) %>% arrange(desc(n))

### 2.4 Combine connectivity metrics into bins ####
## reclassify connectivity feature bins
es_dat_pool <- es_dat_pool %>%
  mutate(connfeat_bin = case_when(
    Conn_feat %in% c("proximity","mean proximity", "area-weighted mean proximity", "area-weighted proximity") ~ "proximity",
    Conn_feat %in% c("area", "proximity-weighted area") ~ "area",
    Conn_feat %in% c("graph network", "graph network-weighted") ~ "graph network",
    Conn_feat %in% c("permeability", "permeability-weighted area", "permeability-weighted graph network") ~ "permeability",
    TRUE ~ Conn_feat)) 

## create potential and resistance bins for connectivity metrics
es_dat_pool <- es_dat_pool %>%
  mutate(connfeat_bin = case_when(
  Conn_metric %in% c("incidence function measure", 
                   "functional connectivity index", 
                   "delta probability of connectivity") ~ "potential",
  TRUE ~ connfeat_bin))

## reclassify conn_types based on the connectivity metric
es_dat_pool <- es_dat_pool %>%
  mutate(Conn_type = case_when(
    connfeat_bin %in% c("proximity", "area", "adjacency") ~ "structural",
    connfeat_bin %in% c("other") ~ "structural-other",
    connfeat_bin %in% c("permeability") ~ "functional",
    TRUE ~ connfeat_bin))

### CLEANUP ENVIRONMENT ####
rm(list = c("es_dat", "cityghsl", "citieslist", "ghsl", "cityage", "cities",
            "es_dat_agg1", "es_dat_agg2", "es_dat_agg3", "es_dat_agg4",
            "agg1", "agg2", "agg3", "agg4", "agg5", "agg6", "agg7", "agg8"))

### remove unnecessary columns from the dataset before saving to file
es_dat_pool <- es_dat_pool %>% select(-c(quality, H75_AREAkm2, H90_AREAkm2, H00_AREAkm2, 
                                         built75_km2, built90_km2, built00_km2, built15_km2,
                                         pop75, pop90, pop00, pop15, BUCAP75, BUCAP90, BUCAP00, BUCAP15,
                                         E_GR_AV90, E_GR_AV00, E_GR_AV14, SDG_LUE9015, SDG_A2G14, SDG_OS15MX,
                                         E_GR_AT90, E_GR_AT00, E_GR_AT14, E_GR_AH90, E_GR_AH00, E_GR_AH14,
                                         E_GR_AM90, E_GR_AM00, E_GR_AM14,E_GR_AL90, E_GR_AL00, E_GR_AL14,
                                         study_year_bin))

# save to csv ######
write_csv(es_dat_pool, "./data/13-effectsize_data_pooled.csv")