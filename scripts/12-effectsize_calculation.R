## HEADER---------------------------
## Script name: 12-effectsize_calculation
##
## Purpose of script: Convert extracted data from eligible articles to comparable effect sizes
##
## Author: Andrew Habrich
##
## Date Created: 2024-02-22
## Date last Modified: 2024-11-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list = ls()) #clear environment before loading raw data

## 1. Load relevant packages---------------------------------------------------
pacman::p_load(tidyverse, patchwork, ggpubr, DataExplorer, RColorBrewer, ggpubr, Hmisc,
               easystats, esc, metafor, orchaRd, emmeans)

## 2. Read in data-------------------------------------------------------------
raw_es_df <- read_csv("./raw_data/12-effectsize_calculations/literature_extracted_rawdata.csv", na = "na") 

## clean up some variables
raw_es_df$ri <- as.numeric(raw_es_df$ri) #coerce to numeric
raw_es_df$Study_no <- as.character(raw_es_df$Study_no) #coerce to character
raw_es_df$r_metric <- ifelse(str_detect(raw_es_df$r_metric, "^genetic"), "genetic dist.", raw_es_df$r_metric)

## create an IDlabel
raw_es_df <- raw_es_df %>% 
  mutate(studyID = str_c(FirstAuth, Pub_year, Study_no, sep = "_")) %>% #concatenate the strings into 1 label
  relocate(studyID, .before = Study_no) #put it at the front of the dataframe

## 3. effect size calculations and fisher's z-transformation ------------------
raw_es_df %>% distinct(corr_type) #what correlation types are there?

## subset into datatypes based on response and correlation types
pearson_es <- raw_es_df %>% filter(corr_type == "pearson")
spearman_es <- raw_es_df %>% filter(corr_type == "spearman") %>% mutate(ri = 2*sinpi(ri/6)) #convert to pearson
mantel_es <- raw_es_df %>% filter(corr_type == "mantel")
regression_es <- raw_es_df %>% filter(corr_type == "regression")
glm_es <- raw_es_df %>% filter(corr_type == "glm-poisson" | 
                               corr_type == "glm-beta" |
                               corr_type == "glm-negativebinomial" |
                               corr_type == "glm-normal" ) %>% filter(is.na(mi))

### 3.1 Correlations coefficients - Calculate effect sizes and convert to fisher's Z
es_p <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = pearson_es)
es_s <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = spearman_es)
es_m <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = mantel_es)
es_reg <- escalc(measure = "ZCOR", ti = ti, ni = ni, data = regression_es)
es_glm <- escalc(measure = "ZCOR", ti = ti, pi = pi, ni = ni, data = glm_es)

### 3.2 Logistic, Odds-ratio, and probability - calculate and convert to correlation by converting to d then r.
OR_es <- raw_es_df %>% filter(corr_type == "glm-OR" | 
                              corr_type == "glm-RR") %>% 
                       mutate(ri = oddsratio_to_r(.$bi, ni = .$ni)) #convert to r
logis_es <- raw_es_df %>% filter(corr_type == "glm-logistic" | 
                                 corr_type == "glm-binomial") %>% 
                       mutate(ri = logoddsratio_to_r(.$bi, ni = .$ni)) #convert to r

es_OR <- escalc(measure = "ZPCOR", mi = mi, ri = ri, ni = ni, data = OR_es)
es_logis <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = logis_es, subset = is.na(mi)) #subset to remove NAs
es_plogis <- escalc(measure = "ZPCOR", mi = mi, ri = ri, ni = ni, data = logis_es, subset = !is.na(mi)) #partial correlations for multivariate models

### 3.3 Partial correlations - Calculate effect sizes and convert to fisher's Z
## glm partial correlations
p_es <- raw_es_df %>% filter(corr_type == "glm-poisson" | 
                                 corr_type == "glm-beta" |
                                 corr_type == "glm-negativebinomial" |
                                 corr_type == "glm-normal" ) %>% 
                      filter(!is.na(mi)) %>% 
  # if ti is NA, calculate it from bi and sdi, otherwise use ti
                      mutate(ti = ifelse(is.na(ti), bi/as.numeric(var_sdi), ti))
es_partial <- escalc(measure = "ZPCOR", mi = mi, ti = ti, ni = ni, pi = pi, data = p_es)

### calculate partial correlation effect sizes
es_pcor <- raw_es_df %>% filter(corr_type == "partial correlation") %>% 
  escalc(measure = "ZPCOR", mi = mi, ri = ri, ni = ni, pi = pi, data = .)
es_preg <- raw_es_df %>% filter(corr_type == "partial regression") %>% 
  escalc(measure = "ZCOR", ti = ti, ni = ni, pi = pi, data = .)
es_glmm <- raw_es_df %>% filter(corr_type == "glmm" | 
                                corr_type == "lmm") %>% 
                         mutate(ti = ifelse(is.na(ti), bi/as.numeric(var_sdi), ti)) %>% 
  escalc(measure = "ZPCOR", mi = mi, ti = ti, ni = ni, pi = pi, data = .) %>% 
  filter(!is.na(yi)) #remove rows with na in yi

### misc other effects 
es_other <- raw_es_df %>% filter(corr_type == "biserial correlation") %>% 
  escalc(measure = "ZCOR", ri = ri, ni = ni, data = .) #NOTE, already converted to r during extraction

### 3.4. Combine into one dataframe 
es_dat <- bind_rows(es_p, es_s, es_m, es_reg, es_glm, es_glmm, es_OR, 
                    es_logis, es_plogis, es_partial, es_pcor, es_preg, es_other)
es_dat <- es_dat %>% mutate(rcorr_type = ifelse(is.na(mi), "full", "part")) %>% #add a column for partial or full
                     mutate(rcorr_type = ifelse(corr_type == "partial correlation", "part", rcorr_type)) %>% #fix the p.cor label
                     mutate(rcorr_type = ifelse(corr_type == "partial regression", "part", rcorr_type))
### what rows from the raw_es_dat are not in the any of the subsetted dataframes?
es_miss <- raw_es_df %>% anti_join(es_dat, by = c("studyID")) 
es_miss %>% nrow() #how many rows are missing?

### how many unique studies are there in the effect size dataframe?
es_dat %>% distinct(studyID) %>% nrow()
### how many unique cities are there in the effect size dataframe?
es_dat %>% distinct(City) %>% nrow()

# save to csv for modelling
write_csv(es_dat, "./data/12-effectsize_data.csv")