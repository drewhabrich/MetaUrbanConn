## HEADER---------------------------
## Script name: 13-effectsize_calculation
##
## Purpose of script: Convert extracted data from eligible articles to comparable effect sizes
##
## Author: Andrew Habrich
##
## Date Created: 2024-02-22
## Date last Modified: 2024-05-31
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
# currently for data for 152 scientific articles

## 1. Load relevant packages---------------------------------------------------
### for stats
library(tidyverse)
library(easystats)
library(metafor)
library(esc)

### for figures
library(DataExplorer)
library(ggpubr)

### misc functions
library(Hmisc)

## 2. Read in data-------------------------------------------------------------
rm(list = ls()) #clear environment before loading raw data
raw_es_df <- read_csv("./raw_data/13-effectsize_calculations/literature_extracted_rawdata.csv", na = "na") 
glimpse(raw_es_df)

#unique values in response column
unique(raw_es_df$r_metric)
unique(raw_es_df$r_method)
unique(raw_es_df$Conn_feat)

## clean up some variables
raw_es_df$ri <- as.numeric(raw_es_df$ri) #coerce to numeric
raw_es_df$Study_no <- as.character(raw_es_df$Study_no) #coerce to character
raw_es_df$r_metric <- ifelse(str_detect(raw_es_df$r_metric, "^genetic"), "genetic dist.", raw_es_df$r_metric)

## create an IDlabel
raw_es_df <- raw_es_df %>% 
  mutate(studyID = str_c(FirstAuth, Pub_year, Study_no, sep = "_")) %>% #concatenate the strings into 1 label
  relocate(studyID, .before = Study_no) #put it at the front of the dataframe

## 3. Overview ----------------------------------------------------------------
raw_es_df %>% n_unique() #how many unique rows per column are there?
raw_es_df %>% distinct(.$r_metric)
raw_es_df %>% distinct(.$corr_type)

### 3.1 Connectivity metrics figures ----
### Barplot of connectivity metrics used
raw_es_df %>% group_by(Conn_type) %>% tally() %>% 
  ggbarplot(x = "Conn_type", y = "n", ggtheme = theme_bw(), label = T, lab.pos = "out", lab.hjust = 1.2,lab.vjust = 0.5,
            xlab = "Category of connectivity metric", ylab = "# of effect sizes", orientation = "horizontal", sort.val = "asc") 
## Barplot of the features for connectivity metrics used
raw_es_df %>% group_by(Conn_feat) %>% tally() %>% 
  ggbarplot(x = "Conn_feat", y = "n", ggtheme = theme_bw(), label = T, lab.pos = "out", lab.hjust = 1.2,lab.vjust = 0.5,
            xlab = "Feature of connectivity", ylab = "# of effect sizes", orientation = "horizontal", sort.val = "asc")

### 3.2 Study information ----
### how many studies per journal
raw_es_df %>%
  group_by(Pub_name) %>%
  summarise(studycount = n_distinct(Study_no)) %>% 
  ggplot(aes(x = reorder(Pub_name, studycount), y = studycount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = studycount), hjust = -0.1, size = 3) +
  labs(title = "Number of studies with data extracted by Journal",
       x = "Journal",
       y = "Number of unique studies") +
  theme_bw() + scale_x_discrete(
    labels = function(x)
      str_trunc(x, 25)) + 
  theme(axis.text.y = element_text(size = 8)) +  coord_flip()

### how many studies per City
raw_es_df %>%
  group_by(City) %>%
  summarise(studycount = n_distinct(Study_no)) %>% 
  ggplot(aes(x = reorder(City, studycount), y = studycount)) +
  geom_bar(stat = "identity", fill = "orchid") +
  geom_text(aes(label = studycount), hjust = -0.1, size = 3) +
  labs(title = "Number of studies with data extracted by City",
       x = "City",
       y = "Number of unique studies") +
  theme_bw() +  theme(axis.text.y = element_text(size = 8)) +  coord_flip()

### How many studies per country?
raw_es_df %>%
  group_by(Country) %>%
  summarise(studycount = n_distinct(Study_no)) %>% 
  ggplot(aes(x = reorder(Country, studycount), y = studycount)) +
    geom_bar(stat = "identity", fill = "orchid") +
    geom_text(aes(label = studycount), hjust = -0.1, size = 3) +
    labs(title = "Number of Unique studies by Country",
       x = "Country",
       y = "Number of unique studies") +
    theme_bw() +  theme(axis.text.y = element_text(size = 8)) +  coord_flip()

### How many effect sizes per country?
raw_es_df %>%
  group_by(Country) %>%
  tally() %>% 
  ggplot(aes(x = reorder(Country, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorchid") +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  labs(title = "Number of effect sizes by Country",
       x = "Country",
       y = "Number of effect sizes") +
  theme_bw() +  theme(axis.text.y = element_text(size = 8)) +  coord_flip()

### Effect sizes by study taxa
raw_es_df %>%
  group_by(Study_class) %>%
  tally(sort = T) %>% 
  ggplot(aes(x = reorder(Study_class, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkorchid") +
  geom_text(aes(label = n), hjust = -0.1, size = 3) +
  labs(title = "Number of effect sizes by taxonomic class",
       x = "Taxonomic class",
       y = "Number of effect sizes") +
  theme_bw() +  theme(axis.text.y = element_text(size = 8)) +  coord_flip()

### Effect sizes by country and taxa
raw_es_df %>%
  group_by(Country, Study_class) %>%
  tally(sort = T) %>% ungroup(Study_class) %>% mutate(total = sum(n)) %>% 
  ggplot(aes(x = reorder(Country, total), y = n, fill = Study_class)) +
  geom_bar(stat = "identity", colour = "black") +
  geom_text(aes(label = n), size = 3, check_overlap = T, position = position_stack(vjust = 0.5)) +
  labs(title = "Number of effect sizes by Country and taxa",
       x = "Country",
       y = "Number of effect sizes") +
  theme_bw() + theme(axis.text.y = element_text(size = 8), legend.position = c(0.85,0.35)) + coord_flip()

### 3.3 Response metrics ----
## quick and dirty with DataExplorer
plot_bar(raw_es_df[,20:24], ggtheme = theme_bw())

### Number of effect sizes per response metric 
raw_es_df %>% group_by(r_metric) %>% tally() %>% 
  ggbarplot(x = "r_metric", y = "n", ggtheme = theme_bw(), fill = "green4",
            label = T, lab.pos = "out", lab.hjust = 1.2, lab.vjust = 0.5, lab.size = 3.5, lab.col = "white",
            xlab = "Response Metric", ylab = "# of effect sizes", orientation = "horizontal", sort.val = "asc",
            position = position_dodge())

### Number of effect sizes per correlational type
raw_es_df %>% group_by(corr_type) %>% tally() %>% 
  ggbarplot(x = "corr_type", y = "n", ggtheme = theme_bw(), fill = "green4",
            label = T, lab.pos = "out", lab.hjust = 1.2, lab.vjust = 0.5, lab.size = 3.5, lab.col = "black",
            xlab = "Correlation metric", ylab = "# of effect sizes", orientation = "horizontal", sort.val = "asc",
            position = position_dodge())

## 4. effect size calculations and fisher's z-transformation ------------------
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

### 4.1 Correlations coefficients - Calculate effect sizes and convert to fisher's Z
es_p <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = pearson_es)
es_s <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = spearman_es)
es_m <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = mantel_es)
es_reg <- escalc(measure = "ZCOR", ti = ti, ni = ni, data = regression_es)
es_glm <- escalc(measure = "ZCOR", ti = ti, pi = pi, ni = ni, data = glm_es)

### 4.2 Logistic, Odds-ratio, and probability - calculate and convert to correlation by converting to d then r.
OR_es <- raw_es_df %>% filter(corr_type == "glm-OR" | 
                              corr_type == "glm-RR") %>% 
                       mutate(ri = oddsratio_to_r(.$bi, ni = .$ni)) #convert to r
logis_es <- raw_es_df %>% filter(corr_type == "glm-logistic" | 
                                 corr_type == "glm-binomial") %>% 
                       mutate(ri = logoddsratio_to_r(.$bi, ni = .$ni)) #convert to r

es_OR <- escalc(measure = "ZPCOR", mi = mi, ri = ri, ni = ni, data = OR_es)
es_logis <- escalc(measure = "ZCOR", ri = ri, ni = ni, data = logis_es, subset = is.na(mi)) #subset to remove NAs
es_plogis <- escalc(measure = "ZPCOR", mi = mi, ri = ri, ni = ni, data = logis_es, subset = !is.na(mi)) #partial correlations for multivariate models

### 4.3 Partial correlations - Calculate effect sizes and convert to fisher's Z
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

### 4.4. Combine into one dataframe 
es_dat <- bind_rows(es_p, es_s, es_m, es_reg, es_glm, es_glmm, es_OR, es_logis, es_plogis, es_partial, es_pcor, es_preg, es_other)
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
write_csv(es_dat, "./data/13-effectsize_data.csv")

### EXTRA visualize ####
# #### by response metric
# es_dat %>% group_by(r_metric) %>% tally() %>% 
#   ggbarplot(x = "r_metric", y = "n", ggtheme = theme_bw(), fill = "green4",
#             label = T, lab.pos = "out", lab.hjust = 1.2, lab.vjust = 0.5, lab.size = 3.5, lab.col = "white",
#             xlab = "Response Metric", ylab = "# of effect sizes", orientation = "horizontal", sort.val = "asc",
#             position = position_dodge())
# 
# #### by connectivity metric
# es_dat %>% group_by(Conn_metric) %>% tally() %>% 
#   ggbarplot(x = "Conn_metric", y = "n", ggtheme = theme_bw(), fill = "green4",
#             label = T, lab.pos = "out", lab.hjust = 1.2, lab.vjust = 0.5, lab.size = 3.5, lab.col = "white",
#             xlab = "Connectivity Metric", ylab = "# of effect sizes", orientation = "horizontal", sort.val = "asc",
#             position = position_dodge())
# 
# #### by correlation type
# es_dat %>% group_by(rcorr_type) %>% tally() %>% 
#   ggbarplot(x = "rcorr_type", y = "n", ggtheme = theme_bw(), fill = "green4",
#             label = T, lab.pos = "out", lab.hjust = 1.2, lab.vjust = 0.5, lab.size = 3.5, lab.col = "white",
#             title = "Correlation vs Partial-correlation", ylab = "# of effect sizes", xlab = "",
#             orientation = "horizontal", sort.val = "asc")
# 
# #### density of effect sizes by studyclass
# ggdensity(data = es_dat, x = "yi", y = "density", add = "mean", rug = T, 
#           color = "Study_class", fill = "Study_class", palette = get_palette("Set1", 9), size = 1)
# 
# ggdensity(data = es_dat, x = "yi", y = "density", add = "mean", rug = T, 
#           color = "Conn_feat", fill = "Conn_feat", palette = get_palette("Set2", 12), size = 1)
