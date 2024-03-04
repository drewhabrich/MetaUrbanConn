## HEADER---------------------------
## Script name: 12 manualcalculations
##
## Purpose of script: To manually calculate correlation coefficients from raw data extracted from literature
##
## Author: Andrew Habrich
##
## Date Created: 2024-02-12
## Date last Modified: 2024-03-04
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
library(tidyverse)
library(easystats)
library(ggpubr)
library(corrplot)
library(vegan)

## 2. Manual calculations from raw data -----
## DRI 2021
dri2021<-read_csv("./raw_figextraction/suppls/Dri_2021.csv")
cor(dri2021)
ggscatter(dri2021, y = "SR_obs", x = "centrality") + stat_cor(method = "pearson")

## JOHNSON 2016
johnson2016<-read_csv("./raw_figextraction/suppls/Johnson_2016_tabl3.csv") %>% select(-"fragID")
cor(johnson2016)
ggscatter(johnson2016, y = "SR", x = "Isolation") + stat_cor(method = "pearson")

## FATTORINI 2016
fattorini2016<-read_csv("./raw_figextraction/suppls/Fattorini_2016_tabl1.csv") %>% select(-"patchID") %>% 
  mutate(prop_forest = forestedarea/area)
ggscatter(fattorini2016, y = "SR", x = "mean_ENN") + stat_cor(method = "pearson")
correlation(fattorini2016, method = "pearson")

## HOWES 2021
Howes_2021_tablS1<-read_csv("./raw_figextraction/suppls/Howes_2021_tablS1.csv") %>% select(-c("Name", "Type"))
ggscatter(Howes_2021_tablS1, y = "SR", x = "correlationlength") + stat_cor(method = "pearson")
correlation(Howes_2021_tablS1, method = "pearson")

## CORTESARZOLA 2021
CortesArzola_2021_tabl1<-read_csv("./raw_figextraction/suppls/Cortes-Arzola_2021_tabl1.csv") %>% select(-"site")
ggscatter(CortesArzola_2021_tabl1, y = "SR", x = "ENN_m") + stat_cor(method = "pearson")
correlation(CortesArzola_2021_tabl1, method = "pearson")

## TAN 2021
Tan_2021_tabl1<-read_csv("./raw_figextraction/suppls/Tan_2021_tabl1.csv") %>% select(-c("name","noise","ExpSR","Park")) %>% relocate("nested_rank", .before = "area")
ggscatter(Tan_2021_tabl1, y = "nested_rank", x = "Isolation1") + stat_cor(method = "spearman")
ggscatter(Tan_2021_tabl1, y = "nested_rank", x = "Isolation2") + stat_cor(method = "spearman")
correlation(Tan_2021_tabl1, method = "spearman", partial = F, ranktransform = T)

## SANESI 2009
sanesi_2009<-read_csv("./raw_figextraction/suppls/Sanesi_2009_tabl.csv") %>% select(-c("Greenspace","Code")) %>% 
  mutate(prop_forest = FC_ha/area_ha) #NOTE: Sites from Bari were removed
ggscatter(sanesi_2009, y = "SR", x = "dist_urb")
correlation(sanesi_2009, method = "pearson")

## DELGADO DE LA FLOR 2020 
data <- read_csv("./raw_figextraction/suppls/DelgadoDeLaFlor2020raw.csv")
glimpse(data)
colnames(data)
min_dat <- data %>% select(
  -c( "Ambush abundance","Ground abundance",      "Orb weavers abundance",      "FolRunStalk abundance",      "Sheet weaver abundance",
      "Space weaver abundance",      "Specialist abundance",      "Hunter NoWeb",      "SheetCob Weaver",      "OrbWeb Weaver",
      "Narrow Diet",      "Broad Diet",      "Ground Dweller",      "Pollution Load Index",      "Treatment",      "Neighborhood",
      "Plant Dweller",       "Diurnal Activity",      "Noctural Activity",       "BodySize CWM",      "Mowing",      "Plant Biomass",
      "Bloom Abundance",      "Bloom Area",      "Plant Height",      "Funct Divergence",      "Funct Dispersion")) 
glimpse(min_dat)
names(min_dat) <- make.names(names(min_dat))
colnames(min_dat)

divvar <- min_dat %>% select(1:15)
lsvar <- min_dat %>% select(c("Perc.Impervious.200","Patch.Isolation.200","Perc.Impervious.1500","Patch.Isolation.1500"))

correlation(data = divvar %>% filter(Year == "yr15"), 
            data2 = lsvar[1:32,], #select appropriate row-years for ls data
            method = "pearson")
correlation(data = divvar %>% filter(Year == "yr16"),
            data2 = lsvar[33:64,], #select appropriate row-years for ls data
            method = "pearson")

## ECKERT 2017
eckert_sites <- read_csv("./raw_figextraction/suppls/Eckert2017raw.csv")
#calculate diversity metrics from the raw data (these were copied to the csv file)
S<-specnumber(eckert_sites[,2:22])
H<-diversity(eckert_sites[,2:22], index = "shannon")
D<-diversity(eckert_sites[,2:22], index = "simpson")
H/log(S)
eckert2017 <- read_csv("./raw_figextraction/suppls/Eckert2017specrich.csv") 
correlation(eckert2017[,3:9])

## STORCKTONON 2013
st_2013 <- readxl::read_excel("./raw_figextraction/suppls/Storck-Tonon2013raw.xlsx")
correlation(st_2013)

## HERMANN 2023
herrmann2023 <- readxl::read_excel("./raw_figextraction/suppls/Herrmann2023raw.xlsx")
hmann2023 <- herrmann2023 %>% select(-c("Site","Long","Lat","Size_patch","Bare_soil","Vegetation_height","Flower_units",
                         "Honeybee_flower_visits","Wild_bee_flower_visits","Butterfly_flower_visits","Hoverfly_flower_visits",
                         "d_species","modularity_species","H2_species",
                         "d_genera","modularity_genera","connectance_genera","NODF_genera","H2_genera")) %>% relocate(connectance_species, .after = `3D_connectivity`)
correlation(data = hmann2023[,1:5], data2 = hmann2023[,6:15], method = "pearson")

## VANSTOCKEM 2019
vs2019 <- read_csv("./raw_figextraction/suppls/Vanstockem2019_suppl4.csv") %>% 
  select(-c("id","Zone","EC","pHKCl","Cnratio","sub_depth","Age","RoofHeigh","SVF","HLI","VisitYear","Vegday","roofclust"))
correlation(data = vs2019[,1:10], data2 = vs2019[,12], method = "pearson", ranktransform = T)

## FABIAN 2021
fab2021 <- read_csv("./raw_figextraction/suppls/Fabian2021rawdata.csv") %>% 
  filter(zone != "vp") %>% #remove the Paravachasca Valley data cause its not in Cordoba
  select(-c("logHeight","logAge","logDepth","zone","trap")) %>% #remove useless columns
  mutate(across(starts_with("log"), ~10^., .names = "raw_{.col}")) #backtransform the log10 columns
  
f21 <- fab2021 %>% group_by(house) %>% 
  summarize(across(everything(), mean)) %>% relocate("raw_logPlantRichness", .after = "LocalPlantCover")
correlation(data = f21, data2 = f21[,19], method = "pearson") 

## OLIVER 2011
oli2011raw <- read_csv("./raw_figextraction/suppls/Oliver2011raw.csv")
oli2011 <- oli2011raw %>% mutate(across(4:8, ~log10(. + 0.001)))
correlation(data = oli2011[,1:3], data2 = oli2011[,4:8], method = "spearman")
ggscatter(oli2011, y = "BR", x = "devl_pix1km") 
ggscatter(oli2011raw, y = "BR", x = "devl_pix1km")

## LIAO 2022
liao22 <- readxl::read_excel("./raw_figextraction/suppls/Liao2022raw.xlsx") %>% mutate(Dis.km = as.numeric(Dis.km),
                                                                                       Bray = as.numeric(Bray))
glimpse(liao22)
l22 <- liao22 %>% group_by(ID) %>% summarize(
    avg_Dis.km = mean(Dis.km, na.rm = TRUE),
    avg_Bray = mean(Bray, na.rm = TRUE))
correlation(l22, method = "pearson")
ggscatter(l22, y = "avg_Bray", x = "avg_Dis.km") + stat_cor(method = "pearson")
hist(l22$avg_Bray)

## VANHEENZIK2008
vh2008 <- readxl::read_excel("./raw_figextraction/suppls/Vanheezik2008raw.xlsx")
a<-vh2008 %>% group_by(class) %>% 
  correlation(select = c("S","Diversity"), select2 = c("ENN", "prop_vegbuffer"), method = "spearman")
