## HEADER---------------------------
## Script name: 12 manualcalculations
##
## Purpose of script: To manually calculate correlation coefficients from raw data extracted from literature
##
## Author: Andrew Habrich
##
## Date Created: 2024-02-12
## Date last Modified: 2024-04-10
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
dri2021<-read_csv("./raw_figextraction/suppls/Dri2021suppl1.csv")
correlation(dri2021)
cor(dri2021)
ggscatter(dri2021, y = "SR_obs", x = "centrality") + stat_cor(method = "pearson")

## JOHNSON 2016
johnson2016<-read_csv("./raw_figextraction/suppls/Johnson2016tabl3.csv") %>% select(-"fragID")
cor(johnson2016)
ggscatter(johnson2016, y = "SR", x = "Isolation") + stat_cor(method = "pearson")

## FATTORINI 2016
fattorini2016<-read_csv("./raw_figextraction/suppls/Fattorini2016tabl1.csv") %>% select(-"patchID") %>% 
  mutate(prop_forest = forestedarea/area)
ggscatter(fattorini2016, y = "SR", x = "mean_ENN") + stat_cor(method = "pearson")
correlation(fattorini2016, method = "pearson")

## HOWES 2021
Howes_2021_tablS1<-read_csv("./raw_figextraction/suppls/Howes2021supplS1.csv") %>% select(-c("Name", "Type"))
ggscatter(Howes_2021_tablS1, y = "SR", x = "correlationlength") + stat_cor(method = "pearson")
correlation(Howes_2021_tablS1, method = "pearson")

## CORTESARZOLA 2021
CortesArzola_2021_tabl1<-read_csv("./raw_figextraction/suppls/CortesArzola2021tabl1.csv") %>% select(-"site") %>% 
  mutate(sr_avg = SR/transects, ab_avg = abundance/transects, taxa_avg = taxa_n/transects)
ggscatter(CortesArzola_2021_tabl1, y = "sr_avg", x = "ENN_m") + stat_cor(method = "pearson")
correlation(data = CortesArzola_2021_tabl1[,1:4], data2 = CortesArzola_2021_tabl1[,11:13], method = "pearson")

## TAN 2021
Tan_2021_tabl1<-read_csv("./raw_figextraction/suppls/Tan2021tabl1.csv") %>% select(-c("name","noise","ExpSR","Park")) %>% relocate("nested_rank", .before = "area")
ggscatter(Tan_2021_tabl1, y = "nested_rank", x = "Isolation1") + stat_cor(method = "spearman")
ggscatter(Tan_2021_tabl1, y = "nested_rank", x = "Isolation2") + stat_cor(method = "spearman")
correlation(Tan_2021_tabl1, method = "spearman", partial = F, ranktransform = T)

## SANESI 2009
sanesi_2009<-read_csv("./raw_figextraction/suppls/Sanesi2009tabl.csv") %>% select(-c("Greenspace","Code")) %>% 
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
vs2019 <- read_csv("./raw_figextraction/suppls/Vanstockem2019suppl4.csv") %>% 
  select(-c("id","Zone","EC","pHKCl","Cnratio","sub_depth","Age","RoofHeigh","SVF","HLI","VisitYear","Vegday","roofclust"))
correlation(data = vs2019[,1:10], data2 = vs2019[,12], method = "pearson", ranktransform = T)

## FABIAN 2021
fab2021 <- read_csv("./raw_figextraction/suppls/Fabian2021raw.csv") %>% 
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

## CROOKS2002
crooks2002 <- read_csv("./raw_figextraction/suppls/Crooks2002raw.csv") %>% 
  mutate(Sall = rowSums(crooks2002[,6:16]),
         Snative = rowSums(crooks2002[,6:14]), #sum values of presence absence for each site to get richness
         logdisty = log(disty_m), #convert to meters
         logdistz = log(distz_m) #convert to meters
)
# correlations of S and disty_m and distz_m
correlation(data = crooks2002, select = c("Sall", "Snative"), select2=c("disty_m", "distz_m", "logdisty", "logdistz"), method = "pearson")
correlation(data = crooks2002)
# ggscatterplot of Sall and disty_m
ggscatter(crooks2002, y = "Sall", x = "logdisty") + stat_cor(method = "pearson")
ggscatter(crooks2002, y = "Sall", x = "disty_m") + stat_cor(method = "pearson")

## MAGLE2010 (ENN is in Km)
magle2010 <- read_csv("./raw_figextraction/suppls/Magle2010raw.csv") %>% mutate(ENN_m = ENN/1000, #convert to m
                                                                                gen_dist = fst/(1-fst)) #convert to genetic distance
correlation(data = magle2010, select=c("fst", "gen_dist"), select2=c("ENN", "ENN_m", "cost_dist"), method = "pearson")

## PLATT2006
platt2006 <- read_csv("./raw_figextraction/suppls/Platt2006raw.csv") #already rank transformed 
correlation(data = platt2006, select = c("sr","nestednesstemp_rank"), select2 = c("prop_green_1kmbuff","ENN_forest"), method = "pearson")

## YANG2020
yang2020 <- read_csv("./raw_figextraction/suppls/Yang2020raw.csv")
correlation(data = yang2020, select = c("s_obs", "s_exp"), select2 = c("ENN_larger_m", "dist_tourbcore_m","dist_naturalarea_m"), method = "pearson")

## STERZYNSKA2018 (sample size = abundance)
ster2018 <- readxl::read_excel("./raw_figextraction/suppls/Sterzynska2018suppl1.xlsx")
correlation(ster2018, select = c("Species richness", "Sample size"), select2 = c("PROX", "AREA"), method = "pearson")
# ggplot of relationship, colour by Plot Group
ggscatter(ster2018, y = "Species richness", x = "PROX", color = "Plot group",
          palette = c("blue", "green", "red")) + 
  scale_x_log10(breaks = c(1,10,100,1000), limits = c(1, 1200)) + 
  stat_cor(method = "pearson")
ggscatter(ster2018, y = "Species richness", x = "AREA", color = "Plot group",
          palette = c("blue", "green", "red")) + 
  scale_x_log10(breaks = c(1,100, 10000), limits = c(1, 10000)) + 
  stat_cor(method = "pearson")

## NIU2023
niu2023 <- read_csv("./raw_figextraction/suppls/Niu2023raw.csv") %>% 
  mutate(abd = abd_animaldisp + abd_otherdisp,
         sr = sr_animaldisp + sr_otherdisp)
correlation(niu2023, select2 = c("abd_animaldisp","abd_otherdisp","sr_animaldisp","sr_otherdisp","abd","sr"), select = c("prop_nonforest1kmbuffer"), method = "pearson")

## FISCHER2016
fischer2016 <- read_csv("./raw_figextraction/suppls/Fischer2016raw.csv") %>% 
  #get species richness and abundance from the raw data
  #if the species is >0 in the site, it is counted as 1 for SR 
  #and the abundance is the sum of all species in the site
  mutate(SR = rowSums(fischer2016[,10:28] > 0),
         abundance = rowSums(fischer2016[,10:28]))
#gghistogram of SR and abundance
ggscatter(fischer2016, y = "SR", x = "isolation") + stat_cor(method = "pearson")
ggscatter(fischer2016, y = "Bom.ter", x = "isolation") + stat_cor(method = "spearman")

correlation(fischer2016, select = c("abundance"), select2 = c("isolation", "urbanization"), method = "spearman")
correlation(fischer2016, select = c("SR"), select2 = c("isolation", "urbanization"), method = "pearson")
correlation(fischer2016, select = c(colnames(fischer2016[,10:28])), select2 = c("isolation"), method = "spearman")

## NOOTEN2018
nooten2018 <- read_csv("./raw_figextraction/suppls/Nooten2018raw.csv")
ggscatter(nooten2018, y = "Plant spp.b", x = "conn_forest_perim") + stat_cor(method = "spearman")
correlation(nooten2018, select = c("Plant spp.b","Native spp.", "Ant spp.", "Bird spp."  ), 
            select2 = c("conn_forest_perim"), method = "spearman")

## ORLANDIN2021
orlandin2021 <- readxl::read_excel("./raw_figextraction/suppls/Orlandin2021raw.xlsx")
ggscatter(orlandin2021, y = "SR", x = "Dist_to_naturalarea") + stat_cor(method = "spearman")
hist(orlandin2021$Dist_to_naturalarea, breaks = 30)
hist(orlandin2021$SR, breaks = 30)
correlation(orlandin2021, select = c("SR", "SR_adj","shannondiv","simpsondiv"), select2 = c("Dist_to_naturalarea"), method = "spearman")

## SCHUTZ2017
schutz2017 <- readxl::read_excel("./raw_figextraction/suppls/Schutz_etal_2017supplraw.xlsx")
#make a model of logistic regression for presence/absence for each species
#then calculate the correlation between the model and the distance to the nearest forest
compare_models(
  glm(D.major ~ `Dist. nearest breeding site GSW (m)`, data = schutz2017, family = "binomial"),
  glm(D.major ~ `CN_{IDW} _stan`, data = schutz2017, family = "binomial"),
  glm(D.major ~ `CA_{IDW} _stan`, data = schutz2017, family = "binomial"))
compare_models(
  glm(D.medius ~ `Dist. nearest breeding site MSW (m)`, data = schutz2017, family = "binomial"),
  glm(D.medius ~ `CN_{IDW} _stan`, data = schutz2017, family = "binomial"),
  glm(D.medius ~ `CA_{IDW} _stan`, data = schutz2017, family = "binomial"))

## SHAKELFORD2019
shackelford2019 <- readxl::read_excel("./raw_figextraction/suppls/Shackelford2019raw.xlsx") %>% 
  mutate(logConn = log10(Connectivity)) #calculate log10 of connectivity to compare with the other metrics
ggscatter(shackelford2019, y = "Native Richness 2007", x = "Connectivity") + stat_cor(method = "pearson")
ggscatter(shackelford2019, y = "Species Turnover", x = "Connectivity") + stat_cor(method = "pearson")

correlation(shackelford2019, select = c("Native Richness 2007", "Species Turnover"), select2 = c("Connectivity","logConn"), method = "pearson")

### Manual test statistics ####
# ## testing values from Magle and Crook 2009
# esc::esc_chisq(chisq = 23.03, totaln = 92, es.type = "r") #ti val, hanski metric
# esc::esc_chisq(chisq = 3.50, totaln = 92, es.type = "r") #proportion other colonies
# 
# #fitzgibbon 2007
# esc::esc_chisq(chisq = 20.95, totaln = 68, es.type = "r") 
# 
# ## MacDougall 2011
# effectsize::F_to_r(18.24,1,39)
# effectsize::F_to_r(7.14,1,33)
