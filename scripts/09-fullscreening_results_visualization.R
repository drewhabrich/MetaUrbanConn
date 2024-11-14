## HEADER---------------------------
## Script name: 09-fullscreening results and visualization of results
##
## Purpose of script: Take a look at the patterns of the initial results after full-screening all the (YES) entries
## identified in previous screening steps
##
## Author: Andrew Habrich
##
## Date Created: 2023-07-12
## Date last modified: 2024-11-14
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------
rm(list = ls())

## 1. Load relevant packages--------
pacman::p_load(tidyverse, rphylopic, patchwork, maps, sf)

## 2. Load .csv file of the screening results ####
fs_yes <- read_csv("./raw_data/09-full_screening/fullscreen_yes_results.csv")
fs_yes <- fs_yes %>% mutate(ex_r = ifelse(excl_reason=="na", "Keep", "Reject")) #this is a useful category, it can be removed from each of the figure calls and just called directly

### 2.1 Criteria DECISIONS ####
### 2.1.1 Proportion of keep/reject
fs_yes %>% mutate(ex_r = ifelse(excl_reason=="na", "Keep", "Reject")) %>% group_by(ex_r) %>% tally() %>% 
  ggplot(aes(x="", y=n, fill=ex_r)) + 
  geom_bar(width=1, stat= "identity") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "Count", title = "Fullscreening decision", fill = "") + 
  theme_minimal()

### 2.1.2 By year
### create table with count by year of each ex_r group on a stacked bar plot
fs_yes %>% mutate(ex_r = ifelse(excl_reason=="na", "Keep", "Reject")) %>% group_by(pub_year, ex_r) %>% tally() %>% 
  ggplot(aes(x=pub_year, y=n, fill= ex_r)) + 
  geom_bar(width=1, stat= "identity", colour="black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("lightgreen","coral"))+ #manually change colours of the bars
  labs(x = "Publication year", y = "Count", title = "Fullscreening decision", fill = "") + 
  theme_minimal() + theme(legend.position = c(0.2,0.8))

### 2.1.3 For the rejections, what criteria did they violate?
#NOTE: See the inclusion criteria list for the description of each of the numbered criteria
exl_crit <- fs_yes %>% filter(excl_reason != "na") %>% group_by(excl_reason) %>% count() %>% ungroup() %>% 
  arrange(as.numeric(excl_reason)) %>% mutate(ID = as.integer(excl_reason)) #create an ID column to plot

exl_crit %>%  
  ggplot(aes(x=ID, y=n)) +
  geom_bar(width=1, stat= "identity", fill = "lightgray", colour= "black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.8)) +
  labs(x = "Criteria", y = "Count", title = "Rejected entries by criteria") + 
  scale_x_continuous(breaks=seq(1,12,1)) + 
  scale_y_continuous(breaks=seq(0,70,10), expand = c(0, 0), limits = c(0,70)) +
  theme_minimal() + theme(axis.line = element_line(linetype = "solid"),
                          panel.background = element_rect(fill = NA))

### 2.2 Authors -----------------------------------------------------------
## create a count of the # of authors on each paper
fs_yes$num_authors <- sapply(strsplit(fs_yes$author, ";\\s*|\\s+and\\s+"), length)

## 2.2.1 plot # of authors by year. 
# Is there a trend of increasing number of authors? 
# How many are KEEP/REJECT
fs_yes %>% mutate(ex_r = ifelse(excl_reason=="na", "Keep", "Reject")) %>% group_by(pub_year) %>% 
  ggplot(aes(x=as.factor(pub_year), y=num_authors, fill = ex_r)) + 
  geom_boxplot(position="dodge", alpha=0.5) +
  labs(x = "Publication year", y = "Avg. Num. Authors", fill="") + 
  scale_fill_manual(values = c("lightgreen","coral")) +
  theme_minimal() + theme(legend.position = c(0.2,0.8),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))

### 2.3 Journals ----------------------------------------------------------------
fs_yes %>% mutate(ex_r = ifelse(excl_reason=="na", "Keep", "Reject")) %>% group_by(journal, ex_r) %>% tally() %>% 
  #summarize(mean_n_auth = mean(num_authors)) %>% ungroup() %>% 
  ggplot(aes(x = n, y = reorder(journal, n), fill = ex_r)) + #reorder journals by descending count
  geom_bar(stat= "identity", colour="black") +
  labs(x = "Count", y = "Journal", fill="") + 
  scale_y_discrete(labels = function(x) str_trunc(x, 35))+
  scale_fill_manual(values = c("lightgreen","coral")) + #manually change colours of the bars
  theme_minimal() +
  theme(axis.text.y = element_text(size = 5),
        legend.position = c(0.8,0.8)) #specific modifications to the theme

### 2.4 Taxonomic group ####
## These are not all defined to the same level, so some broader grouping may be required
### CREATE higher level group to bin them into
tax <- fs_yes %>%
  mutate(group = case_when( #when taxa is %in% the list it will create a new column with the higher level grouping
    taxa %in% c("amphibia", "anuran") ~ "amphibian",
    taxa %in% c("reptilia") ~ "reptilian",
    taxa %in% c("aves") ~ "aves",
    taxa %in% c("aranaea", "arthropod","coleoptera", "collembola", "formicidae", "hymenoptera", "lepidoptera") ~ "arthropoda",
    taxa %in% c("chiroptera", "mammalia", "marsupialia", "primate", "rodentia", "scuiridae") ~ "mammalia",
    taxa %in% c("bacteria", "fungi", "gastropoda", "invertebrates") ~ "other",
    taxa %in% c("plants", "pteridophyta") ~ "plants",
    taxa %in% c("mammalia, reptilia", "multi-taxa", "amphibia, reptilia") ~ "multi-taxa",
    TRUE ~ "na")) 

## 2.4.1 how many total papers on each of the taxa groups?
tax %>% group_by(group) %>% tally() %>% 
  ggplot(aes(x=group, y=n, fill=group)) +
  geom_bar(width=1, stat= "identity", colour= "black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "Count", title = "Number of papers by taxonomic focus", fill = "") + 
  theme_minimal() + theme(legend.position = "none",
                          axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.75))

## 2.4.2 how many total per taxa, for the ones to keep?
tax %>% filter(excl_reason == "na") %>% group_by(group) %>% tally() %>% 
  ggplot(aes(x=group, y=n, fill=group)) + 
  geom_bar(width=1, stat= "identity", colour= "black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "Count", title = "Taxonomic focus of INCLUDED papers", fill = "") + 
  theme_minimal()+ theme(legend.position = "none",
                         axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.75))

## 2.4.3 stacked figure
tax %>% mutate(ex_r = ifelse(excl_reason=="na", "Keep", "Reject")) %>% group_by(group, ex_r) %>% tally() %>% 
  ggplot(aes(x=group, y=n, fill= ex_r)) + 
  geom_bar(width=1, stat= "identity", colour="black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("lightgreen","coral"))+ #manually change colours of the bars
  labs(x = "Taxon grouping", y = "Count", title = "Fullscreening decision by taxa", fill = "") + 
  theme_minimal() + theme(legend.position = c(0.8,0.8), #(x,y coords)
                          axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.75)) +
    add_phylopic(name = "Leiopelma archeyi", x = 1, y = 30, ysize = 5)+
    add_phylopic(name = "Danaus plexippus", x = 2, y = 75, ysize = 6)+
    add_phylopic(name = "songbirds", x = 3, y = 95, ysize = 5)+ 
    add_phylopic(name = "Tamiasciurus hudsonicus", x = 4, y = 65, ysize = 5)+ 
    add_phylopic(name = "Tropidodiscus", x = 7, y = 10, ysize = 4)+
    add_phylopic(name = "Acer rubrum", x = 8, y = 55, ysize = 6)+
    add_phylopic(name = "Ardeosaurus brevipes", x = 9, y = 15, ysize = 6)

### 2.5 Direction of effect ------
### 2.5.1 ALL TAXA
tax %>% filter(excl_reason == "na") %>% group_by(result_sign) %>% tally() %>% 
  ggplot(aes(x = n, y = reorder(result_sign, n), fill=result_sign)) + #reorder journals by descending count
  geom_bar(stat= "identity", colour="black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(x = "Count", y = "Connectivity metric type used") + 
  scale_fill_manual(values = c(`0` = "#FFFFFF", mixed = "#93AA00", na = "#00C19F", neg = "#F45F5F", pos = "#83FF1B"))+
  scale_y_discrete(labels = function(x) str_trunc(x, 35))+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11),
        legend.position = "none") +
  coord_cartesian(xlim = c(0, 70)) +
  add_phylopic(uuid="acf1cbec-f6ef-4a82-8ab5-be2f963b93f5", x = 60, y = 1, ysize = 0.5) 

## 2.5.2 TAXA FACETS
tax %>% filter(ex_r == "Keep") %>% group_by(group, result_sign) %>% tally() %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) +
  geom_bar(stat = "identity",colour = "black") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c(`0` = "#FFFFFF", mixed = "#93AA00", na = "#00C19F", neg = "#F45F5F", pos = "#83FF1B")) +
  theme_minimal() + theme(legend.position = "none") +
  facet_wrap(~group) 

## 2.5.3 Make a figure for each group
ttal <- tax %>% filter(ex_r == "Keep") %>% group_by(group, result_sign) %>% tally() %>% ungroup() %>% 
  add_row(group = "other", result_sign = "0", n = 0)  %>% 
  add_row(group = "other", result_sign = "neg", n = 0)  %>% 
  add_row(group = "reptilian", result_sign = "0", n = 0) %>%  
  add_row(group = "multi-taxa", result_sign = "0", n = 0)

av <- ttal %>% filter(group == "aves") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "songbirds", x = 1, y = 20, ysize = 4)
ma <- ttal %>% filter(group == "mammalia") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Tamiasciurus hudsonicus", x = 1, y = 20, ysize = 4)

ar <- ttal %>% filter(group == "arthropoda") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Danaus plexippus", x = 1, y = 25, ysize = 5)
pl <- ttal %>% filter(group == "plants") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Acer rubrum", x = 1, y = 25, ysize = 4)

re <- ttal %>% filter(group == "reptilian") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Ardeosaurus brevipes", x = 1, y = 10, ysize = 3)
am <- ttal %>% filter(group == "amphibian") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Leiopelma archeyi", x = 1, y = 10, ysize = 2)

mt <- ttal %>% filter(group == "multi-taxa") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Homo sapiens", x = 1, y = 10, ysize = 4)
ot <- ttal %>% filter(group == "other") %>% 
  ggplot(aes(x = result_sign, y=n, fill = result_sign)) + geom_bar(stat = "identity",colour = "black") + 
  add_phylopic(name = "Tropidodiscus", x = 1, y = 10, ysize = 2)

## arrange them all together in one plot (Use & or * to apply theme settings to ALL the plots)
(av | ma ) /
(ar | pl ) /
(re | am ) /
(mt | ot)   & 
geom_text(aes(label = n), position = position_stack(vjust = 0.5)) &
scale_fill_manual(values = c(`0` = "#FFFFFF", mixed = "#93AA00", na = "#00C19F", neg = "#F45F5F", pos = "#83FF1B")) &
theme_minimal() & theme(legend.position = "none") & labs(x="")

### 2.6 Validation data ####
##  PROBLEMS WITH SOME ENTRIES; NEEDS COSOLIDATING IN CSV FILES.
tax %>% filter(excl_reason == "na") %>% group_by(validation) %>% tally() %>% 
  ggplot(aes(x = n, y = reorder(validation, n))) + #reorder journals by descending count
  geom_bar(stat= "identity", colour="black") +
  labs(x = "Count", y = "validation type") + 
  scale_y_discrete(labels = function(x) str_trunc(x, 35))+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11))

## connectivity type
tax %>% filter(excl_reason == "na") %>% group_by(conn_type) %>% tally() %>% 
  ggplot(aes(x = n, y = reorder(conn_type, n))) + #reorder journals by descending count
  geom_bar(stat= "identity", colour="black") +
  labs(x = "Count", y="", title = "Connectivity metric type used") + 
  scale_y_discrete(labels = function(x) str_trunc(x, 35))+
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11))

### 2.7 Country & City ####
## 2.7.0 Prepare Map visualization data
globe <- map_data("world", boundary=T) ##the country names have to match the dataframe for it to plot!
cities<- world.cities #get data on cities from 'maps' package
## create a basemap
basemap <- ggplot() +
  geom_polygon(data = globe, aes(x=long, y=lat, group=group), colour="grey50", fill="grey", alpha=0.3) + 
  theme_minimal() + coord_sf()

#### 2.7.1 COUNTRY ####
ctal <- tax %>% separate_rows(country, sep = ", ") %>% group_by(country, ex_r) %>% count(country) %>% 
  ungroup(ex_r) %>% mutate(total= sum(n)) #create a total column to arrange by

## 2.7.1-1 Frequency of keep/reject by country
ctal %>%  ggplot(aes(y = reorder(country, total), x = n, fill = ex_r)) +
    geom_bar(stat = "identity") +
  scale_fill_manual(values = c(Keep = "lightgreen", Reject = "coral")) +
  labs(x = "Count", y = "Country", fill = "") +
  theme_minimal() + theme(legend.position = c(0.8,0.8))

## 2.7.1-2 just the 'keep's
ctal %>% filter(ex_r == "Keep") %>% ggplot(aes(y = reorder(country, n), x = n, fill = ex_r)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c(Keep = "lightgreen")) +
  labs(x = "Count", y = "Country", fill = "") +
  theme_minimal() + theme(legend.position = "none")

## 2.7.1-3 add plot of proportion retained and count by country
## merge dataframes with frequencies and coordinates of capital cities
cfreq <- merge(ctal, cities %>% filter(capital == 1), by.x = "country", by.y = "country.etc", all.x = T)
cfreq <- cfreq %>% filter(ex_r=="Keep") %>% mutate(prop = n/total)

basemap +  
  geom_point(data=cfreq, aes(x=long,y=lat, size = total, colour=prop), alpha=0.7) +
  theme(legend.position= "bottom") + labs(x="",y="", size="Count", colour="Prop. retained") +
  scale_color_viridis_c(option = "inferno", direction = 1)

#### 2.7.2 CITIES ####
# seperate out the multi-city entries, and count occurrences by city/country
cityfreq <- tax %>% separate_rows(urban_area, sep = "; ") %>% 
                group_by(urban_area, country, ex_r) %>% count(urban_area) %>% 
                ungroup(ex_r) %>% mutate(total= sum(n)) #create a total column to arrange by
# merge with world cities data
cfq <- merge(cityfreq, cities, by.x = "urban_area", by.y = "name", all.x = T)
cfq <- cfq %>% filter(ex_r=="Keep") %>% mutate(prop = n/total)

#Plot together ###NOTE: Some cities do not have coordinates
basemap +  
  geom_point(data=cfq, aes(x=long,y=lat, size = total, colour=prop), alpha=0.7) +
  theme(legend.position= "bottom") + labs(x="",y="", size="Count", colour="Prop. retained")+
  scale_color_viridis_c(option = "inferno", direction = 1)
