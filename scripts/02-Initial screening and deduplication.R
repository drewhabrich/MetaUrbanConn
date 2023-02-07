# 1. Load relevant packages into library -------------------------------------
library(tidyverse)
library(revtools)
library(metagear)


# 2. Import search results and deduplicate --------------------------------
path <- "./raw_data/full search/"
list.files(path=path)
file_names <- paste0(path, list.files(path = path))
data_all<-read_bibliography(file_names)
data_all<-read_bibliography(paste0("./raw_data/full search/", list.files(path="./raw_data/full search/"))) #NOT WORKING CHECK THE INDIVIDUAL FILES
## likely the issues is that they are from different sources, and thus different amounts of columns, even if they are all .ris
dataWOS1<-read_bibliography("./raw_data/full search/WOS_search1.ris")
dataWOS2<-read_bibliography("./raw_data/full search/WOS_search2.ris")
dataWOS3<-read_bibliography("./raw_data/full search/WOS_search3.ris")
dataSCOP<-read_bibliography("./raw_data/full search/scopus_search.ris")
dataPROQ<-read_bibliography("./raw_data/full search/Proquest_grey.ris")
dataCC<-read_bibliography("./raw_data/full search/ConservationCorridor_mixed.ris")
