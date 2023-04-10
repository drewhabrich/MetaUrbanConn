## HEADER---------------------------
## Script name: 
##
## Purpose of script:
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-04
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
library(medrxivr)
library(tidyverse)

## 2. download biorxiv data
#biorx_data <- mx_api_content(server = "biorxiv")
# Import the database
biorx_data <- mx_snapshot()
# Perform an advanced search, by generating a list of search strings by topic
searchstring <- list(
  urban = c("urban", "urban area*", "city", "cities", "town*", "urban environment*","urban forest*","urban matrix","urban greenspace*","urban green","urban landscape*","green infrastructure*"),
  biodiversity = c("biodiversity", "wildli*", "plant*", "tree*", "bird*", "avian", "arthropod*", "insect*", "mammal*", "herptile*", "reptile*", "amphibian*"),
  outcome = c("occupan*",	"occurr*",	"abundan*",	"species richness",	"species response*",	"species densit*",	"species composition*",	"species assemblage*",	"species diversity",	"community composition*",	"community assemblage*",	"population response*",	"population persist*",	"population densit*",	"population differentiation",	"animal movement*",	"dispersal distance*",	"dispersal rate*",	"genetic structure",	"gene flow",	"genetic diversity",	"genetic variation",	"distribution pattern*",	"spatial distribution*",	"diversity"),
  connectivity = c("connectivity",	"patch connectivity",	"landscape connectivity",	"functional connectivity",	"structural connectivity",	"habitat connectivity",	"ecological connectivity",	"habitat network*",	"ecological network*",	"habitat corridor*",	"ecological corridor*",	"least cost",	"least-cost",	"graph theor*",	"graph-theor*",	"circuit theory",	"circuit-theory",	"landscape resistance*",	"landscape permeabilit*",	"resistance surface*",	"isolation",	"patch isolation",	"habitat isolation"),
)

# Create a 'short' string with fewer terms to query the API snapshop
shortstr <- list(
  urban = c(
    "urban",
    "city",
    "cities"),
  biodiversity = c(
    "biodiversity",
    "species"),
  # outcome = c(
  #   "occupan*",
  #   "occurr*",
  #   "abundan*",
  #   "species richness",
  #   "species diversity",
  #   "population densit*",
  #   "genetic structure",
  #   "gene flow",
  #   "genetic diversity",
  #   "genetic variation",
  # ),
  connectivity = c(
    "connectivity",
    "network",
    "corridor")
)

# query the data with the search string
results <- mx_search(data = biorx_data,
                     query = shortstr,
                     report = F,
                     auto_caps = T,
                     deduplicate = F,
                     fields = c("title", "abstract")) #find the search terms that contribute the most to the search

#export to bib
# mx_export(data = results,
          file = "mx_search_results.bib")