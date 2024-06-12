## HEADER---------------------------
## Script name: 11-metadigitize_dataextraction
##
## Purpose of script: To extract raw data and estimates from figures using the metadigitise package (and shinyapp)
## https://www.rdocumentation.org/packages/metaDigitise/versions/1.0.1
## NOTE: ShinyDigitise requires the dev version of the package for certain kinds of figures
##
#remotes::install_github(repo = "EIvimeyCook/ShinyDigitise@6f1099e", force = TRUE)
#remotes::install_github("EIvimeyCook/ShinyDigitise")
#remotes::install_github(repo = "joelpick/metaDigitise", force = TRUE)
##
## Author: Andrew Habrich
##
## Date Created: 2024-01-26
## Date last Modified: 2024-03-04
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

# 1. Load relevant packages--------
library(tidyverse)
library(shiny)
library(metaDigitise)
library(shinyDigitise)
 
# 2. Setup directories and folders with figures----
# Try shinydigitise first
#### CHECK WHAT VERSION IS WORKING, AS OF NOW THE ONE FROM AUGUST 2023 WORKS ####
figdir <- paste0(getwd(),"/raw_figextraction/")
df <- shinyDigitise(dir = figdir)

## or with metadigitise
data <- metaDigitise(dir = "./raw_figextraction", summary = T)

## Use juicr to extract specific values from figures types not available for metadigitize
install.packages("juicr")
install.packages("BiocManager");
BiocManager::install("EBImage")

library(juicr)
GUI_juicr()
