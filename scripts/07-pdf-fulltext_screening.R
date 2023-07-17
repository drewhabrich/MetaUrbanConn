## HEADER---------------------------
## Script name: 7-pdf-fulltext_screening 
##
## Purpose of script: Using R packages to try and extract info from the relevant 
## pdf documents identified script 4 and downloaded with script 6 + zotero
##
## Author: Andrew Habrich
##
## Date Created: 2023-05-18
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

## 1. Load relevant packages--------
library(tidyverse)
library(pdftools)
library(magick)

## 1.1. overview of the articles that need screening
yes <- read_csv("./raw_data/full_screening/YES.csv")
yes %>% group_by(`Publication Year`) %>% count(sort=T)
maybe <- read_csv("./raw_data/full_screening/MAYBE.csv")
maybe %>% group_by(`Publication Year`) %>% count(sort=T)

grid.arrange(
yes %>% group_by(`Publication Year`) %>% count() %>% 
  ggplot(aes(x = `Publication Year`, y = n)) +
  geom_col() +
  xlab("Publication Year") +
  ylab("Count") +
  ggtitle("Number of likely YES by Publication Year") + theme_bw(),
maybe %>% group_by(`Publication Year`) %>% count() %>% 
  ggplot(aes(x = `Publication Year`, y = n)) +
  geom_col() +
  xlab("Publication Year") +
  ylab("Count") +
  ggtitle("Number of MAYBEs by Publication Year") + theme_bw(),
ncol=2)

## 2. PDF tools package ####
getwd()
## create list of pdfs that need to be converted to images
pdflist <- list.files("./raw_data/figures_extract/", pattern = ".pdf") #detect the pdf files in the directory
## create a list of pdf names by extracting the author/year 
pdftitle <- str_extract(pdflist, "^.*?\\d(?=_)") 

# extract main text
path <- "./raw_data/figures_extract/Choi_etal_2021_Landscape Ecol..pdf"
txt <- pdf_text(path)
# table of contents
toc <- pdf_toc(path)
# show as JSON
jsonlite::toJSON(toc, auto_unbox = TRUE, pretty = TRUE)

# What info can we extract with pdf tools?
dat <- pdf_info(path)
str(dat)
pdf_data(path)

# extract individual pages as jpg or png
output_dir <- "./output/pdf images/"
pdf_convert(path,
  format = "png",
  pages = NULL,
  filenames = paste0(output_dir, pdftitle[1], "_p", 1:pdf_info(path)$pages, ".png"),
  dpi = 300)

fig <- image_read("./output/pdf images/Choi_etal_2021page_9.png")
fig_grey <- image_convert(fig, "gray")
fig_edges <- image_canny(fig_grey)

## 3. RefManageR import of zotero library ####
fs_bib <- ReadZotero(user = "8855145", group="na",
           .params = list(collection = 'L5AVXMD6', 
                          key = '2UDSvoj53uc4bCqUmStjgTAq',
                          itemType = 'journalArticle'),
           temp.file = tempfile(fileext = ".bib"),
           delete.file = TRUE
)

## 4. metaDigitise figures for dataextraction ####
devtools::install_github("EIvimeyCook/ShinyDigitise")
library(shinyDigitise)
shinyDigitise()

library(metaDigitise)
library(metagear)
isPDF("./raw_data/Evans_etal_2017_Front. Ecol. Evol..pdf") #check if the files are PDF
x <- PDF_extractImages("./raw_data/Evans_etal_2017_Front. Ecol. Evol..pdf")
y <- hexView::readRaw("./raw_data/Evans_etal_2017_Front. Ecol. Evol..pdf", human = "char")
        