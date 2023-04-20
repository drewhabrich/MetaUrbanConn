## HEADER---------------------------
## Script name: FUNCTIONS COLLECTION
##
## Purpose of script: A collection of functions and their purposes
##
## Author: Andrew Habrich
##
## Date Created: 2023-04-11
##
## Email: 
## - andrhabr@gmail.com
## - andrewhabrich@cmail.carleton.ca
## 
## Notes ---------------------------

### Get TITLE for bibliographic entry from DOI by querying doi.org; REQUIRES 'xml2' and 'httr' packages
get_titles <- function(dois, data) {
  for (i in seq_along(dois)) {
    if (is.na(data$title[i])) {  # check if title is missing
      doi <- dois[i]
      url <- paste0("https://doi.org/", doi)
      response <- GET(url)
      if (status_code(response) == 200) {
        html <- read_html(rawToChar(response$content))
        title <- xml_text(xml_find_first(html, "//title"))
        title <- sub(" \\|.*", "", title)  # remove any text after "|"
        data$title[i] <- title
      } else {
        message(paste0("Error retrieving title for DOI ", doi))
      }
    }
  }
  return(data)
}