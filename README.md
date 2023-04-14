# MetaUrbanConn: A meta-analysis of the impacts of landscape connectivity in urban ecosystems on biodiversity.
This repository is a living document to track changes, version-control softwares, packages, and code use throughout the project. It will document all steps used to search, screen, download, and extract the relevant information on the various metrics of landscape connectivity used in the literature to analyse the impacts on biodiversity in urban areas across the globe. 
Content contained here will contain: 
- raw data files 
- R scripts (and relevant packages)
- output generated through R (modified data files, figures, tables etc.)
- template documents for pdf data extractions along with explanations and rationale.

## ReadMe/Meta-data best practices:
- Include package version information and any external software used
- Describe files in a logical order
- Describe any column/variable names (especially units)
- Include which scripts specific outputs come from

## Project structure: Every folder contains a README to explain what it contains
- Raw Data: Data imported into your project from some data source. Mostly excel or txt documents. Metadata should include date of download or collection, original source and re-use info.
- Output - derived data: Datasets created by altering some raw dataset e.g., merging databases, cleaning up data, subsets, etc. 
- Scripts: Code categorized by what they do and what they generate. 
- Output: Figures, tables, results; anything generated by the script and analysis that isn't another dataset

## File naming: 
- Leading numbers on scripts indicate order they should be run e.g.
  - 01-data_processing.R
  - 02-model_fitting.R
- Output will end with the script number that generated the file (e.g. files generated in script 02 will end with 02.ext)
- Files with additional numbers after the lead identifying number (i.e. 01-1) represent an additional round of updated searching, tidying, screening. 
