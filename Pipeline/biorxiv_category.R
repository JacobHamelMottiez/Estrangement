# ------------------------------
# Title: {filename}
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)


# DATA --------------------------------------------------------------------
all_bio_arxiv_filtered <- read_csv("Data/bioRxiv_raw_data/all_bio_arxiv_filtered.csv")




# WHAT IS THE PERCENTAGE OF PAPERS THAT REFERS TO EVOL. THEORY in BIORXIV?------
all_bio_arxiv_filtered$preprint_category <- toupper(all_bio_arxiv_filtered$preprint_category)

count_category_biorxiv <-  all_bio_arxiv_filtered |> distinct() |>
  count(preprint_category) |> 
  arrange(desc(n)) |> print(n = 100)


count_category_biorxiv |> distinct() |> print(n = 55)# Seems to be unique categories. 


(7498/137237)*100
