# ------------------------------
# Title: Clean bioRxiv columns
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: script to clean bioRxiv references columns once matched from 
# Scopus. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)

# DATA --------------------------------------------------------------------
refs_all_bio_arxiv <- read_csv("Data/refs_all_bio_arxiv.csv")
refs_all_bio_arxiv <- rename(refs_all_bio_arxiv, 
                             cited_id = id, 
                             citing_id = source_eid
                             )

refs_all_bio_arxiv <-  refs_all_bio_arxiv |> mutate(cited_year  = 
                                                      format(refs_all_bio_arxiv$cited_year, format = "%Y"))
refs_all_bio_arxiv <- refs_all_bio_arxiv |> mutate(citing_id = 
                                                     str_extract(citing_id, "(?<=2-s2\\.0-)[0-9]+"))
refs_all_bio_arxiv <- refs_all_bio_arxiv |> select(-...1) |> distinct()


# SAVE --------------------------------------------------------------------


