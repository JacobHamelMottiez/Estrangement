# ------------------------------
# Title: Manual annotations bioRxiv references
# Author: Jacob Hamel-Mottiez
# Date: 2025-06-18
# Description: The goal of this file is to check if biology cites mostly biological references. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)



# DATA --------------------------------------------------------------------
#all_bio_arxiv_filtered <- read_csv("Data/bioRxiv_raw_data/all_bio_arxiv_filtered.csv")
refs_all_bio_arxiv <- read_csv("Data/pybiblio/BIOLOGY/bioRxiv/refs_all_bio_arxiv.csv")


# SAMPLING ----------------------------------------------------------------
#set.seed(123)  # for reproducibility
#sample_size <- 300  # adjust as needed


count_journal_refs <- refs_all_bio_arxiv |> count(cited_journal) |> arrange(desc(n)) |> print(n = 25)



general_science_journal <- refs_all_bio_arxiv |> 
  select(cited_journal) |> 
  filter(
    cited_journal != "Proceedings of the National Academy of Sciences of the United States of America" |
    cited_journal != "Nature" |
    cited_journal != "Science" |
    cited_journal != "PLoS ONE" |
    cited_journal != "Nature Communications"
)
