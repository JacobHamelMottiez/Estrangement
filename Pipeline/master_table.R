# ------------------------------
# Title: master_table.R
# Author: Jacob Hamel-Mottiez
# Date: 2025-04-03
# Description: this file is to create a master table with citing and cited documents.
# The steps are the following 
#   1. Reformating existing data to facilitate manipulation. 
#   2. Merging articles with their refs to get master tables. 
#
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------


# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
library(stringr)

dir_bio <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/"
dir_philo <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/"

# Function to be able to match citing and cited eid. 
reformat_articles  <- function(articles) {
  tibble <- rename(articles, 
                     citing_id = eid, 
                     citing_journal = publicationName, 
                     citing_authors = author_names, 
                     citing_year = coverDate, 
                     citing_title = title, 
                     citing_doi = doi
                     )
  
  tibble$citing_id <- str_extract(tibble$citing_id, "(?<=2-s2\\.0-)[0-9]+")
  tibble <- tibble |> mutate(citing_year = format(tibble$citing_year, format = "%Y"))
  
  tibble <- tibble |> 
    mutate(across(
      .cols = setdiff(names(tibble)[sapply(tibble, is.character)], "description"),
      .fns = str_to_upper
    ))
  
  tibble <- tibble |> distinct()
return(tibble)
}

# Reformat references from biology (data structure not the same as philosophy, we fetched only the strict minimum of info given then number of refs to fetch.)
reformat_references_biology <- function(references){
  
  # This step makes sure that if there is conflicting information for a unique id, the information appearing the most is kept. 
  references <- references |>
    group_by(id) |>
    mutate(
      cited_journal = most_common(cited_journal),
      cited_year  = most_common(cited_year)
    ) |> 
    ungroup()
  
  # Handle possible NA values before applying transformations
  references$cited_journal <- ifelse(is.na(references$cited_journal), NA, toupper(references$cited_journal))
  
  # Extracting citing_id from citing_eid
  references$citing_id <- str_extract(references$source_eid, "(?<=2-s2\\.0-)\\d+")
  
  
  # Rename columns
  references <- rename(references, 
                       cited_id = id 
                       )
  references <- references |> mutate(cited_year = format(references$cited_year, format = "%Y"))
  
  # Drop the `citing_eid` column
  references$...1 <-  NULL
  references <-  references |> mutate(across(where(is.character), str_to_upper))
  
  return(references)
}

most_common <- function(x) {
  x <- na.omit(x)  # Remove NAs first
  if (length(x) == 0) return(NA)  # If all values were NA, return NA
  
  unique_x <- unique(x)
  counts <- tabulate(match(x, unique_x))
  
  unique_x[which.max(counts)]  # Return most frequent value
}

# Reformat references default
reformat_references_philosophy <- function(references){
  
  # This step makes sure that if there is conflicting information for a unique id, the information appearing the most is kept. 
  references <- references |>
    group_by(id) |>
    mutate(
      title = most_common(title),
      sourcetitle = most_common(sourcetitle),
      publicationyear = most_common(publicationyear),
      title = most_common(title), # Fix duplicated column
      authors = most_common(authors) # Fix wrong column reference
    ) |>
    ungroup()
  
  # Handle possible NA values before applying transformations
  references$sourcetitle <- ifelse(is.na(references$sourcetitle), NA, toupper(references$sourcetitle))
  
  # Extracting citing_id from citing_eid
  references$citing_id <- str_extract(references$citing_eid, "(?<=2-s2\\.0-)\\d+")
  
  # Rename columns
  references <- rename(references, 
                       cited_id = id,
                       cited_title = title,
                       cited_journal = sourcetitle,
                       cited_year = publicationyear,
                       cited_authors = authors, # Fix wrong column reference
                       cited_doi = doi
                       )
  
  # Drop the `citing_eid` column
  references$citing_eid <- NULL
  references <-  references |> mutate(across(where(is.character), str_to_upper))
  return(references)
}

# BIOLOGY --------------------------------------------------------------------
pybliometrics_all_bio_arxiv_filtered <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/pybliometrics_all_bio_arxiv_filtered.csv")
refs_all_bio_arxiv <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/refs_all_bio_arxiv.csv")

arts_bioRxiv_reformat <- reformat_articles(pybliometrics_all_bio_arxiv_filtered)
refs_bioRxiv_reformat <- reformat_references_biology(refs_all_bio_arxiv)

write_csv(arts_bioRxiv_reformat, paste0(dir_bio, "articles_bioRxiv_reformat.csv"))
write_csv(refs_bioRxiv_reformat, paste0(dir_bio, "references_bioRxiv_reformat.csv"))

rm(pybliometrics_all_bio_arxiv_filtered, refs_all_bio_arxiv)

# PHILOSOPHY ---------------------------------------------------------
ARTICLES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv")
REFERENCES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/REFERENCES_SPECIAL_PHILO_BIO.csv")

arts_special_philo_bio_reformat <- reformat_articles(ARTICLES_SPECIAL_PHILO_BIO)
refs_special_philo_bio_reformat <- reformat_references_philosophy(REFERENCES_SPECIAL_PHILO_BIO)

write_csv(arts_special_philo_bio_reformat, paste0(dir_philo, "articles_special_philo_bio_reformat.csv"))
write_csv(refs_special_philo_bio_reformat, paste0(dir_philo, "references_special_philo_bio_reformat.csv"))

rm(ARTICLES_SPECIAL_PHILO_BIO, REFERENCES_SPECIAL_PHILO_BIO)


# MERGE ARTICLES AND THEIR REFERENCES -------------------------------------
# Biology
art_and_refs_bioRxiv <- left_join(arts_bioRxiv_reformat, refs_bioRxiv_reformat, by = "citing_id")
art_and_refs_bioRxiv <- art_and_refs_bioRxiv |> select(cited_id, 
                                                        citing_id, 
                                                        cited_year, 
                                                        citing_year, 
                                                        cited_journal, 
                                                        citing_doi, 
                                                        citing_title, 
                                                        citing_authors, 
                                                        citing_journal
)

write_csv(art_and_refs_bioRxiv, paste0(dir_bio, "articles_and_references_bioRxiv.csv"))


# Philosophy
art_and_refs_special_philo_bio <- left_join(arts_special_philo_bio_reformat, refs_special_philo_bio_reformat, by = "citing_id")
art_and_refs_special_philo_bio <- art_and_refs_special_philo_bio |> select(cited_id, 
                                                       citing_id, 
                                                       cited_year, 
                                                       citing_year, 
                                                       cited_journal, 
                                                       citing_doi, 
                                                       citing_title, 
                                                       citing_authors, 
                                                       citing_journal
)

write_csv(art_and_refs_special_philo_bio, paste0(dir_philo, "articles_and_references_special_philo_bio.csv"))
