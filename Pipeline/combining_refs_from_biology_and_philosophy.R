# ------------------------------
# Title: combining_refs_from_biology_and_philosophy.R
# Author: Jacob Hamel-Mottiez
# Date: 2025-02-08
# Description: This file is to showcase how it is possible to look at 
# the attention philosophers of biology give to biology references. 
#
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)

# REFERENCES CORPUS ------------------------------------------------------------------
# Biology
TRENDS_IN_BIOCHEMICAL_SCIENCES_refs_pyblio <- read_csv("Data/pybiblio/TRENDS_IN_BIOCHEMICAL_SCIENCES_refs_pyblio.csv")
MICROBIOLOGY_AND_MOLECULAR_BIOLOGY_REVIEWS_refs_pyblio <- read_csv("Data/pybiblio/MICROBIOLOGY_AND_MOLECULAR_BIOLOGY_REVIEWS_refs_pyblio.csv")
JOURNAL_OF_THEORETICAL_BIOLOGY_refs_pyblio <- read_csv("Data/pybiblio/JOURNAL_OF_THEORETICAL_BIOLOGY_refs_pyblio.csv")

# Philosophy of Biology 
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <- read_csv("Data/Philosophy of Biology/PHILOSOPHY_OF_BIOLOGY_ALL_REFS.csv")


# WHAT IS MOST CITED IN PHILOSOPHY AND BIOLOGY ----------------------------
tbs_counts <- rbind(TRENDS_IN_BIOCHEMICAL_SCIENCES_refs_pyblio, MICROBIOLOGY_AND_MOLECULAR_BIOLOGY_REVIEWS_refs_pyblio, JOURNAL_OF_THEORETICAL_BIOLOGY_refs_pyblio) |> 
  count(id, name = "citations_in_biology")

pob_counts <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> 
  count(id, name = "citations_in_philosophy")

# Full join datasets and get the rank of each references relative to where it is cited. 
full_refs <- tbs_counts |> 
  full_join(pob_counts, by = "id") |> 
  mutate(
    rank_biology = dense_rank(desc(citations_in_biology)),  # Rank in biology
    rank_philosophy = dense_rank(desc(citations_in_philosophy))  # Rank in philosophy
  )

# WHICH REFERENCES ARE FROM BIOLOGY? ---------------------------------------
# Some formating first
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <-  rename(PHILOSOPHY_OF_BIOLOGY_ALL_REFS, citing_eid = source_eid)
PHILOSOPHY_OF_BIOLOGY_ALL_REFS$...1 = NULL

all_references_info <-rbind(TRENDS_IN_BIOCHEMICAL_SCIENCES_refs_pyblio, MICROBIOLOGY_AND_MOLECULAR_BIOLOGY_REVIEWS_refs_pyblio, JOURNAL_OF_THEORETICAL_BIOLOGY_refs_pyblio,  PHILOSOPHY_OF_BIOLOGY_ALL_REFS)
all_references_info |> distinct()


test <- left_join(full_refs, all_references_info |> 
            select(title,sourcetitle,publicationyear, id), by = "id") |>
  arrange(-desc(rank_biology)) |> distinct()


# Cleaning
most_common <- function(x) {
  if (all(is.na(x))) return(NA)  # Handle all NA cases
  x[which.max(tabulate(match(x, unique(x))))]  # Find most frequent value
}

# Cleaning function
clean_references <- function(df) {
  df %>%
    group_by(id) %>%
    mutate(
      title = most_common(title),
      sourcetitle = most_common(sourcetitle),
      publicationyear = most_common(publicationyear)
    ) %>%
    ungroup()
}

# Apply function to your dataset
cleaned_data <- clean_references(test) |> distinct()


# Your list of biology journals
bio_journals <- c(
  "JOURNAL OF THEORETICAL BIOLOGY", "EVOLUTION", "CELL",
  "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES OF THE UNITED STATES OF AMERICA",
  "NATURE REVIEWS GENETICS", "GENETICS", "AMERICAN NATURALIST",
  "CURRENT BIOLOGY", "TRENDS IN COGNITIVE SCIENCES",
  "TRENDS IN ECOLOGY AND EVOLUTION", "JOURNAL OF EVOLUTIONARY BIOLOGY",
  "QUARTERLY REVIEW OF BIOLOGY", "BIOSCIENCE", "COGNITION",
  "PHILOSOPHICAL TRANSACTIONS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES",
  "NATURE GENETICS", "NEURON", "PLOS BIOLOGY", "SYSTEMATIC BIOLOGY",
  "ECOLOGY", "NATURE NEUROSCIENCE", "ANNALS OF THE NEW YORK ACADEMY OF SCIENCES",
  "JOURNAL OF MOLECULAR BIOLOGY", "NATURE REVIEWS MICROBIOLOGY",
  "NATURE REVIEWS NEUROSCIENCE", "BIOLOGICAL REVIEWS",
  "TRENDS IN MICROBIOLOGY", "JOURNAL OF NEUROSCIENCE",
  "MOLECULAR BIOLOGY AND EVOLUTION", "TRENDS IN GENETICS",
  "NATURE REVIEWS MOLECULAR CELL BIOLOGY", "GENOME BIOLOGY",
  "GENOME RESEARCH", "ANNUAL REVIEW OF MICROBIOLOGY",
  "ANNUAL REVIEW OF NEUROSCIENCE", "ECOLOGY LETTERS",
  "ANNUAL REVIEW OF ECOLOGY, EVOLUTION, AND SYSTEMATICS",
  "ANNUAL REVIEW OF GENETICS", "NATURE REVIEWS CANCER",
  "AMERICAN JOURNAL OF HUMAN GENETICS", "TRENDS IN NEUROSCIENCES",
  "MICROBIOLOGY AND MOLECULAR BIOLOGY REVIEWS", "TRENDS IN BIOCHEMICAL SCIENCES"
)

# Add a new column classifying references
cleaned_data$sourcetitle <- toupper(cleaned_data$sourcetitle)
cleaned_data <- cleaned_data %>%
  mutate(
    ref_category = ifelse(sourcetitle %in% bio_journals, "ref_biology", "ref_not_from_biology"))


# HOW MANY REFERENCES TO BIOLOGY ARTICLES ARE PRESENT IN BOTH CORPUS?  ------------------------
results <- cleaned_data |> filter(ref_category == "ref_biology") |> 
  filter(!is.na(rank_biology & rank_philosophy)) 

results |> distinct(sourcetitle)





