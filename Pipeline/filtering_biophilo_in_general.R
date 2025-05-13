# ------------------------------
# Title: filtering_biophilo_in_general.R
# Author: Jacob Hamel-Mottiez
# Date: 2025-02-08
# Description: This file was my first attempt to visualize references from specialized
# philosophy of biology in general philosophy of science journals. See `functions_for_visualizing_data_V2.Rmd` i
# in notebook folder. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(rscopus)
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(plotly)
library(highcharter)

philo_bio_journals <- c("BIOLOGY & PHILOSOPHY", 
                        "BIOLOGY AND PHILOSOPHY", 
                        "BIOLOGICAL THEORY", 
                        "PHILOSOPHICAL TRANSACTIONS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES", 
                        "STUDIES IN HISTORY AND PHILOSOPHY OF SCIENCE PART C :STUDIES IN HISTORY AND PHILOSOPHY OF BIOLOGICAL AND BIOMEDICAL SCIENCES", 
                        "HISTORY AND PHILOSOPHY OF THE LIFE SCIENCES")

# Directory and Data ------------------------------------------------------
dir_od <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"
BJPS_references <- read_csv(paste0(dir_od, "BJPS_query_references_2025-01-16.csv")) |> clean_names()
BJPS_articles <- read_csv(paste0(dir_od, "BJPS_query_papers_2025-01-16.csv")) |> clean_names()

BJPS_references$sourcetitle <- toupper(BJPS_references$sourcetitle)


BJPS_references <- BJPS_references |> mutate(is_philo_bio = ifelse(sourcetitle %in% philo_bio_journals, TRUE, FALSE))


BJPS_references <-  BJPS_references |>
  mutate(
    is_philo_bio_journal = if_else(sourcetitle %in% philo_bio_journals, TRUE, FALSE)
  ) |>
  group_by(citing_art) |>
  mutate(n_ref_bio_philo = sum(is_philo_bio_journal, na.rm = TRUE)) |>
  ungroup()


BJPS_references <- BJPS_references |> select(citing_art, scopus_eid, sourcetitle, is_philo_bio, n_ref_bio_philo)|> 
  filter(!is.na(scopus_eid)) |>
  add_count(citing_art, name = "n_ref")




# Visualization -----------------------------------------------------------
articles_only <- BJPS_references |> select(-scopus_eid, -sourcetitle, -is_philo_bio) |> distinct() 
data_long <- articles_only |> pivot_longer(cols = c(n_ref, n_ref_bio_philo),
               names_to = "category",
               values_to = "count") |> distinct()


ggplot(data_long, aes(x = citing_art, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(x = "Citing Article", y = "Number of References", fill = "Category") +
  theme(axis.text.x = element_blank())



# Match back article titles  ----------------------------------------------
BJPS_articles <- BJPS_articles |> select(dc_identifier, dc_title, dc_creator, prism_publication_name, prism_cover_display_date) |> distinct()



BJPS_articles |> select(dc_identifier, prism_publication_name, prism_cover_date, subtype_description) |> distinct()

annotation <- left_join(articles_only, BJPS_articles, by = join_by(citing_art == dc_identifier)) |> arrange(-n_ref_bio_philo)
write_csv(annotation, paste0(dir_od, 'annotation_', Sys.Date(), "_.csv"))
































