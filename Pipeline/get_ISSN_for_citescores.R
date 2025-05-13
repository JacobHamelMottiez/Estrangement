# ------------------------------
# Title: get_ISSN_for_citescores.R
# Author: Jacob Hamel-Mottiez
# Date: 2025-02-08
# Description: 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)


all_journals_scopus <- read_excel("C:/Users/jacob/OneDrive - Université Laval/DATA/ALL_JOURNALS_SCOPUS_October_2024.xlsx")
all_journals_scopus_filtered <- all_journals_scopus |> select(ISSN, `Source Title`)

# The .csv final_table_OR is from WHAT_IS_THE_AMOUNT_OF_REFS_TO_PHILO_BIO.Rmd. We passed this file in OpenRefine with metaphone algorithm. 
final_table_OR <-  read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/openrefine/most-cited-journal-in-philo-bio_openrefine.csv")
cited_journals <- final_table_OR |> select(cited_id, sourcetitle, cited_title) |> filter(!is.na(cited_title)) |> count(sourcetitle)  |> arrange(desc(n))

all_journals_scopus_filtered$`Source Title` <- toupper(all_journals_scopus_filtered$`Source Title`)
all_journals_scopus_filtered <- rename(all_journals_scopus_filtered, sourcetitle = `Source Title`)
ISSN_test <-  left_join(cited_journals, all_journals_scopus_filtered, by = "sourcetitle")

ISSN_test |> filter(is.na(ISSN))
ISSN_test_NA <- ISSN_test |> filter(n > 100) |> filter(is.na(ISSN))

write_csv(ISSN_test_NA,"C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/ISSN_test_NA.csv")
write_csv(ISSN_test,"C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/ISSN_test.csv")
