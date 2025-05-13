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

dir_bioRxiv <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/BIO_ARXIV_DATA/"

# L'objectif est de déterminer ce qu'est la biologie. 
# Ma stratégie empirique est de regarder toutes les revues 
# Dans lesquelles BioRxiv publie. 



# Articles de BioRxiv -----------------------------------------------------
bio_arxiv_art <- read_csv("Data/pybliometrics_all_bio_arxiv_filtered.csv")
journals_count <- bio_arxiv_art |> count(publicationName) |> arrange(desc(n))


refs_all_bio_arxiv <- read_csv("Data/refs_all_bio_arxiv.csv")

refs_bio_bioRxiv <- refs_all_bio_arxiv |> filter(cited_journal %in% journals_count$publicationName)
refs_bio_bioRxiv$cited_journal <- toupper(refs_bio_bioRxiv$cited_journal)
write_csv(refs_bio_bioRxiv, paste0(dir_bioRxiv, "refs_bio_bioRxiv.csv"))
# ~7M to 5,5M. 


# Articles philosophie de la biologie -------------------------------------
REFERENCES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/REFERENCES_SPECIAL_PHILO_BIO.csv")
refs_bio_philobio <- REFERENCES_SPECIAL_PHILO_BIO|> filter(sourcetitle %in% refs_bio_bioRxiv$cited_journal)
write_csv(refs_bio_philobio, paste0(dir_bioRxiv, "refs_bio_philobio.csv"))
# 31, 889 references on 140, 000. Around 1/4 is to biology. 


ARTICLES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv")
ARTICLES_SPECIAL_PHILO_BIO <-  ARTICLES_SPECIAL_PHILO_BIO |> select(citing_id, citing_year)


# WHICH JOURNALS ARE THE MOST CITED? 
journals_by_year <- refs_bio_philobio |> select(sourcetitle, cited_year, citing_id) |> 
  left_join(ARTICLES_SPECIAL_PHILO_BIO, by = "citing_id")

most_cited_journals <- journals_by_year |> 
  group_by(sourcetitle) |> 
  count(sourcetitle) |> 
  arrange(desc(n)) |> 
  ungroup()

most_cited_journals_each_year <- journals_by_year |> 
  group_by(sourcetitle, citing_year) |> 
  count(sourcetitle) |> 
  arrange(desc(n))


# VISUALIZE MOST CITED JOURNALS 
top_5 <- most_cited_journals |> slice_head(n = 5)
top_5_journals <- most_cited_journals_each_year |> filter(sourcetitle %in% top_5$sourcetitle)

p1 <- ggplot(top_5_journals, aes(x = citing_year, y = n, colour = sourcetitle, group = sourcetitle)) + 
  geom_line(linewidth = 2)
p1


























