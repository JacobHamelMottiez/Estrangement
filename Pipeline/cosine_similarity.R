# ------------------------------
# Title: cosine similarity
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
library(lsa)

# Biology
refs_all_bio_arxiv <- read_csv("Data/refs_all_bio_arxiv.csv")
refs_all_bio_arxiv <-  refs_all_bio_arxiv |> select(id, source_eid)

pybliometrics_all_bio_arxiv_filtered <- read_csv("Data/pybliometrics_all_bio_arxiv_filtered.csv")
pybliometrics_all_bio_arxiv_filtered <- pybliometrics_all_bio_arxiv_filtered |> select(eid, coverDate) |> rename(source_eid = eid, citing_year = coverDate)
pybliometrics_all_bio_arxiv_filtered <- pybliometrics_all_bio_arxiv_filtered |> mutate(citing_year = year(citing_year))


full_table_biology <- refs_all_bio_arxiv |> left_join(pybliometrics_all_bio_arxiv_filtered, join_by(source_eid == source_eid))
full_table_biology <- full_table_biology |> group_by(id, citing_year)
full_table_biology <- full_table_biology |> group_by(citing_year) |> add_count(id) |> arrange(desc(n)) |> ungroup() 

full_table_biology <- rename(full_table_biology, cited_id = id)
full_table_biology <- full_table_biology |> select(cited_id, citing_year, n) |> distinct()


# Philosophy of Biology
ARTICLES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv")
REFERENCES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/REFERENCES_SPECIAL_PHILO_BIO.csv") 


ARTICLES_SPECIAL_PHILO_BIO <- ARTICLES_SPECIAL_PHILO_BIO |> select(citing_year, citing_id)
REFERENCES_SPECIAL_PHILO_BIO <- REFERENCES_SPECIAL_PHILO_BIO |> select(cited_id, cited_year, citing_id)

full_table_philo <- REFERENCES_SPECIAL_PHILO_BIO |> left_join(ARTICLES_SPECIAL_PHILO_BIO, join_by(citing_id == citing_id))
full_table_philo <- full_table_philo |> group_by(cited_id, citing_year)
full_table_philo <- full_table_philo |> group_by(citing_year) |> add_count(cited_id) |> arrange(desc(n)) |> ungroup() 
full_table_philo <- full_table_philo |> select(cited_id, citing_year, n) |> distinct()

citation_data <- left_join(full_table_philo, full_table_biology, by = "cited_id", suffix = c("_j1", "_j2"))

citation_data <- citation_data |> select(-source_eid)
citation_data |> distinct()


citation_data <-  citation_data |> select(cited_id, citing_year_j1, citing_year_j2)
citation_data <- citation_data |> group_by(citing_year_j1) |> add_count(cited_id, name = "n_j1")
citation_data <- citation_data |> group_by(citing_year_j2) |> add_count(cited_id, name = "n_j2")
citation_data <- citation_data |> distinct()

citation_data |> filter(n_j2 == 0)

# Function to assign decades
citation_data <- citation_data %>%
  mutate(decade = (citing_year_j1 %/% 10) * 10)

citation_data <- citation_data %>%
  mutate(n_j1 = ifelse(is.na(n_j1), 0, n_j1),
         n_j2 = ifelse(is.na(n_j2), 0, n_j2))

# Function to calculate cosine similarity within each decade
cosine_similarity_by_decade <- citation_data %>%
  group_by(decade, citing_year_j2) %>%
  summarise(
    similarity = cosine(n_j1, n_j2))

cosine_similarity_by_decade <-  cosine_similarity_by_decade |> filter(!is.na(decade))


ggplot(cosine_similarity_by_decade |> filter(citing_year_j2 >= 2015 & citing_year_j2 <= 2024), aes(x = citing_year_j2, y = similarity, group = decade, color = as.factor(decade))) + 
  geom_line(linewidth = 2)
  


cosine_similarity_by_decade <- cosine_similarity_by_decade |> filter(citing_year_j2 != 2002)
cosine_similarity_by_decade |> print(n = 66)
