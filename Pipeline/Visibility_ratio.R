# ------------------------------
# Title: {filename}
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

library(tidyverse)
library(plotly)
library(readxl)
library(patchwork)


# DATA --------------------------------------------------------------------
articles_and_references_special_philo_bio <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/articles_and_references_special_philo_bio.csv")
articles_and_references_bioRxiv <- read_csv("Data/pybiblio/BIOLOGY/bioRxiv/articles_and_references_bioRxiv.csv")
articles_and_references_bioRxiv <- articles_and_references_bioRxiv |> filter(citing_year != 2001 & citing_year !=2002)
articles_and_references_bioRxiv |> distinct(citing_year)

articles_bioRxiv_reformat <- read_csv("Data/pybiblio/BIOLOGY/bioRxiv/articles_bioRxiv_reformat.csv")
refs_bio_bioRxiv <- read_csv("Data/pybiblio/BIOLOGY/bioRxiv/references_bioRxiv_reformat.csv")

articles_special_philo_bio_reformat <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/articles_special_philo_bio_reformat.csv")
references_special_philo_bio_reformat <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/references_special_philo_bio_reformat.csv")


# TOTAL ARTICLES PUBLISHED IN PHILOSOPHY OF BIOLOGY -----------------------
# Step 1: Tag what is from philo and what is from bio
journals_bio <- articles_bioRxiv_reformat |> distinct(citing_journal) # 4,361.
journals_philo <- articles_special_philo_bio_reformat |> distinct(citing_journal) # 4.


philo_refs_tagged <- articles_and_references_special_philo_bio |> mutate(ref_bio = ifelse(cited_journal %in% journals_bio$citing_journal, TRUE, FALSE))
bio_refs_tagged <- articles_and_references_bioRxiv  |> mutate(ref_philo = ifelse(cited_journal %in% journals_philo$citing_journal, TRUE, FALSE))


# Step 2: Calculate ratios by year
philo_to_bio <- philo_refs_tagged |> 
  group_by(citing_year) %>%
  summarise(
    total = n(),
    to_bio = sum(ref_bio == TRUE, na.rm = TRUE),
    ratio = to_bio / total*100
  ) %>%
  mutate(direction = "Philosophy → Biology")

bio_to_philo <- bio_refs_tagged %>%
  group_by(citing_year) %>%
  summarise(
    total = n(),
    to_philo = sum(ref_philo, na.rm = TRUE),
    ratio = to_philo / total*100
  ) %>%
  mutate(direction = "Biology → Philosophy")

# Step 4: Plot
ggplot(philo_to_bio |> filter(citing_year> 1986 & citing_year < 2025), aes(x = citing_year, y = ratio)) +
  geom_line(linewidth = 2, color = "darkgrey") +
  geom_smooth(linewidth = 3) +
  labs(
    title = "Citation Ratios Towards Biology in Philosophy of Biology Over Time",
    x = "Citing Year",
    y = "Ratio of References from Biology Over All References in Philosophy of Biology"
  ) +
  theme_minimal()


p <- ggplot(bio_to_philo |> filter(citing_year > 2015 & citing_year < 2025), aes(x = citing_year, y = ratio)) +
  geom_line(size = 1, color = "blue") +
  geom_point() +
  labs(
    title = "Citation Ratios Towards Philosophy of Biology in Biology Over Time",
    x = "Citing Year",
    y = "Ratio of References from Philo. of Biology Over All References"
  )


p2 <- ggplot(bio_to_philo |> filter(citing_year > 2015 & citing_year < 2025), aes(x = citing_year, y = to_philo)) +
  geom_line(size = 1, color = "red") +
  geom_point() +
  labs(
    y = "Count",
    x = " "
  )




(p2) / p + plot_layout(widths = c(2, 1), heights = unit(c(5, 1), c('cm', 'null')))



