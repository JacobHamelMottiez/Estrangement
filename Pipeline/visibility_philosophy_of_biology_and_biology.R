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
# Plot 
art_by_year_philo <- articles_special_philo_bio_reformat |> select(citing_id, citing_year) |> count(citing_year)
plot_art_by_year_philo <- ggplot(art_by_year_philo |> filter(citing_year <= 2024), aes(x = citing_year, y = n)) + 
  geom_smooth() + 
  geom_line() + 
  labs(title = "Articles in Philosophy of Biology",
       x = "Year",
       y = "N. Articles") 


# TOTAL ARTICLES PUBLISHED IN BIORXIV -----------------------
# Plot 
art_by_year_bio <- articles_bioRxiv_reformat |> select(citing_id, citing_year) |> count(citing_year)
plot_art_by_year_bio <- ggplot(art_by_year_bio |> filter(citing_year >= 2015 & citing_year <= 2024), aes(x = citing_year, y = n)) + 
  geom_smooth() + 
  geom_line() + 
  labs(title = "Articles in BioRxiv",
       x = "Year",
       y = "N. Articles") 
plot_art_by_year_philo + plot_art_by_year_bio

# VISIBILITY OF PHILOSOPHY IN BIOLOGY -------------------------------------
## Here, we need to find which citing_id from philosophy is CITED by our biology corpus. 
# Who are cited by biologist? How much by journals?  

articles_special_philo_bio_reformat |> 
  filter(citing_id %in% refs_bio_bioRxiv$cited_id) |> 
  count(citing_journal)|> 
  arrange(desc(n))

journals_philo_bio <- articles_special_philo_bio_reformat |> distinct(citing_journal)
philo_citedin_bio <- articles_and_references_bioRxiv |> 
  filter(cited_journal %in% journals_philo_bio$citing_journal) |> 
  group_by(citing_year) %>% 
  count() |> 
  ungroup()

# Plot evolution of references from biology going to philosophy. 
plot_philo_citedin_bio <- ggplot(philo_citedin_bio |> filter(citing_year <= 2024), aes(x = citing_year, y = n))+
  geom_smooth() + 
  geom_line() + 
  labs(
    title = "References to Philosophy Articles in BioRxiv",
    x = "Year",
    y = "N. References"
  )

# VISIBILITY OF BIOLOGY IN PHILOSOPHY -------------------------------------
# Now we do the opposite. We want to find which citing_id from biology is CITED by our philosophy corpus. 
articles_bioRxiv_reformat |> 
  filter(citing_id %in% references_special_philo_bio_reformat$cited_id) |> 
  count(citing_journal) |> arrange(desc(n))

journals_bio <- articles_bioRxiv_reformat |> distinct(citing_journal) # 4,361
bio_citedin_philo <- articles_and_references_special_philo_bio |> 
  filter(cited_journal %in% journals_bio$citing_journal) |> 
  group_by(citing_year) %>% 
  count() |> 
  ungroup()

# Plot evolution of references from philosophy going to biology. 
plot_bio_citedin_philo <- ggplot(bio_citedin_philo |> filter(citing_year <= 2024), aes(x = citing_year, y = n))+
  geom_smooth() + 
  geom_line() + 
  labs(
    title = "References to Biology Articles in Philosophy of Biology",
    x = "Year",
    y = "N. References"
  )

# WHAT ARE THE ALL-AROUND REFERENCES EVOLUTION? ---------------------------
# Only for *Biology and Philosophy*.
bp_refs_all <- articles_and_references_special_philo_bio |> 
  filter(citing_journal == "BIOLOGY AND PHILOSOPHY") |> 
  select(cited_year, citing_year) |> 
  group_by(citing_year) |> 
  count()

plot_bp <- ggplot(bp_refs_all |> filter(citing_year <=2024), aes(x = citing_year, y = n))+ 
  geom_smooth() +
  geom_line()   + 
  labs(
    title = "References in Philosophy of Biology",
    x = "Year",
    y = "N. References"
  )
plot_bp


bp_refs_bio <- articles_and_references_special_philo_bio |> 
  filter(citing_journal == "BIOLOGY AND PHILOSOPHY") |>
  filter(cited_journal %in% journals_bio$citing_journal) |> 
  group_by(citing_year) %>% 
  count() |> 
  ungroup()

bp_refs_both <- left_join(bp_refs_all, bp_refs_bio, by = "citing_year")
bp_refs_both <- rename(bp_refs_both, n_philo = n.x, n_bio = n.y)
bp_refs_both_long <- bp_refs_both %>%
  pivot_longer(cols = c(n_philo, n_bio),
               names_to = "field",
               values_to = "n_citations")

plot_refs_both_long <- ggplot(bp_refs_both_long |> filter(citing_year <=2024), aes(x = citing_year, y = n_citations, group = field, color = field))+ 
  geom_smooth() +
  geom_line()   +
  labs(
    title = "References to Biology in Biology and Philosophy",
    x = "Year",
    y = "N. References"
  )
plot_refs_both_long

# Philosophy
all_ref_philo_bio <- articles_and_references_special_philo_bio |> select(cited_year, citing_year) |> group_by(citing_year) |> count()
plot_all_ref_philo_bio <- ggplot(all_ref_philo_bio |> filter(citing_year <=2024), aes(x = citing_year, y = n))+ 
  geom_smooth() +
  geom_line()   + 
  labs(
    title = "References in Philosophy of Biology",
    x = "Year",
    y = "N. References"
  )
plot_all_ref_philo_bio
plot_all_ref_philo_bio + plot_bio_citedin_philo


# One plot
refs_both <- left_join(all_ref_philo_bio, bio_citedin_philo, by = "citing_year")
refs_both <- rename(refs_both, n_philo = n.x, n_bio = n.y)
refs_both_long <- refs_both %>%
  pivot_longer(cols = c(n_philo, n_bio),
               names_to = "field",
               values_to = "n_citations")


plot_refs_both_long <- ggplot(refs_both_long |> filter(citing_year <=2024), aes(x = citing_year, y = n_citations, group = field, color = field))+ 
  geom_smooth() +
  geom_line()   +
  labs(
    title = "References in Philosophy of Biology",
    x = "Year",
    y = "N. References"
  )
plot_refs_both_long


# Biology
all_ref_bio <- articles_and_references_bioRxiv |> select(citing_year) |> group_by(citing_year) |> count()
plot_all_ref_bio <- ggplot(all_ref_bio |> filter(citing_year <=2024), aes(x = citing_year, y = n))+ 
  geom_smooth() +
  geom_line() + 
  labs(
    title = "References in BioRxiv",
    x = "Year",
    y = "N. References"
  )
plot_all_ref_bio
plot_all_ref_bio + plot_philo_citedin_bio



































