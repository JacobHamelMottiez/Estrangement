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
dir_bio <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/"
articles_and_references_bioRxiv <- read_csv(paste0(dir_bio, "articles_and_references_bioRxiv.csv"))

dir_philo <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/"
articles_and_references_philo <- read_csv(paste0(dir_philo, "articles_and_references_special_philo_bio.csv"))


# PLOT ARTS AND REFS TROUGH TIME ------------------------------------------
## BIORXIV -----------------------------------------
count_art <-  articles_and_references_bioRxiv |> 
  select(citing_year, citing_id) |>
  distinct() |>
  group_by(citing_year) |> 
  count(name = "count_art") |> 
  arrange(-desc(count_art))


count_refs <- articles_and_references_bioRxiv |> 
  select(citing_year) |> 
  group_by(citing_year) |> 
  add_count(name = "count_refs") |> 
  arrange(-desc(count_refs)) |> distinct()


count_arts_refs <- left_join(count_art, count_refs, by = "citing_year") |> arrange(-desc(citing_year))


plot_art <- ggplot(count_arts_refs, aes(x = citing_year, y = count_art))+
  geom_smooth(se = TRUE, color = "#aa2c39", linewidth = 3) +
  geom_line(color = "darkgray", linewidth = 2) + 
  labs(x = "Year", y = "Count", title = "Trends of BioRxiv Articles")+
  xlim(c(2013, 2024))
plot_art

plot_refs <- ggplot(count_arts_refs, aes(x = citing_year, y = count_refs)) + 
  geom_smooth(se = TRUE, color = "#221f20", linewidth = 3) +
  geom_line(color = "darkgray", linewidth = 2) + 
  labs(x = "Year", y = "Count", title = "Trends of BioRxiv References") +
  xlim(c(2013, 2024))
plot_refs


plot_art+plot_refs



## SPECIALIZED PHILO. OF BIOLOGY -------------------------------------------
articles_and_references_philo <- articles_and_references_philo |> select(citing_journal, citing_year)
articles_and_references_philo <- articles_and_references_philo |> group_by(citing_journal) |> 
  add_count(citing_year) |> 
  ungroup() |> 
  distinct()

articles_and_references_philo <- articles_and_references_philo |>
  mutate(citing_journal = case_when(
    citing_journal == "STUDIES IN HISTORY AND PHILOSOPHY OF SCIENCE PART C :STUDIES IN HISTORY AND PHILOSOPHY OF BIOLOGICAL AND BIOMEDICAL SCIENCES" ~ 
      "STUDIES IN HISTORY AND PHILOSOPHY OF SCIENCE PART C",
    TRUE ~ citing_journal
  ))


ggplot(articles_and_references_philo, aes(x = citing_year, y = n, group = citing_journal, color = citing_journal)) + 
  theme(
    panel.background = element_rect(fill = "gray90", color = NA),  # Force the gray background
    panel.grid.major = element_line(color = "white"),
    legend.text = element_text(size=20), 
    axis.title.y = element_text(size=25),
    axis.text.y = element_text(size = 20), 
    axis.text.x = element_text(size = 20),
    strip.text.x = element_text(size = 20), 
    axis.title.x = element_text(size = 25),
    legend.position = "none") +   # Keep white gridlines
  geom_line(linewidth = 0.8, color = "darkgrey") +
  geom_smooth(linewidth = 1)  +  # You can change the theme if needed 
  facet_wrap(~ citing_journal, nrow = 4) +
  xlab("Year") + 
  ylab("N. Articles Published")

