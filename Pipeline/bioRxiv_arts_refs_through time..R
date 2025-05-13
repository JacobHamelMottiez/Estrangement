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


# PLOT ARTS AND REFS THROUGH TIME -----------------------------------------
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
  geom_line(color = "darkgray", linewidth = 1.5) + 
  labs(x = "Year", y = "Count", title = "Trends of BioRxiv Articles") 
plot_art

plot_refs <- ggplot(count_arts_refs, aes(x = citing_year, y = count_refs)) + 
  geom_smooth(se = TRUE, color = "#221f20", linewidth = 3) +
  geom_line(color = "darkgray", linewidth = 1.5) + 
  labs(x = "Year", y = "Count", title = "Trends of BioRxiv References") 
plot_refs


plot_art+plot_refs




