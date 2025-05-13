
# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
library(ggplot2)

# DATA --------------------------------------------------------------------
dir_data <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/"

references <- read_csv(paste0(dir_data, "REFERENCES_SPECIAL_PHILO_BIO.csv"))
articles <- read_csv(paste0(dir_data, "ARTICLES_SPECIAL_PHILO_BIO.csv"))


# Group by citing_id and paste cited_id values into a single row
references_filtered = references |> add_count(cited_id) |> filter(n>10)

df_grouped <- references_filtered %>%
  group_by(citing_id) %>%
  summarise(cited_references = paste0(na.omit(cited_title), collapse = ", "))


df_grouped <- df_grouped |> filter(cited_references != "")

print(df_grouped)
write_csv(df_grouped, paste0(dir_data, "refs_by_articles_for_umap.csv"))
