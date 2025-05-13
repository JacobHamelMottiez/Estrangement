# ------------------------------
# Title: merge_spec_philo_bio.R
# Author: Jacob Hamel-Mottiez
# Date: 2025-02-21
# Description: This file is used to merge all the specialized philosophy of biology
# together. Each journal has been fetched through Scopus API via Pybliometrics package
# in Python. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)
library(ggplot2)

# DATA --------------------------------------------------------------------
dir_data <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/"

journal_names <- c(
  "BIOLOGY_AND_PHILOSOPHY",
  "BIOLOGICAL_THEORY",
  "STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_SCIENCE_PART_C",
  "HISTORY_AND_PHILOSOPHY_OF_THE_LIFE_SCIENCES",
  "BIOLOGY_&_PHILOSOPHY"
)

articles_data <- list()
references_data <- list()

# Load each CSV file and assign it to the list with its clean name
for (i in seq_along(journal_names)) {
  articles_file_path <- paste0(dir_data, journal_names[i], ".csv")
  articles_data[[journal_names[i]]] <- read_csv(articles_file_path)
  
  references_file_path <- paste0(dir_data, journal_names[i], "_refs_pyblio.csv")
  references_data[[paste0(journal_names, "_refs")[i]]] <- read_csv(references_file_path)
}

# Attach the dataframes to the global environment with their clean names
invisible(list2env(articles_data, envir = .GlobalEnv))
invisible(list2env(references_data, envir = .GlobalEnv))


# ARTICLES ----------------------------------------------------------------
# COMMENT : for some reason, type of the columns are different, we need to uniformize. 
articles_data <- map(articles_data, ~ mutate(.x, across(everything(), as.character)))

# Bind articles into a single tibble
combined_tibble <- bind_rows(articles_data)
combined_tibble$coverDate <- as_date(combined_tibble$coverDate) 
combined_tibble <- combined_tibble |> mutate(coverDate = as.numeric(format(coverDate, "%Y")))
combined_tibble <- combined_tibble |> distinct()

# We need to clean some of the publication names. 
combined_tibble |> group_by(publicationName) |> count()
combined_tibble <-  combined_tibble |> mutate(publicationName = case_when(
  publicationName == "Biology &amp; Philosophy" ~ "Biology and Philosophy",
  publicationName == "History and philosophy of the life sciences." ~ "History and Philosophy of the Life Sciences",
  publicationName == "History and philosophy of the life sciences" ~ "History and Philosophy of the Life Sciences",
  TRUE ~ publicationName))


combined_tibble |> group_by(publicationName) |> count()
combined_tibble <- combined_tibble |> filter(publicationName != "Modernist Life Histories: Biological Theory and the Experimental Bildungsroman" & 
                                                                   publicationName != "Evolved Morality: The Biology and Philosophy of Human Conscience")


combined_tibble$publicationName <- toupper(combined_tibble$publicationName)
combined_tibble |> group_by(publicationName) |> count()

# COMMENT : this step is now in master_table.R. 
# Function to be able to match citing and cited eid. 
# clean_articles_fct <- function(articles){
#   articles$citing_id <- str_extract(articles$eid, "(?<=2-s2\\.0-)\\d+")
#   articles$eid <- NULL
#   articles <- rename(articles,
#                      citing_category = subtypeDescription,
#                      citing_title = title)
#   return(articles)
# }  

#combined_tibble <- clean_articles_fct(combined_tibble)
write_csv(combined_tibble, paste0(dir_data, "ARTICLES_SPECIAL_PHILO_BIO.csv"))


# REFERENCES --------------------------------------------------------------
references_data <- map(references_data, ~ mutate_all(.x, as.character))

# Bind them into a single tibble
references_combined_tibble <- bind_rows(references_data)
references_combined_tibble$publicationyear <- as.Date(references_combined_tibble$publicationyear, "%Y")
references_combined_tibble <- references_combined_tibble |> mutate(publicationyear = as.numeric(format(publicationyear, "%Y"))) # cited_year
references_combined_tibble <- references_combined_tibble |> distinct()

# Here, we want to clean a bit our references. 
most_common <- function(x) {
  x <- na.omit(x)  # Remove NAs first
  if (length(x) == 0) return(NA)  # If all values were NA, return NA
  
  unique_x <- unique(x)
  counts <- tabulate(match(x, unique_x))
  
  unique_x[which.max(counts)]  # Return most frequent value
}

# COMMENT : This was before write_csv. However, I want to clean_refs and articles in a new script. 
# Cleaning function
# clean_references <- function(df) {
#   df <- df %>%
#     group_by(id) %>%
#     mutate(
#       title = most_common(title),
#       sourcetitle = most_common(sourcetitle),
#       cited_year = most_common(cited_year),
#       cited_title = most_common(title),  # Fix duplicated column
#       authors = most_common(authors)           # Fix wrong column reference
#     ) %>%
#     ungroup()
# 
#   # Handle possible NA values before applying transformations
#   df$sourcetitle <- ifelse(is.na(df$sourcetitle), NA, toupper(df$sourcetitle))
# 
#   # Extracting citing_id from citing_eid
#   df$citing_id <- str_extract(df$citing_eid, "(?<=2-s2\\.0-)\\d+")
# 
#   # Rename column
#   df <- rename(df, cited_id = id)
# 
#   # Drop the `citing_eid` column
#   df$citing_eid <- NULL
# 
#   return(df)
# }
# 
# references_combined_tibble <- clean_references(references_combined_tibble)

write_csv(references_combined_tibble, paste0(dir_data, "REFERENCES_SPECIAL_PHILO_BIO.csv"))





