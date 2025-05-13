# ------------------------------
# Title: {filename}
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: In this file, I define a bunch of useful functions for cleaning 
# my corpus. We find most of them in a dedicated notebook called 
# `WHAT_IS_THE_AMOUNT_OF_REFS_TO_PHILO_BIO`. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(readr)
library(plotly)


# DATA --------------------------------------------------------------------
BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE_refs_pyblio <- read_csv("Data/pybiblio/BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE_refs_pyblio.csv")
BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE <- read_csv("Data/pybiblio/BRITISH_JOURNAL_FOR_THE_PHILOSOPHY_OF_SCIENCE.csv")
PHILOSOPHY_OF_BIOLOGY_ALL_4_INITIAL_JOURNALS <- read_csv("Data/Philosophy of Biology/PHILOSOPHY_OF_BIOLOGY_ALL_4_INITIAL_JOURNALS.csv")
PHILOSOPHY_OF_BIOLOGY_ALL_4_INITIAL_JOURNALS

# PREPROCESSING FUNCTIONS  -------------------------------------------------------
clean_references_fct <- function(references) {  
  references$sourcetitle <- toupper(references$sourcetitle)
  references$citing_id <- str_extract(references$citing_eid, "(?<=2-s2\\.0-)\\d+")
  references <- rename(references, cited_id = id)
  references$citing_eid <- NULL
  references <- rename(references, cited_year = publicationyear)
  return(references)
}

clean_articles_fct <- function(articles){
  articles$citing_id <- str_extract(articles$eid, "(?<=2-s2\\.0-)\\d+")
  articles$eid <- NULL
  articles <- rename(articles, citing_year = coverDate,
                     citing_category = subtypeDescription)
  return(articles)
}  


# MERGE ARTICLES AND THEIR REFERENCES -------------------------------------
merge_fct <- function(articles, references) { 
  # Merging
  reference_is_article <- left_join(
    articles |> select(citing_id, citing_category, citing_year), 
    references, 
    by = "citing_id") |> 
    filter(citing_category == "Article")
  return(reference_is_article)
}
reference_is_article <- merge_fct(bp_art,bp_refs)


# DETERMINE WHICH REFERENCES ARE FROM OUR INITIAL CORPUS ------------------
is_philo_bio_fct <- function(reference_is_article){
  # is the references from our corpus? TRUE or FALSE
  distinct_journals <- PHILOSOPHY_OF_BIOLOGY_ALL_4_INITIAL_JOURNALS |> distinct(publicationName) 
  distinct_journals$publicationName <- toupper(distinct_journals$publicationName)
  
  # Get the mean by years for 1) total refs, 2) refs from philo. of bio. and 3) refs not from philo. and bio
  reference_is_article <- reference_is_article |>
    mutate(is_philo_bio_journal = sourcetitle %in% distinct_journals$publicationName) |>
    group_by(citing_id) |>
    mutate(
      refs_from_philo_bio = sum(is_philo_bio_journal, na.rm = TRUE),
      refs_not_from_philo_bio = sum(!is_philo_bio_journal, na.rm = TRUE),
      total_refs = n()
    ) |>
    ungroup()
  return(reference_is_article)
}

reference_is_article <- is_philo_bio_fct(reference_is_article)


# CALCULATE THE PROPORTION OF REFERENCES TO PHILO. OF BIO./TOTAL R --------
proportion_fct <- function(reference_is_article) {
  proportion_table <- reference_is_article |> select(citing_id, total_refs, refs_from_philo_bio, refs_not_from_philo_bio, doi) |> arrange(desc(refs_from_philo_bio)) |> distinct()
  proportion_table <- proportion_table |> mutate(ratio_philo_bio = refs_from_philo_bio/total_refs*100)
  return(proportion_table)
}
proportion_table <- proportion_fct(reference_is_article)


# VISUALISATION -----------------------------------------------------------
## Get mean references by year for visualisation purposes. 

year_fct <- function(reference_is_article) {
  philo_bio_articles_in_general <- reference_is_article
  
  # Ensure citing_year is numeric
  philo_bio_articles_in_general <- philo_bio_articles_in_general %>%
    mutate(citing_year = as.numeric(format(citing_year, "%Y")))
  
  # Calculate total references per citing_id first
  philo_bio_articles_in_general <- philo_bio_articles_in_general %>%
    group_by(citing_id) %>%
    mutate(total_refs = n()) %>%
    ungroup()
  
  # Group by citing_year and summarize the mean counts
  philo_bio_articles_in_general <- philo_bio_articles_in_general %>%
    group_by(citing_year) %>%
    summarise(
      mean_n_ref_total = mean(total_refs, na.rm = TRUE),
      mean_n_ref_not_bio_philo = mean(refs_not_from_philo_bio, na.rm = TRUE),
      mean_n_ref_bio_philo = mean(refs_from_philo_bio, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(philo_bio_articles_in_general)
}

philo_bio_articles_in_general <- year_fct(reference_is_article)


## ggplot ------------------------------------------------------------------
philo_bio_articles_in_general <- philo_bio_articles_in_general |> select(citing_year, 
                                                                         mean_n_ref_total, 
                                                                         mean_n_ref_not_bio_philo, 
                                                                         mean_n_ref_bio_philo
)
philo_bio_articles_in_general |> arrange(desc(mean_n_ref_bio_philo))

data_long <- philo_bio_articles_in_general |> pivot_longer(cols = c(mean_n_ref_total, mean_n_ref_bio_philo),
                                                             names_to = "category",
                                                             values_to = "count") |> distinct()
  
g <- ggplot(data_long, aes(x = citing_year, y = count, fill = category)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = "philo_bio_in_general_year()",
      x = "Year", 
      y = "Mean N. of References", 
      fill = NULL) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),   # Bold, centered title
      axis.text.x = element_text(size=14, face = "bold"),              # Bold x-axis text
      axis.text.y = element_text(size=14, face = "bold"),               # Bold y-axis text
      axis.title.x = element_text(size=14, face="bold"),    
      axis.title.y = element_text(size=14, face="bold"),
    )
show(g)

# It is really small and it makes sense. I fetch around 60 references from the initial corpus from a corpus of over 30 000 references. 




