# ------------------------------
# Title: all_philosophy_of_biology.R
# Author: Jacob Hamel-Mottiez
# Date: 2025-01-25
# Description: 
# The goal of this script is to bind all the philosophy of biology data
# in the same .csv. Thus we create two tibbles, one for the articles and 
# one for the references.
#
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------


# PACKAGES AND DIRECTORY ----------------------------------------------------------------
library(tidyverse)
dir <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/Philosophy of Biology/"


# LOAD DATA  --------------------------------------------------------------
names_philo_of_bio_journals = c(
  "BIOLOGY_&_PHILOSOPHY",
  "BIOLOGY_AND_PHILOSOPHY",
  "BIOLOGICAL_THEORY",
  "STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_SCIENCE_PART_C__STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_BIOLOGICAL_AND_BIOMEDICAL_SCIENCES",
  "HISTORY_AND_PHILOSOPHY_OF_THE_LIFE_SCIENCES")


for(i in 1:length(names_philo_of_bio_journals)){
  arts <- read_csv(paste0(dir, names_philo_of_bio_journals[i],".csv"))
  refs <- read_csv(paste0(dir, names_philo_of_bio_journals[i],"_references",".csv"))
  assign(paste0(names_philo_of_bio_journals[i], "_refs"), refs)
  assign(paste0(names_philo_of_bio_journals[i]), arts)
}


# Merge the two variations of the journal "Biology and Philosophy" 
BIOLOGY_AND_PHILOSOPHY_ALL <- rbind(`BIOLOGY_&_PHILOSOPHY`,BIOLOGY_AND_PHILOSOPHY) |> tibble()
BIOLOGY_AND_PHILOSOPHY_ALL_refs <- rbind(`BIOLOGY_&_PHILOSOPHY_refs`,BIOLOGY_AND_PHILOSOPHY_refs) |> tibble()

write_csv(BIOLOGY_AND_PHILOSOPHY_ALL, paste0(dir, "BIOLOGY_AND_PHILOSOPHY_ALL.csv"))
write_csv(BIOLOGY_AND_PHILOSOPHY_ALL_refs, paste0(dir,"BIOLOGY_AND_PHILOSOPHY_ALL_refs.csv"))



# MERGE ALL PHILOSOPHY OF BIOLOGY TOGHETER --------------------------------
PHILOSOPHY_OF_BIOLOGY_ALL <- rbind(BIOLOGY_AND_PHILOSOPHY_ALL, 
                                   BIOLOGICAL_THEORY, 
                                   STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_SCIENCE_PART_C__STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_BIOLOGICAL_AND_BIOMEDICAL_SCIENCES, 
                                   HISTORY_AND_PHILOSOPHY_OF_THE_LIFE_SCIENCES ) |> tibble()

PHILOSOPHY_OF_BIOLOGY_ALL_refs <- rbind(BIOLOGY_AND_PHILOSOPHY_ALL_refs, 
                                        BIOLOGICAL_THEORY_refs, 
                                        STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_SCIENCE_PART_C__STUDIES_IN_HISTORY_AND_PHILOSOPHY_OF_BIOLOGICAL_AND_BIOMEDICAL_SCIENCES_refs, 
                                        HISTORY_AND_PHILOSOPHY_OF_THE_LIFE_SCIENCES_refs) |> tibble()

# As we see, there is some noise in our data. This step is to make sure that we keep only the relevant journals. 
PHILOSOPHY_OF_BIOLOGY_ALL |> distinct(publicationName)
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> mutate(publicationName = case_when(
  publicationName == "Biology &amp; Philosophy" ~ "Biology and Philosophy",
  publicationName == "History and philosophy of the life sciences." ~ "History and Philosophy of the Life Sciences",
  publicationName == "History and philosophy of the life sciences" ~ "History and Philosophy of the Life Sciences",
  TRUE ~ publicationName)) 
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> filter(publicationName != "Modernist Life Histories: Biological Theory and the Experimental Bildungsroman" & 
                                                                   publicationName != "Evolved Morality: The Biology and Philosophy of Human Conscience")

write_csv(PHILOSOPHY_OF_BIOLOGY_ALL, paste0(dir, "PHILOSOPHY_OF_BIOLOGY_ALL.csv"))
write_csv(PHILOSOPHY_OF_BIOLOGY_ALL_refs, paste0(dir,"PHILOSOPHY_OF_BIOLOGY_ALL_REFS.csv"))










