# ------------------------------
# Title: {filename}
# Author: Jacob Hamel-Mottiez
# Date: sys.Date()
# Description: The goal of this script is to see wheater philosophy of biology cites the same journals as biology. 
# Contact : For any issue please write to jacob.hamel-mottiez.1@ulaval.ca. 
# ------------------------------

# PACKAGES ----------------------------------------------------------------
library(tidyverse)
library(plotly)
library(readxl)


final_table_OR <-  read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/openrefine/most-cited-journal-in-philo-bio_openrefine.csv")
scimago_immuno_and_micro <- read_delim("scimagojr 2023  Subject Area - Immunology and Microbiology.csv", delim = ";", col_names = TRUE)
scimago_multi <- read_delim("scimagojr 2023  Subject Area - Multidisciplinary.csv", delim = ";", col_names = TRUE)
scimago_neuro <- read_delim("scimagojr 2023  Subject Area - Neuroscience.csv", delim = ";", col_names = TRUE)
scimago_biochem_gene_molecular_bio <- read_delim("scimagojr 2023  Subject Area - Biochemistry, Genetics and Molecular Biology.csv", delim = ";", col_names = TRUE)
scimago_agri_and_bio_sci <- read_delim("scimagojr 2023  Subject Area - Agricultural and Biological Sciences.csv", delim = ";", col_names = TRUE)


final_table_OR |> select(sourcetitle)


total_journals <- rbind(scimago_immuno_and_micro, scimago_multi, scimago_neuro, scimago_biochem_gene_molecular_bio, scimago_agri_and_bio_sci) |> janitor::clean_names()

total_journals_Q1 <- total_journals |> select(rank, title, sjr_best_quartile, issn, h_index, percent_female, categories, areas) |> filter(sjr_best_quartile == "Q1")


total_journals_Q1 <- total_journals_Q1 |> separate_rows(issn, sep = ", ")
ISSN_median_mean <- read_csv("Data/ISSN_median_mean.csv") |> select(-...1)
final_table_OR |> select(sourcetitle) |> count(sourcetitle) |> arrange(desc(n)) |> filter(n >= 100)

bio_in_philo_bio <- left_join(total_journals_Q1, ISSN_median_mean, join_by(issn == ISSN))
bio_in_philo_bio


# Fill NA values based on non-NA values from the same title
bio_in_philo_bio <- bio_in_philo_bio %>%
  group_by(title) %>%
  mutate(sourcetitle = ifelse(is.na(sourcetitle), na.omit(sourcetitle)[1], title)) %>%
  ungroup()


bio_in_philo_bio$sourcetitle <-  toupper(bio_in_philo_bio$sourcetitle)

bio_in_philo_bio <-  bio_in_philo_bio |> select(-n, -median_citescore, -mean_citescore, -issn) |> distinct()

bio_in_philo_bio$title <- toupper(bio_in_philo_bio$title)
bio_in_philo_bio_na <- bio_in_philo_bio |> filter(is.na(sourcetitle))

final_table_OR |> filter(str_detect(sourcetitle, "NATURE REVIEWS IMMUNOLOGY"))


test <- left_join(bio_in_philo_bio_na, final_table_OR, join_by(title == sourcetitle))

test |> filter(!is.na(sourcetitle))


absent_bio_journals <- bio_in_philo_bio_na |> select(title) |> distinct() # ~ 1400 rows 



sourcetitle <- final_table_OR |> select(sourcetitle) |> distinct()
sourcetitle  |> filter(str_detect(sourcetitle, "SCIENCE IMMUNOLOGY"))
absent_bio_journals |> filter(str_detect(title, "SCIENCE IMMUNOLOGY"))




absent_bio_journals %in% sourcetitle



# Rows in sourcetitle that do not appear in absent_bio_journals
not_in_absent <- anti_join(absent_bio_journals, sourcetitle, by = c("title" = "sourcetitle"))

not_in_absent |> print(n = 100)


sourcetitle  |> filter(str_detect(sourcetitle, "NATURE AGING")) |> print(n = 30) # What the fuck. 

sourcetitle  |> filter(str_detect(sourcetitle, "REVIEWS IN")) |> print(n = 30)








