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


## À Faire. 
# 1. Retrouver les articles en bio grâce à SCOPUS. 
# 2. Regarder leurs références. 
# 4. Regarder celles qui vont vers l'un des 4 journaux. 
# 5. Est-ce que la philo est plus visible dans une certaine biologie? 


# all_bio_arxiv <- all_bio_arxiv |> mutate(date_formatted = as.numeric(format(published_date, "%Y")))
# all_bio_arxiv <- all_bio_arxiv |> distinct() 
# all_bio_arxiv |> filter(date_formatted >= 2013 & date_formatted <= 2016 ) |> distinct() |> write_csv(paste0(dir_od, "bio_arxiv_2013-2016.csv"))

dir_od <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/"
all_bio_arxiv_2022_2024 <- read_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv_2022-2024.csv")
all_bio_arxiv <- read_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv.csv")
all_bio_arxiv |> distinct() |> filter(!is.na(published_doi)) # 137, 236 out of 137, 237

doi_data <- all_bio_arxiv_2022_2024 |> select(published_doi)
doi_data <- doi_data |> mutate(doi_query = paste0("DOI(", published_doi, ")"))
doi_data <- doi_data |>
  filter(!grepl('\\(.*[()]+.*\\)|"', doi_query)) # Filter out rows with parentheses between "DOI(" and the last ")" and also '"'.


collapse_in_chunks <- function(data, chunk_size = 25) {
  # Split the data into chunks of `chunk_size` rows
  chunks <- split(data$doi_query, ceiling(seq_along(data$doi_query) / chunk_size))
  
  # Collapse each chunk into a single string
  collapsed_strings <- sapply(chunks, function(chunk) {
    paste(chunk, collapse = " or ")
  })
  
  # Return a tibble with each chunk as a row
  tibble(chunk = collapsed_strings)
}
result <- collapse_in_chunks(doi_data, chunk_size = 25)
write_csv(result, paste0(dir_od, "bio_arxiv_2022_2024.csv"))

## Fetching through Python. Cleaner and simpler data management. 


# WHICH REFERENCES in BIOARXIV ARE FROM OUR PHILOSOPHY COPRUS ?  ----------------------------------
ARTICLES_SPECIAL_PHILO_BIO <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data//pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv")
pybliometrics_bio_arxiv_2022_2024 <- read_csv("Data/pybliometrics_bio_arxiv_2022_2024.csv") # I find 30 000 out of 50 000 articles. 


doi_data <- pybliometrics_bio_arxiv_2022_2024 |> select(doi)
doi_data <- doi_data |> mutate(doi_query = paste0("DOI(", doi, ")"))
doi_data <- doi_data |>
  filter(!grepl('\\(.*[()]+.*\\)|"', doi_query)) # Filter out rows with parentheses between "DOI(" and the last ")" and also '"'.
result <- collapse_in_chunks(doi_data, chunk_size = 25)
write_csv(result, paste0(dir_od, "scopus_doi_bio_arxiv_2022_2024.csv"))




ARTICLES_SPECIAL_PHILO_BIO <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data//pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv")
all_bio_arxiv <- read_csv(paste0(dir_od,"BIO_ARXIV_DATA/all_bio_arxiv.csv"))
all_bio_arxiv |> distinct() # 137, 237
all_bio_arxiv <- all_bio_arxiv |> mutate(year = year(published_date)) |> filter(year >= 2017 & year <= 2024) |> distinct()


refs_bio_arxiv_2017_2024 <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/refs_bio_arxiv_2017_2024.csv") # 5, 858, 640
refs_bio_arxiv_2013_2016 <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/refs_bio_arxiv_2013_2016.csv")
refs_bio_arxiv <- rbind(refs_bio_arxiv_2013_2016, refs_bio_arxiv_2017_2024)


refs_bio_arxiv <- left_join(refs_bio_arxiv, all_bio_arxiv |> select(published_doi, published_journal, preprint_category), join_by(source_eid == published_doi))
refs_bio_arxiv <- refs_bio_arxiv |> distinct()

nrow(refs_bio_arxiv)/nrow(all_bio_arxiv) # mean of 43 refs per articles. 


test <- refs_bio_arxiv |> mutate(is_ref_philo_bio = ifelse(id %in% ARTICLES_SPECIAL_PHILO_BIO$citing_id, TRUE, FALSE))


test <- test |> filter(is_ref_philo_bio == TRUE)



test2 <- left_join(test, ARTICLES_SPECIAL_PHILO_BIO, join_by(id == citing_id)) # 258 citations to philosophy of biology. 



test3 <- test2 |> select(id, citing_title, author_names, affiliation_country, publicationName, preprint_category, published_journal) |> add_count(id) |> distinct() |> print(n = 250)

test3$published_journal <- toupper(test3$published_journal)
test3$preprint_category <- toupper(test3$preprint_category)

test3 |> count(published_journal) |> arrange(desc(n))
test3 |> count(preprint_category) |> arrange(desc(n))




arxiv_2017_2024 <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/bio_arxiv_2017-2024.csv")https://www.cabidigitallibrary.org/doi/full/10.5555/19511404458




# MOST INFLUENTIAL BIOLOGY DISCIIPLINES THAT CITES PHIILOSOPHY ------------
test3 <- left_join(test, arxiv_2017_2024, join_by(source_eid == published_doi)) |> print(n =100)


test3 <- test3 |> select(-...1, -id) |> distinct()

test3 |> count(preprint_category) |> arrange(desc(n)) |> print(n =21)







# INFLUENTIAL BIOLOGY ABSENT FROM PHILOSOPHY OF BIOLOGY -------------------

REFERENCES_SPECIAL_PHILO_BIO <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/REFERENCES_SPECIAL_PHILO_BIO.csv")



REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "Collective dynamics of")) #5 refs in philosophy of biology. 
REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "Molecular Link"))  #0 refs in philosophy of biology. 


REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "p53 mutations")) #0 refs 

REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "The Ecology")) #0 refs for The Ecology of Natural Disturbance and Patch Dynamics
 




REFERENCES_SPECIAL_PHILO_BIO |>  filter(str_detect(title, "Pleiotropy, natural selection, and the evolution of senescence")) #11 times Pleiotropy, Natural Selection, and the Evolution of Senescence
REFERENCES_SPECIAL_PHILO_BIO |>  filter(str_detect(title, "The Chemical")) # 0 The Chemical Basis of Morphogenesis" by Alan Turing (1952)
REFERENCES_SPECIAL_PHILO_BIO |>  filter(str_detect(title, "Molecular Cloning")) #0 Molecular cloning: a laboratory manual.
REFERENCES_SPECIAL_PHILO_BIO |>  filter(str_detect(title, "A rating scale")) #1
REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "The genetical evolution of social behaviour")) # highly cited. 
REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "Mate selection")) #32
REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "The measurement of diversity")) #1 ? The measurement of diversity in different types of biological collections
REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "Metabolic stability and epigenesis in randomly constructed genetic nets")) #5 
REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "Geometry for the selfish herd")) #5

REFERENCES_SPECIAL_PHILO_BIO |> filter(str_detect(title, "Positional information and the spatial pattern of cellular differentiation")) #7


# Top refs in bioarxiv ----------------------------------------------------
refs_bio_arxiv_2017_2024 <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/refs_bio_arxiv_2017_2024.csv") |> distinct() 




count <- refs_bio_arxiv_2017_2024 |> count(id) |> arrange(desc(n)) 


count |> print(n = 100)


