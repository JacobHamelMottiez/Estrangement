# Author: Your Name
# Date: `r Sys.Date()`
# Description:

library(tidyverse)
library(plotly)
library(readr)
all_bio_arxiv <- read_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv.csv") |> distinct() |> filter(!is.na(published_journal))
#all_bio_arxiv |> write_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv_filtered.csv")
all_bio_arxiv <- read_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv_filtered.csv")


# FILTER PROBLEMATIC DOI --------------------------------------------------
doi_data <- all_bio_arxiv |> mutate(doi_query = paste0("DOI(", published_doi, ")"))
doi_data <- doi_data |>
  filter(!grepl('\\(.*[()]+.*\\)|"', doi_query)) # Filter out rows with parentheses between "DOI(" and the last ")" and also '"'. 


# FUNCTION TO GET GOOD FORMAT FOR QUERY -----------------------------------
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
#write_csv(result, "Data/BIO_ARXIV_DATA/all_bio_arxiv_filtered_doi.csv")

# next file from bio_arxiv_articles_scopus.ipynb
pybliometrics_all_bio_arxiv_filtered <- read_csv("Data/pybliometrics_all_bio_arxiv_filtered.csv")
pybliometrics_all_bio_arxiv_filtered # 134, 756
pybliometrics_all_bio_arxiv_filtered <-  pybliometrics_all_bio_arxiv_filtered |> 
  mutate(citing_year = as.numeric(format(coverDate, "%Y")))
# We fetched references through DOIs so we don't need to look up references with the eid. 

refs_bio_arxiv<- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/refs_bio_arxiv.csv") |> select(-...1)
refs_bio_arxiv <- refs_bio_arxiv |> distinct() # Same value. 



# VISUALIZATION  ----------------------------------------------------------
art_bio_year <- pybliometrics_all_bio_arxiv_filtered |> 
  count(citing_year) |> 
  arrange(desc(n)) |> 
  filter(citing_year <= 2024)

art_bio_year <- art_bio_year %>%
  mutate(citing_year = make_date(as.numeric(citing_year)))


ref_bio_year <- refs_bio_arxiv |> left_join(pybliometrics_all_bio_arxiv_filtered |> 
                                              select(doi, citing_year), join_by(source_eid == doi))

ref_bio_year <- ref_bio_year |> select(-id)|> 
  add_count(source_eid, name = "n_ref") |> 
  select(-source_eid) |> 
  arrange(desc(n_ref)) |> 
  distinct()

ref_bio_year <- ref_bio_year |> group_by(citing_year) |>  summarise(n_ref = sum(n_ref, na.rm = TRUE))

ref_bio_year <- ref_bio_year %>%
  mutate(citing_year = make_date(as.numeric(citing_year)))


full_data <- left_join(art_bio_year, ref_bio_year, by = "citing_year")
full_data <- full_data |> filter(citing_year <= as.Date("2024-01-01"))


ggplot(full_data, aes(x = citing_year, y = n)) + 
  geom_smooth(data = art_bio_year, mapping = aes(x = citing_year, y = n), color = "salmon", linewidth = 3) +  # Why is there a drop in the last two years?
  geom_smooth(data = ref_bio_year, mapping = aes(x = citing_year, y = n_ref), color = "lightblue", linewidth = 3) + 
  xlab("Year") + 
  ylab("N. of Documents")+ 
  scale_x_date(limits = c(as.Date("2013-01-01"), as.Date("2024-01-01")))



# GET BACK PREPRINT CATEGORY FROM BIOARXIV --------------------------------
refs_bio_arxiv <- left_join(refs_bio_arxiv, all_bio_arxiv |> select(published_doi, published_journal, preprint_category), join_by(source_eid == published_doi))
refs_bio_arxiv <- refs_bio_arxiv |> distinct()

test <- refs_bio_arxiv |> mutate(is_ref_philo_bio = ifelse(id %in% ARTICLES_SPECIAL_PHILO_BIO$citing_id, TRUE, FALSE))
test <- test |> filter(is_ref_philo_bio == TRUE)
test2 <- left_join(test, ARTICLES_SPECIAL_PHILO_BIO, join_by(id == citing_id)) # 258 citations to philosophy of biology. 


test3 <- test2 |> select(author_names, citing_title) |> add_count(citing_title) |> arrange(desc(n)) |> distinct() |> print(n = 100)

test3 |> filter(n>=3) |> write_csv("test3.csv") 
test2$preprint_category <- toupper(test2$preprint_category)
test2 |> count(preprint_category) |> arrange(desc(n)) |> distinct()


# COMPARAISON CITATION VS ARTICLES PUBLIÉS  -------------------------------
all_bio_arxiv$preprint_category <- toupper(all_bio_arxiv$preprint_category) 
all_bio_arxiv_category <- all_bio_arxiv |> select(preprint_category) |> count(preprint_category, name = "n_articles")

test2$preprint_category <- toupper(test2$preprint_category)
philo_citation_in_bio <- test2 |> count(preprint_category, name = "n_citations") |> arrange(desc(n_citations)) |> distinct()


full_data <- full_join(all_bio_arxiv_category, philo_citation_in_bio, by = "preprint_category") |> arrange(desc(n_citations))


# Reshape data to long format
full_data_long <- full_data %>%
  pivot_longer(cols = c(n_articles, n_citations), names_to = "type", values_to = "count")

# Plot with two bars per category
ggplot(full_data_long, aes(x = preprint_category, y = count, fill = type)) + 
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(x = "Preprint Category", y = "Count", fill = "")


# RANG EN PHILO DES ARTICLES LES PLUS CITÉS (DE PHILO) EN BIO -------------














