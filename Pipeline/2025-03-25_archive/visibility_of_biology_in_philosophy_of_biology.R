# Author: Jacob Hamel-Mottiez
# Date: `r Sys.Date()`
# Description: 

library(tidyverse)
library(plotly)
library(readr)

all_bio_arxiv <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/bioRxiv_raw_data/all_bio_arxiv.csv") |> distinct() |> filter(!is.na(published_journal))
#all_bio_arxiv |> write_csv("Data/BIO_ARXIV_DATA/all_bio_arxiv_filtered.csv")

pybliometrics_all_bio_arxiv_filtered <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/pybliometrics_all_bio_arxiv_filtered.csv")
refs_all_bio_arxiv <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/refs_all_bio_arxiv.csv")


# DATA --------------------------------------------------------------------
## Biology
all_bio_arxiv_filtered <- all_bio_arxiv |> count(published_journal) |> arrange(desc(n)) |> filter(n>= 100)
all_bio_arxiv_filtered$published_journal <- toupper(all_bio_arxiv_filtered$published_journal)

## Philosophy
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/REFERENCES_SPECIAL_PHILO_BIO.csv") |> distinct()
PHILOSOPHY_OF_BIOLOGY_ALL <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv") |> distinct()
PHILOSOPHY_OF_BIOLOGY_ALL_REFS$sourcetitle <- toupper(PHILOSOPHY_OF_BIOLOGY_ALL_REFS$sourcetitle)


### Match journals in biology to references in philosophy of biology. 
#ISSN_median_mean <- read_csv("Data/ISSN_median_mean.csv")
#ISSN_median_mean <- rename(ISSN_median_mean, published_journal = sourcetitle)
#test = left_join(all_bio_arxiv_filtered, ISSN_median_mean, by = "published_journal") |> arrange(desc(n.y)) |> print(n = 100)
#test |> print(n = 363)


# VISIBILITY OF BIOLOGY IN PHILOSOPHY OF BIOLOGY --------------------------
refs_to_bio <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> filter(sourcetitle %in% all_bio_arxiv_filtered$published_journal)
refs_to_bio |> filter(is.na(cited_year)) # 17. 

refs_to_bio <- refs_to_bio |> select(citing_id, cited_id, sourcetitle) 
refs_to_bio  <- left_join(refs_to_bio, PHILOSOPHY_OF_BIOLOGY_ALL, join_by(citing_id == citing_id))
refs_to_bio <- refs_to_bio |> distinct()
refs_to_bio <- refs_to_bio %>%
    mutate(coverDate= as.numeric(format(coverDate, "%Y")))
refs_to_bio <- refs_to_bio |> count(coverDate) |> filter(coverDate <= 2024)

viz_in_bio <- ggplot(refs_to_bio, aes(x = coverDate, y = n)) + 
  geom_line() +
  geom_smooth() + 
  labs(title = "Visibility of Biology in Philosophy of Biology") + 
  theme_minimal()  # You can change the theme if needed
viz_in_bio

# PAPERS PUBLISHED IN SPECIALIZED JOURNALS OF PHILOSOPHY OF BIOLOGY -------
PHILOSOPHY_OF_BIOLOGY_ALL_ART <- PHILOSOPHY_OF_BIOLOGY_ALL |> filter(citing_category == "Article")
PHILOSOPHY_OF_BIOLOGY_ALL_ART <- PHILOSOPHY_OF_BIOLOGY_ALL_ART |> mutate(coverDate= as.numeric(format(coverDate, "%Y")))
PHILOSOPHY_OF_BIOLOGY_ALL_ART <- PHILOSOPHY_OF_BIOLOGY_ALL_ART |> select(publicationName, coverDate)
PHILOSOPHY_OF_BIOLOGY_ALL_ART <- PHILOSOPHY_OF_BIOLOGY_ALL_ART |> group_by(publicationName) |> add_count(coverDate) |> ungroup() |> distinct()

PHILOSOPHY_OF_BIOLOGY_ALL_ART <- PHILOSOPHY_OF_BIOLOGY_ALL_ART |> mutate(publicationName = case_when(
  publicationName == "Studies in History and Philosophy of Science Part C :Studies in History and Philosophy of Biological and Biomedical Sciences" ~ "Studies in History and Philosophy of Science Part C", TRUE ~ publicationName))

# ARTICLES IN ALL 4 JOURNALS ----------------------------------------------
ggplot(PHILOSOPHY_OF_BIOLOGY_ALL_ART, aes(x = coverDate, y = n, group = publicationName, color = publicationName)) + 
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
  geom_line(linewidth = 2, color = "darkgrey") +
  geom_smooth(linewidth = 3)  +  # You can change the theme if needed 
  facet_grid(~publicationName) +
  xlab("Year") + 
  ylab("N. Articles Published")


# REFERENCES IN ALL 4 JOURNALS --------------------------------------------
cited_ref_year <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS|> select(cited_id, )
  
  
   ggplot(PHILOSOPHY_OF_BIOLOGY_ALL_ART, aes(x = coverDate, y = n, group = publicationName, color = publicationName)) + 
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
    geom_line(linewidth = 2, color = "darkgrey") +
    geom_
  geom_smooth(linewidth = 3)  +  # You can change the theme if needed 
    facet_grid(~publicationName) +
    xlab("Year") + 
    ylab("N. Articles Published")
  
  
  

PHILOSOPHY_OF_BIOLOGY_ALL_REFS






PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> select(eid, coverDate, publicationName)
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> mutate(coverDate= as.numeric(format(coverDate, "%Y")))
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> count(coverDate)  |> filter(coverDate <= 2024)
articles_in_philo_bio = ggplot(PHILOSOPHY_OF_BIOLOGY_ALL, aes(x = coverDate, y = n)) + 
  geom_line() +
  geom_smooth() + 
  labs(title = "Papers published in Philosophy of Biology") + 
  theme_minimal()  # You can change the theme if needed


x <- patchwork::plot_layout(viz_in_bio + articles_in_philo_bio)

ggplotly(viz_in_bio)
ggplotly(articles_in_philo_bio)



# CITATION DELAY ----------------------------------------------------------
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <- read_csv("Data/Philosophy of Biology/PHILOSOPHY_OF_BIOLOGY_ALL_REFS.csv")
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> distinct()
PHILOSOPHY_OF_BIOLOGY_ALL_REFS$sourcetitle <- toupper(PHILOSOPHY_OF_BIOLOGY_ALL_REFS$sourcetitle)
refs_to_bio <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> filter(sourcetitle %in% test$published_journal)
refs_to_bio <- refs_to_bio |> select(source_eid, id, sourcetitle, coverDate) 
refs_to_bio <- refs_to_bio %>%
  mutate(coverDate= as.numeric(format(coverDate, "%Y")))
refs_to_bio <- rename(refs_to_bio, cited_year = coverDate)

PHILOSOPHY_OF_BIOLOGY_ALL <- read_csv("Data/Philosophy of Biology/PHILOSOPHY_OF_BIOLOGY_ALL.csv")
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> distinct()
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> mutate(coverDate= as.numeric(format(coverDate, "%Y")))
PHILOSOPHY_OF_BIOLOGY_ALL <- PHILOSOPHY_OF_BIOLOGY_ALL |> select(coverDate, eid, publicationName)
PHILOSOPHY_OF_BIOLOGY_ALL <- rename(PHILOSOPHY_OF_BIOLOGY_ALL, citing_year = coverDate)

complete_df  <- left_join(PHILOSOPHY_OF_BIOLOGY_ALL_REFS, PHILOSOPHY_OF_BIOLOGY_ALL, join_by(citing_id == citing_id))
complete_df <- complete_df |> mutate(delay = citing_year-cited_year)


min(complete_df$citing_year, na.rm = TRUE)
max(complete_df$citing_year, na.rm = TRUE)

complete_df$decade <- cut(complete_df$citing_year, 
                 breaks = seq(1986, 2024, by = 9), 
                 labels = paste(seq(1986, 2024 - 9, by = 9), 
                                seq(1986 + 8, 2024, by = 9), sep = "-"), 
                 include.lowest = TRUE, right = TRUE)

p2 <- ggplot(complete_df |> filter(!is.na(decade)),  aes(x = delay, group = decade, color = decade)) +
  stat_ecdf(geom = "step", show.legend = TRUE) +
  labs(title = "Citation Delay",
       x = "Delay",
       y = "CDF") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(limits = c(0, 50)) 


ggplotly(p2)



ggplot(complete_df |> filter(!is.na(decade)),  aes(x = delay, group = decade, color = decade, fill = decade))+
  geom_density(alpha =.4) + 
  facet_grid(c("publicationName", "decade"))




complete_df |> filter(publicationName == "History and Philosophy of the Life Sciences") |> distinct(citing_year)

complete_df |> select(publicationName)|> distinct()







philpapers_data_pyblio <- read_csv("Data/pybiblio/philpapers_data_pyblio.csv") |> distinct()
philpapers_data_pyblio_refs_pyblio <- read_csv("Data/pybiblio/philpapers_data_pyblio_refs_pyblio.csv") |> distinct()



clean_references_fct <- function(references) {  
  references$sourcetitle <- toupper(references$sourcetitle)
  references$citing_id <- str_extract(references$citing_eid, "(?<=2-s2\\.0-)\\d+")
  references <- rename(references, cited_id = id, cited_title = title)
  references$citing_eid <- NULL
  references <- rename(references, cited_year = publicationyear)
  return(references)
}

clean_articles_fct <- function(articles){
  articles$citing_id <- str_extract(articles$eid, "(?<=2-s2\\.0-)\\d+")
  articles$eid <- NULL
  articles <- rename(articles, citing_year = coverDate,
                     citing_category = subtypeDescription,
                     citing_title = title)
  return(articles)
}  


philpapers_data_pyblio_refs_pyblio <- clean_references_fct(philpapers_data_pyblio_refs_pyblio)
philpapers_data_pyblio <- clean_articles_fct(philpapers_data_pyblio)



library(tidyverse)
library(plotly)
library(readr)

# Biology
pybliometrics_all_bio_arxiv_filtered <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/pybliometrics_all_bio_arxiv_filtered.csv")
refs_all_bio_arxiv <- read_csv("C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/pybiblio/BIOLOGY/bioRxiv/refs_all_bio_arxiv.csv")

# Philosophy
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/REFERENCES_SPECIAL_PHILO_BIO.csv")
PHILOSOPHY_OF_BIOLOGY_ALL <- read_csv("Data/pybiblio/SPECIALIZED PHILOSOPHY OF BIOLOGY/ARTICLES_SPECIAL_PHILO_BIO.csv") |> distinct()


unique_journal_bio <- refs_all_bio_arxiv |> count(cited_journal) |> arrange(desc(n))
unique_journal_bio$cited_journal <- toupper(unique_journal_bio$cited_journal)

PHILOSOPHY_OF_BIOLOGY_ALL_REFS$sourcetitle <- toupper(PHILOSOPHY_OF_BIOLOGY_ALL_REFS$sourcetitle)
PHILOSOPHY_OF_BIOLOGY_ALL_REFS <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> mutate(in_bio = ifelse(sourcetitle %in% unique_journal_bio$cited_journal, TRUE, FALSE))


bio_by_year <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> select(in_bio, coverDate)


viz_in_bio <- ggplot(PHILOSOPHY_OF_BIOLOGY_ALL_REFS, aes(x = cited_year, y = n)) + 
  geom_line() +
  geom_smooth() + 
  labs(title = "Visibility of Biology in Philosophy of Biology") + 
  theme_minimal()  # You can change the theme if needed


# VISIBILITY OF BIOLOGY IN PHILOSOPHY OF BIOLOGY --------------------------
# Refs all bio
refs_all_bio <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS 
refs_all_bio |> filter(is.na(cited_year)) # 1, 845. 
refs_all_bio <- refs_all_bio |> select(source_eid, id, sourcetitle) 
refs_all_bio  <- left_join(refs_all_bio, PHILOSOPHY_OF_BIOLOGY_ALL, join_by(source_eid == eid))
refs_all_bio <- refs_all_bio |> distinct()
refs_all_bio <- refs_all_bio %>%
  mutate(coverDate= as.numeric(format(coverDate, "%Y")))
refs_all_bio <- refs_all_bio |> count(coverDate) |> filter(coverDate <= 2024)

# refs bio 
refs_to_bio <- PHILOSOPHY_OF_BIOLOGY_ALL_REFS |> filter(sourcetitle %in% unique_journal_bio$cited_journal)
refs_to_bio |> filter(is.na(coverDate)) # 1, 845. 
refs_to_bio <- refs_to_bio |> select(source_eid, id, sourcetitle) 
refs_to_bio  <- left_join(refs_to_bio, PHILOSOPHY_OF_BIOLOGY_ALL, join_by(source_eid == eid))
refs_to_bio <- refs_to_bio |> distinct()
refs_to_bio <- refs_to_bio %>%
  mutate(coverDate= as.numeric(format(coverDate, "%Y")))
refs_to_bio <- refs_to_bio |> count(coverDate) |> filter(coverDate <= 2024)



# Test as of 2025-03-25 ---------------------------------------------------

viz_in_bio <- ggplot(refs_to_bio, aes(x = coverDate, y = n)) + 
  geom_line() +
  geom_smooth() + 
  geom_line(data = refs_all_bio, mapping = aes(x=  coverDate, y = n)) +
  geom_smooth(data = refs_all_bio, mapping = aes(x=  coverDate, y = n), color = "red") + 
  labs(title = "Visibility of Biology in Philosophy of Biology") + 
  theme_minimal()  # You can change the theme if needed 

  