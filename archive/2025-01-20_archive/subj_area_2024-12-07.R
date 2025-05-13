# PACKAGES ----------------------------------------------------------------
library(rscopus)
library(tidyverse)
library(readxl)
library(janitor)


# DIRECTORY AND DATA ------------------------------------------------------
dir_od <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"
journals_scopus <- readxl::read_excel(paste0(dir_od, "ext_list_October_2024.xlsx")) |> clean_names() # 46,534 journals
journals_bp <- read_csv(paste0(dir_od, "cited_journals_bp_2024-12-07.csv")) # 10,147 journals


# MATCH JOURNALS IN B&P AND WHOLE SCOPUS DATABASE -------------------------
columns_starting_with_x <- grep("^x", names(journals_scopus), value = TRUE)

journal_title <- journals_scopus |> select(source_title, all_of(columns_starting_with_x))
journal_title$source_title <- toupper(journal_title$source_title)
journal_title <- journal_title |> rename(sourcetitle = source_title)

journal_join <- left_join(journals_bp, journal_title, by = "sourcetitle")

journal_specialities <- journal_join |> select(-n, -percent, - sourcetitle) |>
  summarise(across(everything(), ~sum(!is.na(.)))) |>
  pivot_longer(cols = everything(), names_to = "column", values_to = "count") |> 
  arrange(desc(count)) |>
  mutate(rank = rank(-count)) 

journal_specialities <- journal_specialities |> mutate(column = str_sub(column, start = 7)) 
journal_specialities |> print(n = 27)


# IS A JOURNAL ASSOCIATED WITH MORE THAN ONE DISCIPLINE? ------------------
journal_join_percent <- journal_join |> select(-sourcetitle, -n, -percent) 
row_counts <- rowSums(!is.na(journal_join_percent))

journal_join_percent <-  journal_join_percent |> mutate(row_sum = row_counts)
journal_join_percent |> select(row_sum) |> arrange(desc(row_sum)) |> unique() # This is maybe problematic. 

percent_df <- tibble(row_counts) |> count(row_counts) 
percent_df |> mutate(percent = (n/nrow(journal_join_percent))*100)




# VERSION LOUIS -----------------------------------------------------------
js_l <- journals_scopus |> 
  unite("d", starts_with("x"), na.rm = TRUE) |> 
  select(source_title, d) |> 
  separate_longer_delim(cols = d, delim = "_") 
  #filter(d != "")

# count(source_title, sort = T) 

js_l |> count(n) |> 
  mutate(nb_j = sum(nn)) # 46282

journals_scopus |> nrow() # 46534
journals_scopus |> distinct(source_title) |> nrow() # 46378

list_d <- journals_scopus |> 
  unite("d", starts_with("x"), na.rm = TRUE) |> 
  select(source_title, d) |> 
  separate_longer_delim(cols = d, delim = "_") |> 
  distinct(d)


# DUPLICATA TITRES --------------------------------------------------------
dt <- journals_scopus |> add_count(source_title) |>
  filter(n>1) 

# Certains se dupliquent, mais très peu. Pour notre travail, 
# nous ignorons ce cas de figure dans notre traitement des données.
j_hp <- js_l |> 
  filter(d == "Health Professions")

dir_od <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"
bio_philo_references <- read_csv(paste0(dir_od, "bio_philo_references.csv"))


test <- bio_philo_references |> 
  left_join(js_l, by = join_by(sourcetitle == source_title)) |>
  count(d)
















dir_od <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"
bio_th_references <- read_csv(paste0(dir_od, "bio_th_references.csv"))

bio_th_references



