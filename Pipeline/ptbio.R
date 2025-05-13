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


library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)
library(plyr)
library(plotly)

dir_od <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"

ptbio_link = "https://quod.lib.umich.edu/cgi/t/text/text-idx?c=ptb;c=ptpbio;page=issues;xc=1;g=ptpbiog"
vol_page = read_html(ptbio_link)


# vol_href = vol_page |> 
#   html_nodes("#byvolume a") |> 
#   html_attr("href")
# 
# vol_links <-  paste0("https://quod.lib.umich.edu", vol_href, sep="")




# get_art_auth =  function(vol_links){
#   art_auth_page = read_html(vol_links)
#   art_auth = art_auth_page |> 
#     html_nodes(".Z3988+ a , td:nth-child(3)") |> 
#     html_text()
# return(art_auth)
# }
# art_auth = sapply(vol_links, FUN = get_art_auth)



# REFERENCES
# Now, we want to get references.First, lets gather all the article URLs. 
vol_href = vol_page |> 
  html_nodes("#byvolume a") |> 
  html_attr("href")

vol_links <-  paste0("https://quod.lib.umich.edu/", vol_href, sep="")

## Get all the articles links
get_refs_link = function(vol_links){
  refs_page = read_html(vol_links)
  refs_href = refs_page |> 
    html_nodes(".Z3988+ a") |> 
    html_attr("href")
  
  
  return(refs_href)
}

refs_link = sapply(vol_links, FUN = get_refs_link)
refs_link_unlist = unlist(refs_link, use.names = FALSE)


# INFORMATION ABOUT ARTICLES ----------------------------------------------
## Next, fetch the references from all the articles Urls we got previously.  
get_refs =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  refs = refs_page |> html_nodes(".list-bibl") |>
    html_text()
  return(refs)
}
refs_art = sapply(refs_link_unlist, FUN = get_refs)

# Get the articles' year
get_year =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  year_art = refs_page |> html_nodes(".periodical span") |>
    html_text()
  return(year_art)
}
year_art = sapply(refs_link_unlist, FUN = get_year)
write_csv(year_art, paste0(dir_od, "year_art.csv"))

# Get the articles' abstract
get_abstract =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  abstract_art = refs_page |> html_nodes("h2+ .prelim") |>
    html_text()
  return(abstract_art)
}
abstract_art = sapply(refs_link_unlist, FUN = get_abstract)


# Get the articles' keywords
get_keywords =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  keywords_art = refs_page |> html_nodes(".subject-terms a") |>
    html_text()
  return(keywords_art)
}
keywords_art = sapply(refs_link_unlist, FUN = get_keywords)


get_author_affiliation =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  author_affiliation_art = refs_page |> html_nodes(".author") |>
    html_text()
  return(author_affiliation_art)
}
author_affiliation_art = sapply(refs_link_unlist, FUN = get_author_affiliation)



get_title =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  title_art = refs_page |> html_nodes(".title") |>
    html_text()
  return(title_art)
}
title_art = sapply(refs_link_unlist, FUN = get_title)


get_type =  function(refs_link_unlist){
  refs_page = read_html(refs_link_unlist)
  type_art = refs_page |> html_nodes("#header p") |>
    html_text()
  return(type_art)
}
type_art = sapply(refs_link_unlist, FUN = get_type)


# CREATE TIBBLE WITH DATA AND SAVE  ------------------------------------------------
library(tibble)
library(readr)

# Each into a tibble format
refs_art_tbl <- tibble(refs, article_id = names(refs))
year_art_tbl <- tibble(year_art, article_id = names(year_art))
abstract_art_tbl <- tibble(abstract_art, article_id = names(abstract_art))
keywords_art_tbl <- tibble(keywords_art, article_id = names(keywords_art))

author_affiliation_art_tbl <- tibble(
  author_affiliation_art,
  article_id = names(author_affiliation_art)
)

# Expand each element of the list into separate rows
author_affiliation_art_tbl <- author_affiliation_art_tbl %>%
  unnest(author_affiliation_art)


title_art_tbl <- tibble(title_art, article_id = names(title_art))
type_art_tbl <- tibble(
  type = type_art[1, ],
  article_id = colnames(type_art)
)


# SAVE THE TIBBLES --------------------------------------------------------
tibble_art_names <- c(
  "refs_art_tbl",
  "year_art_tbl",
  "abstract_art_tbl",
  "keywords_art_tbl",
  "author_affiliation_art_tbl",
  "title_art_tbl",
  "type_art_tbl"
)

for(i in 1:length(tibble_art_names)){
  # Use tibble_art_names[i] directly for article_id
  write_csv(get(tibble_art_names[i]), paste0(dir_od, tibble_art_names[[i]], ".csv"))
}


# LOAD THEM BACK ----------------------------------------------------------
# Initialize an empty list to store each loaded tibble
data_list <- list()

# Loop through each name, load the corresponding CSV, and store it in the list
for(i in seq_along(tibble_art_names)){
  data_list[[tibble_art_names[i]]] <- read_csv(paste0(dir_od, tibble_art_names[i], ".csv"))
}

list2env(data_list, envir = .GlobalEnv)


# MERGE ARTICLES AND REFS DATA --------------------------------------------
full_table <- join_all(list(author_affiliation_art_tbl,
                            year_art_tbl,
                            title_art_tbl,
                            refs_art_tbl,
                            type_art_tbl), by='article_id', type='left')


# Use separate_rows with regex to split on pattern (period followed by capital letter)
full_table_split <- full_table %>%
  separate_rows(refs, sep = "\\.(?=[A-Z])")



full_table_split <- full_table_split |> 
  mutate(cited_year = str_extract(refs, "\\b\\d{4}[a-zA-Z]?\\b")) |> # to handle e.g. 2006a
  mutate(cited_year =  str_replace(cited_year, "[a-zA-Z]$", "")) # to remove the letter. 

full_table_split <- full_table_split |> filter(!is.na(cited_year)) # absent cited year. Problematic regex.



# SAVE FINAL TABLE --------------------------------------------------------
write_csv(full_table_split, paste0(dir_od, "ptbio.csv"))
full_table_split <- read_csv(paste0(dir_od, "ptbio.csv"))