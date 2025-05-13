# ---------------------------------------------------------------------
# Title: FETCH NUMBER OF AUTHORS AND AFFILIATION FROM SCOPUS USING DOI. 
# Author: Jacob Hamel-Mottiez
# Date: 2025-01-22
# --------------------------------------------------------------------


# PACKAGES ----------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(rscopus)


# DIRECTORY AND DATA ------------------------------------------------------
dir_od <- "C:/Users/jacob/OneDrive - Université Laval/DATA/"
doi_data <- read_csv(paste0(dir_od, "overton_can_all_doi.csv"))


# API -----------------------------------------------------------
api_key <- "your_api"
set_api_key(api_key)

insttoken <- "your_insttoken"
insttoken <- inst_token_header(insttoken)


# Does it work with two DOI? Yes!
query <- rscopus::scopus_search("DOI(10.1257/aer.20161923) or DOI(10.1257/aer.20161924)", 
                                view = "COMPLETE", 
                                headers = insttoken)

raw <- gen_entries_to_df(query$entries)
papers <- raw$df 
affiliations <- raw$affiliation
authors <- raw$author

# COMMENT : 
# To keep the code clean, I deleted some tests, notably the one where I tried to pass all the DOI at once. 
# It doesn't work because, the query is too long. 
# In the same fashion, passing one DOI at the time makes you bust your query quota. 
# The reason is that one query is associated with one DOI. 
# Hence, we test in the rest of the code the possibility of grouping 
# multiple DOIs into a single query (max of 25 for "COMPLETE" data and we need this view for author count). 


# FILTER PROBLEMATIC DOI --------------------------------------------------
doi_data <- doi_data |> mutate(doi_query = paste0("DOI(", doi, ")"))
doi_data <- doi_data |>
  filter(!grepl('\\(.*[()]+.*\\)|"', doi_query)) # Filter out rows with parentheses between "DOI(" and the last ")" and also '"'. 

test <- doi_data |> filter(doi_query == 'DOI(10.1016/j.red.2019.04.004)"))')


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



# PREPARE EMPTY LISTS AND STORE DATA --------------------------------------
for(i in 1:nrow(result)){
  query <- rscopus::scopus_search(result[i, ], 
                                view = "COMPLETE", 
                                headers = insttoken)

  raw <- gen_entries_to_df(query$entries)
  papers <- raw$df |> select(`prism:doi`,`author-count.@total`, `author-count.$`)
  affiliations <- raw$affiliation
  
  all_info[[i]] <- list()
  all_info[[i]][["papers"]] <- papers
  
  # Store affiliations or "No affiliation"
  if (!is.null(affiliations)) {
    all_info[[i]][["affiliations"]] <- affiliations
  } else {
    all_info[[i]][["affiliations"]] <- "No affiliation"
  }
  
}






