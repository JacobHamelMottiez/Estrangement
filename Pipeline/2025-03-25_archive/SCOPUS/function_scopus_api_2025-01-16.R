



# PACKAGES ----------------------------------------------------------------
library(rscopus)
library(tidyverse)
library(readxl)
library(janitor)
require(httr)
require(XML)



dir_od <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/"



# API -----------------------------------------------------------
api_key <- Sys.getenv("Elsevier_api")
set_api_key(api_key)

insttoken <- Sys.getenv("Elsevier_insttoken")
insttoken <- inst_token_header(insttoken)



# JOURNALS ----------------------------------------------------------------
philo_of_bio_journals <- c(
  "\"BIOLOGY & PHILOSOPHY\"", 
  "\"BIOLOGY AND PHILOSOPHY\"",
  "\"BIOLOGICAL THEORY\"", 
  "\"PHILOSOPHICAL TRANSACTIONS OF THE ROYAL SOCIETY B: BIOLOGICAL SCIENCES\"",
  "\"STUDIES IN HISTORY AND PHILOSOPHY OF SCIENCE PART C :STUDIES IN HISTORY AND PHILOSOPHY OF BIOLOGICAL AND BIOMEDICAL SCIENCES\"",
  "\"HISTORY AND PHILOSOPHY OF THE LIFE SCIENCES\""
  )

general_philo_of_science  <-  c(
  "\"Philosophy of Science\"", 
  "\"British Journal for the Philosophy of Science\"", 
  "\"Synthese\"", 
  "\"Erkenntnis\"", 
  "\"European Journal for the Philosophy of  Science\"", 
  "\"International Studies in the Philosophy of Science\"", 
  "\"Journal for General Philosophy of Science\"", 
  "\"Foundations of Science\""
)



# DIVISION OF INFORMATION INTO DIFFERENT TABLES  ------------------------
scopus_query_fct <- function(journal) {
  
  # Scopus Query
  query <- rscopus::scopus_search(paste0("EXACTSRCTITLE(", journal, ")"), 
                                  view = "COMPLETE", 
                                  headers = insttoken)
  
  journal_raw <- gen_entries_to_df(query$entries)
  
  # Extract information. 
  journal_papers <- journal_raw$df |> janitor::clean_names() # Papers
  journal_affiliations <- journal_raw$affiliation |> janitor::clean_names() # Affiliations
  journal_authors <- journal_raw$author |> janitor::clean_names() # Authors
  
  cleaned_journal_name <- gsub(":", "_", gsub("\"", "", gsub(" ", "_", journal)))
  print(cleaned_journal_name)
  
  # Write .csv
  readr::write_csv(journal_papers, file.path(dir_od, paste0(cleaned_journal_name, "_papers_", Sys.Date(),".csv")))
  readr::write_csv(journal_affiliations, file.path(dir_od, paste0(cleaned_journal_name, "_affiliations_", Sys.Date(),".csv")))
  readr::write_csv(journal_authors, file.path(dir_od, paste0(cleaned_journal_name, "_authors_", Sys.Date(),".csv")))
  
}


all_articles_gen_philo_of_biology <- map(philo_of_bio_journals, scopus_query_fct)
all_articles_gen_philo_of_biology <- map(philo_of_bio_journals, scopus_query_fct)




# Get references
last_date_data <- Sys.Date()
dir_refs <- "C:/Users/jacob/OneDrive - Université Laval/biophilo/Data/Philosophy of Biology/"

scopus_query_references_fct  <- function(journal) {
  
  cleaned_journal_name <- gsub(":", "_", gsub("\"", "", gsub(" ", "_", journal)))
  print(cleaned_journal_name)
  
  journal_papers <- read_csv(paste0(dir_refs, cleaned_journal_name,"_papers_", last_date_data, ".csv"))
  

  citing_articles <- journal_papers$dc_identifier # extracting the IDs of our articles
  citation_list <- list()
  
  for(i in 1:length(citing_articles)){
    citations_query <- abstract_retrieval(citing_articles[i],
                                          identifier = "scopus_id",
                                          view = "REF",
                                          headers = insttoken)
    if(!is.null(citations_query$df)){ # Checking if the article has some references before collecting them
      
      nb_ref <- as.numeric(citations_query$content$`abstracts-retrieval-response`$references$`@total-references`)
      citations <- gen_entries_to_df(citations_query$content$`abstracts-retrieval-response`$references$reference)$df
      
      if(nb_ref > 40){ # The loop to collect all the references
        nb_query_left_to_do <- floor((nb_ref) / 40)
        cat("Number of requests left to do :", nb_query_left_to_do, "\n")
        for (j in 1:nb_query_left_to_do){
          cat("Request n°", j , "\n")
          citations_query <- abstract_retrieval(citing_articles[i],
                                                identifier = "scopus_id",
                                                view = "REF",
                                                startref = 40*j+1,
                                                headers = insttoken)
          citations_sup <- gen_entries_to_df(citations_query$content$`abstracts-retrieval-response`$references$reference)$df
          citations <- bind_rows(citations, citations_sup)
        }
      }
      
      citations <- citations |>
        as_tibble(.name_repair = "unique") |>
        select_if(~!all(is.na(.)))
      
      citation_list[[citing_articles[i]]] <- citations
    }
  }
  
  journal_references <- bind_rows(citation_list, .id = "citing_art") # References
  readr::write_csv(journal_references, file.path(dir_od, paste0(cleaned_journal_name,"_references_", Sys.Date(),".csv")))
}


all_references_gen_philo_of_biology <- map(philo_of_bio_journals, scopus_query_references_fct)



# NEW WAY WITH CURSOR -----------------------------------------------------
scopus_API_fct <- function(journal) {
  
  myQuery <- paste0("EXACTSRCTITLE(", journal, ")")
  cleaned_journal_name <- gsub(":", "_", gsub("\"", "", gsub(" ", "_", journal)))

  theXML <- searchByString(string = myQuery, content = "complete", outfile = paste0(cleaned_journal_name, "_papers_", Sys.Date(), ".xml"))
  theData <- extractXML(theXML) |> as_tibble()
  write_csv(theData, paste0(dir_od, cleaned_journal_name, "_papers_", Sys.Date(), ".csv"))
}


all_articles_gen_philo_of_biology <- map(general_philo_of_science, scopus_API_fct)
all_articles_gen_philo_of_biology <- map(philo_of_bio_journals, scopus_API_fct)





